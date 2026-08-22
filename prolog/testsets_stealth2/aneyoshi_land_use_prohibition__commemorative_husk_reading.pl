% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Prohibition — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   After the 1933 Sanriku tsunami, survivors carved stones along the coast
 *   marking the height the water reached, with injunctions not to build homes
 *   below. In the account this file instantiates, the injunction held for a
 *   generation and then lost force: engineered seawalls built after the 1960
 *   Chilean tsunami fostered confidence that the sea was handled; coastal
 *   flatland was the only buildable land; generational turnover severed the
 *   living memory that gave the carving its authority. Municipal permits for
 *   subdivisions below the marked elevations became routine. The stones
 *   remained — cleaned, visited on anniversaries, photographed by school
 *   groups — as monuments to a danger the community had stopped acting on. In
 *   March 2011 the water returned at or above the carved lines and the
 *   below-line settlements were destroyed; the deferred cost of the
 *   arrangement arrived entire, and the marked heights proved accurate. KEY
 *   AGENTS (by structural relationship): - coastal_property_developers:
 *   Primary beneficiary (organized/arbitrage) — converts the protected
 *   flatland into salable inventory and exits before the tail risk lands -
 *   mortgage_lenders_insurers: Secondary beneficiary
 *   (institutional/arbitrage) — collects interest and premiums on below-line
 *   collateral; catastrophe losses pass to public reconstruction -
 *   municipal_planning_authorities: Agenda setter (institutional/constrained)
 *   — permits below-line subdivision and tends the stone ceremonially -
 *   below_line_residents: Primary target (moderate/trapped) — bears the
 *   inundation losses when the wave returns -
 *   returnee_descendants_of_high_ground_hamlets: Target
 *   (moderate/identity_locked) — descendants of the 1933 evacuees who drifted
 *   back below the line as witness memory faded - memorial_rite_associations:
 *   Ceremonial steward (moderate/identity_locked) — performs the remembrance
 *   that substitutes for enforcement - disaster_prevention_engineers:
 *   Excluded voice (organized/analytical) — warned against below-line
 *   building and were not seated in permitting deliberations -
 *   prefectural_disaster_council: Analytical observer
 *   (institutional/analytical) — reviews siting and hazard policy from
 *   outside the transaction
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.8).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.08).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.12).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Tsunami Prohibition — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, '952508d7-9c9d-489b-8797-b939d418a03d').
narrative_ontology:cs_kernel_codification('952508d7-9c9d-489b-8797-b939d418a03d', fixed_text).
narrative_ontology:cs_authority_grounding('952508d7-9c9d-489b-8797-b939d418a03d', lineage).
narrative_ontology:cs_interpretation_layer_present('952508d7-9c9d-489b-8797-b939d418a03d').
narrative_ontology:cs_reading_relation('952508d7-9c9d-489b-8797-b939d418a03d', aneyoshi_land_use_prohibition__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('952508d7-9c9d-489b-8797-b939d418a03d', foundational, inscription_binds_only_witness_generation).
narrative_ontology:cs_axiom_status(inscription_binds_only_witness_generation, holdable).
narrative_ontology:cs_axiom_grounding('952508d7-9c9d-489b-8797-b939d418a03d', inscription_binds_only_witness_generation, conventional).
narrative_ontology:cs_axiom('952508d7-9c9d-489b-8797-b939d418a03d', secondary, durable_protection_requires_living_institution).
narrative_ontology:cs_axiom_status(durable_protection_requires_living_institution, holdable).
narrative_ontology:cs_axiom_grounding('952508d7-9c9d-489b-8797-b939d418a03d', durable_protection_requires_living_institution, instrumental).
narrative_ontology:cs_reference_frame('952508d7-9c9d-489b-8797-b939d418a03d', witness_bound_advisory_monument).
narrative_ontology:cs_drift_state('952508d7-9c9d-489b-8797-b939d418a03d', post_2011_runup_survey_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('952508d7-9c9d-489b-8797-b939d418a03d', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_property_developers).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, mortgage_lenders_insurers).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_planning_authorities).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, memorial_rite_associations).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_residents).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, returnee_descendants_of_high_ground_hamlets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assemble and subdivide parcels on the coastal flats below the carved elevation marks, sell finished lots and units, and book the margin. Project horizons run five to ten years; the hazard the stones mark operates on multi-decade recurrence. By the time a major wave arrives, the inventory is sold and the firms have moved to the next corridor. Leaving the trade means giving up the only large flat parcels on the ria coast.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_property_developers, beneficiary,
    organized, biographical, arbitrage, regional).

% Write the mortgages and fire-and-quake policies on below-line properties, collecting interest and premium income on collateral whose worst-case loss is correlated with a single geological event. Catastrophe losses on that scale route to public reconstruction programs and mutual-aid pools rather than staying on the originating books. Withdrawal from the coastal lending market is a portfolio decision available at any quarter-end.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, mortgage_lenders_insurers, beneficiary,
    institutional, biographical, arbitrage, national).

% Administer zoning and issue the building permits that determine what rises below the carved lines, and separately fund the cleaning and ceremonial upkeep of the stones as designated cultural property. Approving below-line subdivision widens the rateable base and satisfies housing demand; refusing it invites compensation claims, legal challenge, and accusations of blocking recovery. Officials serve fixed terms; the recurrence interval of the hazard outlasts every administration.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_planning_authorities, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_planning_authorities, beneficiary).

% Live in homes on the flats, many employed by the ports and fish processors that anchor the local economy. Their houses are their principal assets, priced as safe once the seawalls went in; selling means realizing a discount no inland buyer will waive. The cared-for stones at the edge of town read to newcomers as evidence the danger is remembered and handled. When the water comes, it comes to them first and highest.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_residents, payer,
    moderate, biographical, trapped, local).

% Grandchildren of the families who climbed above the marked line in the 1930s, raised on stories of the evacuation, who have moved back down over the decades to be near the harbor, the schools, and the family graves. Their connection to the high-ground hamlet is constitutive — leaving it again would sever the lineage continuity their grandparents preserved at the cost of livelihood. Each return was a small, individually reasonable decision; collectively they rebuilt the exposed settlement.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, returnee_descendants_of_high_ground_hamlets, payer,
    moderate, generational, identity_locked, local).

% Local elders and parish volunteers who organize the annual reading of the stone inscriptions, maintain the moss and lichen, and escort school groups. Their standing in the community rests on stewardship of the stones; the rite calendar is theirs to keep. They hold no seat in permitting deliberations and issue no rulings on where building may occur — the office they occupy begins and ends with remembrance.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, memorial_rite_associations, beneficiary,
    moderate, generational, identity_locked, local).

% Regional seismologists and civil engineers who surveyed the old runup marks, published recurrence estimates, and petitioned planning committees to treat the carved elevations as binding setbacks. Their submissions were acknowledged and filed; none was seated where permits were decided. Their professional standing depends on being right over long horizons, and their only lever is publication.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_prevention_engineers, excluded,
    organized, biographical, analytical, national).

% Prefectural body that reviews municipal hazard plans, commissions inundation mapping, and issues advisory guidance on coastal siting. It takes testimony from engineers and municipalities, publishes risk assessments, and holds recommendation power only — enforcement instruments remain with the municipalities it advises.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, prefectural_disaster_council, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_property_developers).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: What the arrangement still coordinates is communal remembrance: a shared annual rite, a common site for disaster education, a fixed reference point in local identity narratives. The coordination problem it was built to solve — settling the question of where building may safely occur, once, for everyone — is not currently performed by it.
% TRANSFER_FUNCTION: Moves present-day land value, construction revenue, interest and premium income, and municipal tax base toward developers, lenders, and municipal coffers; the balancing entry is uncompensated inundation risk carried by below-line households, realized in full when runup reaches or exceeds the carved elevations.
% ABSENT_VOICES: The regional seismologists and disaster-prevention engineers who petitioned for binding setbacks were never seated in permitting deliberations. Nor, in any formal sense, are the 1933 dead whose testimony the inscription carries — the record of every below-line permit approval contains no voice speaking for the warning's authors.
% DISAPPEARANCE_RATIONALE: If the stones vanished overnight, the cover function disappears with them: the municipality could no longer point to a tended memorial as evidence the danger is honored, forcing explicit zoning decisions on every below-line parcel; the rite calendar loses its object and the heritage-tourism framing collapses; developers lose the heritage ambience that prices their lots. The arrangements of at least five named seats depend on the stone's continued presence as honored symbol.
% FOUNDING_PROBLEM: After the 1933 Sanriku tsunami killed roughly three thousand people, survivors who had climbed to safety erected stones marking the water's height with the instruction not to build homes below it — a permanent settlement boundary drawn at the measured reach of a hazard that recurs on decadal-to-centennial timescales.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the 2011 runup surveys conducted by university teams and the government's Reconstruction Agency found inundation at or above the carved elevations on the segments where below-line building occurred, confirming both the original markers' accuracy and the founding problem's persistence. Sediment-layer studies of prior Sanriku tsunamis and the instrumental seismic record independently attest the recurrence the founders encoded. No party disputes that the hazard is live; what the development-side seats dispute is whether the carved instruction still obligates anyone.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.80 at interval end) because the standing arrangement transfers the entire cost of a foreseeable catastrophe onto households with no compensating payment: the land-value gains are privatized in the present and the inundation losses are socialized onto the below-line settlements when the recurrence interval closes. Suppression is authored very low (0.08) because nothing actively holds anyone in place — the reading's defining fact is the absence of enforcement force; what remains is informational (newcomers read a cared-for monument and infer the danger is managed) rather than coercive. Theater ratio is authored high (0.82) because nearly all observable activity around the stone is performative — rites, plaques, school visits — while the activity that once constituted the arrangement (refusing below-line permits) has ceased. Accessibility collapse is low (0.12): nothing closes off; building below the line is easy and unopposed, which is precisely the husk signature. Resistance is low (0.18): scattered professional warnings and a few households that refused to move down, meeting indifference rather than reprisal. The suppression_requirement series is the story's spine: it tracks the decay of the arrangement's active force from full community enforcement (0.75) to near zero (0.08), which is exactly the enforcement-capacity trajectory this reading asserts. All three series share one seven-point grid so no metric is sampled against another metric's end-state. The claim/metric independence rule is exercised deliberately: the claim is piton (degraded former coordination rule, persisting by inertia and ceremony) while the metrics describe heavy accumulated extraction — the divergence between what the arrangement is and what it now costs is the datum, not an error to be reconciled.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because the same inert object is four different things depending on position. From the developer seat the stone is ambience — heritage charm that raises lot prices on land the carving nominally forbids selling. From the lender seat it is irrelevant: collateral is collateral until the wave arrives, and the wave arrives on someone else's balance sheet. From the municipal seat the stone is costless heritage that blunts demands for expensive seawall extensions — a monument doing the public-relations work of a defense budget. From the below-line household's seat it is a false guarantee: the cared-for surface of the stone signals that the danger behind it is managed, while the household's exit is blocked by illiquid property, port livelihoods, and grave-bound kinship. The engineer seat sees a violated covenant. Nothing in the authored claim adjudicates among these; the engine derives them from power, exit, and role.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and lenders sit at the beneficiary pole (d near 0.0): they receive the arrangement's gains and their arbitrage-grade exit means the tail risk departs with them. The municipal authority derives mid-to-low: it administers the arrangement and collects the tax base, but it also absorbs post-catastrophe political and fiscal backlash, pulling it off the pure beneficiary pole. Memorial rite associations derive mildly beneficiary: they collect continuity and standing from the stone's persistence without touching land-use outcomes. Below-line residents and returnee descendants sit at the target pole (d near 1.0): trapped and identity-locked respectively, they carry the full deferred cost. Engineers and the prefectural council are analytical seats — they observe and testify but collect from and pay into nothing, so effective extraction is not meaningfully computed for them. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce these positions without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement's mandate — keep settlement off the inundation flats — has outlived its own operation: the form (carved injunction, annual rite, heritage listing) persists while the function (binding siting decisions) is gone, which is the mandatrophy condition, declared resolved here. The classification guards against two symmetrical mislabelings. Calling the husk a rope (a live warning system) erases the victim structure: a warning nobody acts on protects nobody, and the 2011 losses fell exactly as the carveings predicted they would if ignored. Calling it a snare (active predation) overstates the machinery: no coercion maintains the husk, no enforcer collects through it, and its persistence is inertia plus ceremony rather than enforcement. The piton reading holds both facts: the arrangement is mostly performance, and the extraction riding on it is parasitic — development interests profit from the void the husk leaves, not from operating the rule. The cost-asymmetry completes the picture: the municipal authority could restore binding setbacks tomorrow, but the cost to it (compensation liability, legal challenge, lost rateable land) exceeds what it bears, while the households who bear everything hold no lever. Coalition formation among the targets is structurally weak: each household's stay-or-move decision is private, and the losses that would motivate collective action arrive simultaneously with the catastrophe that makes action moot.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_enforcement_record,
    'This constraint instantiates the commemorative_husk_reading of kernel aneyoshi_land_use_prohibition: the carved prohibition decayed to symbol with no behavioral force across the interval. The sibling reading (behavioral_competence_reading) asserts the opposite — operational enforcement across the same 78 years. Which description does the enforcement record support?',
    'Archival reconstruction: municipal permit logs, subdivision approvals, and settlement-height censuses for the hamlet and adjacent coastline from 1933 to 2011, cross-checked against the carved runup elevations.',
    'If the sibling reading is correct, epsilon collapses toward coordination-cost levels and the computed type shifts toward rope or tangled_rope; if this reading is correct, the piton profile with accumulating extraction stands. The two readings cannot both describe the same record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_enforcement_record, empirical, 'Committer-frame omega: which reading of the stone''s prohibition the 1933-2011 enforcement record actually supports.').

omega_variable(
    passive_decay_vs_arranged_nonenforcement,
    'Did the prohibition lose force through passive atrophy (generational forgetting, seawall confidence, economic gravity), or was its non-enforcement actively arranged — zoning decisions taken with the carved line known and set aside?',
    'Planning-committee minutes, petition records from disaster-prevention engineers, and the sequencing of subdivision approvals relative to documented warnings.',
    'Active arrangement would concentrate agency in the municipal seat and pull the computed classification toward snare or tangled_rope; passive decay supports the piton reading with exploitation of the void by development interests rather than maintenance of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_decay_vs_arranged_nonenforcement, empirical, 'Whether the husk state was neglected into existence or deliberately produced.').

omega_variable(
    false_security_contribution,
    'How much below-line settlement is attributable to the stone''s continuing presence as a visible token of remembered danger (inducing false security), as opposed to the independent economic pull of coastal flatland?',
    'Comparative settlement-growth analysis between coastline segments with prominent warning stones and demographically similar segments without them, controlling for seawall construction dates.',
    'If the token itself lured settlement, a share of the measured extraction is caused by the husk''s operation and the victim structure deepens; if settlement followed flatland economics regardless, the husk accompanied rather than produced the exposure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_security_contribution, empirical, 'Size of the moral-hazard contribution of the visible-but-unenforced memorial.').

omega_variable(
    intertemporal_victim_seat,
    'Who occupies the victim seat — the households destroyed when the wave returned (retrospective identification) or the prospective settlers at each moment they chose to move below the line (prospective identification)?',
    'Conceptual choice fixed by the analyst; sensitivity test reruns directionality derivation under each identification.',
    'Retrospective identification concentrates directionality on the deceased and their estates; prospective identification spreads it across every below-line move during the interval and raises effective extraction at earlier time points.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intertemporal_victim_seat, conceptual, 'Framing dependence of the victim seat across the intertemporal transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(aneyoshi_husk_tr_t13, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 13, 0.27).
narrative_ontology:measurement(aneyoshi_husk_tr_t26, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 26, 0.38).
narrative_ontology:measurement(aneyoshi_husk_tr_t39, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 39, 0.5).
narrative_ontology:measurement(aneyoshi_husk_tr_t52, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 52, 0.61).
narrative_ontology:measurement(aneyoshi_husk_tr_t65, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 65, 0.72).
narrative_ontology:measurement(aneyoshi_husk_tr_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 78, 0.82).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(aneyoshi_husk_be_t13, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 13, 0.24).
narrative_ontology:measurement(aneyoshi_husk_be_t26, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 26, 0.36).
narrative_ontology:measurement(aneyoshi_husk_be_t39, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 39, 0.49).
narrative_ontology:measurement(aneyoshi_husk_be_t52, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 52, 0.6).
narrative_ontology:measurement(aneyoshi_husk_be_t65, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 65, 0.71).
narrative_ontology:measurement(aneyoshi_husk_be_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 78, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(aneyoshi_husk_su_t13, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 13, 0.6).
narrative_ontology:measurement(aneyoshi_husk_su_t26, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 26, 0.46).
narrative_ontology:measurement(aneyoshi_husk_su_t39, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 39, 0.33).
narrative_ontology:measurement(aneyoshi_husk_su_t52, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 52, 0.22).
narrative_ontology:measurement(aneyoshi_husk_su_t65, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 65, 0.14).
narrative_ontology:measurement(aneyoshi_husk_su_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 78, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, sanriku_tsunami_stone_corpus).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the natural-language label 'the Aneyoshi stone prohibition' conflates two structurally distinct claims about one carved text. This file instantiates the commemorative_husk_reading (symbol without behavioral force; epsilon high, referent = the standing arrangement of nominal prohibition plus actual below-line development). The sibling file instantiates the behavioral_competence_reading (operationally enforced land-use rule; epsilon near coordination cost). The readings disagree about the same 78-year record and cannot both describe it; they are linked here so contamination and drift analysis sees the family. The upstream corpus node (sanriku_tsunami_stone_corpus) feeds both readings, since each cites the same body of stones as evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
