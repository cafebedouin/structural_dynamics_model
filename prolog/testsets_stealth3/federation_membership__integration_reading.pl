% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Free Movement as Constitutional Right (Federation Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   A continental federation binds its member states into a single labor
 *   market: nationals of any member state may reside, seek work, and settle
 *   in any other on terms the treaties declare unconditional, and a
 *   supranational court strikes down national measures — registration
 *   schemes, quota proposals, discriminatory hiring rules — that would
 *   re-segment it. This story instantiates the integration_reading of the
 *   contested federation-membership kernel: within this reading, membership
 *   is irreversible integration, supranational adjudication is the legitimate
 *   arbiter, and free movement is a constitutional entitlement rather than a
 *   negotiable policy line. The epsilon referent is that standing arrangement
 *   itself, as this reading assesses it: an endorsed constitutional
 *   settlement whose operation nonetheless concentrates gains on mobile
 *   workers and the firms that recruit them, while displacing costs onto
 *   immobile residents of high-inflow regions and onto the depopulating
 *   regions exporting their young — populations to whom this very reading
 *   offers no legitimate remedy, since border restriction is precisely what
 *   it rules out of bounds. The sibling reading
 *   (federation_membership__sovereignty_reading) is a separate constraint
 *   file, not a variant of this one; the two are linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - federation_supranational_authority: agenda-setter (institutional/identity_locked) — adjudicates and enforces the movement-rights regime
 *   - mobile_citizens: primary beneficiary (moderate/arbitrage) — capture wage, opportunity, and portability gains
 *   - transnational_employers: primary beneficiary (institutional/arbitrage) — arbitrage continental wage gradients
 *   - destination_regional_employers: secondary beneficiary (organized/mobile) — draw on enlarged labor pools in receiving regions
 *   - gateway_low_wage_workers: primary target (powerless/trapped) — bear displacement costs at the bottom of receiving labor markets
 *   - left_behind_sending_regions: target with offsetting benefit (organized/constrained) — lose prime-age population, recoup remittances
 *   - member_state_governments: dual-positioned payer/beneficiary (powerful/constrained) — gain market access, lose border discretion
 *   - border_control_advocacy_movements: excluded voice (organized/trapped) — demand defined as illegitimate by the reading itself
 *   - federal_mobility_economists: analytical observer (analytical/analytical) — publish the evidence base all seats argue from
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership__integration_reading, 0.66).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Free Movement as Constitutional Right (Federation Integration Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '5fe41249-f08b-475f-89f0-31a60d7a2071').
narrative_ontology:cs_kernel_codification('5fe41249-f08b-475f-89f0-31a60d7a2071', fixed_text).
narrative_ontology:cs_authority_grounding('5fe41249-f08b-475f-89f0-31a60d7a2071', lineage).
narrative_ontology:cs_interpretation_layer_present('5fe41249-f08b-475f-89f0-31a60d7a2071').
narrative_ontology:cs_reading_relation('5fe41249-f08b-475f-89f0-31a60d7a2071', federation_membership__sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('5fe41249-f08b-475f-89f0-31a60d7a2071', foundational, free_movement_constitutional_entitlement).
narrative_ontology:cs_axiom_status(free_movement_constitutional_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('5fe41249-f08b-475f-89f0-31a60d7a2071', free_movement_constitutional_entitlement, deontological).
narrative_ontology:cs_axiom('5fe41249-f08b-475f-89f0-31a60d7a2071', foundational, supranational_adjudication_supreme).
narrative_ontology:cs_axiom_status(supranational_adjudication_supreme, holdable).
narrative_ontology:cs_axiom_grounding('5fe41249-f08b-475f-89f0-31a60d7a2071', supranational_adjudication_supreme, conventional).
narrative_ontology:cs_reference_frame('5fe41249-f08b-475f-89f0-31a60d7a2071', ever_closer_union_constitutional_integration).
narrative_ontology:cs_drift_state('5fe41249-f08b-475f-89f0-31a60d7a2071', post_brexit_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5fe41249-f08b-475f-89f0-31a60d7a2071', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, transnational_employers).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, destination_regional_employers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, gateway_low_wage_workers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, left_behind_sending_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, left_behind_sending_regions).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, member_state_governments).
narrative_ontology:constraint_victim(federation_membership__integration_reading, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens of member states who take up residence and employment in other member states. They gain access to higher wages, wider job markets, portable pensions, and credential recognition, and can re-optimize their location whenever conditions shift elsewhere. Moving again is cheap relative to staying, and their rights travel with them.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    moderate, biographical, arbitrage, continental).

% Firms operating staffing, logistics, construction, agriculture, and care networks across many member states. They recruit wherever wages and skills are cheapest, fill shortages anywhere in the network, and adjust headcount by country without negotiating with any single national labor system. Relocating operations or shifting sourcing between countries is routine for them.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, transnational_employers, beneficiary,
    institutional, generational, arbitrage, global).

% Employers in regions receiving inflows — farms, warehouses, care homes, food-processing plants, hospitality chains. They draw on a labor pool larger and more flexible than the local workforce alone would supply, at wage levels set by continental rather than local scarcity. Some operate on margins that assume continued access to incoming labor, and the least rooted among them can relocate production if local conditions turn hostile.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, destination_regional_employers, beneficiary,
    organized, biographical, mobile, regional).

% Residents of high-inflow regions employed in manual and service occupations. They compete for shifts, hours, and housing with newly arrived workers willing to accept lower pay or harder conditions; their bargaining position rests on local ties — family, housing tenure, children's schools — that moving would forfeit. Wage growth in their occupations has lagged the regional average for years, and their main lever is the ballot box of a member state whose hands the treaties tie.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, gateway_low_wage_workers, payer,
    powerless, biographical, trapped, regional).

% Communities in labor-exporting regions — rural districts, post-industrial towns, peripheral member states — whose working-age population drains toward the center. Households receive remittances that sustain consumption and local services, but villages lose teachers, nurses, and young families; schools close and median age rises year over year. Remaining in the arrangement keeps the income channel open while continuing the outflow; leaving it forfeits the income outright.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, left_behind_sending_regions, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, left_behind_sending_regions, beneficiary).

% The federation's executive and judicial organs. They administer movement rights, bring infringement proceedings against member states that restrict them, and adjudicate disputes over worker registration, posting, and social-security coordination. The organs' mandate, budget lines, and doctrinal identity are bound up with the integration project they police; stepping back from enforcement would call their own reason-for-being into question.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, federation_supranational_authority, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% National governments that ratified the movement provisions and remain bound by them. They gain access to a continental market and to inward labor where they need it, and they lose the ability to screen or cap inflows; attempts to reintroduce permits or registration draw infringement proceedings and adverse court rulings. Withdrawal from the provisions is legally available but carries severe economic and diplomatic cost, as the one completed exit demonstrated.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, member_state_governments, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, member_state_governments, beneficiary).

% Parties and campaigns whose central platform is restoring national discretion over entry, residence, and employment of foreign nationals. They win elections and referendum votes but hold no seat in the forums where movement law is drafted or adjudicated; their signature policy is treated as legally unavailable short of full withdrawal from the treaties, which their voters punish at the next cycle.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, border_control_advocacy_movements, excluded,
    organized, biographical, trapped, national).

% Academic and statistical researchers studying intra-federation migration flows, wage effects, fiscal balances, and regional convergence. They publish the evidence base all the other seats argue from and hold no stake in the arrangement's continuation or repeal beyond citation.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, federal_mobility_economists, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__integration_reading, diffuse).
narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the allocation problem of a segmented continental economy: labor and skills flow from surplus regions to shortage regions, credentials are recognized once across the whole territory, and the transaction costs of national labor-market boundaries disappear for twenty-plus formerly separate markets solved as one.
% TRANSFER_FUNCTION: Moves labor — and the spending, taxes, and care work attached to it — from labor-abundant lower-wage regions to labor-scarce higher-wage regions. The resulting output gains flow disproportionately to mobile workers, recruiting firms, and destination employers, while the adjustment costs concentrate on immobile residents of both the receiving gateways and the depopulating origins.
% ABSENT_VOICES: Gateway-region residents and depopulating sending regions absorb the arrangement's costs but sat outside the conversation that constitutionalized it — the treaties were negotiated and ratified by governments, without the specific communities that would later receive the flows. Border-control advocates are excluded by the reading itself, which defines their central demand as illegitimate rather than merely wrong; they would argue for re-nationalized movement discretion and are structurally kept out of the drafting and adjudicating forums.
% DISAPPEARANCE_RATIONALE: If the movement-rights regime vanished overnight, member states would reimpose work-permit and registration schemes within weeks; wage floors in gateway occupations would firm as supply competition eased; sending regions would absorb sudden return migration while losing the remittance channel; employers built on cross-border staffing would reorganize procurement at cost; and the supranational court's movement caseload would empty, shrinking the enforcement machinery's raison d'être.
% FOUNDING_PROBLEM: Post-war Europe's segmented national labor markets: chronic shortages in industrializing regions alongside rural and post-industrial underemployment, guest-worker schemes improvised and revoked arbitrarily, credentials worthless across borders, and the recurring strategic failure of economies unable to redeploy people during reconstruction, oil shocks, and recessions.
% FOUNDING_PROBLEM_CORROBORATION: The underlying allocation imbalance is independently attested: regional-convergence research on cohesion data, OECD and IMF analyses of intra-federation mobility and demographic asymmetry (aging west and north, young south and east), and sending-country government statements about emigration and rural decline all corroborate that the founding problem persists. Attestation does not rest solely on the supranational executive or on mobility-benefiting employers.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68) because gains and costs land asymmetrically: wage premia and staffing flexibility accrue to mobile factors, while displacement costs concentrate on immobile residents who cannot price themselves out or move without forfeiting non-wage assets. Suppression (0.66) is structural, not personal: no individual is coerced directly; what is suppressed is the policy space of member states — registration schemes, caps, and permit regimes struck down by infringement proceedings and adverse rulings, with the reading rendering border restriction categorically illegitimate. Suppression scales nothing and is scaled by nothing; only extractiveness passes through directionality and scope in the engine's arithmetic. Theater (0.30) reflects a majority-functional operation with growing symbolic overhead: solidarity consultations, mobility scorecards, and fund announcements that bind no one. Accessibility_collapse (0.58) is partial: emergency brakes, transitional registration moratoria for new entrants, and the demonstrated possibility of full withdrawal leave real but expensive alternatives; the cheap alternatives are the ones foreclosed. Resistance (0.62) is sustained and occasionally decisive — referendum-scale reversals, quota defiance, one completed exit — far above what a settled norm meets. The measurement series share one eight-point grid (1957–2024) so every metric is authored at every examined time point. The suppression_requirement series is included deliberately: the story specifically traces enforcement-capacity buildup, from paper rights to an active infringement-and-adjudication machinery, which is the dynamic that series exists to capture; the flat-scalar alternative would hide the ratchet. Extraction steps sharply at the 2004 accession round, peaks near 2016 amid crisis-era strains, and eases slightly by 2024 as sending-region wages converge. On the receipt surface: each named seat was checked for capture — mobile workers collect wage premia, employers collect staffing margins, sending-region households collect remittance offsets — and no single seat captures the extraction, so gain_flow is authored 'diffuse' as an affirmative checked finding, not a default. Fixing is 'prohibitive': a unilateral fixer exposes itself to infringement action and market disruption, and collective restructuring requires unanimous consent across states whose interests diverge, costing more than any single seat would gain from the fix.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats the same rules read as liberation: a right that travels, a continent-sized job market, a staffing network unbounded by national borders. From the trapped gateway seat the identical rules read as a ceiling: competition one cannot out-vote, administered by courts no local election reaches. The member_state_governments seat computes from both sides at once — market access gained, discretion lost — which is why its dual role matters more than its raw power rating. The authority seat is identity-locked in the institutional sense: its self-concept is constituted by the integration project it polices, so its 'exit' is not a policy choice but the dissolution of its own mandate; if that frame broke, enforcement posture would soften immediately and the arrangement would drift toward negotiated management. Gateway workers' latent coalition power is structurally diffused: the affected population scatters across dozens of regions in different states, speaking different electoral languages, so the paying class lacks the geographic concentration coalition formation requires. Border-control advocates are excluded rather than merely opposed — the reading itself defines their central demand as illegitimate, which is exactly what their excluded role encodes.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations drive the derivation: mobile_citizens, transnational_employers, and destination_regional_employers sit in beneficiaries with arbitrage-grade or mobile exit, placing them near the subsidy end of the index; gateway_low_wage_workers sit in victims with trapped exit (moving forfeits family, tenure, and community), pushing them to the full-target end; left_behind_sending_regions carry payer plus secondary beneficiary (remittances), tempering their derived directionality toward mid-range; member_state_governments carry payer plus secondary beneficiary with constrained exit, landing near symmetric and slightly target-side. No directionality_overrides are authored: every seat's structural relationship to this specific arrangement is already expressed by its beneficiary/victim declaration combined with its exit atom, so the derivation chain produces accurate values without correction. The authority's own directionality derives from its administrative position — it collects legitimacy and mission rather than revenue — and sits near the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — segmented national labor markets stranding workers amid regional shortages — remains live: continental wage and demographic gradients persist, so the arrangement is not running on a dead mandate and mandatrophy is not declared. The dual declaration disciplines both misclassification modes: declaring beneficiaries alone would license calling this pure coordination, while the victims-plus-enforcement declaration blocks the reverse error of reading a functioning credential-recognition and vacancy-matching machine as pure extraction. Rising theater (0.10 to a 0.32 peak) marks growing symbolic-solidarity overhead, but the functional share remains majority, so the structure is a maintained hybrid rather than an inertial shell; the slight 2024 easing tracks genuine narrowing of east-west wage gradients rather than decay of function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accession_refresh_dynamics,
    'Does each accession round permanently absorb its displacement differential through wage convergence, or does enlargement continually refresh the gradient that generates displacement costs?',
    'Cohort analysis of post-accession wage-gap decay rates by entry wave (1973, 1986, 1995, 2004, 2007, 2013 cohorts) compared against pre-accession differentials and moratorium windows.',
    'If convergence absorbs each wave within a decade or two, effective extraction trends toward the coordination floor and the arrangement drifts rope-ward; if successive enlargements perpetually restore steep gradients, extraction accumulates and the enforcement burden grows with it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accession_refresh_dynamics, empirical, 'Whether wage convergence neutralizes displacement or enlargement refreshes it.').

omega_variable(
    displacement_attribution_confound,
    'What share of gateway-region wage stagnation, housing pressure, and service strain is causally attributable to intra-federation labor mobility rather than to automation, import competition, fiscal austerity, and restrictive housing supply?',
    'Quasi-experimental designs exploiting staggered accession timing, pre-existing migrant-network instruments, and abrupt policy reversals (work-registration moratoria and their expirations) to isolate mobility''s marginal effect.',
    'Overattribution inflates this constraint''s apparent epsilon and misassigns grievance; a small causal share would relocate much of the measured harm to other constraints entirely, changing both classification and remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_attribution_confound, empirical, 'Attribution dispute separating mobility''s causal share from confounded regional decline.').

omega_variable(
    kernel_reading_frame_underdetermination,
    'This constraint is authored as the integration_reading of the federation_membership kernel; how would classification change under the sibling sovereignty_reading, and what signals select between frames?',
    'Track constitutional doctrine over time: court reinterpretations of movement rights, treaty amendments re-nationalizing movement decisions, or member-state ratification of exit-accommodating revisions would signal frame migration toward the sovereignty_reading.',
    'Under the sovereignty reading the victim set widens (national polities losing border control become payers), epsilon is reassessed over a negotiable-policy referent rather than a constitutional one, and exits this reading forecloses become legitimate options — the same territorial arrangement classifies as a structurally different constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame_underdetermination, conceptual, 'Committer-frame omega: this story is one reading of the federation_membership kernel; the disagreement with the sovereignty_reading is located in the movement-status premise (constitutional entitlement vs negotiable policy) and the membership-irrevocability premise (irreversible integration vs conditional treaty).').

omega_variable(
    enforcement_legitimacy_feedback,
    'Does court-driven enforcement of movement rights deepen voluntary compliance by entrenching expectations, or does it erode the legitimacy reserves that voluntary compliance draws on?',
    'Comparative compliance data across member states correlated with cumulative infringement volume and the salience of adverse rulings; long-run survey series on perceived fairness of supranational adjudication.',
    'If enforcement erodes legitimacy, the suppression_requirement series understates future enforcement needs and stability comes to depend on rising coercive input; if it entrenches expectations, measured suppression is front-loaded and declining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy_feedback, empirical, 'Feedback between enforcement intensity and the legitimacy sustaining compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 1957, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1957, federation_membership__integration_reading, theater_ratio, 1957, 0.1).
narrative_ontology:measurement_basis(fede_tr_t1957, observed).
narrative_ontology:measurement(fede_tr_t1968, federation_membership__integration_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement_basis(fede_tr_t1968, observed).
narrative_ontology:measurement(fede_tr_t1973, federation_membership__integration_reading, theater_ratio, 1973, 0.14).
narrative_ontology:measurement_basis(fede_tr_t1973, observed).
narrative_ontology:measurement(fede_tr_t1992, federation_membership__integration_reading, theater_ratio, 1992, 0.18).
narrative_ontology:measurement_basis(fede_tr_t1992, observed).
narrative_ontology:measurement(fede_tr_t2004, federation_membership__integration_reading, theater_ratio, 2004, 0.25).
narrative_ontology:measurement_basis(fede_tr_t2004, observed).
narrative_ontology:measurement(fede_tr_t2008, federation_membership__integration_reading, theater_ratio, 2008, 0.27).
narrative_ontology:measurement_basis(fede_tr_t2008, observed).
narrative_ontology:measurement(fede_tr_t2016, federation_membership__integration_reading, theater_ratio, 2016, 0.32).
narrative_ontology:measurement_basis(fede_tr_t2016, observed).
narrative_ontology:measurement(fede_tr_t2024, federation_membership__integration_reading, theater_ratio, 2024, 0.3).
narrative_ontology:measurement_basis(fede_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t1957, federation_membership__integration_reading, base_extractiveness, 1957, 0.34).
narrative_ontology:measurement_basis(fede_be_t1957, observed).
narrative_ontology:measurement(fede_be_t1968, federation_membership__integration_reading, base_extractiveness, 1968, 0.39).
narrative_ontology:measurement_basis(fede_be_t1968, observed).
narrative_ontology:measurement(fede_be_t1973, federation_membership__integration_reading, base_extractiveness, 1973, 0.42).
narrative_ontology:measurement_basis(fede_be_t1973, observed).
narrative_ontology:measurement(fede_be_t1992, federation_membership__integration_reading, base_extractiveness, 1992, 0.49).
narrative_ontology:measurement_basis(fede_be_t1992, observed).
narrative_ontology:measurement(fede_be_t2004, federation_membership__integration_reading, base_extractiveness, 2004, 0.64).
narrative_ontology:measurement_basis(fede_be_t2004, observed).
narrative_ontology:measurement(fede_be_t2008, federation_membership__integration_reading, base_extractiveness, 2008, 0.67).
narrative_ontology:measurement_basis(fede_be_t2008, observed).
narrative_ontology:measurement(fede_be_t2016, federation_membership__integration_reading, base_extractiveness, 2016, 0.71).
narrative_ontology:measurement_basis(fede_be_t2016, observed).
narrative_ontology:measurement(fede_be_t2024, federation_membership__integration_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(fede_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1957, federation_membership__integration_reading, suppression_requirement, 1957, 0.22).
narrative_ontology:measurement_basis(fede_su_t1957, observed).
narrative_ontology:measurement(fede_su_t1968, federation_membership__integration_reading, suppression_requirement, 1968, 0.31).
narrative_ontology:measurement_basis(fede_su_t1968, observed).
narrative_ontology:measurement(fede_su_t1973, federation_membership__integration_reading, suppression_requirement, 1973, 0.34).
narrative_ontology:measurement_basis(fede_su_t1973, observed).
narrative_ontology:measurement(fede_su_t1992, federation_membership__integration_reading, suppression_requirement, 1992, 0.47).
narrative_ontology:measurement_basis(fede_su_t1992, observed).
narrative_ontology:measurement(fede_su_t2004, federation_membership__integration_reading, suppression_requirement, 2004, 0.59).
narrative_ontology:measurement_basis(fede_su_t2004, observed).
narrative_ontology:measurement(fede_su_t2008, federation_membership__integration_reading, suppression_requirement, 2008, 0.62).
narrative_ontology:measurement_basis(fede_su_t2008, observed).
narrative_ontology:measurement(fede_su_t2016, federation_membership__integration_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement_basis(fede_su_t2016, observed).
narrative_ontology:measurement(fede_su_t2024, federation_membership__integration_reading, suppression_requirement, 2024, 0.66).
narrative_ontology:measurement_basis(fede_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the federation_membership kernel per the epsilon-invariance principle: 'how member states relate to the federation' covers two structurally distinct claims with different epsilon values, victim sets, and failure modes. This file authors the integration_reading (movement as constitutional entitlement; membership irreversible; supranational adjudication supreme; epsilon authored for that arrangement as this reading sees it, with displacement-bearing locals in the victim set). The sibling file authors the sovereignty_reading (movement as negotiable policy; membership a conditional treaty; national border authority legitimate), whose victim set additionally includes national polities deprived of border discretion. Each story links to the other via affects_constraints; neither hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
