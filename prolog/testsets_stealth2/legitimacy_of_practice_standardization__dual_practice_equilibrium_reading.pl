% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Domain-Partitioned Practice Legitimacy (Dual-Practice Equilibrium Reading)
 *   domain: political_history/institutional_change/modernization
 *
 * SUMMARY:
 *   A modernizing state, lacking the capacity to standardize practice
 *   everywhere, fixes a domain partition: the civil register (Gregorian
 *   dates, prescribed dress, procedural uniformity) governs everything the
 *   state must administer — taxation, courts, offices, treaties — while the
 *   inherited register (lunar festivals, household dress, ritual observance)
 *   remains sovereign in domestic and ceremonial life. The settlement is
 *   presented as temporary pragmatism but operates as a durable equilibrium:
 *   no convergence is expected, and compliance with the public register is
 *   strategic rather than internalized. This file instantiates ONE reading of
 *   the kernel legitimacy_of_practice_standardization — the
 *   dual_practice_equilibrium_reading — and its epsilon refers to the
 *   standing dual-register arrangement as this reading assesses it, never to
 *   the converged or fully-standardized arrangements the sibling readings
 *   would endorse. The claim/metric gap is deliberate: the reading CLAIMS a
 *   stable hybrid settlement, and the authored metrics describe a hybrid with
 *   real asymmetric costs — the engine measures the divergence rather than
 *   the author reconciling it.
 *
 * KEY AGENTS:
 *   - modernizing_state_bureaucracy: agenda-setter (institutional/arbitrage) — defines the civil register and collects its yields
 *   - traditional_ritual_specialists: primary beneficiary (organized/constrained) — hold exclusive legitimacy over the private register
 *   - rural_taxpaying_households: primary target (powerless/trapped) — bear both registers' demands with no outside option
 *   - urban_office_workers: target with secondary benefit (moderate/constrained) — pay code-switching costs for wage access
 *   - court_calendar_officials: dispossessed target (moderate/identity_locked) — their hereditary office was the abolished function itself
 *   - foreign_treaty_partners: external beneficiary (institutional/arbitrage) — consume the public register's interoperability
 *   - modernization_historians: analytical observer (analytical/analytical) — see both registers in the record at once
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.6).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Domain-Partitioned Practice Legitimacy (Dual-Practice Equilibrium Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/institutional_change/modernization").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '6f31a029-28a2-4214-8bdb-a3890012dab6').
narrative_ontology:cs_kernel_codification('6f31a029-28a2-4214-8bdb-a3890012dab6', implicit).
narrative_ontology:cs_authority_grounding('6f31a029-28a2-4214-8bdb-a3890012dab6', self_enforcing).
narrative_ontology:cs_reading_relation('6f31a029-28a2-4214-8bdb-a3890012dab6', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f31a029-28a2-4214-8bdb-a3890012dab6', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('6f31a029-28a2-4214-8bdb-a3890012dab6', foundational, legitimacy_is_domain_scoped).
narrative_ontology:cs_axiom_status(legitimacy_is_domain_scoped, holdable).
narrative_ontology:cs_axiom_grounding('6f31a029-28a2-4214-8bdb-a3890012dab6', legitimacy_is_domain_scoped, conventional).
narrative_ontology:cs_axiom('6f31a029-28a2-4214-8bdb-a3890012dab6', foundational, ritual_jurisdiction_inheres_in_communities).
narrative_ontology:cs_axiom_status(ritual_jurisdiction_inheres_in_communities, holdable).
narrative_ontology:cs_axiom_grounding('6f31a029-28a2-4214-8bdb-a3890012dab6', ritual_jurisdiction_inheres_in_communities, deontological).
narrative_ontology:cs_reference_frame('6f31a029-28a2-4214-8bdb-a3890012dab6', domain_partitioned_dual_authority).
narrative_ontology:cs_drift_state('6f31a029-28a2-4214-8bdb-a3890012dab6', late_interval_observation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6f31a029-28a2-4214-8bdb-a3890012dab6', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernizing_state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_specialists).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, foreign_treaty_partners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_taxpaying_households).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, urban_office_workers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, court_calendar_officials).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, urban_office_workers).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, domain_partitioned_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and enforces the rules that define official practice: the civil calendar used for taxation and courts, the dress and etiquette required in offices and official ceremonies, standard timekeeping. Collects predictable tax dates, legible population records, and treaty-compatible administration. Deliberately leaves festivals, funerals, and household observance unregulated, having found that pressing standard practice into those spaces provokes unrest that costs more than it yields.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernizing_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Shrine and temple networks, festival organizers, and local elders who conduct marriages, funerals, and seasonal observances on the older calendar. Their authority over these occasions goes unchallenged by the state, and their standing depends on households continuing to need them. They lost official posts and public sponsorship but kept the occasions themselves; their acquiescence to the settlement is what keeps the private sphere off the state's reform agenda.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_specialists, beneficiary,
    organized, generational, constrained, regional).

% Farm by seasonal and lunar rhythms but owe taxes, register births, and answer summonses on civil-calendar dates. Maintain two calendars in the same house, two wardrobes for town and field, and absorb the bookkeeping and memory load of keeping both straight. Compliance with official practice is performed where officials can see and reverted where they cannot; nobody asked them before the split was fixed.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_taxpaying_households, payer,
    powerless, biographical, trapped, regional).

% Wear prescribed dress and keep office hours in the workplace, then change back into household dress and household observance at home. The wage and the job depend on passing inspection in the public register; the double wardrobe, the commute between registers, and the code-switching itself are unpaid costs carried daily. Their children increasingly treat the workplace register as normal, which quietly shrinks the private one.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, urban_office_workers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, urban_office_workers, beneficiary).

% Hereditary astronomers and calendar-makers whose bureau once issued the official almanac and whose craft defined legitimate timekeeping. The reform transferred calendar issuance to the new ministry and reduced them to pensioners of a function that no longer exists. Their skill, lineage, and status were bound up in the official calendar specifically; teaching the old reckoning to apprentices has no market, and their standing cannot be relocated to the private festival economy they never served.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, court_calendar_officials, payer,
    moderate, generational, identity_locked, local).

% Trading partners and chancelleries that need dates, weights, protocols, and correspondence formats to interoperate with the reforming state. Every step of public-domain standardization lowers their transaction costs; they pressed for exactly this scope of reform in treaty negotiations and have no stake in the private sphere either way.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, foreign_treaty_partners, beneficiary,
    institutional, generational, arbitrage, global).

% Researchers who reconstruct the settlement from fiscal records, edict collections, diaries, and festival attendance rolls. They can see both registers at once — the edicts and the evasions — and their accounts are the main outside check on the state's own narrative of smooth, welcomed modernization.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernizing_state_bureaucracy).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the conflict between two legitimacy orders by assigning each an exclusive jurisdiction: the civil register (dates, dress, procedure) governs wherever the state must administer — taxation, courts, offices, treaties — while the inherited register governs weddings, funerals, festivals, and farm timing. Each occasion has exactly one applicable register, so officials and households know which rules hold where, and neither authority has to conquer the other's territory to operate.
% TRANSFER_FUNCTION: Moves compliance-performance and fiscal legibility from households and workers to the state whenever an occasion falls in the public register, and moves deference, fees, and ritual participation from households to shrine, temple, and elder networks whenever it falls in the private register. Households operating in both registers pay the translation costs — two calendars, two wardrobes, two sets of obligations — out of their own time and money.
% ABSENT_VOICES: The households paying the double burden had no seat: the split was fixed between the ministries and the established ritual networks, and the first generation subject to it simply received it. Radical Westernizers who wanted the private register abolished and restorationists who wanted the public one surrendered were both frozen out of the settlement. The hereditary calendar-makers whose office was abolished learned of the transfer when it was announced.
% DISAPPEARANCE_RATIONALE: If the partition norm vanished overnight, every occasion would again be contested between the registers: tax deadlines would collide with festivals, offices would have no settled dress rule, and either the state would have to impose the civil register everywhere by force or ritual networks would reclaim public occasions. Administration, commerce, and household life would all have to renegotiate which rules hold where.
% FOUNDING_PROBLEM: A state rebuilding its finances and standing after isolation needed legible, predictable administration — fixed tax dates, countable populations, treaty-compatible timekeeping — but lacked the enforcement capacity to standardize practice in every household and village, and earlier attempts to do so had provoked unrest it could not afford.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal archives and treaty negotiation records from outside both benefiting camps corroborate the founding problem: tax-collection chaos under the old calendar and counterpart demands for interoperable dates and protocols are documented independently of the ministries that benefited. Whether the problem remains live is corroborated by no one outside the state, which alone keeps citing new administrative domains needing standardization; household and ritual-network sources treat the original problem as solved and the split as self-perpetuating.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is set at 0.58 because the arrangement's costs are real but bounded: fiscal legibility and compliance-performance flow upward on every public occasion, and the double-register burden falls on those who live in both worlds, yet the private register survives intact and the arrangement ended a conflict neither side could win. Suppression is 0.60 and almost entirely structural — legal penalty attaches to nonstandard practice in the public register and there is no exit from the tax roll — with negligible internalized component, since compliance is avowedly strategic; this is the proportion the suppression-mechanism omega tracks. Theater is 0.30: the administrative function is genuine, but official ceremony increasingly performs modernity for domestic and foreign audiences. Accessibility_collapse is 0.45 — alternatives collapse completely in the public domain but remain fully available in the private one, which is the partition's defining shape. Resistance is 0.42: early calendar imposition met documented unrest and dress rules met grudging evasion, but open resistance subsided once the bargain gave traditionalists a protected sphere. The temporal series share one grid: base_extractiveness climbs (0.50 to 0.63) as monetization and office employment enlarge the public domain's surface — accumulation by expansion, not rate-ratcheting, which is what the T17 trigger should investigate; theater_ratio climbs in step as official performance thickens; suppression_requirement FALLS (0.72 to 0.48) as enforcement machinery is thinned because strategic compliance stays reliable while observation costs remain high — enforcement decay, not liberalization, and the strategic-vs-internalized omega determines which reading of that decay is correct.
 *
 * PERSPECTIVAL GAP:
 *   From the ministry seat the settlement is prudence: it purchased legibility where it mattered and avoided a cultural war it would have lost. From the household seat the same settlement is a standing levy paid in doubled calendars, doubled wardrobes, and constant translation between registers — visible as a burden precisely on the occasions that straddle the boundary. The specialist seat experiences protection; the dispossessed calendar-official seat, nominally on the traditional side of the ledger, experienced confiscation of a lineage function. Coalition potential among the payer seats is weak by design: rural households, urban workers, and court officials share no institutions, and the partition assigns each a different relationship to the arrangement, which is exactly how a hybrid settlement prevents its targets from becoming a single constituency.
 *
 * DIRECTIONALITY LOGIC:
 *   The state and foreign treaty partners sit near the beneficiary pole: the arrangement subsidizes their operations and both can reposition if it fails. Ritual specialists also sit beneficiary-side but with constrained exit — their gain is real yet conditional on households continuing to need them, which couples their fate to the arrangement's persistence. Rural households sit near the target pole: trapped between registers, they bear the transfer with no outside option. Urban workers are targets tempered by wage access — their secondary beneficiary position pulls their derived directionality back toward symmetric. Court calendar officials are the sharpest targets: identity_locked, their craft WAS the abolished function, so no exit exists at any price. No directionality overrides are authored: the derivation chain already separates the two moderate-power seats (workers versus officials) through their exit options, which is precisely the differentiation an override would otherwise have to supply by hand.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the settlement as pure coordination would erase the dispossessed specialists and the household double-burden — the same structure that protects the festival calendar strips the public one. Reading it as pure extraction would erase the genuine survival of ritual practice and the real conflict the partition ended. The tangled-rope classification holds both facts: a coordination function (one register per occasion, ending a war over the whole social fabric) and asymmetric extraction through the same structure (legibility flows up, translation costs stay down, one lineage office was abolished outright). On obsolescence: the founding problem — cheap legibility — was substantially solved within a generation, yet the partition persists because both elite camps now hold assets priced in its continuation; the contested genealogy status combined with the world_rearranges verdict marks the capture/zombie profile the mismatch consumer cross-checks, without asserting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_frame_underdetermination,
    'Is the observed bifurcation a permanent dual-practice equilibrium (this reading), a transitional stage that ends in convergence (endogenous_displacement_reading), or an incomplete imposition awaiting stronger state override (exogenous_override_reading)?',
    'Multi-generation panel data on private-register practice density after public-register standardization stabilizes: if lunar festival observance and household dress retention decay monotonically across generations, the equilibrium reading is a mislabeled transition.',
    'If displacement is underway, this constraint is a scaffold-like transition misread as permanent — classification shifts toward sunset semantics and the strategic-compliance mechanism becomes a countdown rather than a steady state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_frame_underdetermination, empirical, 'Whether the domain partition is terminal or transitional — the core contest between this reading and its siblings.').

omega_variable(
    strategic_vs_internalized_compliance,
    'Is compliance with the public register purely strategic (reverting when unobserved), or has code-switching fused into identity across generations?',
    'Observe practice in unobserved settings across generations: compare private-register retention among third-generation office families against first-generation migrants into office work; survey whether the workplace register is described as an imposition or as simply how things are done.',
    'If internalized, the falling suppression_requirement series reflects genuine habituation and the equilibrium is robust; if strategic, the decline reflects falling enforcement need only while observation costs stay high — the arrangement is fragile to any change in monitoring, and measured suppression understates latent coercive dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_internalized_compliance, empirical, 'Whether the equilibrium rests on deterrence or on habituated identity.').

omega_variable(
    domain_boundary_location_contest,
    'Where exactly does the public/private line sit, and who holds the power to move it?',
    'Track adjudication of mixed cases over time — weddings of officials, funerals in state facilities, festival days coinciding with tax deadlines, dress rules for semi-official occasions — and record which register wins each borderline ruling.',
    'Boundary movement redistributes extraction invisibly: each expansion of the ''public'' category raises the effective burden on households without moving any headline metric, so a stable-looking equilibrium could be steadily annexing the private register case by case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_location_contest, conceptual, 'The location and controllability of the partition boundary — the settlement''s least specified term.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_practice_equilibrium_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(dual_practice_equilibrium_tr_t0, observed).
narrative_ontology:measurement(dual_practice_equilibrium_tr_t8, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(dual_practice_equilibrium_tr_t8, observed).
narrative_ontology:measurement(dual_practice_equilibrium_tr_t16, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(dual_practice_equilibrium_tr_t16, observed).
narrative_ontology:measurement(dual_practice_equilibrium_tr_t24, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(dual_practice_equilibrium_tr_t24, observed).
narrative_ontology:measurement(dual_practice_equilibrium_tr_t32, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement_basis(dual_practice_equilibrium_tr_t32, observed).
narrative_ontology:measurement(dual_practice_equilibrium_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(dual_practice_equilibrium_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(dual_practice_equilibrium_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(dual_practice_equilibrium_be_t0, observed).
narrative_ontology:measurement(dual_practice_equilibrium_be_t8, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(dual_practice_equilibrium_be_t8, observed).
narrative_ontology:measurement(dual_practice_equilibrium_be_t16, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement_basis(dual_practice_equilibrium_be_t16, observed).
narrative_ontology:measurement(dual_practice_equilibrium_be_t24, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(dual_practice_equilibrium_be_t24, observed).
narrative_ontology:measurement(dual_practice_equilibrium_be_t32, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(dual_practice_equilibrium_be_t32, observed).
narrative_ontology:measurement(dual_practice_equilibrium_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement_basis(dual_practice_equilibrium_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(dual_practice_equilibrium_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(dual_practice_equilibrium_su_t0, observed).
narrative_ontology:measurement(dual_practice_equilibrium_su_t8, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(dual_practice_equilibrium_su_t8, observed).
narrative_ontology:measurement(dual_practice_equilibrium_su_t16, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(dual_practice_equilibrium_su_t16, observed).
narrative_ontology:measurement(dual_practice_equilibrium_su_t24, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement_basis(dual_practice_equilibrium_su_t24, observed).
narrative_ontology:measurement(dual_practice_equilibrium_su_t32, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement_basis(dual_practice_equilibrium_su_t32, observed).
narrative_ontology:measurement(dual_practice_equilibrium_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(dual_practice_equilibrium_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'practice standardization during modernization' decomposes into three structurally distinct claims about what makes practice change legitimate: a negotiated domain partition (this file), voluntary utility-driven displacement (endogenous_displacement_reading), and state decree for collective benefit (exogenous_override_reading). Each carries its own epsilon, beneficiary/victim structure, and classification; they form one constraint family linked through affects_constraints. Historical pressure runs both directions across the family: decree episodes created the public register this reading describes, and the equilibrium's observed durability is the principal evidence the displacement reading cites for gradualism — which is why the decomposition matters: averaging the three into one story would produce an epsilon belonging to none of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
