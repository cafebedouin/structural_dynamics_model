% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Settler-Colonial Reading of Jewish Self-Determination: Zionist Dispossession Structure
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story authors the settler-colonial reading of Jewish
 *   self-determination as a single ε-invariant constraint: the standing
 *   Zionist-Israeli institutional arrangement, read as a European-origin
 *   settler formation whose operating function is the acquisition of land and
 *   the demographic and legal subordination of the Palestinian Arab
 *   population. On this reading the arrangement's mechanisms are: pre-state
 *   land purchase consolidated into state land regimes after 1948; the
 *   displacement of roughly 700,000-800,000 Palestinians and the
 *   absentee-property laws that transferred their holdings; nearly two
 *   decades of military government over the remaining Arab citizens;
 *   post-1967 settlement expansion under dual legal systems in the West Bank;
 *   the Law of Return's asymmetric citizenship gate; and the post-2007
 *   blockade of Gaza. Persistence depends on continuous coercive enforcement
 *   — military administration, permit regimes, administrative detention — and
 *   on foreclosing the alternatives (refugee return, equal citizenship,
 *   binational equality) that would dissolve the structure. The claim/metric
 *   independence rule applies: claimed_type is authored from this reading's
 *   structural verdict; the metrics are authored as descriptive of the
 *   arrangement's operation under this reading; the engine computes per-seat
 *   classifications independently. KEY AGENTS (by structural relationship): -
 *   israeli_state_apparatus: Agenda setter ([institutional]/[arbitrage]) —
 *   administers land, law, and force; captures the gains -
 *   west_bank_settler_movement: Primary beneficiary
 *   ([organized]/[identity_locked]) — collects land and subsidy;
 *   ideologically unable to leave - law_of_return_immigrants: Secondary
 *   beneficiary ([moderate]/[mobile]) — receive citizenship and absorption
 *   benefits - palestinian_refugees_descendants: Primary target
 *   ([powerless]/[trapped]) — bear permanent displacement; excluded from
 *   negotiating seats - west_bank_palestinians: Primary target
 *   ([moderate]/[trapped]) — bear occupation, land takings, permit regime -
 *   gaza_strip_residents: Primary target ([powerless]/[trapped]) — bear
 *   blockade - palestinian_citizens_of_israel: Differentiated target
 *   ([moderate]/[constrained]) — bear legal hierarchy inside formal
 *   citizenship - united_states_patron_state: External beneficiary
 *   ([institutional]/[mobile]) — collects alliance rents - arab_host_states:
 *   Cost-bearing hosts ([organized]/[constrained]) -
 *   international_criminal_tribunals: Analytical observer
 *   ([institutional]/[analytical]) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.88).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.82).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Settler-Colonial Reading of Jewish Self-Determination: Zionist Dispossession Structure").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '39390b29-e338-4a74-bcab-b6f0cbc4c362').
narrative_ontology:cs_kernel_codification('39390b29-e338-4a74-bcab-b6f0cbc4c362', distributed).
narrative_ontology:cs_authority_grounding('39390b29-e338-4a74-bcab-b6f0cbc4c362', distributed).
narrative_ontology:cs_reading_relation('39390b29-e338-4a74-bcab-b6f0cbc4c362', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('39390b29-e338-4a74-bcab-b6f0cbc4c362', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('39390b29-e338-4a74-bcab-b6f0cbc4c362', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('39390b29-e338-4a74-bcab-b6f0cbc4c362', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('39390b29-e338-4a74-bcab-b6f0cbc4c362', foundational, zionism_structurally_european_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_structurally_european_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('39390b29-e338-4a74-bcab-b6f0cbc4c362', zionism_structurally_european_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('39390b29-e338-4a74-bcab-b6f0cbc4c362', foundational, palestinian_dispossession_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(palestinian_dispossession_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('39390b29-e338-4a74-bcab-b6f0cbc4c362', palestinian_dispossession_constitutive_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('39390b29-e338-4a74-bcab-b6f0cbc4c362', secondary, law_of_return_operates_as_exclusion_machinery).
narrative_ontology:cs_axiom_status(law_of_return_operates_as_exclusion_machinery, holdable).
narrative_ontology:cs_axiom_grounding('39390b29-e338-4a74-bcab-b6f0cbc4c362', law_of_return_operates_as_exclusion_machinery, conventional).
narrative_ontology:cs_reference_frame('39390b29-e338-4a74-bcab-b6f0cbc4c362', european_settler_colonial_formation).
narrative_ontology:cs_drift_state('39390b29-e338-4a74-bcab-b6f0cbc4c362', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('39390b29-e338-4a74-bcab-b6f0cbc4c362', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, west_bank_settler_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, law_of_return_immigrants).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_refugees_descendants).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, gaza_strip_residents).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, united_states_patron_state).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, arab_host_states).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, arab_host_states).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, wolfean_settler_colonial_elimination_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and administers the land regime (state land declarations, Israel Land Authority allocations), the citizenship gate (Law of Return), and the military governance of the occupied territories. Collects the land, revenue, and demographic outcomes the arrangement produces, and directs the enforcement machinery — courts, permits, detention — that maintains it. Exiting the arrangement would mean dismantling its own founding statutes; instead it can restructure commitments externally through patron alliances and diplomatic shielding.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Builds and expands residential settlements in the West Bank under state subsidies, bypass roads, and military protection. Gains land, housing, and infrastructure priority unavailable inside the Green Line. Relocation would negate the movement's core theological-national purpose, so departure is treated as betrayal rather than option.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, west_bank_settler_movement, beneficiary,
    organized, generational, identity_locked, regional).

% Arrive from any country to immediate citizenship, absorption packages, housing mortgages, and settlement incentives. Entry is voluntary and emigration remains open; the benefits flow automatically by ancestry regardless of any prior connection to the land.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, law_of_return_immigrants, beneficiary,
    moderate, biographical, mobile, global).

% Displaced in 1948 and 1967, now numbering millions across camps in Lebanon, Jordan, Syria, and beyond. Barred from return by the absentee-property regime and citizenship law; original property passed into state custody. Hold no seat in final-status negotiations; representation runs through factions and host states they do not control. Blocked in both directions — return denied, host-state integration restricted.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_refugees_descendants, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, palestinian_refugees_descendants, excluded).

% Live under fragmented jurisdiction (Areas A, B, C), checkpoint and permit regimes, and land takings for settlements, roads, and the barrier. Water and building permits are rationed by the civil administration. The Palestinian Authority exercises limited autonomy and cannot alter the underlying land or movement regime. Emigration is possible but forfeits residence rights that are not recoverable.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, west_bank_palestinians, payer,
    moderate, biographical, trapped, regional).

% Live under air, sea, and land blockade since 2007: restricted fishing zones, buffer strips inside agricultural land, infrastructure destroyed in recurring escalations and rebuilt under import restrictions. No legal exit channel exists; border crossings open only conditionally.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, gaza_strip_residents, payer,
    powerless, biographical, trapped, local).

% Hold formal citizenship and vote, but sit under a legal hierarchy: the 2018 Nation-State Law frames self-determination as Jewish-only and demotes Arabic; admissions committees screen purchasers in hundreds of communities; unrecognized villages lack basic services; per-capita municipal and education funding trails systematically. Emigration is available but severs family and linguistic life.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% Supplies multi-year military financing, Security Council shielding, and diplomatic recognition. Receives in return an aligned regional client, intelligence cooperation, and domestic constituency satisfaction. Redirecting support is politically costly, but the relationship is discretionary — no treaty binds it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, united_states_patron_state, beneficiary,
    institutional, generational, mobile, global).

% Host long-term refugee populations under unequal legal status — most extremely in Lebanon, where camps sit outside labor and property law. Bear service and security costs while converting the refugee question into diplomatic leverage; normalization agreements have traded the question for strategic gains without consulting the refugees themselves.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, arab_host_states, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, arab_host_states, beneficiary).

% Prosecutor offices and courts examining occupation legality, settlement-related war-crimes allegations, and apartheid claims. Issue rulings and warrants that alter legitimacy conditions and officials' travel exposure, but command no independent enforcement arm; compliance depends on member states.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispersed Jewish immigration, capital, and military capacity into a single territorial-sovereign collective: ingathering, defense integration, land titling, and institution-building solved centrally rather than per-community.
% TRANSFER_FUNCTION: Moves land, water, housing, and political sovereignty from Palestinian Arab holders to the Israeli state and Jewish nationals; moves financial and military resources from external patrons and diaspora donors into the state and settlement apparatus; moves labor from the occupied territories into the Israeli economy on permit-controlled terms.
% ABSENT_VOICES: Refugee populations denied return, Gaza's residents, and the diaspora-born descendants of the displaced have no seat in final-status frameworks; host-state refugee communities are spoken for by governments that trade the question diplomatically. Their objection — that the arrangement's legitimacy was never consented to by those it dispossessed — is registered only through protest and litigation channels outside the negotiating room.
% DISAPPEARANCE_RATIONALE: Overnight removal would force simultaneous rearrangement of citizenship regimes (the Law of Return asymmetry gone), land title (absentee-property transfers unwound or litigated), settlement geography, refugee return flows across four host states, and the patron-alliance architecture built around the arrangement — the regional order reorganizes around whatever replaces the extraction structure.
% FOUNDING_PROBLEM: European antisemitism and the demonstrated failure of emancipation to secure Jewish physical survival — the arrangement was built to concentrate Jewish national life behind defensible sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Holocaust historiography and contemporary antisemitism monitoring (EU Fundamental Rights Agency incident data) attest the founding problem's reality and persistence; Palestinian historiography and the Israeli New Historians' archival work independently attest the dispossession sequence this reading treats as constitutive. No corroboration exists for the further claim that the founding problem justifies the specific extraction structure — that inference is disputed by every non-beneficiary seat.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.88 at interval end): land, water, housing stock, and labor-market control transfer continuously from Palestinian holders to state and settler hands, and the transfer rate has stepped upward with each annexation episode. Suppression (0.82) is authored as a raw structural property — the checkpoint system, permit regime, administrative detention, and blockade are the machinery holding the arrangement in place; it is not scaled by power or scope, unlike extractiveness, which the engine scales by directionality and spatial scope. Theater ratio (0.58) reflects the widening share of activity that is performative: negotiation processes maintained while settlement expansion proceeds, democratic framing maintained over a differential-rights regime. Accessibility collapse (0.65) sits below mountain levels because alternatives remain conceivable — return, equal citizenship, binational arrangements — but are foreclosed by force and law rather than by impossibility. Resistance (0.75) is high and recurrent: mass uprisings, boycott movements, litigation, and armed struggle impose real enforcement costs. The measurement series run on one shared eight-point grid spanning 1897-2023 so every metric is authored at every examined time point; the trajectory is monotonic intensification with a step change at the 1948 state transition (t=51), not cyclical oscillation — enforcement capacity ratchets upward and does not relax between episodes.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently by construction. From the state and settler seats the arrangement presents as national restoration and security management — the coordination story is lived experience, not cynicism. From the refugee, West Bank, Gaza, and citizen seats the same structure presents as dispossession wearing a legal mask. The US patron seat experiences alliance rent at negligible domestic cost. The engine computes these divergent per-seat types from the structural data; the authored snare claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus is the structural beneficiary and receipt seat (d near the beneficiary end): it writes the land regime, enforces it, and collects. Settlers and Law-of-Return immigrants derive low d from declared beneficiary status; settlers' identity_locked exit does not push them toward the target end because they are not targets — identity lock here stabilizes the beneficiary position. Refugee, West Bank, and Gaza seats derive high d from declared victim status plus trapped exit; Gaza's total closure places it nearest the full-target end. Palestinian citizens of Israel occupy intermediate d: declared victims bearing legal hierarchy, but holding citizenship services that partially offset the burden. The US patron derives moderately low d from beneficiary status with mobile exit. Host states sit near symmetric: they bear refugee costs and harvest diplomatic leverage in comparable measure. International tribunals take the analytical seat — no d computation applies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Jewish physical insecurity in Europe — remains live by the testimony of outside corroborators, so the arrangement is not a piton kept alive by inertia: its extraction is current function, not residue, and its maintenance is substantive rather than theatrical. It carries no sunset clause and was not designed as transitional, ruling out scaffold. The snare-versus-tangled-rope boundary is the live analytical risk: if the ingathering-and-security coordination function were shown inseparable from the dispossession mechanism, part of the measured extraction would requalify as coordination cost and the classification would drift toward tangled_rope; the omega coordination_extraction_separability holds that question open rather than resolving it by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (settler_colonial_reading) of the contested kernel jewish_self_determination — which structural element do the sibling readings relocate, and how would the classification move if another reading were adopted?',
    'Cross-reading comparison within the kernel family: the indigenous_return_reading inverts the beneficiary/victim sets and drives ε toward coordination-cost levels; the liberal_nationalist_reading retains the victim set but reframes the transfer as ordinary state-building cost; the diasporist_reading dissolves the territorial arrangement entirely.',
    'Classification is reading-indexed: under indigenous_return the same history computes as decolonization coordination; under this reading it computes as snare. The corpus must keep the readings as separate files to preserve ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: one-of-five readings; disagreement located in the structural characterization of the founding migration and the resulting beneficiary/victim assignment.').

omega_variable(
    coordination_extraction_separability,
    'Within this reading, is the ingathering-and-security coordination function structurally separable from the dispossession mechanism, or does the coordination depend on the extraction?',
    'Counterfactual institutional analysis: evaluate whether binational or autonomy-based arrangements could have delivered Jewish physical security without land alienation; compare with national-ingathering cases that proceeded without mass displacement.',
    'If separable, the snare classification is unqualified — the coordination story is pure cover. If inseparable, a residual coordination component qualifies the extraction profile toward tangled_rope at the margins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extractive components are structurally separable.').

omega_variable(
    palestinian_coalition_potential,
    'Can the fragmented Palestinian polity — divided between rival administrations, refugee populations across multiple host states, and a citizen minority — achieve coalition power sufficient to alter the structure?',
    'Track unified-representation episodes (reconciliation attempts, unified electoral lists, coordinated international litigation) and correlate with measurable concession events.',
    'Effective coalition would raise realized resistance above the authored scalar, increase enforcement costs, and open negotiated-restructuring paths that the current trapped-exit profile forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_coalition_potential, empirical, 'Coalition-power question for a multi-seat victim class.').

omega_variable(
    epsilon_temporal_referent,
    'Does this reading''s ε treat the pre-state land-purchase phase and the post-1948 state phase as one continuous arrangement, or as distinct constraints with separate ε?',
    'Conceptual analysis of mechanism continuity: land acquired by purchase before 1948 versus land taken by state decree after; if the mechanisms differ structurally, decompose into two stories linked by network edges.',
    'A decomposed corpus would date the snare transition precisely at the 1948 state transition; a unified referent keeps the single high-ε arc authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_temporal_referent, conceptual, 'Temporal boundary of the ε referent within the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 0, 126).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__settler_colonial_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t18, jewish_self_determination__settler_colonial_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t18, observed).
narrative_ontology:measurement(jewi_tr_t36, jewish_self_determination__settler_colonial_reading, theater_ratio, 36, 0.25).
narrative_ontology:measurement_basis(jewi_tr_t36, observed).
narrative_ontology:measurement(jewi_tr_t54, jewish_self_determination__settler_colonial_reading, theater_ratio, 54, 0.35).
narrative_ontology:measurement_basis(jewi_tr_t54, observed).
narrative_ontology:measurement(jewi_tr_t72, jewish_self_determination__settler_colonial_reading, theater_ratio, 72, 0.38).
narrative_ontology:measurement_basis(jewi_tr_t72, observed).
narrative_ontology:measurement(jewi_tr_t90, jewish_self_determination__settler_colonial_reading, theater_ratio, 90, 0.45).
narrative_ontology:measurement_basis(jewi_tr_t90, observed).
narrative_ontology:measurement(jewi_tr_t108, jewish_self_determination__settler_colonial_reading, theater_ratio, 108, 0.55).
narrative_ontology:measurement_basis(jewi_tr_t108, observed).
narrative_ontology:measurement(jewi_tr_t126, jewish_self_determination__settler_colonial_reading, theater_ratio, 126, 0.58).
narrative_ontology:measurement_basis(jewi_tr_t126, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__settler_colonial_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t18, jewish_self_determination__settler_colonial_reading, base_extractiveness, 18, 0.3).
narrative_ontology:measurement_basis(jewi_be_t18, observed).
narrative_ontology:measurement(jewi_be_t36, jewish_self_determination__settler_colonial_reading, base_extractiveness, 36, 0.45).
narrative_ontology:measurement_basis(jewi_be_t36, observed).
narrative_ontology:measurement(jewi_be_t54, jewish_self_determination__settler_colonial_reading, base_extractiveness, 54, 0.8).
narrative_ontology:measurement_basis(jewi_be_t54, observed).
narrative_ontology:measurement(jewi_be_t72, jewish_self_determination__settler_colonial_reading, base_extractiveness, 72, 0.82).
narrative_ontology:measurement_basis(jewi_be_t72, observed).
narrative_ontology:measurement(jewi_be_t90, jewish_self_determination__settler_colonial_reading, base_extractiveness, 90, 0.84).
narrative_ontology:measurement_basis(jewi_be_t90, observed).
narrative_ontology:measurement(jewi_be_t108, jewish_self_determination__settler_colonial_reading, base_extractiveness, 108, 0.86).
narrative_ontology:measurement_basis(jewi_be_t108, observed).
narrative_ontology:measurement(jewi_be_t126, jewish_self_determination__settler_colonial_reading, base_extractiveness, 126, 0.88).
narrative_ontology:measurement_basis(jewi_be_t126, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__settler_colonial_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t18, jewish_self_determination__settler_colonial_reading, suppression_requirement, 18, 0.15).
narrative_ontology:measurement_basis(jewi_su_t18, observed).
narrative_ontology:measurement(jewi_su_t36, jewish_self_determination__settler_colonial_reading, suppression_requirement, 36, 0.3).
narrative_ontology:measurement_basis(jewi_su_t36, observed).
narrative_ontology:measurement(jewi_su_t54, jewish_self_determination__settler_colonial_reading, suppression_requirement, 54, 0.65).
narrative_ontology:measurement_basis(jewi_su_t54, observed).
narrative_ontology:measurement(jewi_su_t72, jewish_self_determination__settler_colonial_reading, suppression_requirement, 72, 0.7).
narrative_ontology:measurement_basis(jewi_su_t72, observed).
narrative_ontology:measurement(jewi_su_t90, jewish_self_determination__settler_colonial_reading, suppression_requirement, 90, 0.74).
narrative_ontology:measurement_basis(jewi_su_t90, observed).
narrative_ontology:measurement(jewi_su_t108, jewish_self_determination__settler_colonial_reading, suppression_requirement, 108, 0.78).
narrative_ontology:measurement_basis(jewi_su_t108, observed).
narrative_ontology:measurement(jewi_su_t126, jewish_self_determination__settler_colonial_reading, suppression_requirement, 126, 0.82).
narrative_ontology:measurement_basis(jewi_su_t126, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, diasporist_reading).

% DUAL FORMULATION NOTE:
% Family decomposition per the ε-invariance principle: the colloquial label 'Zionism' conflates at least five structurally distinct claims, each with its own ε, beneficiary/victim structure, and classification. This story authors the settler-colonial member (high ε, snare). The liberal_nationalist_reading is the upstream legitimation claim this reading treats as cover; the indigenous_return_reading is its direct contrary (the kernel's foreclosure pair); the diasporist_reading consumes this reading's historical findings as downstream evidence. All members link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
