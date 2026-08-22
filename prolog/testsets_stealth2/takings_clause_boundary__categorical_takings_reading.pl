% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Takings Boundary: Per Se Poles with Penn Central Middle
 *   domain: legal/constitutional/property_rights
 *
 * SUMMARY:
 *   The categorical takings reading draws the compensation boundary with two
 *   bright lines and a balancing screen: permanent physical occupation and
 *   total value elimination are per se takings (Loretto 1982, Lucas 1992),
 *   while every other regulatory burden is weighed under Penn Central's three
 *   factors (1978). The arrangement coordinates genuinely: it gives planners
 *   and investors a predictable map of worst-case exposure and gives
 *   governments a workable screen for routine regulation. It also transfers
 *   systematically: the balancing tier almost never awards compensation even
 *   for severe-but-partial value destruction, so a large share of regulatory
 *   value destruction flows to governments uncompensated. The claim/metric
 *   gap is deliberate: the constraint is CLAIMED as tangled_rope from the
 *   authoring seat because both the coordination function and the asymmetric
 *   transfer are structurally present, while the metrics are authored
 *   descriptively from the doctrine's observable operation; the engine
 *   computes per-seat types independently and any divergence is the datum.
 *
 * KEY AGENTS:
 *   - supreme_court: agenda_setter (institutional/analytical) — authors and polices the category lines; collects nothing
 *   - government_regulators: primary beneficiary (institutional/arbitrage) — captures uncompensated middle-zone regulatory value; pays at the poles
 *   - regulated_landowners: target (powerful/constrained) — bears severe-but-partial losses with costly, usually futile recourse
 *   - small_parcel_owners: target (powerless/trapped) — bears silent losses with no litigation capacity
 *   - pole_category_claimants: beneficiary (moderate/constrained) — protected by the bright lines
 *   - land_use_investors: beneficiary (organized/mobile) — purchases predictability and routes capital around hostility
 *   - compensation_expansion_advocates: excluded (organized/constrained) — presses the sibling reading without an operative seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.53).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.55).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.53).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Boundary: Per Se Poles with Penn Central Middle").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "legal/constitutional/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '917ac505-e5f4-4b18-ba61-fa25715ae798').
narrative_ontology:cs_kernel_codification('917ac505-e5f4-4b18-ba61-fa25715ae798', fixed_text).
narrative_ontology:cs_authority_grounding('917ac505-e5f4-4b18-ba61-fa25715ae798', lineage).
narrative_ontology:cs_interpretation_layer_present('917ac505-e5f4-4b18-ba61-fa25715ae798').
narrative_ontology:cs_reading_relation('917ac505-e5f4-4b18-ba61-fa25715ae798', takings_clause_boundary__physical_appropriation_reading, forecloses).
narrative_ontology:cs_reading_relation('917ac505-e5f4-4b18-ba61-fa25715ae798', takings_clause_boundary__regulatory_takings_reading, influences).
narrative_ontology:cs_axiom('917ac505-e5f4-4b18-ba61-fa25715ae798', foundational, total_value_elimination_per_se_compensable).
narrative_ontology:cs_axiom_status(total_value_elimination_per_se_compensable, holdable).
narrative_ontology:cs_axiom_grounding('917ac505-e5f4-4b18-ba61-fa25715ae798', total_value_elimination_per_se_compensable, deontological).
narrative_ontology:cs_axiom('917ac505-e5f4-4b18-ba61-fa25715ae798', foundational, non_extreme_regulations_resolved_by_penn_central_balancing).
narrative_ontology:cs_axiom_status(non_extreme_regulations_resolved_by_penn_central_balancing, holdable).
narrative_ontology:cs_axiom_grounding('917ac505-e5f4-4b18-ba61-fa25715ae798', non_extreme_regulations_resolved_by_penn_central_balancing, conventional).
narrative_ontology:cs_reference_frame('917ac505-e5f4-4b18-ba61-fa25715ae798', two_track_doctrine_per_se_poles_penn_central_middle).
narrative_ontology:cs_drift_state('917ac505-e5f4-4b18-ba61-fa25715ae798', contemporary_post_cedar_point_sheetz_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('917ac505-e5f4-4b18-ba61-fa25715ae798', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, government_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, pole_category_claimants).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, land_use_investors).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, regulated_landowners).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, small_parcel_owners).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, police_power_primacy_in_partial_diminution_zone).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, administrability_of_ad_hoc_balancing).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, investment_backed_expectations_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and polices the category boundary through case-by-case rulings: it fixed the physical-occupation line (Loretto), the total-elimination line (Lucas), and the balancing screen for everything else (Penn Central), and it adjusts the edges (Cedar Point, Sheetz, Murr). It collects no revenue; its stake is doctrinal authority and administrability. Once the constitutional text made the question unavoidable, it cannot decline ownership of the boundary.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Federal, state, and local agencies impose zoning, environmental, preservation, and access restrictions on private land. In the middle zone their rules destroy substantial private value without triggering compensation; they draft programs knowing the Lucas threshold caps their worst-case exposure. At the poles they pay compensation from public treasuries. Their characteristic move is arbitrage: calibrate regulation to sit just short of the categorical lines.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, government_regulators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, government_regulators, agenda_setter).

% Large holders and developers whose parcels absorb severe-but-partial burdens: development moratoria, habitat designations, historic-preservation controls that destroy 60-95% of value. They can litigate, but a Penn Central claim runs years, costs heavily, and almost never succeeds; selling into the market the regulation itself depressed realizes the loss rather than escaping it.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, regulated_landowners, payer,
    powerful, biographical, constrained, national).

% Owners of homes and small lots facing overlay districts, wetland buffers, shoreline setbacks, and access restrictions. They lack the resources to sustain a multi-year federal takings claim; their losses pass silently into property values and are invisible in any compensation statistic. Exit means selling at the discounted price the regulation created, on their own land, inside the same jurisdiction.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, small_parcel_owners, payer,
    powerless, biographical, trapped, regional).

% Owners whose situation hits a bright line: a permanent physical occupation (mandated antenna installations, equipment housing) or near-total value elimination. They obtain compensation under rules that require no proof of purpose, reasonableness, or proportionality. Even before any award, the bright line itself is their principal gain: it tells them where they stand.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, pole_category_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Developers, lenders, and title insurers price land against a known rule-set. They know exactly which exposures are compensable, can model residual middle-zone risk as a cost of doing business, and can route capital away from hostile jurisdictions entirely. Mobility of capital is an exit the immobile parcel does not have.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, land_use_investors, beneficiary,
    organized, biographical, mobile, continental).

% Property-rights litigators, allied scholars, and sympathetic officeholders pressing the broader regulatory-takings reading argue through briefs, model legislation, and dissenting opinions, but hold no operative seat: the prevailing reading routes their clients' severe-but-partial claims into the balancing tier, where they almost never succeed. Their exclusion is maintained by the same category boundaries they contest.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, compensation_expansion_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__categorical_takings_reading, government_regulators).
narrative_ontology:fixing_cost_class(takings_clause_boundary__categorical_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, administrable answer to when compensation is owed: bright-line rules at the extremes let planners, investors, and governments coordinate land-use decisions without relitigating every severe restriction, and the balancing screen disposes of the mass of ordinary regulations in a uniform (if contestable) way.
% TRANSFER_FUNCTION: Moves forgone compensation: economic value destroyed by middle-tier regulations flows from affected property owners to governments and the diffuse public, which receive the regulatory benefit unpaid. Secondarily, at the poles, it moves public funds to pole-category claimants as formal compensation.
% ABSENT_VOICES: Owners bearing severe-but-partial losses whose claims never reach a courtroom: their non-filing makes the arrangement look more consensual than it is, since the doctrine's operation records only the claims brought, not the losses absorbed silently. Compensation-expansion advocates are present in briefs and scholarship but excluded from the operative majority that sets the categories.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, compensation liability would suddenly attach to middle-zone losses: land-use regulation would contract or reprice immediately, local budgets exposed to retroactive claims would face fiscal shock, land prices would reprice across every regulated market, and investment models built on the current rule-set would fail until an interim boundary was improvised.
% FOUNDING_PROBLEM: Reconcile the Fifth Amendment's compulsory-compensation command with the government's regulatory police power. The general tension dates to the Republic's founding; the specific problem this reading was built to solve was the unpredictability left by Penn Central's open-ended factors: by the early 1980s, neither investors nor governments could forecast when a severe restriction would trigger liability, so the categorical rails at Loretto (1982) and Lucas (1992) were laid to stabilize the extremes.
% FOUNDING_PROBLEM_CORROBORATION: Governments and much of the legal academy attest the structure works and the stabilizing function is delivered. Outside the beneficiary set, property-rights litigating organizations (their certiorari petitions and published dockets), cross-spectrum scholarship from both expansive and restrictive camps, and ballot-initiative campaigns (Oregon Measure 37 and its successor fight) attest that the middle-zone problem remains live and that the founding instability has been contained, not resolved. No single attester outside the benefiting parties speaks for the whole; the contest itself is the corroborated finding.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.53, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.53 because the doctrine's transfer is real but bounded: the poles pay out, and the middle tier destroys value that is compensated at close to zero incidence, but the arrangement also delivers a predictability good that offsets part of the burden for the best-positioned actors. Suppression is 0.55 and is a raw structural input, unscaled by the engine: the barriers are structural (immobile land, multi-year litigation cost, doctrinal dismissal rates), not violent, and no suppression_requirement series is authored because the enforcement picture is comparatively static across the interval — the scalar carries it. Theater_ratio 0.43 reflects the growing conclusory use of Penn Central's factors: the language of balancing is routinely invoked while outcomes track deference to the regulating government, though the per se rules themselves do genuine classificatory work. Accessibility_collapse 0.55: alternatives (selling, relocating use, reframing a claim toward a pole) persist but collapse for the actor who has fully understood the middle tier's odds. Resistance 0.65 is high for a legal construct: sustained litigation campaigns, state ballot initiatives, and cross-spectrum scholarly attack meet the doctrine continuously. The victim seats warrant a coalition check: small_parcel_owners are individually powerless, and parcel-specific heterogeneous losses frustrate durable coalition formation, but the Oregon Measure 37 episode demonstrates that coalition power at ballot scale is possible; the doctrine's persistence despite that episode indicates the coalition channel is intermittent, not closed. The measurement series run on one shared eight-point grid (1978-2025) so every tracked metric is authored at every examined year; the slowly rising base_extractiveness series is the kind of accumulation that feeds the T17 abductive trigger as a hypothesis, not a reclassification.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical doctrine. The government seat experiences a rope: a flexible screen that lets it govern without fiscal exposure, with occasional bounded payouts at the poles. The middle-tier payer seats experience something snare-flavored: a screen that reliably consumes their claims. Pole-category claimants experience protection and clarity. The analytical seat sees a hybrid whose classification depends on which tier of the same structure the seat occupies. The engine computes this divergence from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Government_regulators are declared beneficiaries and derive a low d (subsidy side): the arrangement hands them uncompensated regulatory capacity, and their arbitrage-grade exit (recalibrating regulation to sit under the Lucas line) pushes them further toward the beneficiary end. Regulated_landowners and small_parcel_owners are declared victims deriving high d: trapped or constrained exit amplifies their position toward the full-target end, with small_parcel_owners sitting nearest it (trapped, powerless, regional scope where verification of their losses is weakest). Pole_category_claimants and land_use_investors are beneficiaries with low d — they collect predictability and compensation. The supreme_court seat has no structural beneficiary/victim declaration and sits analytically, receiving the fallback near-symmetric d, which is descriptively right: the court administers and absorbs no rents. No directionality_overrides are authored because the derivation chain produces accurate d values from the declared structure; the one candidate (governments' pole-side payments nudging their composite d upward from pure-beneficiary) is captured adequately by their arbitrage exit and does not warrant a per-story override.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the whole doctrine as a rope would hide the middle-tier transfer behind the poles' genuine coordination goods; classifying it as a snare would erase the real predictability function that investors and pole claimants verifiably consume and would misread the poles' payout obligations. Tangled_rope holds both facts in one structure: coordination and extraction ride the same category boundary. The R5 genealogy interview locates the mandatrophy risk precisely: the founding problem (stabilizing the extremes) is substantially addressed, but the arrangement persists over a middle zone whose problem status is disputed — hence founding_problem_status is authored contested rather than dead, avoiding a false zombie flag while keeping the mismatch consumer armed. The theater_ratio trajectory (0.20 to 0.43) is the watched symptom: if the balancing tier degenerates into pure performance while the categories ossify, the structure drifts toward piton in the middle tier even as the poles remain live. The rising extraction series and expanding category list are monitored as the two forces that would resolve the contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the categorical_takings_reading of the takings_clause_boundary kernel; how would instantiating a sibling reading change the structural facts this story encodes?',
    'Track successive Supreme Court composition and rulings shifting category membership: a court retiring the Lucas category moves the operative constraint toward the physical_appropriation_reading; a court collapsing all evaluation into a universal ''too far'' judgment moves it toward the regulatory_takings_reading.',
    'Under the physical_appropriation_reading the victim set shrinks to physically occupied owners and middle-zone extraction leaves the compensation ledger entirely; under the regulatory_takings_reading the pole/middle asymmetry dissolves, compensation incidence rises sharply, and the government_regulator seat flips from net beneficiary to net payer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one of three live readings of the takings-boundary kernel; siblings instantiate structurally different constraints with different victim sets and epsilon.').

omega_variable(
    middle_zone_extraction_share,
    'What fraction of regulatory value destruction lands in the Penn Central middle zone and terminates uncompensated?',
    'Compile jurisdiction-level regulatory impact valuations (fiscal notes, appraisal records for restricted parcels) against recorded compensation awards and successful claims over the interval.',
    'A dominant uncompensated middle share confirms the tangled_rope profile with heavy transfer; a small share would support a rope-dominant reading in which the poles carry nearly all compensable exposure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middle_zone_extraction_share, empirical, 'Magnitude of the transfer flowing through the balancing tier rather than either bright line.').

omega_variable(
    penn_central_indeterminacy_valence,
    'Is Penn Central''s indeterminacy a deliberate flexibility reserve that lets police-power regulation adapt to novel harms, or an unpredictability tax that extracts reliance value from owners who cannot forecast outcomes?',
    'Expert-panel replication studies of Penn Central outcomes (would independent panels predict the same verdicts?) cross-checked against documented regulatory innovations that genuinely required case-specific judgment.',
    'If unpredictability dominates, effective extraction in the middle tier exceeds the authored epsilon and the arrangement trends toward snare; if adaptive flexibility dominates, the middle functions closer to a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_indeterminacy_valence, conceptual, 'Whether the middle tier''s vagueness is functional coordination or cover for uncompensated transfer.').

omega_variable(
    categorical_expansion_trajectory,
    'Will the categorical pole keep accreting new per se categories (Cedar Point access appropriations, exaction standardization after Sheetz), and toward what terminal configuration?',
    'Count distinct categorical triggers recognized per decade in certiorari-granted takings cases post-2021; monitor whether new categories generalize or remain narrow.',
    'Sustained expansion converts the arrangement toward fuller compensation coverage, shrinking the uncompensated middle and moving classification toward rope; abandonment of categories reverses the drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_expansion_trajectory, empirical, 'Direction and durability of the observed expansion of bright-line territory.').

omega_variable(
    cs_framing_underdetermination,
    'Is the takings boundary best framed as a commitment system grounded in the constitutional text (fixed_text kernel with lineage authority), or as distributed adjudicative practice with no stabilized kernel?',
    'Examine whether rulings treat the Fifth Amendment clause as an adjudicating canon or as one input among evolving doctrinal practice; check whether interpretive disagreement is absorbed below the text or surfaces as amendment-level revision pressure.',
    'Adopting the distributed framing removes interpretation_layer_present and shifts authority_grounding to practice, weakening the lineage legitimacy claim on which this reading''s stability rests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two coherent framings of the same boundary produce different commitment-system classifications; signals guiding the chosen framing are doctrinal citation practice and the clause''s entrenched textual status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcb_categorical_reading_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement_basis(tcb_categorical_reading_tr_t1978, observed).
narrative_ontology:measurement(tcb_categorical_reading_tr_t1982, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1982, 0.24).
narrative_ontology:measurement_basis(tcb_categorical_reading_tr_t1982, observed).
narrative_ontology:measurement(tcb_categorical_reading_tr_t1992, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1992, 0.28).
narrative_ontology:measurement_basis(tcb_categorical_reading_tr_t1992, observed).
narrative_ontology:measurement(tcb_categorical_reading_tr_t1998, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1998, 0.32).
narrative_ontology:measurement_basis(tcb_categorical_reading_tr_t1998, observed).
narrative_ontology:measurement(tcb_categorical_reading_tr_t2005, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement_basis(tcb_categorical_reading_tr_t2005, observed).
narrative_ontology:measurement(tcb_categorical_reading_tr_t2012, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2012, 0.38).
narrative_ontology:measurement_basis(tcb_categorical_reading_tr_t2012, observed).
narrative_ontology:measurement(tcb_categorical_reading_tr_t2019, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2019, 0.41).
narrative_ontology:measurement_basis(tcb_categorical_reading_tr_t2019, observed).
narrative_ontology:measurement(tcb_categorical_reading_tr_t2025, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2025, 0.43).
narrative_ontology:measurement_basis(tcb_categorical_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tcb_categorical_reading_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.4).
narrative_ontology:measurement_basis(tcb_categorical_reading_be_t1978, observed).
narrative_ontology:measurement(tcb_categorical_reading_be_t1982, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1982, 0.42).
narrative_ontology:measurement_basis(tcb_categorical_reading_be_t1982, observed).
narrative_ontology:measurement(tcb_categorical_reading_be_t1992, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1992, 0.44).
narrative_ontology:measurement_basis(tcb_categorical_reading_be_t1992, observed).
narrative_ontology:measurement(tcb_categorical_reading_be_t1998, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1998, 0.46).
narrative_ontology:measurement_basis(tcb_categorical_reading_be_t1998, observed).
narrative_ontology:measurement(tcb_categorical_reading_be_t2005, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement_basis(tcb_categorical_reading_be_t2005, observed).
narrative_ontology:measurement(tcb_categorical_reading_be_t2012, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2012, 0.5).
narrative_ontology:measurement_basis(tcb_categorical_reading_be_t2012, observed).
narrative_ontology:measurement(tcb_categorical_reading_be_t2019, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2019, 0.52).
narrative_ontology:measurement_basis(tcb_categorical_reading_be_t2019, observed).
narrative_ontology:measurement(tcb_categorical_reading_be_t2025, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2025, 0.53).
narrative_ontology:measurement_basis(tcb_categorical_reading_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(takings_clause_boundary__categorical_takings_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the takings clause' decomposes into three structurally distinct constraints sharing one kernel (takings_clause_boundary). The categorical reading (this file) sits between its siblings: it inherits the physical_appropriation_reading's physical-occupation core (upstream, higher empirical confidence, cited as settled ground) and absorbs the regulatory_takings_reading's value-diminution concern into the Penn Central middle (downstream, contested, where the compensation fights actually occur). The sibling readings are separate stories with their own epsilon, victim sets, and classifications; this file's epsilon (0.53) is authored for the two-track arrangement only and is not averaged across readings. Family members link through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
