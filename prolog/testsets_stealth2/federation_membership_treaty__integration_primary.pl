% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive Market Freedom (Integration-Primary Reading)
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   This story instantiates the integration_primary reading of the
 *   federation_membership_treaty kernel: free movement of persons is
 *   constitutive of the single market, and national restrictions are
 *   presumptively illegitimate unless narrowly justified. The standing
 *   arrangement under contest — the referent for epsilon — is the operative
 *   regime itself: the treaty provisions, the court's case line, and the
 *   commission's enforcement practice, assessed in this reading's own lights,
 *   including the distributional record this reading does not emphasize. The
 *   kernel decomposes into three readings (integration_primary,
 *   sovereignty_primary, subsidiarity_balance) that are separate constraint
 *   stories with their own epsilon values and victim sets; this file authors
 *   only the integration-primary instantiation and links its siblings through
 *   the network block. KEY AGENTS (by structural relationship):
 *   supranational_court — agenda-setter and enforcement organ
 *   (institutional/identity_locked); european_commission — agenda-setter
 *   (institutional/constrained); mobile_workers — primary beneficiary
 *   (moderate/mobile); multinational_employers — principal receipt seat
 *   (powerful/arbitrage); domestic_low_wage_workers — primary target
 *   (powerless/trapped); gateway_public_services — secondary target
 *   (organized/constrained); member_state_governments — dual-positioned
 *   payer-beneficiary (institutional/constrained); labor_sending_states —
 *   beneficiary carrying brain-drain costs (institutional/constrained);
 *   restrictionist_political_movements — excluded seat
 *   (organized/identity_locked); comparative_federalism_analysts — analytical
 *   observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.62).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.78).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive Market Freedom (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '3cf72c2b-346e-48b4-b682-1f272b92a0e7').
narrative_ontology:cs_kernel_codification('3cf72c2b-346e-48b4-b682-1f272b92a0e7', fixed_text).
narrative_ontology:cs_authority_grounding('3cf72c2b-346e-48b4-b682-1f272b92a0e7', lineage).
narrative_ontology:cs_interpretation_layer_present('3cf72c2b-346e-48b4-b682-1f272b92a0e7').
narrative_ontology:cs_reading_relation('3cf72c2b-346e-48b4-b682-1f272b92a0e7', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('3cf72c2b-346e-48b4-b682-1f272b92a0e7', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('3cf72c2b-346e-48b4-b682-1f272b92a0e7', foundational, free_movement_constitutes_single_market).
narrative_ontology:cs_axiom_status(free_movement_constitutes_single_market, holdable).
narrative_ontology:cs_axiom_grounding('3cf72c2b-346e-48b4-b682-1f272b92a0e7', free_movement_constitutes_single_market, instrumental).
narrative_ontology:cs_axiom('3cf72c2b-346e-48b4-b682-1f272b92a0e7', secondary, worker_mobility_is_personal_right_not_concession).
narrative_ontology:cs_axiom_status(worker_mobility_is_personal_right_not_concession, holdable).
narrative_ontology:cs_axiom_grounding('3cf72c2b-346e-48b4-b682-1f272b92a0e7', worker_mobility_is_personal_right_not_concession, deontological).
narrative_ontology:cs_reference_frame('3cf72c2b-346e-48b4-b682-1f272b92a0e7', mobility_as_constitutive_market_freedom).
narrative_ontology:cs_drift_state('3cf72c2b-346e-48b4-b682-1f272b92a0e7', contemporary_restrictionist_backlash, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3cf72c2b-346e-48b4-b682-1f272b92a0e7', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, multinational_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, labor_sending_states).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, domestic_low_wage_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, gateway_public_services).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, member_state_governments).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, labor_sending_states).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, teleological_integration_jurisprudence).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, union_citizenship_market_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hears every challenge to a national measure restricting movement and applies the narrow-justification standard that decides which restrictions survive. Its accumulated case line defines what counts as a permissible limit; striking down national restrictions is not incidental to its role but constitutive of it. Abandoning the case line would mean repudiating the institution's own doctrinal identity.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, supranational_court, agenda_setter,
    institutional, generational, identity_locked, continental).

% Opens infringement proceedings against member states maintaining restrictive measures, issues guidance that narrows the range of acceptable justifications, and publishes mobility statistics framed as market success. Its enforcement agenda is bounded by council politics and shifting political weather in the capitals.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, european_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Exercise the right to seek work and residence in any member state, taking wage uplifts relative to origin-country markets. They carry relocation costs, credential-recognition friction, and discrimination risk personally, but their defining capacity is the one the arrangement guarantees: they can leave any jurisdiction that turns hostile.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Recruit across borders at will, staff sites where labor is cheapest, and rely on elastic cross-border labor supply to cap wage growth at the bottom of destination markets. If any jurisdiction tightens conditions, they can shift hiring or production elsewhere; the arrangement constrains states, not them.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, multinational_employers, beneficiary,
    powerful, generational, arbitrage, global).

% Compete with incoming labor in construction, agriculture, care work, and food processing, where wages and hours compress at the bottom of the distribution. Moving abroad themselves would require language, recognized credentials, and family resources most do not have; immobility is their defining condition, and the arrangement provides no compensation channel for the competition it authorizes.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, domestic_low_wage_workers, payer,
    powerless, biographical, trapped, national).

% Local authorities, schools, clinics, and housing providers in destination regions absorb population inflows faster than funding formulas adjust; waiting lists lengthen and integration costs land on municipal budgets. They lobby for impact funding but hold no seat in the treaty framework that produces the flows they absorb.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, gateway_public_services, payer,
    organized, biographical, constrained, local).

% Formal parties to the treaty and beneficiaries of barrier-free market access, yet every restriction they enact to protect a labor market or welfare system faces presumptive invalidity and possible infringement action. Their regulatory authority over movement is precisely what the arrangement withdraws; recovering it in full means exiting the entire market, a step demonstrated to be economically ruinous and taken once.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, member_state_governments, beneficiary).

% Net origin countries collect remittance inflows, shed unemployment, and gain export demand tied to diaspora networks, while watching younger cohorts — care workers, builders, nurses — depart in numbers that hollow out domestic sectors. Their economies adapt to dependence on continued outflow even as the outflow depletes them.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, labor_sending_states, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, labor_sending_states, payer).

% Parties and campaigns organized around restoring national control over borders and labor markets. Inside the doctrinal framework their core demand is classified in advance as presumptively illegitimate, so they compete in electoral politics but stand outside the legal conversation that actually governs the arrangement; their identity is fused with the demand the framework refuses to hear.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, restrictionist_political_movements, excluded,
    organized, biographical, identity_locked, national).

% Study how federations allocate mobility authority between center and parts, and document the distributional record of the movement regime across regions and skill bands. They hold analytic standing only: no revenue flows to them from the arrangement and no restriction lands on them.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, comparative_federalism_analysts, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__integration_primary, multinational_employers).
narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation of national labor markets inside a common market: matches workers to cross-border demand without negotiating a bilateral corridor for each flow, gives firms a continent-wide hiring pool, and removes the unilateral veto point by which any single state could price other members' factors out of the market.
% TRANSFER_FUNCTION: Moves labor from labor-abundant to labor-scarce regions; concomitantly moves wage-bargaining leverage at the bottom of destination labor markets from resident workers to employers, and moves the costs of absorption — housing, schooling, health, translation — onto gateway-region budgets while the returns accrue to mobile workers, employers, and aggregate output.
% ABSENT_VOICES: Holders of the sovereignty_primary reading — national legislatures and publics asserting authority over their own labor markets — are structurally absent from the doctrinal conversation: within this reading's framework their core demand is defined as presumptively illegitimate before argument begins. Gateway-region residents bear the concentrated costs but negotiate at municipal distance from the treaty table where the flows were designed.
% DISAPPEARANCE_RATIONALE: Border controls and work-permit regimes would return within months; staffing models in agriculture, care, construction, and food processing would break until rewired to domestic or third-country labor; remittance-dependent regions would lose a major income stream; and the single market's standing as more than a goods agreement would collapse. The rearrangement would be large, fast, and unevenly distributed.
% FOUNDING_PROBLEM: Post-war national economic closure: fragmented labor markets behind border controls, bilateral guest-worker regimes negotiated corridor by corridor, and a common market that existed on paper for goods but not for people. Free movement was built to make the market real by removing the national veto over factor mobility.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and the federation's own founding documents attest the original problem was real: post-war labor markets were closed and bilaterally rationed. On current status, labor economists studying gateway-region wage and service effects — attesting from outside the benefiting parties — find the original fragmentation problem substantially solved while distributional costs persist; no source outside the beneficiary set supports the claim that the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 because genuine coordination value and concentrated uncompensated costs coexist in the same structure: the regime solved a real collective-action problem and simultaneously transfers bottom-end bargaining leverage to employers while dumping absorption costs on immobile parties. The temporal series shows accumulation rather than steady state — extraction roughly tripled over the interval as enlargement scaled flows faster than gateway adjustment capacity, a trajectory consistent with rent layering onto coordination. Suppression is high (0.78) and is a raw structural property, unscaled by power or scope: the arrangement persists by striking down national measures, not by participant consensus — the alternative (restored state control) is suppressed by design. Theater ratio (0.35) reflects proportionality review that does real work at the margins while increasingly certifying foregone conclusions. Accessibility_collapse (0.60) is partial: exception categories exist (public-policy grounds, transitional accession controls) but each invocation must survive review, so understanding the doctrine collapses most restriction strategies without eliminating all of them. Resistance (0.70) is sustained and organized: infringement fights, safeguard-clause demands, and one full exit. Coalition potential among the powerless victims is blunted by the same immobility that traps them — dispersed across regions and sectors with no natural focal point, unlike geographically concentrated gateway interests, which do organize but lack standing in the treaty forum. All three metric series run on one shared eight-point grid spanning 1968-2025 (t=0 Regulation-era codification; t=24 Maastricht citizenship; t=32-40 eastern enlargement and the lifting of transitional controls; t=48-57 the backlash era).
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the mobile-worker seat the arrangement computes as liberation — a subsidy delivered through enforceable rights and backed by literal mobility of exit. From the trapped domestic-worker seat the same structure operates as extraction with no exit and no compensation channel. From the court's seat it is constitutional necessity: the market cannot exist without the freedom, so restriction review is housekeeping, not coercion. From the member-state seat it is constrained sovereignty — formal treaty consent purchased at the price of regulatory authority, with exit priced beyond reach. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: mobile_workers sit near the beneficiary end (the constraint subsidizes them and grants arbitrage over jurisdictions), multinational_employers sit nearest it (arbitrage-grade exit — they can relocate around any tightening), and labor_sending_states derive net benefit tempered by brain-drain costs. Victim declarations drive high directionality: domestic_low_wage_workers approach the full-target end because trapped exit amplifies extraction — their immobility is the precise condition the arrangement monetizes; gateway_public_services bear concentrated, unfunded costs with constrained exit; member_state_governments carry high d despite formal treaty consent because the thing extracted from them is regulatory authority itself, with their secondary beneficiary role tempering but not reversing the position. Continental spatial scope amplifies effective extraction modestly through verification difficulty; suppression enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents two symmetric errors. Calling the regime a pure snare erases the real coordination function: fragmented post-war labor markets were a genuine collective-action problem, the founding problem is historically corroborated, and the disappearance verdict confirms the world would rearrange without the arrangement. Calling it a pure rope erases the asymmetric extraction: identifiable victims, accumulating extraction, active enforcement against the alternative, and a receipt seat that captures durable surplus. The founding-problem interview keeps the hybrid honest — status contested rather than dead, so no zombie flag fires, but the corroboration requirement forces acknowledgment that the original problem is largely solved and the arrangement now generates its own cost structure. The theater-ratio trajectory tracks proportionality review drifting toward certification without claiming full theatricality: the function has degraded at the margin, not atrophied, so piton is not warranted. Identity-lock dynamics matter at two seats: the court has institutionally become its free-movement jurisprudence (abandoning the case line would dissolve its authority), and restrictionist movements are ideologically fused with the demand the framework refuses to hear — both locks stabilize the current configuration regardless of its performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the extraction profile authored here belong to the free-movement arrangement itself, or to the integration-primary lens through which this story reads the federation_membership_treaty kernel?',
    'Generate the sibling readings (sovereignty_primary, subsidiarity_balance) over the same referent and compare epsilon, victim sets, and computed types; divergence that tracks the reading''s premise rather than the arrangement''s operation locates the disagreement in the lens.',
    'Under sovereignty_primary the victim set relocates — mobile workers lose standing and states regain it — and the type could compute as snare from the mobile-worker seat; under subsidiarity_balance extraction moderates toward rope. The classification of this story is valid only within its reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a contested treaty kernel.').

omega_variable(
    wage_compression_causality,
    'How much of the measured harm to domestic low-wage workers and gateway services is causally attributable to free movement rather than to automation, austerity, housing supply constraints, or global trade?',
    'Difference-in-differences across the staggered lifting of transitional controls at successive enlargements, exploiting phased exposure of otherwise-similar regions.',
    'A small causal share weakens the victim declarations and pulls the computed type toward rope; a large share raises effective extraction toward the snare boundary and strengthens the case for compensating transfers as a condition of the arrangement''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_compression_causality, empirical, 'Causal attribution of gateway-region costs to the mobility regime.').

omega_variable(
    proportionality_review_genuineness,
    'Does the ''narrowly justified'' clause operate as a genuine balancing test or as structured deference to mobility that approves almost no restriction?',
    'Systematic coding of restriction-challenge outcomes: success rates of invoked justifications, and whether outcomes track the quality of the justification or merely the category of measure challenged.',
    'If deference dominates, the authored theater_ratio understates performative maintenance and suppression carries more of the persistence burden than measured; the arrangement sits closer to enforced orthodoxy than to adjudicated balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_review_genuineness, empirical, 'Whether proportionality review is substantive or ceremonial.').

omega_variable(
    compensating_transfer_design_question,
    'Is the absence of automatic fiscal compensation to gateway regions a contingent design choice correctable within the constitutive premise, or inherent to a reading that treats mobility as self-justifying?',
    'Institutional analysis of proposed solidarity instruments — impact funds, correction mechanisms, funded integration obligations — and whether any survives enactment without violating the narrow-justification discipline.',
    'If correctable, the arrangement can shed much of its extraction while remaining a coordination mechanism; if inherent, the extraction is structural to the reading and the tangled-rope classification hardens toward its extractive pole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compensating_transfer_design_question, preference, 'Whether gateway compensation is compatible with the constitutive-mobility premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__integration_primary, theater_ratio, 8, 0.11).
narrative_ontology:measurement_basis(fede_tr_t8, observed).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__integration_primary, theater_ratio, 16, 0.15).
narrative_ontology:measurement_basis(fede_tr_t16, observed).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__integration_primary, theater_ratio, 24, 0.2).
narrative_ontology:measurement_basis(fede_tr_t24, observed).
narrative_ontology:measurement(fede_tr_t32, federation_membership_treaty__integration_primary, theater_ratio, 32, 0.26).
narrative_ontology:measurement_basis(fede_tr_t32, observed).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(fede_tr_t40, observed).
narrative_ontology:measurement(fede_tr_t48, federation_membership_treaty__integration_primary, theater_ratio, 48, 0.33).
narrative_ontology:measurement_basis(fede_tr_t48, observed).
narrative_ontology:measurement(fede_tr_t57, federation_membership_treaty__integration_primary, theater_ratio, 57, 0.35).
narrative_ontology:measurement_basis(fede_tr_t57, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__integration_primary, base_extractiveness, 8, 0.27).
narrative_ontology:measurement_basis(fede_be_t8, observed).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__integration_primary, base_extractiveness, 16, 0.34).
narrative_ontology:measurement_basis(fede_be_t16, observed).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__integration_primary, base_extractiveness, 24, 0.41).
narrative_ontology:measurement_basis(fede_be_t24, observed).
narrative_ontology:measurement(fede_be_t32, federation_membership_treaty__integration_primary, base_extractiveness, 32, 0.49).
narrative_ontology:measurement_basis(fede_be_t32, observed).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(fede_be_t40, observed).
narrative_ontology:measurement(fede_be_t48, federation_membership_treaty__integration_primary, base_extractiveness, 48, 0.59).
narrative_ontology:measurement_basis(fede_be_t48, observed).
narrative_ontology:measurement(fede_be_t57, federation_membership_treaty__integration_primary, base_extractiveness, 57, 0.62).
narrative_ontology:measurement_basis(fede_be_t57, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__integration_primary, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(fede_su_t8, observed).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__integration_primary, suppression_requirement, 16, 0.55).
narrative_ontology:measurement_basis(fede_su_t16, observed).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__integration_primary, suppression_requirement, 24, 0.61).
narrative_ontology:measurement_basis(fede_su_t24, observed).
narrative_ontology:measurement(fede_su_t32, federation_membership_treaty__integration_primary, suppression_requirement, 32, 0.67).
narrative_ontology:measurement_basis(fede_su_t32, observed).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(fede_su_t40, observed).
narrative_ontology:measurement(fede_su_t48, federation_membership_treaty__integration_primary, suppression_requirement, 48, 0.76).
narrative_ontology:measurement_basis(fede_su_t48, observed).
narrative_ontology:measurement(fede_su_t57, federation_membership_treaty__integration_primary, suppression_requirement, 57, 0.78).
narrative_ontology:measurement_basis(fede_su_t57, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'free movement in the federation' covers three structurally distinct claims that share one treaty text but assign different victim sets and different epsilon values. This story (integration_primary) authors the constitutive-freedom claim with mobile workers as beneficiaries and immobile domestic workers, gateway services, and state regulatory authority as victims. The sovereignty_primary sibling authors the state-authority claim over the same text — its victim set relocates to mobile workers and its extraction profile inverts. The subsidiarity_balance sibling authors the proportionality claim, with moderated extraction. The upstream/downstream structure runs from this reading outward: decades of integration-primary case law define the default against which both siblings negotiate, so this story links to both. Epsilon differences across the family reflect the readings, not measurement error over one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
