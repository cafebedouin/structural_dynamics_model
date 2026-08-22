% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive of Federation Single Market (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   The federation's core legal doctrine treats free movement of labor and
 *   services as constitutive of the single market: any member-state
 *   restriction on labor mobility, work permits, or welfare access is
 *   presumptively illegitimate unless the state can demonstrate a narrow,
 *   overriding justification (typically security or public health). This
 *   constraint instantiates the integration-primary reading of the federation
 *   membership treaty — a commitment that mobility rights are supreme and
 *   national regulatory authority is subordinate. The reading benefits mobile
 *   workers and multinational employers (arbitrage is the core beneficiary
 *   function) while imposing costs on incumbent resident workers in protected
 *   sectors, local labor markets that lose rents under open supply, and
 *   national welfare systems that cannot condition access on origin or
 *   contribution history. The constraint operates as a hybrid: it coordinates
 *   a genuine single-market function (solves inefficiencies from fragmented
 *   labor markets) while simultaneously extracting rents from the immobile.
 *   The claim/metric gap is deliberate: the constraint's formal
 *   classification is as coordination (rope) within the integration-primary
 *   reading's own framework, while the authored metrics describe
 *   substantially extractive operation with high suppression of national
 *   alternatives — the engine measures that divergence from the reading's
 *   perspective.
 *
 * KEY AGENTS:
 *   - mobile_workers: primary beneficiaries (access, arbitrage opportunity)
 *   - multinational_employers: secondary beneficiary (labor-cost leverage)
 *   - local_labor_markets: primary victim (wage suppression in open sectors)
 *   - national_welfare_systems: victim (fiscal pressure from eligibility requirements)
 *   - incumbent_resident_workers: victim (powerless, trapped, wage-suppressed)
 *   - member_state_governments: agenda-setter, simultaneously payer (identity-locked into enforcement despite electoral pressure)
 *   - supranational federation authority: interpreter/enforcer (derives authority from the reading itself)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.68).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.79).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive of Federation Single Market (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, 'b8d566df-2f1c-490d-9181-20e635c2c328').
narrative_ontology:cs_kernel_codification('b8d566df-2f1c-490d-9181-20e635c2c328', formalized).
narrative_ontology:cs_authority_grounding('b8d566df-2f1c-490d-9181-20e635c2c328', extraction).
narrative_ontology:cs_interpretation_layer_present('b8d566df-2f1c-490d-9181-20e635c2c328').
narrative_ontology:cs_reading_relation('b8d566df-2f1c-490d-9181-20e635c2c328', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('b8d566df-2f1c-490d-9181-20e635c2c328', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('b8d566df-2f1c-490d-9181-20e635c2c328', foundational, free_movement_constitutive_of_single_market).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_single_market, holdable).
narrative_ontology:cs_axiom_grounding('b8d566df-2f1c-490d-9181-20e635c2c328', free_movement_constitutive_of_single_market, instrumental).
narrative_ontology:cs_axiom('b8d566df-2f1c-490d-9181-20e635c2c328', foundational, national_restrictions_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(national_restrictions_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('b8d566df-2f1c-490d-9181-20e635c2c328', national_restrictions_presumptively_illegitimate, deontological).
narrative_ontology:cs_axiom('b8d566df-2f1c-490d-9181-20e635c2c328', secondary, treaty_supremacy_doctrine).
narrative_ontology:cs_axiom_status(treaty_supremacy_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('b8d566df-2f1c-490d-9181-20e635c2c328', treaty_supremacy_doctrine, conventional).
narrative_ontology:cs_reference_frame('b8d566df-2f1c-490d-9181-20e635c2c328', federation_single_market_supremacy).
narrative_ontology:cs_drift_state('b8d566df-2f1c-490d-9181-20e635c2c328', contemporary_electoral_backlash_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b8d566df-2f1c-490d-9181-20e635c2c328', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, multinational_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, consumers_in_destination_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, incumbent_resident_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, consumers_in_destination_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain unrestricted access to labor markets across all federation member territories. Can arbitrage wages, working conditions, and career progression without national licensing barriers or residence permits. Exit consists of mobility itself — the constraint guarantees the exit option that defines their structural position. Protected by treaty supremacy doctrine.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, arbitrage, global).

% Can source labor from federation-wide labor pools without national work permit schemes, labor protections, or employer taxes that vary by origin. Can locate production across member states and move workers to match demand. Benefit from labor-cost arbitrage and regulatory shopping. Protected by reading's supremacy logic.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, multinational_employers, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from expanded service provision (healthcare, childcare, construction, hospitality) by mobile workers willing to work at lower wage points than local workers. Also bear indirect cost through fiscal pressure on public services when the federation's reading constrains member states from conditioning welfare access on contribution history.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, consumers_in_destination_markets, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, consumers_in_destination_markets, payer).

% Experience wage suppression in lower-skill occupations from open labor supply; lose labor-market rents that would accrue under closure. Cannot deploy national labor-market regulation (sectoral wage floors, apprenticeship protections) that would be superseded by free movement rights. Cannot condition hiring on national origin or citizenship.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    moderate, generational, constrained, national).

% Under the integration-primary reading, are obliged to extend benefits to mobile workers on the same terms as residents, irrespective of contribution history, presence duration, or the home member state's reciprocal obligations. Cannot restrict access based on fiscal capacity or national insurance fund sustainability. The reading treats welfare-eligibility restrictions as illegitimate exclusions from the single market.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Face wage suppression and job-displacement risk in sectors open to mobile workers; cannot organize sectoral or occupational protections that would restrict competition; lack exit options (unlike mobile workers, their exit IS the constraint, not freedom). Bear the distributional cost of labor-market integration while benefiting indirectly from consumer-price gains.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, incumbent_resident_workers, payer,
    powerless, biographical, trapped, national).

% Enforce the integration-primary reading against their own electorates' localist pressures. Set and maintain the supremacy doctrine that subordinates member-state labor-market and welfare regulation to free movement rights. Are locked into federation membership (exit costs are civilizational — cultural, economic, security); must administer constraints their electorates increasingly resist. Derive legitimacy from federation membership and lose it on exit.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, member_state_governments, payer).

% Interprets and enforces the integration-primary reading across member states. Overrides member-state restrictions on grounds of treaty supremacy. Derives legitimacy and institutional power from the doctrine that free movement is constitutive, not optional. Has no exit option (it is the analytical seat that embodies the doctrine).
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, supranational_federation_authority, agenda_setter,
    institutional, generational, analytical, global).

% Member-state governments and domestic labor coalitions advocating the sovereignty-primary reading (member states retain authority to condition mobility on labor-market impact). Are structurally excluded from shaping the constraint because the integration-primary reading treats national restrictions as presumptively illegitimate. Would argue for proportionality tests and member-state consent; are out-voted or overridden by treaty interpretation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, sovereignty_primary_coalition, excluded,
    moderate, biographical, constrained, national).

% Interpret and adjudicate whether member-state restrictions meet the 'narrow justification' bar. Currently apply lenient scrutiny to restrictions on free movement and strict scrutiny to member-state protective measures. Serve as both enforcer and legitimacy-source for the integration-primary reading through their interpretation choices.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, judicial_review_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__integration_primary, multinational_employers).
narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: member states gain access to a federation-wide labor pool, employers access unrestricted talent, and consumers benefit from expanded service provision. The coordination function is real — fragmented labor markets produce inefficiencies that a single-market framework does solve.
% TRANSFER_FUNCTION: Moves labor-market rents from incumbent resident workers and national welfare systems to mobile workers and multinational employers. The transfer is substantial: national wage floors, apprenticeship protections, sectoral regulations, and welfare-eligibility conditions are overridden by the supremacy of free movement. Local labor markets absorb the cost of open supply in lower-skill sectors.
% ABSENT_VOICES: Member-state governments under electoral pressure from incumbent workers, domestic labor unions, and welfare-dependent populations. These actors would advocate for the sovereignty-primary or subsidiarity-balance readings but are structurally excluded by treaty supremacy doctrine. Their objections are treated as illegitimate protectionism under the integration-primary reading.
% FOUNDING_PROBLEM: Fragmented labor markets created inefficiencies in post-war European federation: workers could not freely pursue opportunity across borders, employers could not match talent to demand, and economic integration was constrained by national labor-market closure. The integration-primary reading was designed to eliminate these frictions.
% FOUNDING_PROBLEM_CORROBORATION: The supranational authority and multinational employers attest the founding problem is still live and free movement remains economically necessary. Member-state governments and labor organizations, especially in high-unemployment or high-welfare-cost jurisdictions, attest the problem is substantially solved and the reading now persists as rent extraction; academic economists and independent policy analysts (outside both the beneficiary and victim seats) report that measurable wage suppression in lower-skill sectors and fiscal strain on welfare systems indicate the coordination function has been exceeded by extractive rent-collection.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68) reflects the sustained wage suppression in lower-skill sectors and fiscal strain on welfare systems, balanced against genuine single-market efficiency gains. The measurement series shows extractiveness rising from 0.48 to 0.68 over 40 time units, then plateauing: early periods show labor-market adjustment costs as mobility increases; later periods stabilize as the new equilibrium distributes rents from incumbent workers to mobile workers. Suppression (0.79) is high because member states actively defend national labor restrictions (apprenticeship protections, sectoral wage floors, welfare-contribution requirements) that are overridden by treaty supremacy; the suppression is structural (contractual, enforced by courts) and increasingly internalized (member-state judiciaries adopt the integration-primary reading as legitimate doctrine). Theater (0.41) reflects moderate performativity: the supremacy doctrine is genuine legal doctrine, not pure cover, but enforcement activity increasingly focuses on preventing member-state workarounds rather than protecting the coordination function itself. Accessibility collapse (0.72) reflects that alternatives to free movement (national labor-market closure, bilateral work agreements) are structurally impossible within the federation's authority frame; once the integration-primary reading is accepted, exit is identity-locked for member states. Resistance (0.58) reflects sustained electoral pressure in high-unemployment member states and labor-union opposition, but constrained by federation structure that insulates judicial interpretation from democratic pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the supranational authority's and mobile workers' positions, the constraint is legitimate coordination: it solves genuine inefficiencies and benefits parties who contribute to the single market. From the incumbent resident workers' and local labor-market positions, the same structure operates as suppressed extraction: their objections to wage suppression are ruled out of bounds by the supremacy doctrine. From member-state governments' positions, the constraint creates an irresolvable tension: they benefit from federation membership (security, economic integration) while bearing electoral pressure from the victim populations they represent. The engine computes directionality asymmetrically: mobile workers get d near 0.1 (beneficiaries, high arbitrage exit); incumbent resident workers get d near 0.85 (victims, trapped exit); member states get d near 0.6 (caught between enforcement obligation and electoral resistance). This is not a dispute about facts but about which reading's framing governs the federation's authority structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers occupy the pure beneficiary position (d ≈ 0.1): they benefit from the constraint directly (access and arbitrage), have the highest exit options (mobility itself is their exit option), and are protected by treaty supremacy. Multinational employers are secondary beneficiaries (d ≈ 0.15): they benefit from labor-cost arbitrage and regulatory shopping but could relocate if national restrictions were restored. Incumbent resident workers are the pure target (d ≈ 0.85): they bear wage suppression, have the lowest exit options (trapped, identity-locked into local labor markets), and cannot use federation mobility to exit their own immobility. Local labor markets are structural targets (d ≈ 0.8): their rents are extracted by open supply and they cannot deploy national protections. National welfare systems are institutional targets (d ≈ 0.75): they bear fiscal pressure from non-contributory eligibility requirements. Member-state governments occupy an ambiguous position (d ≈ 0.55): they enforce the constraint (d lower toward 0.4 as agenda-setters) but simultaneously bear electoral pressure as the constraint's costs concentrate on their residents (d higher toward 0.65 as trapped enforcers). The supranational authority is not a payer (it derives authority from interpreting the reading correctly) but also not a beneficiary in the extraction sense — it is the keeper of the doctrine and occupies the analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint started as genuine coordination (solving labor-market fragmentation inefficiencies) and has accumulated extractive overlay (using supremacy doctrine to prevent national protections even where efficiency gains are exhausted). The mandatrophy question is whether the founding problem (fragmented labor markets limiting opportunity) remains live or is substantially solved while the constraint persists. At t=0 (early federation), the founding problem was genuinely acute: workers faced visa restrictions, employers faced certification barriers, and labor markets were poorly matched. By t=40 (contemporary), the founding problem is substantially solved in high-income member states (workers move freely, labor matching has improved) but the constraint persists with rising suppression of national workarounds (member states cannot restore labor protections even where local labor markets are saturated). This is the classic mandatrophy pattern: founding function solved, constraint persists as rent extraction. The integration-primary reading prevents member states from even testing whether subsidiarity (proportionality constraints on mobility) would restore welfare gains; they are locked into the supremacy doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_supremacy,
    'Is free movement a natural consequence of economic integration (a mathematical property of single markets), or is it a constructed legal doctrine that benefits mobile actors and imposes costs on immobile ones?',
    'Compare federation histories: do all historical single markets spontaneously produce free movement, or do many single markets operate with internal migration restrictions? Examine whether the integration-primary reading was discovered in treaty language or imposed through judicial interpretation.',
    'If natural law: the reading is a mountain (fixed boundary condition). If constructed: the reading is a snare disguised by naturalness claims, and the beneficiary/victim structure becomes the classification key.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_supremacy, conceptual, 'Whether free movement supremacy is a necessary property of single markets or a constructed doctrine.').

omega_variable(
    welfare_access_ambiguity,
    'Does the integration-primary reading require welfare-eligibility equivalence across member states, or only labor-market access without welfare claims?',
    'Examine case law on welfare eligibility for mobile workers; compare treatment of residence requirements versus service access; test whether member states can condition welfare on contribution history.',
    'If welfare is included: the suppression on national welfare systems is high and the constraint is more extractive. If welfare is excluded: member states retain fiscal autonomy and the constraint is narrower in scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_access_ambiguity, empirical, 'Whether the constraint extends to welfare access or is limited to labor-market entry.').

omega_variable(
    sovereignty_primary_foreclosure,
    'Does the integration-primary reading logically foreclose the sovereignty-primary reading, or do they coexist as competing legitimate interpretations within the federation''s authority structure?',
    'Examine whether member states retain any reserved powers under the integration-primary reading, or whether treaty supremacy doctrine fully occupies the field of labor mobility.',
    'If foreclosed: the sovereignty-primary reading is a defeated alternative and cannot live alongside the integration-primary reading in the same framework. If coexisting: the readings are a genuine kernel contest and the federation contains an unresolved contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_primary_foreclosure, conceptual, 'Whether the two readings of the federation membership treaty are logically incompatible or merely competing.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of national labor restrictions structural (member states are contractually barred and face enforcement mechanisms) or internalized (member state officials and judiciaries have adopted the integration-primary reading as legitimate)?',
    'Monitor exit costs for member states attempting to restrict mobility; examine whether internal enforcement comes from external treaty bodies or from member-state judicial adoption of supremacy doctrine; test whether suppression persists if treaty enforcement were suspended.',
    'If structural: suppression is reversible; member states could exit or reform without changing minds. If internalized: suppression persists even absent external enforcement; the reading has become self-policing and more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of national restrictions is external/contractual or internal/ideological.').

omega_variable(
    incumbent_worker_coalition_power,
    'Can incumbent resident workers and national labor unions form a coalition powerful enough to reshape the constraint toward the sovereignty-primary or subsidiarity-balance readings, despite their individual powerlessness in the federation structure?',
    'Track electoral pressure in member states with high unemployment or wage suppression; examine whether national governments can coordinate on reading reinterpretation; test whether constitutional amendments or treaty revision become politically viable.',
    'If coalition-forming succeeds: the constraint may shift toward subsidiarity balancing or sovereignty recovery. If blocked: incumbent workers remain trapped and the integration-primary reading persists indefinitely despite mass resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incumbent_worker_coalition_power, empirical, 'Whether powerless incumbent workers can organize effective resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(fede_tr_t5, federation_membership_treaty__integration_primary, theater_ratio, 5, 0.26).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__integration_primary, theater_ratio, 10, 0.31).
narrative_ontology:measurement(fede_tr_t15, federation_membership_treaty__integration_primary, theater_ratio, 15, 0.36).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__integration_primary, theater_ratio, 20, 0.39).
narrative_ontology:measurement(fede_tr_t25, federation_membership_treaty__integration_primary, theater_ratio, 25, 0.4).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__integration_primary, theater_ratio, 30, 0.41).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(fede_be_t5, federation_membership_treaty__integration_primary, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__integration_primary, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(fede_be_t15, federation_membership_treaty__integration_primary, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__integration_primary, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(fede_be_t25, federation_membership_treaty__integration_primary, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__integration_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(fede_su_t5, federation_membership_treaty__integration_primary, suppression_requirement, 5, 0.69).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__integration_primary, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(fede_su_t15, federation_membership_treaty__integration_primary, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__integration_primary, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(fede_su_t25, federation_membership_treaty__integration_primary, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__integration_primary, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.79).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(fede_grid_01, federation_membership_treaty__integration_primary, accessibility_collapse(class), 0, 0.71).
narrative_ontology:measurement(fede_grid_02, federation_membership_treaty__integration_primary, accessibility_collapse(class), 40, 0.75).
narrative_ontology:measurement(fede_grid_03, federation_membership_treaty__integration_primary, accessibility_collapse(individual), 0, 0.74).
narrative_ontology:measurement(fede_grid_04, federation_membership_treaty__integration_primary, accessibility_collapse(individual), 40, 0.78).
narrative_ontology:measurement(fede_grid_05, federation_membership_treaty__integration_primary, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(fede_grid_06, federation_membership_treaty__integration_primary, accessibility_collapse(organizational), 40, 0.68).
narrative_ontology:measurement(fede_grid_07, federation_membership_treaty__integration_primary, accessibility_collapse(structural), 0, 0.65).
narrative_ontology:measurement(fede_grid_08, federation_membership_treaty__integration_primary, accessibility_collapse(structural), 40, 0.72).
narrative_ontology:measurement(fede_grid_09, federation_membership_treaty__integration_primary, resistance(class), 0, 0.71).
narrative_ontology:measurement(fede_grid_10, federation_membership_treaty__integration_primary, resistance(class), 40, 0.68).
narrative_ontology:measurement(fede_grid_11, federation_membership_treaty__integration_primary, resistance(individual), 0, 0.48).
narrative_ontology:measurement(fede_grid_12, federation_membership_treaty__integration_primary, resistance(individual), 40, 0.42).
narrative_ontology:measurement(fede_grid_13, federation_membership_treaty__integration_primary, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(fede_grid_14, federation_membership_treaty__integration_primary, resistance(organizational), 40, 0.58).
narrative_ontology:measurement(fede_grid_15, federation_membership_treaty__integration_primary, resistance(structural), 0, 0.51).
narrative_ontology:measurement(fede_grid_16, federation_membership_treaty__integration_primary, resistance(structural), 40, 0.48).
narrative_ontology:measurement(fede_grid_17, federation_membership_treaty__integration_primary, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(fede_grid_18, federation_membership_treaty__integration_primary, stakes_inflation(class), 40, 0.71).
narrative_ontology:measurement(fede_grid_19, federation_membership_treaty__integration_primary, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(fede_grid_20, federation_membership_treaty__integration_primary, stakes_inflation(individual), 40, 0.67).
narrative_ontology:measurement(fede_grid_21, federation_membership_treaty__integration_primary, stakes_inflation(organizational), 0, 0.59).
narrative_ontology:measurement(fede_grid_22, federation_membership_treaty__integration_primary, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(fede_grid_23, federation_membership_treaty__integration_primary, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(fede_grid_24, federation_membership_treaty__integration_primary, stakes_inflation(structural), 40, 0.61).
narrative_ontology:measurement(fede_grid_25, federation_membership_treaty__integration_primary, suppression(class), 0, 0.73).
narrative_ontology:measurement(fede_grid_26, federation_membership_treaty__integration_primary, suppression(class), 40, 0.84).
narrative_ontology:measurement(fede_grid_27, federation_membership_treaty__integration_primary, suppression(individual), 0, 0.61).
narrative_ontology:measurement(fede_grid_28, federation_membership_treaty__integration_primary, suppression(individual), 40, 0.71).
narrative_ontology:measurement(fede_grid_29, federation_membership_treaty__integration_primary, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(fede_grid_30, federation_membership_treaty__integration_primary, suppression(organizational), 40, 0.81).
narrative_ontology:measurement(fede_grid_31, federation_membership_treaty__integration_primary, suppression(structural), 0, 0.61).
narrative_ontology:measurement(fede_grid_32, federation_membership_treaty__integration_primary, suppression(structural), 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__integration_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This story is part of a constraint family decomposing the federation_membership_treaty kernel into three structurally distinct readings. The integration-primary reading treats free movement as constitutive of the single market and member-state restrictions as presumptively illegitimate (high extraction from local labor markets and welfare systems). The sovereignty-primary reading treats free movement as conditional on member-state consent and reserves national labor-market authority. The subsidiarity-balance reading treats free movement as operating within proportionality bounds set by legitimate national interests. Each reading instantiates a different constraint with different beneficiary/victim structures, different suppression profiles, and different classifications. The readings are linked via network.affects_constraints to enable cross-reading analysis of authority contestation and constraint family dynamics. The integration-primary reading influences (but does not foreclose) the other two: it sets the baseline supremacy doctrine that the other readings push back against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, powerless, 0.85).
constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, moderate, 0.75).
constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
