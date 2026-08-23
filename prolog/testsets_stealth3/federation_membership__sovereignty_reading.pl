% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty — Sovereignty Reading (Mobility Carve-Out Regime)
 *   domain: political/economic/federalism-migration
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_reading of the
 *   federation_membership kernel: membership understood as a conditional
 *   treaty in which national authority retains border legitimacy and free
 *   movement is negotiable policy. The standing arrangement under contest —
 *   and the fixed ε referent for this file — is the actual operating regime:
 *   a federation-wide mobility framework continuously qualified by nationally
 *   negotiated carve-outs (opt-out protocols, accession transition periods,
 *   safeguard clauses, 'temporary' internal border controls that renew
 *   indefinitely). Under this arrangement, mobile citizens bear permit
 *   burdens, waiting periods, credential gaps, and revocable access;
 *   protected local labor markets collect reduced-competition margins during
 *   every restriction window; national governments convert the negotiability
 *   itself into bargaining stock and electoral credit. Per the reading's own
 *   lights, border control is legitimate governance — the reading does not
 *   count the enforcement apparatus as illegitimate coercion — so the
 *   extraction it registers is the price mobile citizens pay for access that
 *   is conceded rather than guaranteed. The ε-invariance decomposition rule
 *   applies: the sibling integration_reading of the same kernel is a separate
 *   story with a different seat map and ε, linked via
 *   network.affects_constraints. The claim and the metrics are independently
 *   authored facts: the claim (tangled_rope) reflects my structural read — a
 *   genuine coalition-preserving coordination function fused, in the same
 *   enforcement machinery, with asymmetric costs falling on identifiable
 *   movers — while the metrics report the arrangement's observed operation.
 *
 * KEY AGENTS:
 *   - - national_governments: Agenda setter (institutional/arbitrage) — negotiates and enforces mobility carve-outs, converting them into electoral credit and bargaining leverage
 *   - - protected_local_labor_markets: Primary beneficiary (organized/constrained) — collects protection rents whenever movement is throttled
 *   - - mobile_citizens: Primary target (moderate/constrained) — bears permit burdens, waiting periods, and revocable access
 *   - - acceding_state_jobseekers: Secondary target (powerless/constrained) — assigned second-tier status during accession transition windows
 *   - - frontier_region_economies: Dual-positioned seat (moderate/constrained) — benefits from the general regime, absorbs closure shocks
 *   - - supranational_commission: Counter-agenda setter (institutional/constrained) — drafts the openness side of each bargain and usually loses
 *   - - supranational_court: Adjudicative seat (institutional/analytical) — strikes down discriminatory measures short of the negotiated exemptions
 *   - - future_mobile_cohorts: Excluded voice (powerless/trapped) — inherits each re-cut of the terms without a seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.72).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.6).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty — Sovereignty Reading (Mobility Carve-Out Regime)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political/economic/federalism-migration").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '8573b586-e93c-41c5-a3b1-b66b4a037cbc').
narrative_ontology:cs_kernel_codification('8573b586-e93c-41c5-a3b1-b66b4a037cbc', fixed_text).
narrative_ontology:cs_authority_grounding('8573b586-e93c-41c5-a3b1-b66b4a037cbc', lineage).
narrative_ontology:cs_interpretation_layer_present('8573b586-e93c-41c5-a3b1-b66b4a037cbc').
narrative_ontology:cs_reading_relation('8573b586-e93c-41c5-a3b1-b66b4a037cbc', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('8573b586-e93c-41c5-a3b1-b66b4a037cbc', foundational, free_movement_extent_set_by_intergovernmental_consent).
narrative_ontology:cs_axiom_status(free_movement_extent_set_by_intergovernmental_consent, holdable).
narrative_ontology:cs_axiom_grounding('8573b586-e93c-41c5-a3b1-b66b4a037cbc', free_movement_extent_set_by_intergovernmental_consent, conventional).
narrative_ontology:cs_axiom('8573b586-e93c-41c5-a3b1-b66b4a037cbc', foundational, border_control_authority_rests_nationally).
narrative_ontology:cs_axiom_status(border_control_authority_rests_nationally, holdable).
narrative_ontology:cs_axiom_grounding('8573b586-e93c-41c5-a3b1-b66b4a037cbc', border_control_authority_rests_nationally, deontological).
narrative_ontology:cs_reference_frame('8573b586-e93c-41c5-a3b1-b66b4a037cbc', conditional_treaty_compact).
narrative_ontology:cs_drift_state('8573b586-e93c-41c5-a3b1-b66b4a037cbc', post_crisis_restriction_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8573b586-e93c-41c5-a3b1-b66b4a037cbc', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, protected_local_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, acceding_state_jobseekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, frontier_region_economies).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, frontier_region_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer the terms: they negotiate exemption protocols at summits, legislate permit and quota schemes, run or suspend border checks, and renew 'temporary' controls at their discretion. Each government answers to a domestic electorate that rewards visible border control, and uses the credible threat of exit or veto to win mobility concessions from partners. What flows to them is discretion and electoral credit; what it costs them is partner friction and administration budgets.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, national_governments, beneficiary).

% Incumbent firms and established workforces in sheltered sectors — construction, seasonal agriculture, licensed trades, public employment. When movement is throttled through transition periods, permit gates, or quota caps, they face thinner labor-market competition and can hold wage and price positions they would otherwise lose. Their capital and skills are place-bound, so they lobby for renewal of every restriction window rather than seek opportunity elsewhere.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, protected_local_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% Citizens who live, work, study, or retire across member-state lines. They carry the arrangement's costs: permit queues and fees, credential-recognition gaps, waiting periods before equal treatment, residence rights that lapse with policy moods, and family separations at enforcement frontiers. Their livelihoods already span borders, so pulling out means dismantling homes and careers; staying means accepting that access is a concession up for renegotiation rather than an entitlement.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, constrained, continental).

% Workers from newly admitted member states during the transition windows written into their accession terms. They arrive — if they arrive — into designated second-tier status: work permits, sector exclusions, longer qualifying periods for benefits and family reunion. They have courts they can petition and home governments that protest, but no votes in the councils that wrote their exclusion, and their mobility was traded as an accession concession.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, acceding_state_jobseekers, payer,
    powerless, biographical, constrained, continental).

% Border towns and regions running on daily commuter flows — health care, retail, construction staffing across lines. Openness is their operating condition; closures during crises cut off half their workforce overnight. They benefit from the general regime yet absorb the shocks whenever a neighbor exercises its control prerogatives, and they hold no lever over the decisions that close the bridges they live from.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, frontier_region_economies, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, frontier_region_economies, payer).

% The federation's executive arm: it drafts mobility-rights legislation, monitors compliance, opens infringement proceedings against discriminatory national measures, and proposes packages trading funds for openness. Its proposals advance only where member states consent, so on mobility carve-outs it repeatedly drafts the losing side of the bargain; its leverage is agenda-setting and litigation, not command.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_commission, agenda_setter,
    institutional, generational, constrained, continental).

% The federation's judicial organ. It hears cases brought by mobile citizens and employers challenging national permit schemes, sector exclusions, and unequal treatment, and its rulings have struck down the most nakedly discriminatory restrictions. It adjudicates within the treaty text the states themselves wrote — including the exception protocols — so its corrections stop at the edge of the negotiated exemptions.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_court, observer,
    institutional, generational, analytical, continental).

% Younger residents and not-yet-mobile citizens whose future schooling, work, and family formation will unfold inside whatever mobility terms survive today's negotiations. They attend no summit, hold no permit scheme, cast no vote in the councils that keep re-cutting the terms their adult lives will run on; they inherit each successive carve-out as settled fact.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, future_mobile_cohorts, excluded,
    powerless, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, protected_local_labor_markets).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a heterogeneous set of member states inside one treaty organization: by making mobility exposure negotiable per state, the arrangement converts potential outright defection into bounded, renewable exemptions, preserving the shared goods, capital, and services markets every member values. The flexibility also gives each government a domestic-facing instrument for absorbing migration shocks without breaking the wider compact.
% TRANSFER_FUNCTION: Converts mobility from entitlement into concession: security of access is transferred away from mobile citizens and vested in national governments as discretionary bargaining stock; permit fees and compliance burdens flow from movers to state administrations; restricted labor supply transfers wage and pricing margin to incumbents in sheltered sectors; electoral credit for visible control accrues to sitting governments.
% ABSENT_VOICES: Future cohorts of mobile citizens, and mobile citizens themselves at bargaining moments: carve-outs are struck in intergovernmental conferences among sitting governments, and those whose rights are traded hold no seat (modeled as stakeholder future_mobile_cohorts, role excluded). Acceding-state publics likewise learned of their workers' transition periods as negotiated terms, not consulted positions.
% DISAPPEARANCE_RATIONALE: Overnight removal would restore unconditional movement across the whole federation: permit schemes and transition statuses lapse, protected sectors meet immediate labor competition and wage compression, border infrastructures idle, frontier regions boom, and restriction-leaning governments face immediate domestic backlash — several would invoke emergency clauses or pursue exit, so the wider compact itself would be renegotiated within months.
% FOUNDING_PROBLEM: The founders needed to bind together states whose publics differed sharply in tolerance for inward movement, without letting that difference dissolve the common market. Membership terms therefore carried mobility obligations qualified by negotiated exemptions — opt-out protocols, accession transition periods, safeguard clauses — so that reluctant states would sign at all.
% FOUNDING_PROBLEM_CORROBORATION: Attested outside the benefiting set by accession-treaty archives and the contemporaneous objections of acceding-state governments, which accepted worker transition periods as the price of entry; by treaty-history scholarship documenting the origin of the opt-out protocols; and by the recurring, unprompted demand for new exemptions from every subsequent entrant — a pattern no current beneficiary needed to manufacture. Brexit additionally demonstrates by revealed preference that at least one major member treated membership as terminable and mobility as severable.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because access to the federation's core freedom is priced as a concession: permit regimes, sector exclusions, unequal-treatment waiting periods, and revocability all land on movers whose cross-border lives are already sunk. Suppression is authored at 0.6 and is deliberately below what a rights-lens account would assign: per this reading's own lights, border enforcement is legitimate governance, so what counts as suppression is the residual — the absence of any jurisdiction offering unconditional mobility, and the inability of individuals (as opposed to states) to exit the negotiability itself. Suppression is a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream. Accessibility collapse is moderate (0.5): moving anyway remains possible and most movers do, but the alternative of guaranteed mobility exists nowhere for them, and litigation recovers only the non-exempt margin. Resistance is moderate (0.55): court challenges have real wins, employer coalitions lobby for openness, and foot-voting disciplines extreme closures. Theater ratio (0.31) tracks the growing symbolic layer — 'temporary' controls renewed dozens of times past their stated purpose, safeguard clauses never triggered, sovereignty performances with thin operational yield — while the permit and enforcement core remains functionally real. The temporal series run on one shared grid (T=0..30 in five-year steps, anchored to the mid-1990s consolidation of the mobility framework through the post-crisis restriction era); the dynamic is a crisis ratchet rather than oscillation: each shock (eastern enlargement, the 2015 migration wave, the pandemic closures) leaves permanent additions that no calm period removes, which is why all three series rise monotonically.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the national_governments seat the arrangement presents as prudential coalition management — flexibility that kept reluctant publics inside the treaty and absorbed shocks without rupture; that seat should compute toward the coordination-heavy end. From the mobile_citizen and acceding_state_jobseeker seats the identical machinery presents as enforced concession — access held hostage to periodic renegotiation, with real costs in wages, family life, and planning security; those seats should compute toward the extraction-heavy end. Frontier region economies experience whiplash: net winners in calm periods, casualties of every exercise of the control prerogative. The supranational commission experiences persistent agenda-setting defeat, and the excluded cohort experiences the arrangement purely as an inherited ceiling. Latent coalition power deserves note: mobile citizens are numerous, geographically concentrated in frontier regions and destination cities, and backed by employer interests — their weak organization, not their size, is what keeps the payer seats computationally divided.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: protected_local_labor_markets sit near the subsidy end (place-bound assets, organized lobbying, direct rent capture during restriction windows). national_governments are dual-positioned — declared beneficiary (discretion and electoral credit accrue to them) while also administering the machinery; the derived d should sit low-to-moderate, reflecting net collection rather than net payment. mobile_citizens and acceding_state_jobseekers sit near the full-target end, amplified by constrained exit: their cross-border lives are sunk costs, and no jurisdiction offers them the unconditional-mobility alternative, so arbitrage-grade exit is unavailable precisely to the people with the most mobility. frontier_region_economies average toward symmetry with shock asymmetry. The commission and court are not declared in either structural list; their seats fall to fallback handling, and no directionality overrides are authored because the beneficiary/victim-plus-exit derivation already reproduces the true relationships for every declared seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling in both directions. Read through the states' framing alone, the carve-out regime looks like pure coordination — indispensable flexibility, nothing to see; the victim declarations block that error by forcing the mover-seat costs into the ledger. Read through the advocates' framing alone, it looks like pure rights-violation — cover story, nothing else; the coordination analysis blocks that error by establishing that the negotiability mechanism demonstrably retains members who would otherwise defect (the Brexit rupture is the counterfactual witness of what the valve was worth). Tangled_rope holds both halves: genuine coordination function, asymmetric extraction through the same structure, active enforcement required to maintain the exemptions. The arrangement is not a piton: enforcement is real, parties profit visibly, and the founding problem — heterogeneous domestic consent for mobility — remains live, generating new carve-outs at every enlargement and shock, so no mandatrophy resolution is declared. Growing theater_ratio signals an accumulating performative layer worth monitoring, but the functional core has not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_of_federation_kernel,
    'This constraint instantiates the sovereignty_reading of kernel federation_membership; how would classification and the seat map change under the sibling integration_reading?',
    'Author and compile the sibling story integration_reading over the same underlying arrangements; compare computed per-seat types, epsilon, and beneficiary/victim assignments across the two files.',
    'Under the integration reading, free movement is a constitutional right and supranational authority is legitimate: mobile citizens shift toward the beneficiary set, the restriction apparatus becomes the extractive surface, border control loses its legitimacy warrant, and the claimed type for the same arrangements likely changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_of_federation_kernel, conceptual, 'Committer structure: this story is one reading of a contested membership kernel; the sibling reading re-keys the entire seat map.').

omega_variable(
    disagreement_location_within_kernel,
    'Where exactly do the two readings locate their disagreement — the reversibility of membership, the legitimate locus of border authority, or the legal-constitutional status of free movement?',
    'Test which element moves outcomes: observe whether states that concede supranational authority in trade and competition policy nonetheless refuse it for borders (locating the dispute in movement''s legal status), and track whether court rulings or treaty-amendment attempts shift state behavior on mobility specifically.',
    'If the binding disagreement is movement''s legal status, remedies run through treaty amendment and court doctrine rather than reversibility or authority-locus arguments; both readings'' classifications stabilize only when that element resolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_within_kernel, conceptual, 'Locating the structural element on which sibling readings of the kernel diverge.').

omega_variable(
    carve_out_valve_genuineness,
    'Is the negotiability mechanism genuinely load-bearing for federation cohesion — does its availability actually prevent exits and preserve the wider treaty — or is that cohesion function mostly a cover narrative for protection rents?',
    'Counterfactual and comparative analysis of rupture episodes: whether opt-out availability preceded memberships retained that would otherwise have ended, and what the failed valve of the Brexit rupture reveals about the value of the successful ones.',
    'If the valve is cover, the arrangement trends toward pure extraction and the claimed tangled_rope collapses toward snare; if load-bearing, the coordination half of the hybrid is confirmed and the cost of removal assessments rise accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carve_out_valve_genuineness, empirical, 'Whether negotiability''s coalition-preserving function is real or cover.').

omega_variable(
    protection_rent_asymmetry,
    'How do the wage and pricing rents captured by protected sectors during restriction windows compare with the mobility costs borne by mobile citizens across the same windows?',
    'Sectoral wage and price studies exploiting staggered transition-period endings across member states as natural experiments, paired with survey-based costing of permit burdens and forgone mobility for movers.',
    'Large rents against modest mover costs strengthen the extraction reading of the hybrid; the reverse supports reading restrictions as expensive-but-genuine insurance purchased by host communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_rent_asymmetry, empirical, 'Quantifying which side of the arrangement captures net value.').

omega_variable(
    realized_vs_anticipated_costs,
    'Is the dominant cost to mobile citizens realized restriction (permits, closures, waiting periods actually encountered) or anticipatory insecurity (life plans discounted because access is renegotiable)?',
    'Panel data on cross-border life-course decisions — offers declined, family formation deferred, residence investment avoided — compared against observed encounters with actual enforcement, across populations inside and outside transition windows.',
    'Anticipation-dominant costs widen measured extraction beyond what enforcement statistics show and make the arrangement harder to dislodge, since no single reform removes the discount; realization-dominant costs localize it in specific instruments amenable to targeted reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(realized_vs_anticipated_costs, empirical, 'Whether the cost to movers lands in enforced incidents or in discounted life plans.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership__sovereignty_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership__sovereignty_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership__sovereignty_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership__sovereignty_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership__sovereignty_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership__sovereignty_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(fede_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership__sovereignty_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership__sovereignty_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership__sovereignty_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership__sovereignty_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership__sovereignty_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership__sovereignty_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(fede_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership__sovereignty_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership__sovereignty_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership__sovereignty_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership__sovereignty_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership__sovereignty_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership__sovereignty_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(fede_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, integration_reading).

% DUAL FORMULATION NOTE:
% Kernel 'federation_membership' decomposes into two readings authored as separate constraint stories per the epsilon-invariance principle. This file is the sovereignty_reading (membership as conditional treaty; national border authority legitimate; free movement as negotiable policy; extraction located in mobility restrictions borne by mobile citizens for the benefit of protected labor markets). The sibling integration_reading reads the same kernel as irreversible integration with supranational authority legitimate and free movement as a constitutional right, producing a different epsilon referent, a flipped seat map, and its own claimed type. The upstream/downstream coupling is mutual rather than linear: each reading cites the other's failures as evidence (integrationists cite carve-out abuses; sovereigntists cite court overreach), so the family is linked bidirectionally through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
