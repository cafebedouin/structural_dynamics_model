% ============================================================================
% CONSTRAINT STORY: vendor_certification_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vendor_certification_capture, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vendor_certification_capture
 *   human_readable: Vendor Certification Capture in Supply Chain Governance
 *   domain: economic_governance/regulatory_capture
 *
 * SUMMARY:
 *   Vendor certification capture occurs when certification standards intended
 *   to ensure quality become mechanisms for incumbent vendors to exclude
 *   competitors while extracting rents. The constraint exhibits classic
 *   regulatory capture dynamics but operates at the private standard-setting
 *   level rather than through government agencies. Incumbent vendors
 *   influence certification body governance, participate in standard-setting
 *   committees, and fund audit operations. New market entrants face opaque
 *   standards, expensive compliance audits, and discretionary enforcement
 *   that systematically advantages incumbents. The constraint provides
 *   genuine coordination benefit — common standards do reduce buyer
 *   verification costs and enable supply chain transparency — but this
 *   coordination function is paired with asymmetric extraction that
 *   concentrates benefits with incumbents and costs with entrants. The
 *   theater ratio has increased over time as certification bodies shift from
 *   technical standard-setting toward performative compliance administration.
 *   The constraint's extractiveness has grown as incumbents have consolidated
 *   control over standard-setting, audit procedures, and validation
 *   narratives.
 *
 * KEY AGENTS:
 *   - Market Entrants: Primary victims (powerless/trapped) — face absolute barriers to market entry without certification; cannot exit the certification gate
 *   - Small-Scale Suppliers: Secondary victims (moderate/constrained) — face high compliance costs and discretionary audit judgment; technically can exit by leaving market
 *   - Incumbent Vendors: Primary beneficiaries (institutional/arbitrage) — influence standard-setting, benefit from market protection, capture rents from entrant compliance costs
 *   - Certification Bodies: Institutional actors (institutional/constrained) — provide genuine coordination function but are structurally captured by incumbent influence; constrained by dependence on incumbent funding
 *   - Buyer Organizations: Secondary beneficiaries (powerful/mobile) — benefit from supply chain coordination; have mobility to demand alternative schemes
 *   - Legacy Regulatory Framework: Institutional actor (institutional/arbitrage) — government role persists theatrically while private certification bodies perform actual gate-keeping
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the capture mechanism clearly from outside the system; identifies theater ratio drift as indicator of capture progression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vendor_certification_capture, 0.58).
domain_priors:suppression_score(vendor_certification_capture, 0.65).
domain_priors:theater_ratio(vendor_certification_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vendor_certification_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(vendor_certification_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vendor_certification_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vendor_certification_capture, tangled_rope).
narrative_ontology:human_readable(vendor_certification_capture, "Vendor Certification Capture in Supply Chain Governance").
narrative_ontology:topic_domain(vendor_certification_capture, "economic_governance/regulatory_capture").

domain_priors:requires_active_enforcement(vendor_certification_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vendor_certification_capture, incumbent_vendors).
narrative_ontology:constraint_beneficiary(vendor_certification_capture, certification_bodies).
narrative_ontology:constraint_victim(vendor_certification_capture, market_entrants).
narrative_ontology:constraint_victim(vendor_certification_capture, end_consumers).
narrative_ontology:constraint_victim(vendor_certification_capture, supply_chain_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARKET ENTRANT (SNARE) — New vendors face certification requirements designed by incumbent vendors; cannot enter market without certification; certification standards are opaque and capture-prone. No exit from the certification gate — entry barrier is absolute. Maximum experienced extraction.
constraint_indexing:constraint_classification(vendor_certification_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL-SCALE SUPPLIER (TANGLED ROPE) — Has genuine coordination need (certification ensures quality baseline for buyer confidence) but faces asymmetric extraction through opaque standards, expensive audit procedures, and incumbent vendor influence on certification criteria. Can technically exit by not selling to buyers requiring certification, but this eliminates market access. Mixed experience.
constraint_indexing:constraint_classification(vendor_certification_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT VENDOR (ROPE) — Experiences certification as coordination mechanism: common standards reduce verification costs, enable quality signaling, and create market trust. Net beneficiary through first-mover advantage and influence over standard-setting. Can arbitrage by participating in certification body governance while maintaining insider status.
constraint_indexing:constraint_classification(vendor_certification_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CERTIFICATION BODY (TANGLED ROPE) — Provides genuine coordination function (establishing and maintaining quality standards) but is structurally captured by incumbent vendors who serve on advisory boards, fund operations, and define audit criteria. Active enforcement of standards is real; asymmetric extraction toward incumbents is structural. The certification body is itself a victim of the capture dynamic.
constraint_indexing:constraint_classification(vendor_certification_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BUYER ORGANIZATION (ROPE) — Certification reduces verification burden and enables supply chain transparency. Benefits from coordination across vendors. Has mobility — can shift between certified vendors or pressure for alternative certification schemes. Experiences the constraint as net positive coordination.
constraint_indexing:constraint_classification(vendor_certification_capture, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY REGULATORY FRAMEWORK (PITON) — Government standards bodies originally designed certification to ensure safety and quality; now perform largely theatrical compliance role. Real regulatory function has been displaced by private certification bodies. Government role persists through institutional inertia rather than functional necessity. Theater ratio high — compliance theater replaces actual oversight.
constraint_indexing:constraint_classification(vendor_certification_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational perspective, vendor certification capture represents a pure extraction mechanism disguised as coordination. The high theater ratio and suppression of alternatives (market entrants cannot bypass certification) reveal the snare structure. Alternative certification schemes are prevented from gaining traction through incumbent vendor control of validation narratives.
constraint_indexing:constraint_classification(vendor_certification_capture, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vendor_certification_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vendor_certification_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vendor_certification_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vendor_certification_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vendor_certification_capture, TR),
    TR >= 0.70.

:- end_tests(vendor_certification_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. Initial value (0.35) reflected genuine coordination function with manageable incumbent advantage. Growth to 0.58 reflects accumulation of incumbent control over standard-setting committees, audit procedures, and fee structures. The trajectory shows classic capture progression: initial coordination benefits are preserved while extraction barriers are layered on top. Suppression (0.65): High. Market entrants face multiple suppression mechanisms: opaque standard-setting processes, expensive audit fees, discretionary compliance judgment, and absence of credible alternative certification schemes. Suppression reflects both structural barriers (genuine cost of quality assurance) and artificial barriers (incumbent-designed standards). Theater ratio (0.68): High and increasing. Certification body operations have increasingly become performative — audit procedures follow ritualistic pathways rather than evidence-based quality verification; compliance theater substitutes for actual safety oversight; standards bodies issue glossy compliance reports while incumbent vendors control substantive decisions.
 *
 * PERSPECTIVAL GAP:
 *   The captured certification body is the critical divergence point. From the outside, it appears institutional and powerful (arbitrage options). From within, it is constrained by funding dependence and board composition lock-in that effectively makes exit costly. This produces a perspectival gap between the institutional/arbitrage perspective (which would classify the constraint as rope for the certification body) and the institutional/constrained perspective (which would reveal tangled rope — the certification body is both enforcer and victim of the capture). An identity-locked institutional perspective (certification bodies whose professional identity has fused with their role as incumbent-aligned gatekeepers) would show even stronger entanglement.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents derive low d values (high beneficiary status, arbitrage-level mobility) producing negative or near-zero chi — the constraint subsidizes them. Market entrants derive high d values (victim status, trapped exit) producing high chi — they experience maximum extraction. Certification bodies occupy an intermediate position: they are technically institutional actors with arbitrage options (could theoretically leave the certification business), but their structural capture creates identity lock with incumbent interests, effectively raising their d toward constrained values. The directional flow is clear: extraction runs from entrants toward incumbents, with certification bodies as extraction infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by decomposing the apparent single coordination mechanism into two distinct flows: genuine quality assurance (coordination, low ε) and incumbent rent extraction (capture, high ε). The tangled rope classification captures both flows operating simultaneously. The theater ratio increase (0.42 → 0.68) signals that the theatrical coordination component is growing relative to functional oversight — a diagnostic marker of capture progression. The original intent was pure rope (certification coordination); the current state is tangled rope (coordination + extraction); if capture continues unchecked, the trajectory leads toward piton (theatrical compliance while real gatekeeping happens elsewhere) or snare (pure extraction, coordination function fully displaced).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    standard_necessity_vs_capture,
    'Are the technical requirements within certification standards genuinely necessary for quality assurance, or are they designed to disadvantage market entrants?',
    'Comparative analysis of incumbent vendor vs new entrant compliance costs; correlation between standard requirements and incumbent vendor production capabilities; audit of standards against documented safety incidents vs incumbent vendor behavior',
    'If genuinely necessary: constraint is rope with asymmetric friction (tangled rope). If designed for incumbent advantage: constraint is snare with coordination theater. Determines whether suppression reflects genuine safety requirements or artificial barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standard_necessity_vs_capture, empirical, 'Whether certification standards reflect genuine quality needs or incumbent advantage').

omega_variable(
    audit_transparency_mechanism,
    'Do certification audits follow transparent, reproducible criteria, or do auditors exercise discretionary judgment that advantages incumbents?',
    'Audit decision tree analysis; comparison of audit findings for similar violations by incumbent vs entrant vendors; statistical analysis of pass/fail rates by vendor size and incumbency status',
    'If transparent and reproducible: suppression is legitimate cost of qualification. If discretionary: suppression is mechanism of capture — same standard applied differently to different vendors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(audit_transparency_mechanism, empirical, 'Audit transparency and fairness analysis').

omega_variable(
    alternative_certification_viability,
    'Could alternative certification schemes emerge and gain market acceptance, or are incumbents systematically preventing competing standards from establishing credibility?',
    'Historical analysis of certification scheme emergence attempts; buyer willingness-to-accept alternative schemes; incumbent vendor advocacy against alternative schemes; network analysis of certification body board composition',
    'If alternatives are viable: this is capture with an exit path (scaffold with long sunset). If alternatives are systematically suppressed: this is snare with illusory exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_certification_viability, empirical, 'Viability of competing certification schemes').

omega_variable(
    cost_distribution_asymmetry,
    'What proportion of certification costs are borne by entrants vs incumbents, and how does this compare to the value created by the standard?',
    'Cost accounting for certification compliance across vendor cohorts; comparison to documented risk reduction or quality improvement; analysis of cost allocation mechanisms within standards bodies',
    'If costs are proportional to value: tangled rope is appropriate classification. If costs are asymmetric beyond coordination necessity: extractiveness should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_distribution_asymmetry, empirical, 'Cost distribution asymmetry across vendor types').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vendor_certification_capture, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vcc_tr_t0, vendor_certification_capture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(vcc_tr_t7, vendor_certification_capture, theater_ratio, 7, 0.58).
narrative_ontology:measurement(vcc_tr_t14, vendor_certification_capture, theater_ratio, 14, 0.68).

% Extraction over time
narrative_ontology:measurement(vcc_be_t0, vendor_certification_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vcc_be_t7, vendor_certification_capture, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(vcc_be_t14, vendor_certification_capture, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vendor_certification_capture, information_standard).
narrative_ontology:boltzmann_floor_override(vendor_certification_capture, 0.15).
narrative_ontology:affects_constraint(vendor_certification_capture, supply_chain_opacity).
narrative_ontology:affects_constraint(vendor_certification_capture, market_concentration_dynamics).
narrative_ontology:affects_constraint(vendor_certification_capture, standards_body_governance_capture).

% DUAL FORMULATION NOTE:
% Vendor certification capture is downstream of standards-body governance capture but represents a distinct structural constraint with its own extractiveness metrics. The upstream constraint (governance capture) determines whether certification bodies can resist incumbent influence; this constraint (certification capture) determines the market-level effects of that governance failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vendor_certification_capture, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
