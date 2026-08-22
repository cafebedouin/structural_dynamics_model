% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Commitment Installation via Fringe Demonstration
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the endogenous legitimacy-climbing reading of
 *   state commitment installation. New institutional commitments (novel legal
 *   doctrines, administrative practices, policy frameworks) gain traction
 *   when fringe innovators demonstrate superiority through practice, early
 *   adoption constituencies volunteer as proof-of-concept sites, and
 *   comparative performance visibility creates legitimacy pressure that
 *   climbs toward apex authorities. The constraint's persistence depends on
 *   the capacity of fringe actors to build constituencies, on the visibility
 *   of performance differences, and on the willingness of mid-tier
 *   gatekeepers to manage adoption transitions. Incumbent apex authorities
 *   bear costs: they must either adopt (reputationally costly admission of
 *   prior suboptimality) or resist (reputational cost as superior practices
 *   remain visible but withheld). This reading COEXISTS with
 *   exogenous-imposition and hybrid-cascade readings held by different
 *   analytical communities; it INFLUENCES the hybrid reading (the hybrid
 *   reading describes what happens when endogenous climb intersects with
 *   top-down cascade); it does NOT foreclose the exogenous reading
 *   (authorities can still impose commitments top-down when endogenous climb
 *   is visible — the two mechanisms compete in institutional history).
 *
 * KEY AGENTS:
 *   - Fringe institutional innovators (moderate power, regional scope) — develop novel commitments; benefit by gaining legitimacy through demonstrated superiority
 *   - Grassroots advocacy networks (organized power, national scope) — champion innovations by mobilizing constituencies and publishing results
 *   - Early adopter constituencies (moderate power, regional scope, constrained exit) — implement innovations early, bearing pilot costs but gaining reputation for progressiveness
 *   - Incumbent apex authorities (institutional power, national scope, constrained exit) — custodians of standing commitments; pay by managing legitimacy erosion
 *   - Mid-tier institutional gatekeepers (powerful, regional scope, constrained exit) — implementers of adoption transitions; manage retraining, performance dips, political pressure
 *   - Status quo constituencies (moderate power, local scope, trapped exit) — structurally excluded because their resistance is pre-classified as defensive
 *   - Comparative institutional observers (analytical, global scope) — measure and broadcast performance differentials; create the legitimacy gradient fringe innovations climb
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.38).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.22).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Commitment Installation via Fringe Demonstration").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, 'c317d08d-c5c5-4607-a318-9fa2bf85c448').
narrative_ontology:cs_kernel_codification('c317d08d-c5c5-4607-a318-9fa2bf85c448', distributed).
narrative_ontology:cs_authority_grounding('c317d08d-c5c5-4607-a318-9fa2bf85c448', distributed).
narrative_ontology:cs_reading_relation('c317d08d-c5c5-4607-a318-9fa2bf85c448', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('c317d08d-c5c5-4607-a318-9fa2bf85c448', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('c317d08d-c5c5-4607-a318-9fa2bf85c448', foundational, legitimacy_from_demonstrated_superiority).
narrative_ontology:cs_axiom_status(legitimacy_from_demonstrated_superiority, holdable).
narrative_ontology:cs_axiom_grounding('c317d08d-c5c5-4607-a318-9fa2bf85c448', legitimacy_from_demonstrated_superiority, empirically_contingent).
narrative_ontology:cs_axiom('c317d08d-c5c5-4607-a318-9fa2bf85c448', foundational, decentralized_adoption_drives_innovation).
narrative_ontology:cs_axiom_status(decentralized_adoption_drives_innovation, holdable).
narrative_ontology:cs_axiom_grounding('c317d08d-c5c5-4607-a318-9fa2bf85c448', decentralized_adoption_drives_innovation, instrumental).
narrative_ontology:cs_reference_frame('c317d08d-c5c5-4607-a318-9fa2bf85c448', institutional_legitimacy_through_tradition_and_continuity).
narrative_ontology:cs_drift_state('c317d08d-c5c5-4607-a318-9fa2bf85c448', performance_visibility_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c317d08d-c5c5-4607-a318-9fa2bf85c448', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_institutional_innovators).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocacy_networks).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_constituencies).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_apex_authorities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, mid_tier_institutional_gatekeepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actors at institutional peripheries who develop new commitment frameworks (new legal doctrines, administrative practices, governance norms). They benefit by gaining legitimacy if their innovations outperform incumbent approaches. They can move between institutions, publish results, and build reputation through demonstrated success. Their primary exit is horizontal: shifting to institutions that value innovation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_institutional_innovators, beneficiary,
    moderate, biographical, mobile, regional).

% Organized constituencies (professional guilds, reform movements, networks of practitioners) that champion new commitments by demonstrating their superiority through practice. They gain legitimacy when adoption accelerates and mobilize support by showing results rather than by authority claim. They can dissolve, migrate to neighboring causes, or consolidate into formal institutions.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocacy_networks, beneficiary,
    organized, generational, mobile, national).

% Institutions and communities that volunteer to implement novel commitments early, before widespread adoption, bearing implementation costs and learning-curve friction. They benefit if the innovation succeeds and their jurisdiction gains reputation for progressiveness. They pay by assuming pilot risk and incurring early transition costs.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_constituencies, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_constituencies, payer).

% Established authorities at the apex of state/institutional hierarchies whose legitimacy rests partly on custodianship of standing commitments. They bear costs when fringe innovations accumulate social proof: they must either adopt (signaling that prior commitments were suboptimal) or resist (reputational cost as superior practices are visible but withheld). Their exit is constrained by historical continuity expectations.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_apex_authorities, payer,
    institutional, generational, constrained, national).

% Regional administrators, departmental leadership, professional bodies that control implementation timelines and resource allocation for new commitments. They pay by managing the adoption transition, retraining personnel, absorbing short-term performance dips, and navigating political pressure from constituencies invested in the status quo. Constrained because institutional position depends on capacity to manage change without chaos.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, mid_tier_institutional_gatekeepers, payer,
    powerful, biographical, constrained, regional).

% Individuals and organizations whose status, career path, or material interests depend on the specific architecture of standing commitments. They would resist if heard, but are structurally excluded from the adoption conversation because their objections are pre-classified as defensive rather than epistemic. They cannot easily exit the jurisdictions implementing new commitments.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, constituencies_invested_in_status_quo, excluded,
    moderate, biographical, trapped, local).

% Scholars, international bodies, and epistemic communities that track comparative performance of different commitment structures across jurisdictions and time. They measure outcomes and broadcast performance differentials, creating the legitimacy gradient that fringe innovations climb. They have no direct stakes in adoption outcomes.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, comparative_institutional_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_institutional_innovators).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A decentralized discovery and validation mechanism for institutional innovation: demonstrated superiority (measured in performance, adoption rates, and outcome data) creates legitimacy without requiring top-down mandate. Solves the problem of how new commitments spread when no central authority has authority to impose them.
% TRANSFER_FUNCTION: Transfers legitimacy and institutional capacity from incumbent apex authorities to fringe innovators and early adopters by making performance visibility the primary legitimacy vector. Movement is from 'we do this because it is tradition' to 'we do this because it works.' Resources flow as institutions adopt (staffing, research funding, political support).
% ABSENT_VOICES: Constituencies invested in the status quo are structurally excluded — they are not in the rooms where adoption decisions are made because their objections are pre-classified as defensive rather than epistemic. They would argue that legitimacy should not depend on comparative performance alone; that stability and continuity carry values independent of efficiency metrics. That voice is absent from the adoption conversation.
% DISAPPEARANCE_RATIONALE: If endogenous climb legitimacy mechanisms disappeared and were replaced with exogenous top-down imposition, institutional innovation would decelerate: fringe actors would lose the ability to build proof-of-concept constituencies; apex authorities would hold adoption gatekeeping power based on political alignment rather than performance; comparative institutional advantage would erode as jurisdictions could not freely adopt superior practices. The institutional ecosystem would reorganize around centralized legitimacy gates.
% FOUNDING_PROBLEM: How do new institutional commitments (legal doctrines, administrative practices, governance norms, social policies) gain acceptance in decentralized institutional landscapes where no single authority can mandate adoption across jurisdictions, when incumbents have interest in resisting displacement? The endogenous reading answers: through demonstrated superiority, proof-of-concept constituencies, and performance visibility that create legitimacy pressure from below.
% FOUNDING_PROBLEM_CORROBORATION: Historical comparative institutional analysis (Mahoney and Thelen on gradual institutional change; Meyer and Rowan on legitimacy as decoupled from efficiency; Johnson and Kwak on financial regulation innovation) attests that major institutional transformations (abolition of slavery codes, adoption of merit-based civil service, universal suffrage expansion, environmental regulation) often originated in fringe jurisdictions and climbed through demonstrated superiority. Comparative government data on policy diffusion shows S-curves consistent with proof-of-concept driven adoption. International development agencies and policy networks explicitly model fringe-to-apex diffusion. This corroboration comes from outside any benefiting faction; the incumbent apex authorities themselves often document this mechanism in retrospect (though resisting it contemporaneously).
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).
:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at ceiling) because fringe innovators benefit without bearing the apex's resistance costs, and early adopters gain reputation while paying pilot costs — the flow is asymmetric but not extreme. Early adopters are simultaneously beneficiaries and payers (secondary_role: payer), modeling their dual position. Suppression is low (0.22) because the mechanism depends on voluntary adoption, not coercion; the climb works through proof-of-concept and visibility, not through barriers. Theater ratio remains low (0.18) because the legitimacy claim rests on actual performance data and comparative outcomes, though strategic framing of metrics adds modest theatrical elements (which metrics count as 'performance'? who defines success?). Resistance is high (0.71) because incumbent apex authorities actively resist adoption, and constituencies invested in status quo oppose change (though their voices are excluded from formal adoption conversations). The measurement series shows extraction plateauing at t=20 as adoption cascades complete and new equilibrium stabilizes; suppression requirement rising through t=25 as apex resistance intensifies, then stabilizing as the new commitment becomes institutionalized. The series are on a shared time grid (every metric authored at every time point).
 *
 * PERSPECTIVAL GAP:
 *   From the fringe innovators' and grassroots advocacy seats, the mechanism is a genuine coordination function: it allows superior practices to spread despite apex resistance, solving the institutional innovation problem. From the incumbent apex seat, the mechanism operates as a legitimacy-erosion and authority-displacement mechanism — the same structure that benefits fringe actors imposes costs on authorities by making their custodianship visibility-dependent rather than tradition-dependent. From the status quo constituencies' seat (though excluded), the mechanism extracts by overriding stability and continuity values with efficiency metrics. The engine will compute different types per seat from this structural asymmetry: beneficiary seats will compute lower extraction; payer seats will compute higher. This divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Fringe innovators and grassroots advocates are beneficiaries with moderate-to-organized power and mobile exit options — they can shift between institutions and build reputation through demonstrated success. Their directionality is low (toward beneficiary end): they collect legitimacy gains and can exit if an institution rejects their innovation. Early adopter constituencies are dual-positioned (beneficiary + payer): they gain reputation and validation but pay by assuming pilot risk and transition costs; constrained exit (mobile within regional scope but organizational continuity pressure limits jumping) puts them near symmetric. Incumbent apex authorities are payers with institutional power and constrained exit (legitimacy depends on custodianship continuity); they are forced to choose between adoption (reputational cost) and resistance (reputational cost). Status quo constituencies are excluded payers: trapped exit means they cannot leave jurisdictions adopting new commitments, and their voice is pre-classified as defensive. Mid-tier gatekeepers are structural payers: they implement adoption and bear transition costs (retraining, performance dips, political navigation); institutional power and regional scope constrain but don't eliminate their ability to slow adoption, but slowing is itself costly (reputation for obstruction). The directionality derivation shows all payers at high d (toward target end) and all beneficiaries at low d (toward beneficiary end), with the exclusion of status quo constituencies being the structural suppression mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how do new commitments spread in decentralized landscapes without top-down mandate) is LIVE: institutional innovation remains a constant feature of state formation and policy change. The disappearance verdict is WORLD_REARRANGES: if endogenous climb legitimacy vanished and was replaced with exogenous imposition, institutional change would decelerate and become gatekept by apex authority alignment rather than performance. The constraint does not exhibit mandatrophy: it is not a zombie arrangement persisting after its function atrophied. It does show modest theater (0.18) where metrics selection and framing add theatrical elements to performance visibility — but this is strategic framing around a real substantive function, not performance-as-theater replacing coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_visibility_mechanism,
    'How do fringe innovations become visible as ''superior'' if apex authorities control official measurement and framing? Is the legitimacy gradient genuine comparative advantage or narrative construction by innovators?',
    'Comparative analysis of early-adoption outcome data: do jurisdictions adopting innovations early show measurable improvements in their stated metrics (crime rates, economic productivity, administrative efficiency, outcome distributions) within timescales observed by contemporary institutional actors? Or does the perception of superiority depend on selective metric choice and post-hoc narrative?',
    'If outcomes are genuinely superior by most plausible metrics, the endogenous climb is a real coordination mechanism driven by performance. If outcomes depend on cherry-picked metrics or reveal superiority only in retrospect, the mechanism is more about narrative legitimacy-seeking than genuine superiority, and extraction may be higher than authored (fringe actors gain by narrative construction, not substance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_visibility_mechanism, empirical, 'Whether the legitimacy climb is driven by genuine performance superiority or by strategic narrative framing.').

omega_variable(
    apex_resistance_mechanism,
    'Is apex authority resistance to fringe-climbed commitments rooted in defensiveness of incumbent interests, or in genuine epistemic caution about unproven innovations scaling beyond their pilot contexts?',
    'Analysis of adoption-resistance rhetoric: do apex authorities cite specific implementation risks, context-dependency concerns, or transition costs? Or do they rest on appeals to tradition, stability, and continuity without substantive risk analysis? Do they sponsor comparative analysis or suppress it?',
    'If resistance is substantive (rooted in identifiable scaling risks), apex authorities are rational gatekeepers and the constraint''s suppression (0.22) may underestimate the active epistemic work they perform. If resistance is defensive (rooted in interest protection), suppression and extraction are accurately sized. The distinction affects whether apex costs are coordination overhead or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apex_resistance_mechanism, conceptual, 'Whether apex resistance to fringe-climbed commitments rests on substantive scaling concerns or on interest-protective defensiveness.').

omega_variable(
    structural_exclusion_mechanism,
    'Is the exclusion of status quo constituencies (the pre-classification of their objections as defensive rather than epistemic) a structural feature of the endogenous climb mechanism, or a contingent fact about how this reading is empirically instantiated?',
    'Counterfactual institutional design: would an endogenous climb mechanism that included status quo constituencies'' epistemic input (not just their resistance) change the path of institutional change? Would it accelerate or decelerate adoption of superior commitments?',
    'If inclusion would decelerate adoption of genuinely superior practices, the exclusion is functional to the mechanism''s coordination role, and the structural suppression (pre-classification of objections) is justified as efficiency cost. If inclusion would improve outcomes (by identifying genuine scaling risks that fringe pilots missed), the exclusion represents real epistemic loss and the suppression is pure extraction. This affects mandatrophy certification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_exclusion_mechanism, conceptual, 'Whether the structural exclusion of status quo constituencies is a feature of the coordination mechanism or an instance of extractive suppression.').

omega_variable(
    kernel_reading_identity,
    'Does the endogenous-climb reading accurately capture a distinct mechanism in institutional history, or is it a retrospective narrative imposed on mixed cases that involved both endogenous proof-of-concept and exogenous elite mandate?',
    'Detailed case analysis of major institutional transformations (abolition, suffrage, environmental regulation, monetary policy): what role did each mechanism play? Can cases be cleanly sorted into endogenous, exogenous, and hybrid categories, or do all major transformations involve multiple mechanisms in sequence?',
    'If clean sorting is impossible (all major cases are hybrid), the three readings are all partially true and the kernel is under-resolved — the question ''how do commitments gain legitimacy'' may have no single answer mechanism. If cases do sort (e.g., legal doctrines typically endogenous, regulatory frameworks typically exogenous), the readings capture real structural differences. This affects the foundational claim of the reading: whether legitimacy is a function of demonstrated superiority (endogenous) or authority mandate (exogenous).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the endogenous-climb mechanism is a distinct institutional pathway or a retrospective narrative over cases driven by hybrid dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t5, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(stat_tr_t5, observed).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t15, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(stat_tr_t15, observed).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t25, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t35, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(stat_tr_t35, observed).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(stat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t5, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(stat_be_t5, observed).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t15, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(stat_be_t15, observed).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t25, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t35, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement_basis(stat_be_t35, observed).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(stat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t5, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 5, 0.08).
narrative_ontology:measurement_basis(stat_su_t5, observed).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t15, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement_basis(stat_su_t15, observed).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t25, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t35, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 35, 0.22).
narrative_ontology:measurement_basis(stat_su_t35, observed).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(stat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__endogenous_climb_reading, 0.12).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (endogenous climb) of a contested kernel about state commitment installation mechanisms. The sibling readings (exogenous imposition, hybrid cascade) are separate constraints with their own ε values, beneficiary/victim structures, and directionality profiles. All three describe pathways by which new institutional commitments gain legitimacy and spread; they differ in the primary legitimacy vector (performance visibility vs. authority mandate vs. both-in-sequence) and the primary adoption dynamic (voluntary pull from below vs. coerced push from above vs. sequenced). The disagreement is located in what legitimacy fundamentally IS: a function of demonstrated superiority, or of authority endorsement, or of both. Each reading carries its own constraint because the structural relationships differ — who benefits, who pays, what gets excluded — and the ε invariance principle requires separate constraint stories for structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__endogenous_climb_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
