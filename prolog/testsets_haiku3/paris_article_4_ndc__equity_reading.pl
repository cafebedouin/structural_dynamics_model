% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: NDC Equity Reading: Common But Differentiated Responsibilities Framework
 *   domain: international/climate/political_economy
 *
 * SUMMARY:
 *   Article 4 of the Paris Agreement stipulates that NDCs (Nationally
 *   Determined Contributions) should reflect parties' highest possible
 *   ambition in light of differing national circumstances. The equity reading
 *   interprets this clause through Common But Differentiated Responsibilities
 *   (CBDR), demanding that developed states bear asymmetric mitigation and
 *   transfer obligations grounded in historical responsibility and capacity
 *   differences. This reading has been institutionalized through UNFCCC
 *   coalitions (Alliance of Small Island States, Least Developed Countries,
 *   African Group) and equity advocacy networks. The constraint operates as a
 *   tangled rope: genuine coordination function (preventing a uniform-burden
 *   regime that would collapse developing-state participation) intertwined
 *   with asymmetric extraction (developed states face binding commitments and
 *   transfer obligations they argue exceed their fair share; developing
 *   states retain policy discretion others lack). The measurement trajectory
 *   shows extraction rising through 2027 as developed-state transfer
 *   obligations are specified and equity coalitions assert veto over
 *   supranational enforcement, then moderating slightly by 2030 as political
 *   fatigue and resource constraints temper implementation intensity.
 *
 * KEY AGENTS:
 *   - Developed states with binding commitments — bear transfer obligations and high mitigation ceilings
 *   - Developing-state equity coalitions (AOSIS, LDCs, African Group) — set the reading's agenda and veto supranational acceleration
 *   - Equity advocacy networks (environmental justice, global South, Indigenous rights) — frame the reading's legitimacy
 *   - Supranational governance proponents — excluded by the reading's emphasis on state consent and coalition veto
 *   - International climate observers (IPCC, UNEP) — provide the empirical baseline on historical emissions and adequacy gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.55).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.48).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "NDC Equity Reading: Common But Differentiated Responsibilities Framework").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international/climate/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, 'c64b2351-6ca6-471d-9234-98ae1cd4bf7b').
narrative_ontology:cs_kernel_codification('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', formalized).
narrative_ontology:cs_authority_grounding('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', distributed).
narrative_ontology:cs_reading_relation('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_axiom('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', foundational, historical_emissions_responsibility_binds_present_obligation).
narrative_ontology:cs_axiom_status(historical_emissions_responsibility_binds_present_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', historical_emissions_responsibility_binds_present_obligation, deontological).
narrative_ontology:cs_axiom('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', foundational, differentiated_capacity_requires_asymmetric_commitment).
narrative_ontology:cs_axiom_status(differentiated_capacity_requires_asymmetric_commitment, holdable).
narrative_ontology:cs_axiom_grounding('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', differentiated_capacity_requires_asymmetric_commitment, deontological).
narrative_ontology:cs_reference_frame('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', cbdr_differentiated_treaty_system).
narrative_ontology:cs_drift_state('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', contemporary_2024, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c64b2351-6ca6-471d-9234-98ae1cd4bf7b', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_state_coalitions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_advocacy_networks).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_states_with_binding_commitments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, vulnerable_low_income_populations).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, unilateral_developed_state_actors).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, historical_responsibility_doctrine).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, capacity_differentiated_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the equity reading, bear asymmetric mitigation obligations and mandatory technology/finance transfer commitments to developing states. Their NDC pledges are interpreted as binding floors, not voluntary targets. They face veto power from equity coalitions if they attempt to weaken commitments or reduce transfer flows. Exit means treaty withdrawal with severe diplomatic cost and climate reputation damage.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_states_with_binding_commitments, payer,
    institutional, generational, constrained, global).

% Retain policy space to set NDC ambition levels based on development needs and capacity constraints. Collective veto authority over supranational enforcement mechanisms and unilateral developed-state interpretations. Receive technology transfer, climate finance, and capacity-building commitments from developed states. Can block consensus on tightening mechanisms or enforcement without their consent.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_state_coalitions, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, developing_state_coalitions, agenda_setter).

% Environmental justice, global South, and Indigenous rights networks that frame climate action through equity and historical responsibility lenses. Gain legitimacy and framing power by centering CBDR as the authoritative reading. Translate equity framing into policy vetoes and transfer-flow enforcement.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% Would advocate for binding international enforcement, automatic ratcheting mechanisms, and centralized accountability structures that override national veto. Their participation would shift the constraint toward supranational governance; their exclusion is maintained by the equity reading's insistence on state consent and coalition veto.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, supranational_governance_proponents, excluded,
    institutional, generational, constrained, global).

% Industrial interests in developed and developing states that oppose binding mitigation commitments and transfer obligations. Their access to negotiation is structurally limited by the equity reading's framing, which isolates them as obstruction to historic justice rather than legitimate economic actors.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, fossil_fuel_incumbents, excluded,
    powerful, biographical, constrained, global).

% Bear immediate climate impacts (rising seas, drought, extreme weather) regardless of mitigation outcome. Under the equity reading, theoretically benefit from developed-state transfer obligations and developing-state policy space to invest adaptation finance. In practice, dependent on intermediate institutions (national governments, NGOs) for resource flow; capture risk is high.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, vulnerable_low_income_populations, beneficiary,
    powerless, immediate, trapped, global).

% Individual developed states or coalitions (Nordic, EU) that adopt stricter unilateral climate policies and attempt to set higher domestic NDC targets or enforcement standards. The equity reading constrains their ability to impose these standards on other developed states or to condition trade/finance on exceeding CBDR-differentiated commitments without coalition consent.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, unilateral_developed_state_actors, payer,
    powerful, biographical, mobile, global).

% Scientific bodies, climate monitoring agencies, IPCC, and research institutions that measure emissions, track commitments, and assess adequacy gaps. Provide the empirical record on which claims of binding vs. voluntary, achieved vs. pledged, and equity-consistent vs. equity-violating rest.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, international_climate_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, developing_state_coalitions).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a differentiated framework for collective greenhouse-gas reduction that acknowledges historical responsibility and development capacity disparities, allocating obligations asymmetrically so developed states bear the largest cuts and transfer obligations while developing states maintain policy space to balance mitigation with poverty reduction and growth. Solves the coordination problem of how to create a global climate regime that does not impose identical burdens on unequal parties.
% TRANSFER_FUNCTION: Moves climate finance, technology transfers, and capacity-building commitments from developed to developing states. Moves mitigation burden (emissions reductions, domestic policy constraints, transition costs) asymmetrically toward developed states. Moves veto authority over enforcement and ratcheting mechanisms to equity coalitions and developing-state blocs.
% ABSENT_VOICES: Supranational governance advocates and those who would prioritize speed-to-net-zero over equity-first interpretation are structurally isolated by the reading's framing; their participation would reweight toward binding supranational enforcement and away from state consent and coalition veto. Fossil-fuel incumbents are excluded from legitimacy by design.
% DISAPPEARANCE_RATIONALE: If the equity reading and its CBDR differentiation framework disappeared overnight, developed states would face pressure to adopt unilateral supranational enforcement mechanisms or impose binding standards on all NDCs regardless of capacity; developing states would either exit the regime entirely or face resource-extraction dynamics (climate finance tied to supranational compliance rather than allocated by equity); transfer commitments would collapse without the equity coalition veto power that sustains them.
% FOUNDING_PROBLEM: Industrial North's historical emissions created atmospheric CO2 concentrations; the South faces acute climate impacts despite minimal contribution to the stock problem. A climate regime imposing equal per-capita or equal-effort obligations on unequal historical actors and unequal development needs would replicate historical injustice and free-ride on Northern responsibility avoidance.
% FOUNDING_PROBLEM_CORROBORATION: Developing-state blocs, equity advocacy networks, and IPCC Special Reports on climate justice attest the founding problem is live and central. Developed states and supranational governance advocates dispute both the problem framing and the equity solution, arguing it weakens mitigation urgency; their testimony attests to the contest, not to corroboration outside benefiting parties. The historical emissions record (UNEP Emissions Gap, IPCC AR6 attribution) corroborates the carbon-stock asymmetry that grounds the equity reading's diagnosis.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.55, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness of 0.55 reflects moderate structural asymmetry: developed states face real constraints (binding ambition ceilings, mandatory transfers) that developing states avoid, but the equity reading does not impose unilateral supranational enforcement or coercive emissions verification — it operates through coalition consensus and state consent. Suppression of 0.48 is below extractiveness because the constraint is maintained by coalition veto power and active resistance from developing states and equity advocates; developed states resist but lack the coalition density or legitimacy within the equity frame to suppress the commitments. Theater of 0.29 reflects real coordination function but rising proportion of enforcement activity devoted to managing veto disputes and transfer-flow negotiation rather than emissions reduction. The temporal trajectory shows extraction rising as the equity framework is operationalized (differentiated finance mechanisms, technology-transfer protocols, veto procedures) from 2015–2027, then declining slightly as resource constraints and political fatigue moderate developed-state compliance with transfer obligations. Suppression rises early (2015–2024) as developing states build coalition capacity and voting blocs; it begins to stabilize by 2027 as the veto structure is consolidated. Theater rises consistently as theatrical elements (pledges without finance, announcements without action, capacity-building photo opportunities) displace real transfer and mitigation.
 *
 * PERSPECTIVAL GAP:
 *   The developed-state payer and the equity-coalition beneficiary seats compute maximally divergent types. From the payer seat (institutional power, constrained exit, binding obligations), the constraint reads as a snare — asymmetric extraction justified by a coordinate story (CBDR) that redistributes legitimacy rather than generating genuine mutual benefit. From the beneficiary seat (organized power, mobile exit, policy space retained), the constraint reads as a rope — it solves a genuine coordination problem (enabling global participation without uniformity) and distributes fairly. The supranational observer would compute it as a snare with weak enforcement (real extraction, but veto power limits suppression). These divergences should emerge from the structural data without reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states are identified as victims (payers) because the equity reading imposes binding commitments and transfer obligations they do not unilaterally control. Their exit options are constrained — leaving the regime means climate reputation collapse and loss of negotiating legitimacy. Their power is institutional, but within the CBDR framework their structural relationship is to bear costs others avoid. Developing-state coalitions are beneficiaries because the reading grants them policy space, veto authority, and transfer entitlements. Their power is organized (coalition-based) but their exit options are mobile — they can credibly threaten regime withdrawal if equity terms are violated. Equity advocates are beneficiaries through framing legitimacy — the reading vindicates their normative claims and translates those into negotiating power. Supranational proponents are excluded not by explicit rule but by the reading's structural insistence on state sovereignty and coalition consent, which makes supranational enforcement mechanisms impossible without equity coalition support. Vulnerable populations are theoretically beneficiaries (through transfer commitments and adaptation finance) but are trapped in dependency on intermediate institutional actors (national governments, climate funds) for resource flow; this creates a secondary extraction mechanism not directly authored here but visible in the theater ratio's rise.
 *
 * MANDATROPHY ANALYSIS:
 *   The equity reading avoids mandatrophy classification by tightly coupling its mandate (differentiated obligations grounded in historical responsibility and capacity) to its operating function (preventing regime collapse from uniform-burden refusal by developing states). However, a secondary mandatrophy risk exists: if developing states' climate impacts accelerate faster than adaptation capacity increases, the equity reading's emphasis on policy space over enforced action could become a victim-blaming mechanism — the reading vindicates historical responsibility while deferring enforcement precision and fund-disbursement speed. This risk is not structural mandatrophy (the founding problem is live) but functional drift toward theater (the veto mechanism increasingly used to block acceleration rather than to protect capacity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_reading_vs_sovereigntist_core_premise,
    'Is historical responsibility a binding normative constraint on present-day NDC ambition, or is each state''s self-determined commitment the complete moral/political baseline?',
    'Examine whether post-1992 climate negotiations treat historical emissions as a bar on developed-state NDC revision downward or as contextual but not binding. Assess whether any developed state successfully justifies NDC weakening by appeal to non-responsibility (framing as if it inherited clean slate) — if rejected, the equity constraint holds; if accepted, sovereigntist framing gains ground.',
    'If historical responsibility is binding, the equity reading''s asymmetric obligation structure is justified and forecloses pure sovereigntist readings within the treaty framework. If it is not binding, the sovereigntist reading''s voluntary-pledge framing becomes coherent and the equity reading collapses to performative framing without structural force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_reading_vs_sovereigntist_core_premise, empirical, 'Whether historical responsibility is a binding constraint on current-state NDC ambition or merely contextual.').

omega_variable(
    developing_state_veto_authority_persistence,
    'Can developing-state equity coalitions sustain veto power over supranational enforcement indefinitely, or is that veto vulnerable to erosion as climate impacts mount and unilateral developed-state coalitions bypass the CBDR framework?',
    'Monitor whether AOSIS, LDCs, and African coalitions maintain negotiating cohesion through 2030 and beyond. Assess whether developed states (EU, US, Nordic) attempt to create parallel binding architectures (bilateral agreements, net-zero clubs, trade-conditional climate conditionality) that circumvent CBDR veto.',
    'If veto authority persists, the equity reading''s structural protection of developing-state policy space holds and the constraint remains tangled_rope. If veto erodes, the reading becomes a facade and the constraint reclassifies toward snare (equity coalition legitimacy is revoked while extraction from developing states remains).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_state_veto_authority_persistence, empirical, 'Whether developing-state veto authority over supranational enforcement can be sustained.').

omega_variable(
    technology_transfer_capture_risk,
    'Is the climate finance and technology transfer moving from developed to developing states, or is the equity-reading veto mechanism capturing transfer flows in ways that benefit equity advocates and state elites while bypassing vulnerable populations?',
    'Track actual disbursement of pledged climate finance, measure reach to last-mile adaptation and distributed solar, assess whether funds bypass or concentrate in governance institutions and international consultancy networks.',
    'If transfer flows reach vulnerable populations, the equity reading''s coordination function is real and the constraint remains tangled_rope with asymmetric distribution favoring those in need. If transfer is captured, the equity reading becomes a Snare: it extracts legitimacy from equity framing while the actual gains accrue to institutional intermediaries and state elites.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_capture_risk, empirical, 'Whether climate finance transfer actually reaches vulnerable populations or is captured by intermediate institutions.').

omega_variable(
    equity_reading_forecloses_pure_sovereigntist,
    'Within a single treaty framework that has institutionalized CBDR language, can a pure sovereigntist reading (highest ambition is self-determined, no differentiation required) logically coexist with the equity reading, or does accepting one mandate preclude the other?',
    'Analyze the Paris Agreement''s legal text and successive COP decisions: does the language of Article 4 (differing national circumstances, highest possible ambition) admit both readings, or does the institutionalization of CBDR operationals (differentiated finance, capacity-building entitlements, veto-over-ratcheting procedures) foreclose the sovereigntist reading within the treaty''s own authority structure?',
    'If foreclosure is real, the equity reading eliminates the sovereigntist alternative within the treaty framework; the supranational reading remains in coexistence or influence relation. If the text admits both readings, they genuinely coexist within different factions'' commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_reading_forecloses_pure_sovereigntist, conceptual, 'Whether the equity reading''s institutionalization in CBDR operationals forecloses pure sovereigntist readings within the treaty framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(pari_tr_t2015, observed).
narrative_ontology:measurement(pari_tr_t2018, paris_article_4_ndc__equity_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement_basis(pari_tr_t2018, observed).
narrative_ontology:measurement(pari_tr_t2021, paris_article_4_ndc__equity_reading, theater_ratio, 2021, 0.26).
narrative_ontology:measurement_basis(pari_tr_t2021, observed).
narrative_ontology:measurement(pari_tr_t2024, paris_article_4_ndc__equity_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement_basis(pari_tr_t2024, observed).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__equity_reading, theater_ratio, 2027, 0.3).
narrative_ontology:measurement_basis(pari_tr_t2027, projected).
narrative_ontology:measurement(pari_tr_t2030, paris_article_4_ndc__equity_reading, theater_ratio, 2030, 0.29).
narrative_ontology:measurement_basis(pari_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement_basis(pari_be_t2015, observed).
narrative_ontology:measurement(pari_be_t2018, paris_article_4_ndc__equity_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement_basis(pari_be_t2018, observed).
narrative_ontology:measurement(pari_be_t2021, paris_article_4_ndc__equity_reading, base_extractiveness, 2021, 0.52).
narrative_ontology:measurement_basis(pari_be_t2021, observed).
narrative_ontology:measurement(pari_be_t2024, paris_article_4_ndc__equity_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(pari_be_t2024, observed).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__equity_reading, base_extractiveness, 2027, 0.62).
narrative_ontology:measurement_basis(pari_be_t2027, projected).
narrative_ontology:measurement(pari_be_t2030, paris_article_4_ndc__equity_reading, base_extractiveness, 2030, 0.55).
narrative_ontology:measurement_basis(pari_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement_basis(pari_su_t2015, observed).
narrative_ontology:measurement(pari_su_t2018, paris_article_4_ndc__equity_reading, suppression_requirement, 2018, 0.38).
narrative_ontology:measurement_basis(pari_su_t2018, observed).
narrative_ontology:measurement(pari_su_t2021, paris_article_4_ndc__equity_reading, suppression_requirement, 2021, 0.45).
narrative_ontology:measurement_basis(pari_su_t2021, observed).
narrative_ontology:measurement(pari_su_t2024, paris_article_4_ndc__equity_reading, suppression_requirement, 2024, 0.51).
narrative_ontology:measurement_basis(pari_su_t2024, observed).
narrative_ontology:measurement(pari_su_t2027, paris_article_4_ndc__equity_reading, suppression_requirement, 2027, 0.49).
narrative_ontology:measurement_basis(pari_su_t2027, projected).
narrative_ontology:measurement(pari_su_t2030, paris_article_4_ndc__equity_reading, suppression_requirement, 2030, 0.48).
narrative_ontology:measurement_basis(pari_su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__equity_reading, 0.18).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, climate_finance_architecture).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, technology_transfer_mechanisms).

% DUAL FORMULATION NOTE:
% The paris_article_4_ndc kernel decomposes into three structurally distinct constraint stories under different readings: (1) equity_reading (this story) — moderate extractiveness, asymmetric distribution, state-coalition veto, CBDR operationalized; (2) sovereigntist_reading — lower extractiveness, self-determined pledges, minimal asymmetry, treaty-as-coordination-device; (3) supranational_reading — higher extractiveness, binding ratcheting trajectory, international accountability, supranational enforcement. Each reading instantiates a different epsilon value because they measure different arrangements (differentiated obligations vs. self-determined vs. supranational). The three stories form a constraint family: each reading influences the others' institutional viability. The equity reading's institutionalization of CBDR operationals (capacity-building funds, technology-transfer entitlements, veto procedures) may foreclose the pure sovereigntist reading within the treaty while coexisting with supranational readings held by different coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__equity_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
