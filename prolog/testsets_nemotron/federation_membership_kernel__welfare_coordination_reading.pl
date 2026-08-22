% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Free Movement via Welfare Coordination (Anti-Social-Dumping Regime)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   The EU's free movement regime operates through coordination of national
 *   welfare systems (Regulation 883/2004, Posting of Workers Directive)
 *   rather than supranational harmonization. The coordination reading claims
 *   this preserves member state welfare design autonomy while preventing
 *   social dumping through enforcement mechanisms (Enforcement Directive
 *   2014/67/EU). However, the 2-year social security exemption for posted
 *   workers and cabotage rules in transport create a structural arbitrage:
 *   employers in receiving states access lower-cost labor without bearing
 *   full social costs; sending states lose workers and their fiscal
 *   contributions without compensation; native and permanent migrant workers
 *   face wage and displacement pressure. The regime requires active
 *   enforcement (labour inspectorates, A1 portable document system, joint
 *   investigations) and has no sunset clause. The claimed_type is
 *   tangled_rope because the coordination function (portable rights,
 *   anti-dumping) is genuine but asymmetrically extractive.
 *
 * KEY AGENTS:
 *   - receiving_state_employers: Primary beneficiary (institutional/arbitrage) — access lower-cost posted labor
 *   - posted_worker_employers: Primary beneficiary (organized/arbitrage) — exploit 2-year exemption and cabotage rules
 *   - eu_institutions_coordination_mandate: Agenda setter (institutional/analytical) — administers coordination framework
 *   - posted_workers: Primary victim (powerless/trapped) — bear social dumping costs, limited exit
 *   - receiving_state_native_workers: Victim (organized/constrained) — wage undercutting, displacement
 *   - permanent_migrant_workers: Victim (moderate/constrained) — dual pressure from posted workers and native competition
 *   - sending_state_fiscal_authorities: Victim (institutional/constrained) — lose workers and contributions without compensation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.42).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Free Movement via Welfare Coordination (Anti-Social-Dumping Regime)").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, '6b583914-eea2-4c71-9bf5-947e4442582a').
narrative_ontology:cs_kernel_codification('6b583914-eea2-4c71-9bf5-947e4442582a', formalized).
narrative_ontology:cs_authority_grounding('6b583914-eea2-4c71-9bf5-947e4442582a', extraction).
narrative_ontology:cs_interpretation_layer_present('6b583914-eea2-4c71-9bf5-947e4442582a').
narrative_ontology:cs_reading_relation('6b583914-eea2-4c71-9bf5-947e4442582a', federation_membership_kernel__integration_reading, influences).
narrative_ontology:cs_reading_relation('6b583914-eea2-4c71-9bf5-947e4442582a', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('6b583914-eea2-4c71-9bf5-947e4442582a', foundational, coordination_over_harmonization).
narrative_ontology:cs_axiom_status(coordination_over_harmonization, holdable).
narrative_ontology:cs_axiom_grounding('6b583914-eea2-4c71-9bf5-947e4442582a', coordination_over_harmonization, conventional).
narrative_ontology:cs_axiom('6b583914-eea2-4c71-9bf5-947e4442582a', foundational, anti_social_dumping_enforcement_sufficiency).
narrative_ontology:cs_axiom_status(anti_social_dumping_enforcement_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('6b583914-eea2-4c71-9bf5-947e4442582a', anti_social_dumping_enforcement_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('6b583914-eea2-4c71-9bf5-947e4442582a', post_maastricht_coordination_settlement).
narrative_ontology:cs_drift_state('6b583914-eea2-4c71-9bf5-947e4442582a', post_enlargement_posting_surge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6b583914-eea2-4c71-9bf5-947e4442582a', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, receiving_state_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, posted_worker_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_institutions_coordination_mandate).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_native_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, permanent_migrant_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_fiscal_authorities).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, coordination_over_harmonization_principle).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, anti_social_dumping_enforcement).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, member_state_welfare_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the welfare coordination framework (Regulation 883/2004, Posting Directive, Enforcement Directive) through the Commission, ECJ, and Administrative Commission. Collects legitimacy and institutional mandate from managing the coordination regime. Does not directly extract fiscal resources but derives authority from the regime's operation. Exit is analytical — the institution cannot leave its own mandate.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_institutions_coordination_mandate, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, eu_institutions_coordination_mandate, beneficiary).

% Employ posted workers under the 2-year social security exemption (A1 certificate) and cabotage rules, capturing the difference between home-state and host-state social contribution rates. Benefit from labor cost arbitrage without bearing full social costs. Can switch between posted and local labor; exit is arbitrage-grade — they choose the regime that minimizes cost.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_employers, beneficiary,
    organized, biographical, arbitrage, national).

% Post workers to higher-wage member states while paying home-state social contributions (often lower). Exploit the 2-year exemption and cabotage wage undercutting in transport/construction. Their business model depends on the coordination regime's arbitrage window; exit means losing the cost advantage.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_worker_employers, beneficiary,
    organized, biographical, arbitrage, national).

% Work in receiving states under posted status: pay home-state social contributions but lack full access to host-state benefits (unemployment, family, housing). Subject to wage undercutting and precarious contracts. Exit is identity-locked — return migration means loss of earnings, career disruption, and often worse home-state conditions; transition to permanent migrant status is legally and linguistically constrained.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, biographical, identity_locked, national).

% Face wage pressure and displacement from posted worker competition in sectors with high posting incidence (construction, transport, care). Organized through unions but constrained exit — labor market attachment, sector-specific skills, and national welfare ties limit mobility. Bear the cost of social dumping through depressed wages and weakened bargaining.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_native_workers, payer,
    organized, biographical, constrained, national).

% Face dual pressure: competition from posted workers (who accept lower effective compensation) and from native workers (who defend established positions). Have made irreversible migration investments (language, integration, family) — exit is constrained by sunk costs. Pay full host-state social contributions while competing with posted workers who do not.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, permanent_migrant_workers, payer,
    moderate, biographical, constrained, national).

% Lose prime-age workers and their lifetime fiscal contributions to receiving states without compensation. Home-state social systems retain theoretical responsibility for posted workers (2-year rule) but receive no revenue from host-state economic activity. Constrained exit — EU law prevents restricting outbound mobility; fiscal reform requires coordination they cannot unilaterally impose.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_fiscal_authorities, payer,
    institutional, generational, constrained, national).

% Attempt to organize posted workers and enforce equal treatment, but structurally excluded from the coordination regime's design (which is negotiated by governments and employer associations). Their enforcement tools are limited to national labor inspectorates and the European Labour Authority (weak mandate). Would object to the arbitrage structure but lack agenda-setting power.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, trade_unions_cross_border, excluded,
    organized, biographical, constrained, continental).

% Adjudicates disputes between coordination and integration readings (e.g., Viking, Laval, Ruffert cases). Its jurisprudence oscillates between deferring to coordination (welfare autonomy) and enforcing integration (equal treatment). Neither collects nor pays; provides the analytical seat that interprets the kernel.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_court_of_justice, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the portable welfare rights problem for mobile workers without requiring member states to harmonize their welfare systems: coordination of 27 distinct social security systems via Regulation 883/2004, anti-dumping enforcement via Posting Directive and Enforcement Directive.
% TRANSFER_FUNCTION: Moves social contribution revenue from sending states (lost fiscal base) and effective compensation from posted workers (lower benefits, wage undercutting) to receiving state employers and posted worker employers (labor cost arbitrage). EU institutions collect institutional mandate/legitimacy.
% ABSENT_VOICES: Posted workers themselves (structurally unable to organize across borders), sending state citizens who lose public services from fiscal erosion, third-country nationals excluded from free movement entirely. Trade unions are present but excluded from agenda-setting.
% DISAPPEARANCE_RATIONALE: If the coordination regime vanished, posted workers would either gain full host-state rights (integration_reading outcome) or face exclusion (member_sovereignty_reading outcome). Employers would lose arbitrage; sending states would retain workers but lose remittances. The 27 welfare systems would need new coordination or face fragmentation.
% FOUNDING_PROBLEM: Post-1957 free movement created mobile workers who fell between national welfare systems — no portable rights, double contributions, gaps in coverage. The coordination regime was built to solve this without forcing welfare harmonization (which member states rejected).
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and Administrative Commission attest the coordination problem remains live (new mobility patterns, digital platforms). European Trade Union Confederation and European Parliament studies attest the regime has shifted function: posting volumes now reflect structural arbitrage (2.3M posted workers 2022, up from 1.2M 2010) not genuine mobility. OECD and academic analyses corroborate the functional shift.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the structural arbitrage: employers capture the delta between national social contribution rates; sending states lose fiscal base; posted workers accept lower effective compensation. Suppression (0.42) is moderate — the constraint operates through legal coordination rules, not overt coercion, but exit is constrained by EU law primacy and labor market integration. Theater ratio (0.31) captures the gap between anti-dumping rhetoric and enforcement capacity (inspectorates cover <15% of postings). Accessibility collapse (0.48) and resistance (0.55) reflect that alternatives (full harmonization, renationalization) are politically contested but not foreclosed — member states periodically push for revision. Measurements show rising extraction (0.45→0.68) and theater (0.18→0.31) over 2000-2025 as posting volumes grew and enforcement lagged.
 *
 * PERSPECTIVAL GAP:
 *   From the EU institution seat, this is a rope: genuine coordination solving the portable rights problem with manageable enforcement. From posted worker and sending state seats, it is a snare: extraction disguised as coordination. From receiving state employer seats, it is a beneficial rope. The engine computes this divergence from the structural data — the authored claim (tangled_rope) reflects the analytical seat seeing both functions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving state employers and posted worker employers are structural beneficiaries (d ≈ 0.15-0.25): they collect the coordination surplus via labor cost arbitrage. EU institutions are agenda setters (d ≈ 0.3): they administer the framework but do not directly extract. Posted workers are full targets (d ≈ 0.9): identity-locked by migration status, trapped by return costs and home-state conditions. Native and permanent migrant workers are high targets (d ≈ 0.7-0.8): organized but constrained exit (labor market attachment). Sending state fiscal authorities are institutional targets (d ≈ 0.6): they bear fiscal losses but have voice in Council negotiations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating welfare access for mobile workers without harmonizing systems) remains live — but the anti-dumping enforcement mandate has accumulated extraction as posting volumes shifted from genuine mobility to structural arbitrage. The mandate has not been resolved; it has been captured by employer arbitrage. The constraint persists because no coalition of victims has sufficient power to force harmonization (which sending states resist) or renationalization (which receiving states resist).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine operational regime (welfare coordination with anti-dumping enforcement) or a transitional framing that masks deeper extraction dynamics?',
    'Longitudinal analysis of posting directive enforcement outcomes vs. social dumping incidence across 2014-2024; comparison with integration_reading''s equal-treatment jurisprudence and member_sovereignty_reading''s exclusionary practices.',
    'If transitional framing, the constraint is a scaffold toward either supranational harmonization (integration_reading) or renationalized exclusion (member_sovereignty_reading). If genuine regime, the tangled_rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the welfare coordination reading instantiates a stable constraint or a contested framing within the federation_membership_kernel.').

omega_variable(
    posted_worker_victim_status_ambiguity,
    'Are posted workers structurally victims of the 2-year social levy exemption and cabotage undercutting, or voluntary participants in a coordination arrangement that lowers their immediate costs?',
    'Empirical study of posted worker exit options: return migration rates, transition to permanent contracts, wage trajectory comparison with native and permanent migrant workers.',
    'If victims, extraction is higher and directionality for posted_workers approaches 1.0; if voluntary coordinators, their directionality is lower and the constraint''s coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(posted_worker_victim_status_ambiguity, empirical, 'Structural position of posted workers within the anti-social-dumping regime.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers to alternative coordination, enforcement of posting rules) or internalized (member states accepting coordination as the only legitimate framing)?',
    'Post-exit suppression trajectory: observe member state behavior when coordination rules are relaxed (e.g., during COVID emergency measures) — if suppression persists via internalized norms, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests; the constraint carries its own legitimacy enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the welfare coordination regime.').

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (portable rights, anti-dumping enforcement) end and asymmetric extraction (employer arbitrage, sending-state fiscal loss) begin?',
    'Counterfactual modeling: simulate a regime with full social security harmonization vs. current coordination; measure welfare changes for each stakeholder seat.',
    'Determines whether the constraint is primarily a rope with extraction leakage, or a tangled_rope where extraction is structural to the coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Boundary between coordination and extraction in the welfare coordination architecture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fwcr_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(fwcr_tr_t0, observed).
narrative_ontology:measurement(fwcr_tr_t5, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(fwcr_tr_t5, observed).
narrative_ontology:measurement(fwcr_tr_t10, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(fwcr_tr_t10, observed).
narrative_ontology:measurement(fwcr_tr_t15, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement_basis(fwcr_tr_t15, observed).
narrative_ontology:measurement(fwcr_tr_t20, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(fwcr_tr_t20, observed).
narrative_ontology:measurement(fwcr_tr_t25, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(fwcr_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(fwcr_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(fwcr_be_t0, observed).
narrative_ontology:measurement(fwcr_be_t5, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(fwcr_be_t5, observed).
narrative_ontology:measurement(fwcr_be_t10, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(fwcr_be_t10, observed).
narrative_ontology:measurement(fwcr_be_t15, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(fwcr_be_t15, observed).
narrative_ontology:measurement(fwcr_be_t20, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(fwcr_be_t20, observed).
narrative_ontology:measurement(fwcr_be_t25, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(fwcr_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(fwcr_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(fwcr_su_t0, observed).
narrative_ontology:measurement(fwcr_su_t5, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(fwcr_su_t5, observed).
narrative_ontology:measurement(fwcr_su_t10, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(fwcr_su_t10, observed).
narrative_ontology:measurement(fwcr_su_t15, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(fwcr_su_t15, observed).
narrative_ontology:measurement(fwcr_su_t20, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(fwcr_su_t20, observed).
narrative_ontology:measurement(fwcr_su_t25, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(fwcr_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__welfare_coordination_reading, 0.15).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, eu_posting_directive_enforcement).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, eu_social_security_coordination_regulation).

% DUAL FORMULATION NOTE:
% The federation_membership_kernel decomposes into three readings with distinct ε values and victim structures. This reading (welfare_coordination) has ε=0.68 with posted workers and sending states as victims. integration_reading has lower ε (coordination benefit dominates) but forecloses national welfare autonomy. member_sovereignty_reading has higher ε (exclusionary extraction) but protects welfare solidarity. All three share the kernel's commitment to free movement but disagree on its welfare-state boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, institutional, 0.3).
constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, powerless, 0.9).
constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, organized, 0.75).
constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
