% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money Origin — First-Held Reading (Practical Store-of-Value Adoption)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the first_held_reading of the
 *   digital_money_origin kernel: digital money is dated to the moment
 *   individuals FIRST PRACTICALLY HELD non-physical monetary instruments as
 *   everyday stores of value (early bank deposits, then debit/card balances,
 *   then mobile money such as M-Pesa) — not to the moment the concept became
 *   conceivable, and not to the moment regulators formally counted it in
 *   monetary aggregates. This dating choice pushes the origin later than a
 *   conceptual reading and structurally different from a regulatory reading,
 *   and it introduces a constraint set the other readings lack:
 *   implementation barriers (identification, device ownership, connectivity,
 *   minimum balances) that determine who could hold digital money AT ALL.
 *   Because access was infrastructure-gated rather than universally available
 *   at the moment of origin, this reading generates a genuine
 *   beneficiary/victim split absent from a purely conceptual dating.
 *
 * KEY AGENTS:
 *   - early_infrastructure_adopters: primary beneficiary (moderate/mobile) — captured first-mover convenience and creditworthiness effects
 *   - digital_payment_platform_operators: agenda_setter (institutional/arbitrage) — built and enforce the access gates
 *   - unbanked_rural_populations: primary target (powerless/trapped) — excluded from first-holding by infrastructure absence, now bear rising cost of remaining in cash
 *   - monetary_historians: analytical observer — sees the full access-gated structure of origin dating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.58).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.42).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin — First-Held Reading (Practical Store-of-Value Adoption)").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, 'c26fee93-07e9-4d8d-a420-28f6f0acccad').
narrative_ontology:cs_kernel_codification('c26fee93-07e9-4d8d-a420-28f6f0acccad', distributed).
narrative_ontology:cs_authority_grounding('c26fee93-07e9-4d8d-a420-28f6f0acccad', distributed).
narrative_ontology:cs_reading_relation('c26fee93-07e9-4d8d-a420-28f6f0acccad', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('c26fee93-07e9-4d8d-a420-28f6f0acccad', digital_money_origin__regulatory_recognition_reading, coexists_with).
narrative_ontology:cs_axiom('c26fee93-07e9-4d8d-a420-28f6f0acccad', foundational, practical_holding_constitutes_monetary_reality).
narrative_ontology:cs_axiom_status(practical_holding_constitutes_monetary_reality, holdable).
narrative_ontology:cs_axiom_grounding('c26fee93-07e9-4d8d-a420-28f6f0acccad', practical_holding_constitutes_monetary_reality, conventional).
narrative_ontology:cs_axiom('c26fee93-07e9-4d8d-a420-28f6f0acccad', secondary, access_asymmetry_is_intrinsic_to_origin_not_incidental).
narrative_ontology:cs_axiom_status(access_asymmetry_is_intrinsic_to_origin_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('c26fee93-07e9-4d8d-a420-28f6f0acccad', access_asymmetry_is_intrinsic_to_origin_not_incidental, empirically_contingent).
narrative_ontology:cs_created_at('c26fee93-07e9-4d8d-a420-28f6f0acccad', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_infrastructure_adopters).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, digital_payment_platform_operators).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, banked_urban_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_rural_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, informal_cash_economy_workers).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, populations_without_reliable_connectivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with bank accounts, card networks, or mobile money access at the moment digital balances became practically usable as everyday stores of value. They captured the convenience, interest-bearing, and transactional benefits first, establishing habits and creditworthiness records that compounded advantage over time.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_infrastructure_adopters, beneficiary,
    moderate, biographical, mobile, national).

% Banks, card networks, and mobile-money issuers built and enforced the rails (KYC requirements, minimum balances, device and network prerequisites) that determined who could actually hold non-physical money. They set the terms of access and collect fees, float income, and data value from every transaction routed through their systems.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, digital_payment_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, digital_payment_platform_operators, beneficiary).

% Urban populations with existing bank relationships and connectivity infrastructure moved into digital holding quickly and painlessly, benefiting from convenience and reduced physical-cash risk without bearing the access costs faced by others.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, banked_urban_populations, beneficiary,
    moderate, biographical, mobile, national).

% Populations lacking bank branches, identification documents, or reliable electricity and connectivity were excluded from the moment of first practical holding. As commerce, employment, and government benefits increasingly assume digital holding as the baseline, their continued reliance on cash becomes progressively more costly and stigmatized, even though they never had a genuine choice to adopt.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_rural_populations, payer,
    powerless, biographical, trapped, regional).

% Day laborers, market vendors, and gig workers paid in cash face rising friction as formal-sector counterparties (landlords, wholesalers, employers) shift default expectations to digital transfer. Their labor and savings are penalized for remaining outside the first-holder cohort's infrastructure.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, informal_cash_economy_workers, payer,
    powerless, biographical, constrained, local).

% Where mobile networks or electricity are unreliable, digital balances are theoretically available but practically unusable as a store of value — funds can become inaccessible at moments of need. This group bears the structural cost of a system optimized around the connectivity levels of the first-holder cohort.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, populations_without_reliable_connectivity, payer,
    powerless, biographical, trapped, regional).

% Study when digital money 'really' began by tracking first practical holding events (payroll deposits, early debit card balances, M-Pesa adoption) rather than conceptual or regulatory milestones. Their dating choice privileges lived economic experience over institutional recognition.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__first_held_reading, diffuse).
narrative_ontology:fixing_cost_class(digital_money_origin__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Digital holding infrastructure solves a genuine coordination problem: it lets value be stored, transferred, and verified without the costs, risks, and friction of physical cash — enabling commerce at distance and scale.
% TRANSFER_FUNCTION: Moves transactional convenience, security, and creditworthiness-building capacity toward those with infrastructure access first, while shifting the cost of remaining outside the system (friction, exclusion from formal credit, stigma) onto those without access, as digital holding becomes the assumed default.
% ABSENT_VOICES: Unbanked rural populations, undocumented workers, and those in low-connectivity regions have no seat in the standards bodies or platform design processes that determined the access prerequisites (identification, minimum balances, device ownership) — their exclusion from first-holding was structural, not chosen, yet they bear the downstream costs of a cash-marginalizing default.
% DISAPPEARANCE_RATIONALE: If the practical infrastructure for holding digital money vanished, commerce would revert to physical-cash and barter-adjacent mechanisms; payroll, remittances, and credit systems built on digital balances would collapse, and the early-adopter advantage that compounded into differential creditworthiness and formal-economy access would evaporate along with it.
% FOUNDING_PROBLEM: Physical cash was costly and risky to store, transport, and transact at scale — theft risk, counterfeiting, settlement delay, and the impossibility of remote commerce. Non-physical monetary instruments (bank deposits, then electronic balances, then mobile money) solved this by making value storable and transferable as information rather than as object.
% FOUNDING_PROBLEM_CORROBORATION: Development economists and financial-inclusion researchers (outside the platform-operator beneficiary set) corroborate that the underlying storage/transfer problem remains live and unevenly solved — citing persistent unbanked populations and documented exclusion effects (e.g., World Bank Findex reports) as evidence the founding problem's SOLUTION, not the problem itself, has been distributed unequally.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high and rising (0.30 to 0.58) because as digital holding becomes the assumed default across commerce, employment, and credit systems, those without infrastructure access face compounding costs — not from malicious design, but from a default shifting around them. Suppression is moderate (0.42) — no one is legally barred from holding digital money, but device, identity, connectivity, and minimum-balance requirements function as structural gates. Theater ratio stays low (0.20) because the coordination function (efficient value storage/transfer) is real and substantially delivered, not merely performed. Accessibility collapse (0.52) is mid-range: alternatives (cash, barter) persist but are increasingly costly to use as the digital default hardens.
 *
 * DIRECTIONALITY LOGIC:
 *   Early infrastructure adopters and platform operators sit near the beneficiary end: they captured convenience and rent respectively at the point of origin and their advantage compounds. Unbanked and low-connectivity populations sit near the target end: they bear the cost of a hardening default they were never positioned to adopt from, with trapped exit options (no genuine substitute exists once formal-economy participation assumes digital holding). This is a first-holding-specific asymmetry: it did not exist at the conceptual-possibility stage (nobody had access yet) and is different in kind from the regulatory-recognition stage's asymmetry (which centers on statistical visibility, not practical access).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (safe, efficient, remote-capable value storage) remains live and is NOT mandatrophic at the aggregate level — but the classification here as tangled_rope (not rope) captures that the SAME infrastructure solving the coordination problem for adopters simultaneously imposes rising costs on non-adopters through no fault or choice of their own. This prevents the mistake of reading digital money's spread as pure Pareto-improving coordination (a rope story) when the first-holding definition foregrounds exactly the access asymmetry a conceptual or regulatory dating would background.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_dating_choice_ambiguity,
    'Should digital money''s origin be dated to conceptual feasibility, first practical holding, or regulatory recognition — and does the choice of dating point determine which populations appear as beneficiaries versus victims of the same historical process?',
    'Compare structural outcomes across the three readings: examine whether the beneficiary/victim sets and extraction profiles differ enough to constitute genuinely different constraints (per the epsilon-invariance principle) rather than three descriptions of one process.',
    'If the readings are structurally distinct constraints (as this decomposition assumes), each requires independent classification; conflating them would average away the access-asymmetry that is only visible under the first_held_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_dating_choice_ambiguity, conceptual, 'Whether kernel decomposition into three readings is warranted or whether one unified dating is more defensible.').

omega_variable(
    infrastructure_gate_naturalness,
    'Are the access prerequisites (identification, device ownership, connectivity, minimum balances) a natural consequence of the technology''s requirements, or a constructed set of gates that could have been designed more inclusively from the outset?',
    'Comparative institutional analysis: examine early mobile-money systems (e.g., M-Pesa) that achieved much lower access barriers than bank-deposit-based digital money, to test whether the barrier level was a design choice or a technical necessity.',
    'If barriers were largely a design choice, the tangled_rope classification is well-supported (avoidable extraction riding on genuine coordination); if barriers were largely technically necessary at the time, the constraint sits closer to a scaffold that has since had many of its barriers technically resolved but not yet retired.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_gate_naturalness, empirical, 'Whether infrastructure access barriers were designed or technically necessitated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__first_held_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(digi_tr_t10, digital_money_origin__first_held_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(digi_tr_t20, digital_money_origin__first_held_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(digi_tr_t30, digital_money_origin__first_held_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__first_held_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(digi_tr_t50, digital_money_origin__first_held_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__first_held_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(digi_be_t10, digital_money_origin__first_held_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(digi_be_t20, digital_money_origin__first_held_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(digi_be_t30, digital_money_origin__first_held_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__first_held_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(digi_be_t50, digital_money_origin__first_held_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__first_held_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(digi_su_t10, digital_money_origin__first_held_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(digi_su_t20, digital_money_origin__first_held_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(digi_su_t30, digital_money_origin__first_held_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__first_held_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(digi_su_t50, digital_money_origin__first_held_reading, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_origin__first_held_reading, 0.15).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the digital_money_origin kernel. became_thinkable_reading dates origin to conceptual/technical feasibility (pre-implementation, minimal beneficiary/victim structure). regulatory_recognition_reading dates origin to formal statistical/regulatory incorporation (beneficiaries: compliant institutions and monetary authorities; victims: entities operating outside recognized instruments). This first_held_reading is the only one of the three that centers PRACTICAL ACCESS as origin-defining, which is what generates its distinctive beneficiary set (early infrastructure adopters) and victim set (populations without infrastructure access) at the moment of origin itself — a structural feature the other two readings do not share at their respective origin points.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
