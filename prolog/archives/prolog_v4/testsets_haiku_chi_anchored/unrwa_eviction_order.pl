% ============================================================================
% CONSTRAINT STORY: unrwa_eviction_order
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unrwa_eviction_order, []).

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
 *   constraint_id: unrwa_eviction_order
 *   human_readable: Israeli Land Authority's Eviction Order for UNRWA HQ in East Jerusalem
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   The Israeli Land Authority's 30-day eviction order for UNRWA headquarters
 *   in East Jerusalem represents a structural extraction mechanism masked by
 *   legal language and sovereignty claims. UNRWA operates as the de facto
 *   social safety net for 5.9 million Palestinian refugees across the Middle
 *   East, providing health, education, and social services. The headquarters
 *   closure would disrupt regional operations and symbolically demonstrate
 *   that humanitarian agencies cannot maintain infrastructure in territories
 *   under Israeli control without Israeli permission. The constraint exhibits
 *   structural characteristics of a pure snare: high suppression
 *   (institutional dependency on Israeli sufferance), high extractiveness
 *   (forced service abandonment), moderate theater (legal justifications for
 *   political objectives), and trapped victims (UNRWA has no meaningful exit
 *   options and Palestinian refugees cannot substitute services). However,
 *   the constraint also displays perspectival complexity: Israeli authorities
 *   frame it as legitimate property enforcement (Rope from their
 *   perspective); international humanitarian governance experiences it as
 *   mixed coordination-extraction (Tangled Rope); the degraded UN immunity
 *   framework appears as institutional inertia (Piton); and analytical
 *   observers see structural dependency as the real constraint (Snare). The
 *   theater ratio reflects increasing resort to legal language to justify
 *   political extraction — initial justifications were vague, but over months
 *   they crystallized into specific contract and construction allegations,
 *   indicating performative legalization of a foundational sovereignty claim.
 *
 * KEY AGENTS:
 *   - Israeli Land Authority: Primary beneficiary (institutional/arbitrage) — exercises state sovereignty, establishes property control, removes infrastructure perceived as supporting Palestinian institutional claims
 *   - UNRWA Operations: Primary victim (powerless/trapped) — faces eviction with no legal recourse, cannot relocate facilities, forced to cease regional coordination from HQ location
 *   - Palestinian Refugee Population: Primary victim (moderate/trapped) — depends on UNRWA services; cannot obtain equivalent services through PA; trapped by legal status and displacement history
 *   - International Humanitarian System: Secondary actor (powerful/mobile) — can invoke international law and diplomatic pressure but constrained by dependence on Israeli cooperation and host-state sovereignty
 *   - UN Legal Immunity Framework: Institutional actor (institutional/constrained) — nominally protects UN agencies but functionally dependent on enforcement by host states and Security Council (degraded piton)
 *   - Analytical Observer: Institutional perspective (analytical/analytical) — reveals that humanitarian institutions operate under structural dependency, not legal immunity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unrwa_eviction_order, 0.68).
domain_priors:suppression_score(unrwa_eviction_order, 0.78).
domain_priors:theater_ratio(unrwa_eviction_order, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unrwa_eviction_order, extractiveness, 0.68).
narrative_ontology:constraint_metric(unrwa_eviction_order, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unrwa_eviction_order, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unrwa_eviction_order, snare).
narrative_ontology:human_readable(unrwa_eviction_order, "Israeli Land Authority's Eviction Order for UNRWA HQ in East Jerusalem").
narrative_ontology:topic_domain(unrwa_eviction_order, "geopolitical/legal").

domain_priors:requires_active_enforcement(unrwa_eviction_order).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unrwa_eviction_order, israeli_state_authority).
narrative_ontology:constraint_victim(unrwa_eviction_order, unrwa_humanitarian_operations).
narrative_ontology:constraint_victim(unrwa_eviction_order, palestinian_refugee_population).
narrative_ontology:constraint_victim(unrwa_eviction_order, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNRWA OPERATIONS (SNARE) — Faces 30-day eviction order with no legal recourse in Israeli courts; cannot relocate humanitarian infrastructure; trapped by sovereignty and jurisdictional asymmetry. Extraction occurs through forced abandonment of facilities, cessation of services, and erosion of operational capacity. d≈0.92, f(d)≈1.39, σ=0.8 → χ≈0.76.
constraint_indexing:constraint_classification(unrwa_eviction_order, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PALESTINIAN REFUGEE POPULATION (SNARE) — Depends on UNRWA services (health, education, social assistance); cannot obtain equivalent services through Palestinian Authority; trapped by displacement history and legal status. UNRWA closure represents extraction of essential services and hardening of refugee condition. d≈0.88, f(d)≈1.32, σ=0.9 → χ≈0.79.
constraint_indexing:constraint_classification(unrwa_eviction_order, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI STATE AUTHORITY (ROPE) — Experiences eviction order as enforcement of territorial sovereignty and legal property claims. Framed as coordination: assert state authority over disputed property; establish legal clarity; remove infrastructure perceived as supporting adverse claims to land. d≈0.08, f(d)≈-0.09, σ=1.1 → χ≈-0.07. Negative effective extraction = net beneficiary. Exit option is arbitrage: can shift ground to alternative legal claims or international pressure without operational cost.
constraint_indexing:constraint_classification(unrwa_eviction_order, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL HUMANITARIAN GOVERNANCE (TANGLED ROPE) — UN agencies operate under coordination mandate (humanitarian access, impartiality) but experience extraction through political pressure, jurisdictional constraints, and delegitimization. Mobile because can invoke international law, diplomatic channels, and funding constituencies; but constrained by host-state sovereignty and dependence on Israeli cooperation for operational access. d≈0.62, f(d)≈0.78, σ=1.2 → χ≈0.41.
constraint_indexing:constraint_classification(unrwa_eviction_order, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UN LEGAL IMMUNITY FRAMEWORK (PITON) — UNRWA's operational immunity (Convention on Privileges and Immunities of the United Nations) is institutionally degraded: nominal protection exists but enforcement is performative because implementation depends on host-state compliance and Security Council backing (which Israel can veto). Theater ratio 0.65 reflects gap between stated immunity and actual enforceability. The legal framework persists through inertia despite low functional protection. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(unrwa_eviction_order, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the constraint reveals structural dependency: UNRWA's continued operation depends on the sufferance of host states; humanitarian agencies have no independent enforcement mechanism; UN immunity is aspirational rather than real. The eviction order demonstrates that UNRWA is trapped by institutional dependency, not protected by it. d≈0.78, f(d)≈1.18, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(unrwa_eviction_order, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unrwa_eviction_order_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unrwa_eviction_order, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unrwa_eviction_order, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unrwa_eviction_order, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unrwa_eviction_order, TR),
    TR >= 0.70.

:- end_tests(unrwa_eviction_order_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High. The eviction order forces UNRWA to abandon HQ operations, cease regional coordination from East Jerusalem, and transfer services elsewhere — representing material extraction of institutional capacity. The extraction is not absolute (UNRWA could theoretically relocate) but severe given infrastructure investment, operational disruption, and symbolic loss of presence. Suppression (0.78): Very high. Multiple suppressive mechanisms operate simultaneously: (1) Israeli sovereignty claims preclude legal recourse; (2) Palestinians and UN agencies lack enforcement mechanisms; (3) 30-day timeline creates artificial urgency; (4) alternative locations for HQ are diplomatically fraught or unavailable; (5) UNRWA operates in 5 host countries but has no coordination mechanism independent of host-state permission. Theater ratio (0.65): Moderate-high. The eviction order is framed in legal language (contract violations, building code enforcement) but these justifications appear post-hoc: the underlying driver is political assertion of Israeli control over East Jerusalem. Legal claims may have genuine basis (construction without permits is typical in Palestinian areas), but selective enforcement against UN agencies rather than Palestinian or Israeli violators indicates performative legalism. Theater ratio increases over the interval as arguments crystallize into legalistic form.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates severe perspectival divergence. Israeli authorities experience it as legitimate sovereignty enforcement (Rope) — they are solving the problem of establishing clear legal control over disputed territory. UNRWA experiences it as an inescapable trap (Snare) — the agency has no option to remain, no ability to legally contest the order, and no political constituency powerful enough to block enforcement. Palestinian refugees experience it as service extraction (Snare) — UNRWA closure means loss of services they depend on and cannot replace. International humanitarian governance experiences it as a mixed constraint (Tangled Rope) — they benefit from coordination with UNRWA (humanitarian effectiveness) but face extraction through political pressure to comply or risk losing access. The UN legal immunity framework appears institutionally degraded (Piton) — it nominally protects UNRWA but has no enforcement mechanism independent of Israeli willingness to comply. The analytical observer sees this as revealing structural dependency (Snare) — humanitarian institutions are not protected by international law but rather operate at the sufferance of host states, and the eviction order demonstrates this powerlessness.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli State Authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary. Can walk away from any legal challenge (arbitrage exit) and benefits from territorial assertion. UNRWA: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. Has no exit option — cannot stay without Israeli permission, cannot legally contest, cannot relocate without losing operational effectiveness. Palestinian Refugees: Victim + trapped → d≈0.88, f(d)≈1.32. High extraction. Depend on UNRWA services and cannot exit even if dissatisfied; loss of UNRWA means loss of services. International Humanitarian Governance: Victim + mobile (but constrained) → d≈0.62, f(d)≈0.78. Moderate extraction. Can invoke international law and diplomatic pressure (mobile) but depends on Israeli cooperation for access (constrained). UN Legal Framework: Institutional + constrained → d≈0.45, f(d)≈0.50. Cannot exit the dependency on host-state enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CONFIRMATION: The high extractiveness (0.68) combined with high suppression (0.78) and the presence of clear victims (UNRWA, Palestinian refugees) confirms snare classification despite Israeli framing as property enforcement. The mandatrophy resolution depends on recognizing that 'legitimate legal authority' and 'extraction mechanism' are not mutually exclusive — states can exercise legal sovereignty while simultaneously extracting value from trapped populations. The constraint's mandatrophy is resolved by the perspectival analysis: the beneficiary (Israeli state) experiences it as Rope (coordination of property control), but the victims experience it as Snare (inescapable harm). The analytical observer's role is to recognize that when a legal mechanism produces trapped victims and asymmetric extraction, the legal language is secondary to the structural reality. The theater ratio (0.65) indicates that legal justifications are significant but not dominant — the order is partially performative. Mandatrophy is fully resolved: this is a snare (ε=0.68, suppression=0.78, χ≈0.76 from victim perspective), with perspectival variation showing why beneficiaries might deny or minimize the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_jurisdictional_ambiguity,
    'Does Israeli Land Authority have legitimate legal jurisdiction over the UNRWA compound in East Jerusalem under international law, domestic Israeli law, or Oslo Accords provisions?',
    'Legal analysis by International Court of Justice or UN legal experts; review of Oslo Accords land designation (Area C vs Area A/B); examination of chain of title and UNRWA''s legal status as UN agency',
    'If Israel has legitimate jurisdiction: eviction order becomes standard property enforcement (Rope/Piton). If jurisdiction is contested: eviction becomes political extraction (Snare/Tangled Rope). If UNRWA has legal immunity: order is void (Mountain-like legal constraint on enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_jurisdictional_ambiguity, conceptual, 'Whether Israeli jurisdiction is legally valid').

omega_variable(
    humanitarian_replacement_feasibility,
    'Can Palestinian Authority or international bodies replace UNRWA services (health, education, social assistance) within 30 days or any reasonable timeframe?',
    'Assessment of PA institutional capacity; cost and timeline estimates for service transfer; documentation of service gaps in refugee communities if UNRWA closes',
    'If replacement feasible: constraint is temporary disruption (Scaffold). If infeasible: constraint is permanent harm (Snare). If partial: tangled outcome (Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_replacement_feasibility, empirical, 'Whether alternative service provision is possible').

omega_variable(
    enforcement_mechanism_reliability,
    'Can Israeli authorities actually enforce a 30-day eviction order against a UN agency with claimed immunity, or does the order require international compliance?',
    'Monitoring of actual enforcement attempts; observation of whether UNRWA leaves voluntarily, is physically removed, or remains; tracking of UN and international response',
    'If enforced unilaterally: snare classification confirmed (state power overrides immunity). If blocked by international action: order becomes theater (Piton). If prolonged conflict: reveals tangled rope (extraction + coordination hybrid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_reliability, empirical, 'Whether eviction order can be actually enforced').

omega_variable(
    political_motive_vs_legal_claim,
    'Are the stated grounds (contract violation, illegal construction) genuine legal violations or pretext for political objectives (reduce Palestinian institutional presence, delegitimize UNRWA, restrict aid)?',
    'Forensic examination of construction records; comparison with enforcement patterns against Israeli and Palestinian entities; analysis of timing relative to geopolitical events; expert engineering assessment',
    'If genuine legal violations: order is legitimate enforcement (Rope). If pretext: order is extraction through false legality (Snare). If mixed: order is tangled rope (coordination language masking extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_motive_vs_legal_claim, preference, 'Whether legal claims are primary motivation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unrwa_eviction_order, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unrwa_tr_t0, unrwa_eviction_order, theater_ratio, 0, 0.4).
narrative_ontology:measurement(unrwa_tr_t2, unrwa_eviction_order, theater_ratio, 2, 0.52).
narrative_ontology:measurement(unrwa_tr_t4, unrwa_eviction_order, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(unrwa_be_t0, unrwa_eviction_order, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(unrwa_be_t2, unrwa_eviction_order, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(unrwa_be_t4, unrwa_eviction_order, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unrwa_eviction_order, enforcement_mechanism).
narrative_ontology:affects_constraint(unrwa_eviction_order, palestinian_authority_institutional_capacity).
narrative_ontology:affects_constraint(unrwa_eviction_order, east_jerusalem_legal_status).
narrative_ontology:affects_constraint(unrwa_eviction_order, refugee_services_substitution).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the fundamental legal status of East Jerusalem and the sovereignty question. The eviction order is a specific enforcement action that reveals the broader structural constraint: UNRWA's continued operation depends on the sufferance of host states and the fragile assumption that humanitarian agencies have meaningful legal immunity. The upstream constraint is jurisdictional ambiguity; the downstream constraint is service delivery system vulnerability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unrwa_eviction_order, analytical, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
