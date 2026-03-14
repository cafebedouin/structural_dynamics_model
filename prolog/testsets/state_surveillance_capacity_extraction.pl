% ============================================================================
% CONSTRAINT STORY: state_surveillance_capacity_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_surveillance_capacity_extraction, []).

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
 *   constraint_id: state_surveillance_capacity_extraction
 *   human_readable: State Surveillance Capacity Extraction
 *   domain: political/technological
 *
 * SUMMARY:
 *   State surveillance capacity extraction models the structural asymmetry
 *   between state security apparatus and the population under surveillance.
 *   The constraint emerges from technological capability (signals
 *   intelligence, data aggregation, pattern analysis) combined with
 *   asymmetric knowledge (state knows what it collects; population does not)
 *   and asymmetric exit options (population cannot opt out; state apparatus
 *   can reallocate resources if surveillance is curtailed). The surveillance
 *   regime's extractiveness has increased over the measurement interval (0.35
 *   → 0.68) as technical capabilities expanded and oversight mechanisms
 *   proved inadequate. The theater ratio (0.55) reflects the persistent
 *   performance of democratic oversight (warrants, court review, legislative
 *   debate) that operates on a different timescale and with different
 *   information access than the actual surveillance apparatus. The constraint
 *   exhibits snare classification from most perspectives because the
 *   suppression (0.75) is structural and comprehensive — citizens cannot
 *   meaningfully exit the surveillance infrastructure, and the constraint
 *   extracts behavioral modification (chilling effects on expression and
 *   assembly) with minimal coordination benefit to the population.
 *
 * KEY AGENTS:
 *   - State Security Apparatus: Primary beneficiary (institutional/mobile) — captures threat intelligence, targeting capability, population behavior monitoring; can reallocate resources to alternative intelligence methods but efficiency reduced
 *   - Population Under Surveillance: Primary victim (powerless/trapped) — cannot exit; bears full cost of behavioral chilling effects
 *   - Democratic Deliberation (Epistemic Commons): Secondary victim (powerless/identity_locked) — cannot exit without abandoning identity; surveillance degrades open discourse and information access
 *   - Journalists and Activists: Secondary victim (moderate/constrained) — can employ operational security or relocate but at high professional cost
 *   - Judicial and Legislative Oversight Bodies: Institutional actor (institutional/arbitrage) — maintain nominal authority but lack effective enforcement; surveillance capacity is too technical for legislative review and too compartmentalized for judicial access
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — observes ratchet mechanism: surveillance capacity, once developed, is retained and expanded without reversal; no democratic state has successfully reduced surveillance scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_surveillance_capacity_extraction, 0.68).
domain_priors:suppression_score(state_surveillance_capacity_extraction, 0.75).
domain_priors:theater_ratio(state_surveillance_capacity_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_surveillance_capacity_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_surveillance_capacity_extraction, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(state_surveillance_capacity_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_surveillance_capacity_extraction, snare).
narrative_ontology:human_readable(state_surveillance_capacity_extraction, "State Surveillance Capacity Extraction").
narrative_ontology:topic_domain(state_surveillance_capacity_extraction, "political/technological").

domain_priors:requires_active_enforcement(state_surveillance_capacity_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_surveillance_capacity_extraction, state_security_apparatus).
narrative_ontology:constraint_victim(state_surveillance_capacity_extraction, population_under_surveillance).
narrative_ontology:constraint_victim(state_surveillance_capacity_extraction, democratic_deliberation).
narrative_ontology:constraint_victim(state_surveillance_capacity_extraction, press_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED CITIZEN (SNARE) — Citizens cannot opt out of surveillance infrastructure. Exit options are trapped: relocating to another jurisdiction is materially difficult, legally restricted, or provides only illusory escape (surveillance is international). The constraint extracts behavioral modification (chilling effect on expression, communication, political participation) without corresponding coordination benefit. Maximum experienced extraction.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEMOCRATIC DELIBERATION (SNARE, identity_locked) — The epistemic commons cannot exit. Its identity is constituted through open deliberation and shared information access. Surveillance degrades both through targeted suppression of certain speakers and through mass chilling effects. The commons cannot perceive alternatives from within its own frame because the frame assumes open discourse as foundational. This is an identity lock, not a trapped boundary: the epistemic infrastructure could restructure itself to function under surveillance (encrypted, pseudonymous, federated), but doing so would require abandoning the identity claim of 'democratic deliberation' and becoming something else entirely.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: JOURNALISTS AND ACTIVISTS (SNARE) — Constrained exit: they can relocate, use pseudonyms, or adopt operational security practices, but these actions carry high costs (professional isolation, reduced impact, career disruption). Surveillance extracts through source chilling, restricted access to information, and targeting of organizing networks. The constraint produces snare classification because effective exit costs are near-total for remaining in their profession.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE SECURITY APPARATUS (TANGLED ROPE) — The security apparatus benefits from surveillance capacity (access to threat intelligence, targeting capability, population control) while also bearing genuine coordination costs (legal oversight requirements, operational complexity, budget constraints). They experience the constraint as both enabling and constraining. The apparatus can reallocate resources (exit the current surveillance regime for alternative intelligence methods), but operational efficiency is reduced. Tangled rope reflects genuine security coordination alongside asymmetric extraction from the population.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: OVERSIGHT BODIES (PITON) — Courts and legislatures maintain the performative function of overseeing surveillance (issuing warrants, debating authority, setting limits) but lack effective enforcement mechanisms. Surveillance capabilities are too technical for legislative review, data requests from courts are routinely delayed, and national security exceptions override formal oversight. The theater persists through institutional inertia — oversight bodies maintain their identity as constraint-setters even as their practical authority has atrophied. Theater ratio reflects the gap between nominal oversight authority and actual surveillance scope.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE, civilizational scope) — From a civilizational perspective, modern surveillance is a structural feature of state apparatus itself — the technological capacity, once developed, is retained and expanded without reversal. No democratic state has meaningfully reduced surveillance capacity after expansion. The constraint appears unchangeable at civilizational scale because the ratchet mechanism (capacity enabling new threat perception enabling expanded authority) is self-reinforcing. From this vantage, surveillance is snare across all observations.
constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_surveillance_capacity_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_surveillance_capacity_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_surveillance_capacity_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_surveillance_capacity_extraction, TR),
    TR >= 0.70.

:- end_tests(state_surveillance_capacity_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state apparatus extracts information asymmetrically and uses that information to modify population behavior through selective targeting and ambient panopticism. The measurement trajectory shows continuous expansion: initial capability (0.35) drove threat inflation, which justified expanded collection, which created capacity for expanded extraction, which in turn created new threat categories. This is a ratchet mechanism. The 0.68 value reflects that extraction has reached comprehensive scale while retention of alternative intelligence methods remains technically feasible (preventing maximum extraction). Suppression (0.75): Very high. The suppression is structural and multi-layered: technological complexity prevents understanding what is collected; classification prevents disclosure of collection methods; legal carve-outs (national security exceptions) prevent remedy; international coordination prevents jurisdictional escape. Exit costs are near-absolute for remaining in normal society. Theater ratio (0.55): Moderate-high. Oversight mechanisms (warrant requirements, FISA court review, congressional intelligence committees) persist as formal constraints but operate with massive information asymmetry and compartmentalization. Their theater is real — they produce binding formal requirements — but actual enforcement capability is limited by the state's classification authority and technical complexity. The ratio is increasing over the interval as oversight bodies' capability lag increases with collection scale.
 *
 * PERSPECTIVAL GAP:
 *   The security apparatus sees tangled rope (both coordination and extraction) because surveillance capacity genuinely enables threat detection while also creating operational constraints and legal oversight burden. But the population sees snare (pure extraction with minimal coordination benefit). This gap reflects asymmetric information access and divergent beneficiary/victim status. The population cannot perceive the genuine security coordination benefits because they lack threat intelligence; the security apparatus cannot perceive the extraction it imposes because it operates under classified threat models. The oversight bodies' piton perspective reveals the mechanism: they perform the role of constraint-setter while operating with a fundamentally different information base than the apparatus being constrained. The epistemic commons' identity_locked perspective reveals that democratic deliberation as a social form is threatened by surveillance not through material barriers but through the impossibility of open deliberation while under panopticism — the identity of 'democratic' and 'open' cannot be maintained while populations self-censor.
 *
 * DIRECTIONALITY LOGIC:
 *   The security apparatus (institutional/mobile) derives low d because they are structural beneficiaries with exit options (can shift to alternative intelligence methods). Their d is approximately 0.20. The population (powerless/trapped) derives high d because they are structural victims with no exit — their d is approximately 0.95. Journalists and activists (moderate/constrained) derive d approximately 0.65 because exit is materially possible but carries high professional costs. The epistemic commons (powerless/identity_locked) derives d approximately 0.92 because while structurally mobile (could relocate to non-surveillance jurisdictions), they cannot exercise this mobility without abandoning the identity that constitutes them as 'democratic deliberation.' The identity lock is a perceptual filter: the commons cannot perceive restructuring options as genuine alternatives because doing so would require them to become something other than what they are. Oversight bodies (institutional/arbitrage) derive low-to-moderate d (approximately 0.35) because they nominally benefit from security apparatus function while also being partially captured by it — their directionality is mixed, reflected in the tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that surveillance capacity is fundamentally asymmetric: the apparatus cannot be constrained by the population it surveils because constraint mechanisms depend on information the population does not possess and cannot access. Traditional extraction constraint types (rope, scaffold) assume relatively symmetric information access and exit options. Snare classification is appropriate because suppression is comprehensive, exit is unavailable, and there is no genuinely mutual coordination function — the apparatus benefits; the population bears costs. The tangled rope perspective from the apparatus reflects their genuine experience, but it does not undermine the snare classification from the population's perspective. The analysis prevents mislabeling surveillance as 'coordination for security' by requiring explicit declaration of coordination benefit to the population. Surveillance provides zero coordination benefit to the population (no shared security dividend, no information exchange, no mutual constraint reduction). The apparatus benefits unilaterally. This is extraction, not coordination. The scaffold perspective would be appropriate only if surveillance capacity had a genuine sunset clause (e.g., emergency powers with legislative reauthorization requirement, declining over a defined interval). Current surveillance regimes have ratchet mechanisms that prevent sunset. Therefore, scaffold does not apply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_for_mass_surveillance_extraction,
    'At what surveillance coverage threshold does the mechanism shift from targeted extraction to mass behavioral modification through ambient panopticism?',
    'Cross-national comparison of surveillance capacity vs measured chilling effects in speech, assembly, and political participation. Identify correlation between coverage density and self-censorship rates.',
    'If threshold is low (< 30% coverage): ambient surveillance is the mechanism, snare classification is robust. If threshold is high (> 70% coverage): targeted extraction dominates, and some populations might experience the constraint as lower extraction than others, shifting some perspectives toward tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_for_mass_surveillance_extraction, empirical, 'Coverage threshold for transition from targeted to mass behavioral extraction').

omega_variable(
    security_coordination_necessity,
    'Is the security apparatus''s coordination benefit genuine (surveillance prevents real threats) or illusory (perceived threats are artifacts of surveillance-driven threat inflation)?',
    'Longitudinal analysis of threat prevention claims. Comparison of attack prevention rates in high-surveillance vs low-surveillance jurisdictions controlling for threat density. Assessment of how threat perception changed before and after surveillance expansion.',
    'If genuine: tangled rope classification of security apparatus perspective is correct; some asymmetry is justified. If illusory: apparatus perspective should be snare; extraction has no coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_coordination_necessity, empirical, 'Whether security apparatus coordination benefit is genuine or threat-inflation artifact').

omega_variable(
    international_surveillance_coupling,
    'Are national surveillance regimes operating independently or are they coordinated through intelligence-sharing treaties and shared infrastructure?',
    'Documentation of data-sharing agreements, common technical standards, joint operations, and personnel exchanges among national security agencies. Assessment of whether the regime is better modeled as single global surveillance apparatus or as competing national programs.',
    'If coordinated: global scope analysis is justified, and exit via relocation is truly trapped (nowhere outside the network). If independent: individuals might arbitrage between jurisdictions, shifting some perspectives from trapped to constrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_surveillance_coupling, empirical, 'Whether surveillance regimes are coordinated or independent').

omega_variable(
    identity_lock_vs_constrained_distinction,
    'For the democratic deliberation perspective, is the binding mechanism identity-lock (cannot imagine deliberation under surveillance) or constraint (high cost to restructure deliberation infrastructure)?',
    'Comparison of how deliberative communities actually adapt when under surveillance: do they develop encrypted, pseudonymous, or federated alternatives that function as deliberation by other means, or do they collapse? Evidence of psychological inability to imagine alternatives vs structural inability to implement them.',
    'If identity-lock: the epistemic commons is genuinely trapped by its own framing and cannot self-restructure. If constrained: communities can and do restructure, and the snare classification is too strong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_distinction, conceptual, 'Whether democratic deliberation is identity-locked or constrained by surveillance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_surveillance_capacity_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_surveillance_capacity_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stat_tr_t10, state_surveillance_capacity_extraction, theater_ratio, 10, 0.45).
narrative_ontology:measurement(stat_tr_t20, state_surveillance_capacity_extraction, theater_ratio, 20, 0.55).
narrative_ontology:measurement(stat_tr_t5, state_surveillance_capacity_extraction, theater_ratio, 5, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_surveillance_capacity_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t10, state_surveillance_capacity_extraction, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(stat_be_t20, state_surveillance_capacity_extraction, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(stat_be_t5, state_surveillance_capacity_extraction, base_extractiveness, 5, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_surveillance_capacity_extraction, enforcement_mechanism).
narrative_ontology:affects_constraint(state_surveillance_capacity_extraction, press_freedom_under_threat).
narrative_ontology:affects_constraint(state_surveillance_capacity_extraction, political_dissent_suppression).
narrative_ontology:affects_constraint(state_surveillance_capacity_extraction, encrypted_communication_restriction).

% DUAL FORMULATION NOTE:
% State surveillance capacity extraction is upstream of constraints on specific freedoms (press, dissent, encrypted communication). The surveillance apparatus creates the technical and legal infrastructure for downstream restrictions. Each downstream constraint should link back to this root constraint via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_surveillance_capacity_extraction, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
