% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Joint Liability Attribution via Causal Contribution and Control
 *   domain: technology_governance/legal_theory
 *
 * SUMMARY:
 *   Joint liability attribution regimes allocate legal responsibility for
 *   technology harms based on causal contribution and control. This
 *   constraint exists at the intersection of legal doctrine, technology
 *   governance, and risk allocation. The shared_liability reading holds that
 *   both developers (who design and control the technology) and deployers
 *   (who choose when and how to deploy) bear joint and several liability for
 *   harm caused by the technology. This contrasts with alternative readings:
 *   developer_liability (full responsibility on the technology creator) and
 *   deployer_liability (full responsibility on the operational actor). The
 *   shared_liability reading produces a tangled structure: it coordinates
 *   safety incentives across the value chain (both developers and deployers
 *   have reason to invest in safety) while simultaneously extracting
 *   compliance costs from all parties, suppresses alternatives through legal
 *   doctrine, and generates theater through retroactive causal contribution
 *   litigation. The constraint exhibits all six DR types from different
 *   perspectives because different actors occupy radically different
 *   structural positions: platform developers with architectural control and
 *   contractual exit options, deployers with regulatory constraints and
 *   operational responsibility, distributed suppliers with no exit, and the
 *   legal apparatus itself maintaining doctrine through case law.
 *
 * KEY AGENTS:
 *   - Platform Developer: Institutional beneficiary (institutional/arbitrage) — controls architecture and design choices; can shift liability to deployers and suppliers through contractual allocation; retains IP value
 *   - Technology Deployer: Mixed victim/beneficiary (moderate/constrained) — benefits from rapid deployment and cost-shifting; also bears joint liability exposure and regulatory responsibility; operationally trapped by deployment dependencies
 *   - Distributed Supplier: Primary victim (powerless/trapped) — small component manufacturer or service provider with no negotiating power; subject to indemnification cascades; bears uninsurable liability exposure
 *   - Insurance/Indemnity Market: Organized actor (organized/constrained) — coordinates risk allocation through pricing; extracts information rents through asymmetry between insurer knowledge and insured uncertainty
 *   - Courts and Legal Doctrine: Institutional actor (institutional/arbitrage) — maintains shared liability framework through case law and precedent; generates theater through retroactive causal contribution tests; produces billable legal work
 *   - Injured Parties: Structural beneficiary (powerless/trapped) — hold joint and several claims against multiple defendants; benefit from expanded liability regime but do not control doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.52).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.65).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.52).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Joint Liability Attribution via Causal Contribution and Control").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '2fbd4b40-a28c-453a-85d3-c74d01dc0b47').
narrative_ontology:cs_kernel_codification('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', formalized).
narrative_ontology:cs_authority_grounding('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', extraction).
narrative_ontology:cs_interpretation_layer_present('2fbd4b40-a28c-453a-85d3-c74d01dc0b47').
narrative_ontology:cs_reading_relation('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', foundational, causal_contribution_basis_for_allocation).
narrative_ontology:cs_axiom_status(causal_contribution_basis_for_allocation, holdable).
narrative_ontology:cs_axiom_grounding('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', causal_contribution_basis_for_allocation, deontological).
narrative_ontology:cs_axiom('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', secondary, control_as_liability_anchor).
narrative_ontology:cs_axiom_status(control_as_liability_anchor, overridden).
narrative_ontology:cs_axiom_grounding('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', control_as_liability_anchor, empirically_contingent).
narrative_ontology:cs_reference_frame('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', joint_causal_responsibility).
narrative_ontology:cs_drift_state('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', contemporary_complex_systems, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2fbd4b40-a28c-453a-85d3-c74d01dc0b47', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, injured_parties).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_markets).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, technology_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, technology_deployers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, supply_chain_intermediaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISTRIBUTED SUPPLIER (SNARE) — Small component manufacturer or service provider in a global supply chain. Cannot exit (material dependency), cannot defend (asymmetric legal resources), cannot predict liability exposure (rules allocate retroactively). Trapped bears full extraction with zero agency. Maximum suppression through contractual indemnification cascades.
constraint_indexing:constraint_classification(liability_attribution__shared_liability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER DEPLOYER (TANGLED ROPE) — Company deploying technology in operational setting (healthcare, transportation, infrastructure). Benefits from rapid deployment capability and cost-shifting through contractual liability allocation. Also bears significant risk: causal contribution doctrine makes them a joint defendant in harm cases. Constrained exit (regulatory requirements, market pressure, customer expectations force continued deployment despite liability exposure). Mixed coordination (liability rules coordinate safety incentives) and extraction (asymmetric burden).
constraint_indexing:constraint_classification(liability_attribution__shared_liability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM DEVELOPER (ROPE) — Large technology firm controlling algorithmic, architectural, or design decisions. Benefits from broad architectural control and ability to disclaim liability through contractual allocation to deployers and users. Experiences shared liability as a coordination mechanism: causal contribution rules incentivize safety-by-design. Arbitrage exit (can shift deployment to regions with favorable liability regimes, or exit the market entirely while retaining IP). Net beneficiary — extraction runs toward this agent through contractual pass-through.
constraint_indexing:constraint_classification(liability_attribution__shared_liability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSURANCE/INDEMNITY MARKET (TANGLED ROPE) — Organized institutional structure (insurers, reinsurers, captive insurance arms, indemnity funds). Coordinates risk allocation by pricing liability exposure. Also extracts surplus: information asymmetry between insurers (who know causal contribution tests empirically) and insured parties (who must estimate exposure). Constrained: regulatory oversight and competitive pressure limit extraction. This perspective shows genuine coordination function (risk pooling and pricing) coupled with asymmetric information extraction.
constraint_indexing:constraint_classification(liability_attribution__shared_liability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGAL DOCTRINE APPARATUS (PITON) — Courts, precedent, doctrinal interpretation of 'causal contribution,' 'control,' 'foreseeability.' Maintains the shared liability framework through case law and statutory interpretation. Theater-heavy: extensive litigation over causal contribution doctrine produces elaborate legal reasoning with unclear real-world verification (what counts as 'control' is litigated retrospectively, creating theater). The apparatus sees itself as degraded — judges acknowledge doctrine's vagueness but maintain it through inertia. Institutional arbitrage (courts generate billable controversy; legal certainty is not the apparatus's product).
constraint_indexing:constraint_classification(liability_attribution__shared_liability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, shared liability appears to reflect an immutable principle: whoever causes harm bears responsibility. Causation and control are presented as objective features of the world, not contingent institutional constructs. This perspective risks false summitry — naturalizing what is actually a contested doctrinal choice (causal contribution is a legal construct with alternatives like strict liability, no-fault regimes, or developer-only liability). The engine will flag this as a false summit if beneficiary declarations are present.
constraint_indexing:constraint_classification(liability_attribution__shared_liability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liability_attribution__shared_liability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liability_attribution__shared_liability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(liability_attribution__shared_liability, TR),
    TR >= 0.70.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Shared liability extracts compliance costs from all participants in the value chain — developers must invest in safety-by-design, deployers must monitor and control deployment, suppliers must maintain documentation and insurance. The extraction is not maximal (0.70+) because the coordination function is genuine: shared liability does create safety incentives across the value chain. But the extraction exceeds a pure coordination mechanism (0.30–0.45) because the burden falls asymmetrically on those with less bargaining power, and the legal doctrine itself is ambiguous enough to create perpetual litigation costs. Suppression (0.65): High. Parties cannot exit shared liability: regulatory requirements often mandate liability insurance, contractual escape clauses may be unenforceable against third-party claimants, and reputational damage from 'trying to escape liability' is severe. Suppression mechanisms include: contractual indemnification cascades (each party pushes liability downstream), insurance requirements (parties must maintain expensive coverage), and doctrinal ambiguity (no party knows ex ante what 'causal contribution' will mean in litigation). Theater ratio (0.58): Moderate-high. The causal contribution test itself generates substantial theater. Litigation retroactively determines what 'control' meant, what 'causal contribution' included, and how to allocate responsibility. The legal apparatus produces elaborate doctrinal reasoning about causation while the actual causal structures in complex systems (AI, distributed infrastructure, supply chains) resist clear attribution. Theater increased over the interval (0.35 → 0.58) as systems became more complex and liability allocation became more contested.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a radical perspectival divide driven by structural position in the value chain and exit options. The platform developer (institutional/arbitrage) experiences shared liability as a coordination mechanism that they control — they write the indemnification clauses, set the liability caps, and retain architectural choices. The deployer (moderate/constrained) experiences shared liability as a mixed burden — they benefit from rapid deployment capability but bear significant operational liability exposure. The distributed supplier (powerless/trapped) experiences shared liability as pure extraction — they cannot negotiate, cannot exit, and cannot predict or control their liability exposure. The insurance market experiences shared liability as a coordination + information extraction opportunity. The legal apparatus experiences its own doctrine as degraded (theater-heavy, ambiguous, retroactive). The natural law view risks seeing causation as an objective feature to which liability naturally aligns, missing that causation is itself a doctrinal construct with alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position and exit options. Platform developers (beneficiary + arbitrage exit) get low d values (~0.15) — they experience low or negative effective extraction because they control the rules. Deployers (victim + constrained exit) get moderate-high d values (~0.60) — they experience significant extraction but retain some agency through regulatory and market choices. Distributed suppliers (victim + trapped exit) get very high d values (~0.92) — they experience maximum extraction with zero agency. The insurance market (beneficiary + constrained exit) gets moderate d (~0.50) — they benefit from risk pooling but face regulatory and competitive constraints. The legal doctrine itself (beneficiary + arbitrage exit, creates theater) gets moderate d (~0.55) — it coordinates doctrine but extracts through billable litigation. The engine applies the sigmoid f(d) to convert these directionality values into experienced extractiveness modifiers, which then scale base extractiveness via χ = ε × f(d) × σ(S). Large scope (global) multiplier σ(1.2) amplifies the constraint's effective extraction because regulatory arbitrage and supply chain opacity increase extraction hideability.
 *
 * MANDATROPHY ANALYSIS:
 *   The shared_liability reading resolves mandatrophy by showing that joint liability is genuinely tangled — it coordinates safety incentives (developers and deployers both invest in safety) while simultaneously extracting compliance costs asymmetrically. The classification is stable across base properties (ε=0.52, χ in the tangled_rope range) but produces a perspectival gap: institutional beneficiaries (developers, courts) see rope or scaffold; victims with exit (deployers) see tangled_rope; trapped victims (suppliers) see snare; the analytical observer risks seeing natural law. The mandatrophy is not 'which type is correct?' but 'who controls the allocation rules and who bears the burden?' The shared_liability reading succeeds as a doctrinal choice precisely because it can be framed as coordination (safety incentives for all) while actually concentrating extraction on trapped actors (suppliers) and shifting burden from beneficiaries (developers) to constrained victims (deployers).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_contribution_operationalization,
    'What operational test distinguishes ''causal contribution'' from ''causal involvement''? Where does a component supplier''s contribution end and a deployer''s control begin?',
    'Analysis of settled vs. ongoing liability cases: identify categories of parties that litigation consistently places in snare vs. tangled_rope positions. Develop empirical typology of causal contribution tests.',
    'If test is strict (binary control): shared liability collapses toward developer_liability reading. If test is permissive (any causal involvement): most parties in value chain become joint defendants, suppression increases, classification shifts toward snare. If test is deliberately ambiguous: theater_ratio increases, doctrine becomes self-perpetuating (litigation over the test itself).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_contribution_operationalization, empirical, 'Operational definition of ''causal contribution'' in liability allocation').

omega_variable(
    contractual_allocation_enforceability,
    'To what extent can parties contractually override shared liability through indemnification clauses? Are such allocations enforceable against third-party claimants?',
    'Comparative legal analysis across jurisdictions; case law on indemnity agreement enforceability; empirical study of actual liability burden distribution in major technology deployments.',
    'If fully enforceable: shared liability becomes facade (de facto single-actor liability through contractual pass-through). Extractiveness drops, tangled_rope classification shifts toward rope for large firms with negotiating power. If unenforeable: shared liability is binding joint and several liability; no contractual escape. Extractiveness remains high, suppression increases, classification remains snare for trapped actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contractual_allocation_enforceability, empirical, 'Enforceability of contractual liability allocation clauses').

omega_variable(
    control_definition_scope,
    'Does ''control'' include passive design (algorithms run without active human intervention) or only active operational control? Does a developer retain control after deployment if they can push updates?',
    'Doctrinal analysis of control test across AI/automated systems liability law; case law on developer liability for post-deployment updates; empirical study of actual control distribution in live systems.',
    'If control includes passive design: developers bear significant liability even after deployment. Developer_liability reading becomes more salient; shared liability shifts toward strict developer responsibility. If control is operational-only: developers escape liability unless they actively intervene. Deployer_liability reading becomes more salient; shared liability shifts toward deployer responsibility. Scope of ''control'' directly determines which reading is structurally dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_definition_scope, empirical, 'Definition of ''control'' in causal contribution test').

omega_variable(
    information_asymmetry_under_liability,
    'Does shared liability increase or decrease transparency about safety risks, failure modes, and design tradeoffs in technology supply chains?',
    'Comparative study of information disclosure before and after shared liability regimes; analysis of trade-secret claims and confidentiality assertions in liability cases; empirical measurement of how liability allocation affects developer disclosures to deployers.',
    'If liability increases transparency: shared liability functions as a coordination mechanism (reduces hidden risks, enables informed deployment decisions). Extractiveness drops, tangled_rope classification is stable. If liability decreases transparency (parties withhold information to reduce liability exposure): opacity becomes a suppression mechanism. Theater_ratio rises (litigation over what should have been disclosed). Suppression increases; shared liability becomes a mechanism for extracting information rents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_under_liability, empirical, 'Effect of shared liability on information disclosure').

omega_variable(
    kernel_reading_alternative_doctrines,
    'This constraint instantiates the ''shared_liability'' reading of the liability_attribution kernel. How do the sibling readings (developer_liability, deployer_liability) represent structurally different doctrinal choices?',
    'Doctrinal comparison: identify the foundational axiom each reading rests on. Map the causal_contribution_operationalization, control_definition_scope, and contractual_allocation_enforceability omegas across all three readings to show how different resolutions produce different dominant readings.',
    'If causal_contribution is strict and control is operational: developer_liability reading forecloses shared_liability (developers cannot be held for deployer''s operational choices). If causal_contribution is permissive and control includes design: shared_liability coexists with both alternatives depending on case type. If control is actively contested in doctrine: all three readings coexist, each reflecting different judicial coalitions'' preferences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_doctrines, conceptual, 'Relationship of shared_liability reading to alternative readings of the liability kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_shared_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(liab_shared_tr_t3, liability_attribution__shared_liability, theater_ratio, 3, 0.48).
narrative_ontology:measurement(liab_shared_tr_t6, liability_attribution__shared_liability, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(liab_shared_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(liab_shared_be_t3, liability_attribution__shared_liability, base_extractiveness, 3, 0.46).
narrative_ontology:measurement(liab_shared_be_t6, liability_attribution__shared_liability, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(liab_shared_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(liab_shared_su_t3, liability_attribution__shared_liability, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(liab_shared_su_t6, liability_attribution__shared_liability, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% The shared_liability reading is part of a constraint family decomposing the liability_attribution kernel into three structurally distinct readings. Each reading (shared, developer-only, deployer-only) has different ε, different beneficiary/victim structures, and different dominant perspectives. The readings coexist in live judicial and regulatory competition — different courts and jurisdictions adopt different readings. This story captures the shared_liability reading only; sibling stories capture developer_liability and deployer_liability readings. Affects edges show causal influence: shared_liability impacts both alternatives by defining the reference point against which they compete.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__shared_liability, institutional, 0.12).
constraint_indexing:directionality_override(liability_attribution__shared_liability, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
