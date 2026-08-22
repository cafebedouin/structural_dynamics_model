% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Interstate Commerce Clause — Originalist Narrow Reading
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   The Commerce Clause grants Congress power to regulate 'Commerce... among
 *   the several States.' The originalist narrow reading interprets this
 *   phrase strictly: commerce means the exchange of goods; among the states
 *   means transactions crossing state borders; instrumentalities means the
 *   physical infrastructure facilitating interstate movement. Under this
 *   reading, federal regulatory authority is confined to border-crossing
 *   transactions and the channels that carry them. Intrastate production,
 *   local services, and internal state commerce remain under state police
 *   power regardless of their downstream effects on national markets. This is
 *   one reading of a contested constitutional kernel—the Commerce Clause text
 *   itself—where different jurisprudential schools (originalist, living
 *   constitutionalist, effects-based moderate) instantiate structurally
 *   different constraints from the same canonical text.
 *
 * KEY AGENTS:
 *   - State governments: primary beneficiaries, retain regulatory authority over intrastate commerce
 *   - Anti-federal-consolidation advocates: beneficiaries, vindicate limited enumerated powers doctrine
 *   - Federal regulatory agencies: agenda-setters whose jurisdiction is confined by text-based boundaries
 *   - Interstate externality-management regime: victim—loses authority to set nationwide standards for intrastate sources of interstate harm
 *   - Uniform national standards regime: victim—loses constitutional footing for nationwide labor, environmental, consumer-protection mandates
 *   - Originalist judiciary: agenda-setters, enforce the text-based boundary through judicial review
 *   - Expansive-reading coalition: excluded, structurally foreclosed by the originalist core premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.62).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.41).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Interstate Commerce Clause — Originalist Narrow Reading").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '15095808-eccd-4a32-be18-8b56ff6abbd6').
narrative_ontology:cs_kernel_codification('15095808-eccd-4a32-be18-8b56ff6abbd6', fixed_text).
narrative_ontology:cs_authority_grounding('15095808-eccd-4a32-be18-8b56ff6abbd6', lineage).
narrative_ontology:cs_interpretation_layer_present('15095808-eccd-4a32-be18-8b56ff6abbd6').
narrative_ontology:cs_reading_relation('15095808-eccd-4a32-be18-8b56ff6abbd6', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('15095808-eccd-4a32-be18-8b56ff6abbd6', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('15095808-eccd-4a32-be18-8b56ff6abbd6', foundational, enumerated_powers_strictly_construed).
narrative_ontology:cs_axiom_status(enumerated_powers_strictly_construed, holdable).
narrative_ontology:cs_axiom_grounding('15095808-eccd-4a32-be18-8b56ff6abbd6', enumerated_powers_strictly_construed, deontological).
narrative_ontology:cs_axiom('15095808-eccd-4a32-be18-8b56ff6abbd6', foundational, original_textual_meaning_controls_scope).
narrative_ontology:cs_axiom_status(original_textual_meaning_controls_scope, holdable).
narrative_ontology:cs_axiom_grounding('15095808-eccd-4a32-be18-8b56ff6abbd6', original_textual_meaning_controls_scope, empirically_contingent).
narrative_ontology:cs_axiom('15095808-eccd-4a32-be18-8b56ff6abbd6', secondary, state_police_power_residual).
narrative_ontology:cs_axiom_status(state_police_power_residual, holdable).
narrative_ontology:cs_axiom_grounding('15095808-eccd-4a32-be18-8b56ff6abbd6', state_police_power_residual, conventional).
narrative_ontology:cs_reference_frame('15095808-eccd-4a32-be18-8b56ff6abbd6', federalist_enumerated_powers_doctrine).
narrative_ontology:cs_drift_state('15095808-eccd-4a32-be18-8b56ff6abbd6', contemporary_integrated_national_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15095808-eccd-4a32-be18-8b56ff6abbd6', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, uniform_national_standards_regime).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, interstate_externality_management).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, interstate_merchant_class).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, intrastate_merchant_class).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, enumerated_federal_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, state_police_power_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain regulatory authority over all commercial activity occurring wholly within state borders under police power doctrine. Can set their own labor standards, environmental rules, consumer protections, and tax regimes without federal override. Enforce state law against intrastate merchants without federal mandate. Under this reading, the federal commerce power cannot reach local manufacturing, local agricultural practice, or local service provision regardless of downstream interstate effects.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__originalist_narrow_reading, state_governments, agenda_setter).

% Interpret the Constitution as limiting federal power to its enumerated text. Cite the Tenth Amendment and original-meaning jurisprudence. Argue the reading protects liberty by maintaining structural limits on centralized authority. Include constitutional scholars, federalism advocates, and political movements opposing federal regulatory expansion.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, generational, mobile, national).

% The regime that coordinates workplace safety, environmental protection, consumer protection, and economic regulation across state lines under expansive Commerce Clause readings. Under the originalist narrow reading, this regime is confined to transactions where goods or persons physically cross state borders. Loses authority to set nationwide labor standards, nationwide environmental baselines, nationwide food/drug safety when those standards target intrastate production.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, uniform_national_standards_regime, payer,
    institutional, generational, constrained, national).

% The coordination mechanism for managing costs that one state's activities impose on others: air and water pollution crossing borders, labor cost arbitrage that races standards downward, health/safety races-to-the-bottom. Under the originalist narrow reading, intrastate sources of interstate harm are beyond federal commerce power and must be addressed through state-to-state negotiation or dormant commerce clause doctrine—structurally weaker remedies.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, interstate_externality_management, payer,
    institutional, generational, trapped, national).

% Merchants whose business depends on moving goods across state borders. The federal commerce power clearly reaches their activity under the originalist reading. Can invoke federal preemption against state regulations of interstate transport, avoiding patchwork state rules.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, interstate_merchant_class, beneficiary,
    powerful, biographical, arbitrage, national).

% Local manufacturers, service providers, agricultural producers whose business operates wholly within a single state. Subject to state police power and state regulation without federal commercial clause oversight. Benefit from regulatory flexibility and can lobby for favorable state rules as concentrated local interests.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, intrastate_merchant_class, beneficiary,
    moderate, biographical, constrained, local).

% EPA, OSHA, NLRB, FTC, FDA implement commerce-clause-dependent federal regulation. Under the originalist narrow reading, their authority to regulate intrastate activity with only aggregated or indirect interstate effects is constitutionally limited. Mandates shrink; jurisdictional scope is confined to activities that Congress can plausibly characterize as instrumentalities of interstate movement or direct border-crossing transactions.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Courts that enforce the originalist narrow reading through judicial review. Apply the text-and-history constraint: what did 'Commerce... among the several States' mean at ratification? Reject substantial-effects doctrines and require a showing that regulated activity is either commerce or instrumentalities of interstate movement. Invalidate federal regulations that overstep this boundary.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, originalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Congress, federal agencies, and courts operating under the substantial-effects and expansive-federal readings. Would argue the originalist narrow reading forecloses federal solutions to national coordination problems. Their arguments are structurally excluded by the originalist reading's core premise: original meaning confined federal power to text-delimited categories.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, expansive_reading_coalition, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_text__originalist_narrow_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory authority between state and federal governments: confines federal commerce power to transactions where goods or persons physically cross state borders and to instrumentalities that facilitate such movement, leaving all other commercial activity under state police power. Solves the structural boundary problem—what is 'commerce among the several States' as opposed to commerce wholly within a state—by text-based enumeration rather than effects-based reasoning.
% TRANSFER_FUNCTION: Transfers regulatory authority away from the federal commercial-regulation regime to individual state governments. States gain authority to set their own standards for production, labor, environment, and consumer protection in intrastate markets. Federal agencies lose jurisdiction over purely intrastate activity. The national standards regime loses constitutional footing for nationwide baselines and must rely on state coordination or dormant commerce clause doctrine.
% ABSENT_VOICES: Interstate externality-managers are structurally absent because their existence depends on federal authority to regulate intrastate sources of interstate harm. Also absent: future consumers in low-standard states (cannot lobby for uniform baselines). Also absent: small merchants whose business model depends on nationwide uniform rules.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and expansive-federal reading became canonical, Congress would gain immediate authority to regulate intrastate activity based on substantial effects logic; federal agencies would expand mandates; interstate externality management would be recentralized. If this reading is sustained, states gain back regulatory authority; intrastate standards become heterogeneous; interstate coordination must proceed through state-to-state negotiation and dormant commerce clause review.
% FOUNDING_PROBLEM: Prevented the federal government from using commerce power as a pretext for regulating all economic activity (reductio ad absurdum of an unlimited effects-based reading: anything touches commerce somewhere, so federal power becomes plenary). Preserved the Tenth Amendment structure: federal power is enumerated and limited; state power is residual. Maintained a constitutional boundary between federal and state spheres that the Framers intended.
% FOUNDING_PROBLEM_CORROBORATION: Originalist constitutional scholars (Randy Barnett, Ilya Somin, historical analyses of the Founding era) attest the founding problem is live and well-grounded in constitutional text and original meaning. Expansive-reading advocates and federal agencies attest the problem is no longer relevant because the modern economy is nationally integrated and effects-based reading is the only coherent management approach. The Supreme Court's jurisprudence across five decades shows active contestation with substantial-effects doctrine firmly established but intermittent originalist challenges.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the originalist reading transfers significant regulatory authority from federal to state spheres—a substantial shift in who gets to set rules. The extraction is real: intrastate commercial actors gain flexibility (can lobby states individually; avoid federal mandates) while interstate externality-management loses capacity (cannot regulate intrastate pollution sources, intrastate labor practices, intrastate production standards on national grounds). Suppression is moderate (0.41) because the reading is enforced through judicial review that invalidates overstep regulations, but the suppression is not brutal—Congress can still legislate within the boundary, states can still coordinate through interstate compacts, and the dormant commerce clause remains available as an alternative brake on state protectionism. Theater ratio is low-moderate (0.28): the originalist reading presents itself as textual fidelity and Tenth Amendment vindication (performance of constitutional discipline), but significant enforcement activity is devoted to policing the boundary and rejecting congressional attempts to expand federal reach (actual enforcement work). Accessibility collapse is moderate (0.67): the boundary between intrastate and interstate is conceptually knowable but operationally slippery—is a local manufacturing facility that sells to out-of-state consumers engaged in interstate commerce? The measurement series spans the interval from ratification (1787) to present (2026), tracking how extractiveness, suppression, and theater increased as the national economy integrated and federal regulatory scope expanded relative to the originalist narrow boundary.
 *
 * PERSPECTIVAL GAP:
 *   The state government seat and the federal agency seat compute entirely different types from the same constraint. From the state perspective (beneficiary, powerful, arbitrage exit), the originalist reading is a genuine coordination mechanism protecting federalism balance and state autonomy—a rope or scaffold transitioning power back to the states. From the federal agency perspective (constrained, institutional, unable to exit), the same structure is enforced extraction—courts are taking away statutory authority Congress passed, invalidating rules agencies promulgated, and forcing federal policy into narrow channels. The engine computes this divergence from power, exit, and structural role: states can arbitrage to alternative coordinate systems (interstate compacts, their own police power); federal agencies are trapped within the constitutional framework and cannot exit to an alternative authority source.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are the structural beneficiaries: d approaches 0.2 because they gain regulatory authority, face no enforcement costs (courts protect their police power), and have high exit optionality (they can coordinate interstate through compacts if the originalist boundary proves unsustainable). Uniform national standards regime is the structural target: d approaches 0.85 because it loses jurisdiction, cannot regulate intrastate sources of interstate problems, and is trapped within the federal system (cannot exit to state-level authority without Congressional action). Federal agencies sit between: d around 0.65 because they are partly beneficiaries (they still regulate the interstate channel; originalism does not eliminate federal commerce power, only confines it to the text) and partly targets (they cannot regulate intrastate activity even when effects-logic would justify it). Interstate externality-management is the victim: d approaches 0.9 because it loses all authority over intrastate sources of interstate harm and has no exit (externalities are by definition cross-boundary problems that no single state can solve alone).
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading instantiates genuine mandatrophy when the founding problem (prevent federal power from becoming unlimited through effects-based reasoning) outlives the functional necessity of the constraint. The modern economy is nationally integrated; most intrastate activity has demonstrable interstate effects; the reading's own boundary-drawing creates edge cases (online retail, cloud computing, data flows) that strain the text-based categories. Yet the reading persists because: (1) it has captured a constituency of constitutional scholars and judges committed to originalist method; (2) it vindicates state police power and federalism as stand-alone values, not merely as instrumental means to efficiency; (3) the alternative readings (expansive, substantial-effects) have their own costs (plenary federal power, loss of state regulatory flexibility). The mandatrophy is partial and contested, not terminal: the founding problem (unlimited federal authority) remains live for those committed to constitutional limits, and the reading continues to serve that function even as its practical operation creates coordination problems the reading cannot internally solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrastate_interstate_boundary_ambiguity,
    'Where exactly does the boundary lie between intrastate commerce (subject to state police power) and interstate commerce (subject to federal commerce power) in a modern integrated national economy? How do digital commerce, supply chains, and multi-state production networks fit into text-based categories designed for nineteenth-century goods movement?',
    'Accumulation of edge-case judicial decisions (online retail, cloud data, modular manufacturing). If courts can develop stable, predictable rules for digital and supply-chain commerce that stay within text-based categories, the boundary holds; if boundary cases proliferate and require increasing degrees of line-drawing and atextual reasoning, the text-based approach erodes.',
    'If the boundary proves structurally incoherent in modern contexts, the originalist reading faces either (a) breakdown via case-by-case unprincipled decisions (theater-ratio spike), or (b) pressure toward one of the sibling readings that uses standards (substantial effects) rather than rules (text-based categories) to accommodate modern commerce.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrastate_interstate_boundary_ambiguity, empirical, 'Whether text-based categories remain operationally stable for modern commerce patterns.').

omega_variable(
    interstate_externality_management_necessity,
    'Is the interstate externality-management regime (federal regulation of intrastate sources of interstate harm) structurally necessary for solving genuine coordination problems, or can the originalist reading''s state-to-state negotiation and dormant commerce clause alternatives adequately manage externalities?',
    'Empirical track record: do states coordinate effectively through interstate compacts and dormant commerce clause litigation, or do races-to-the-bottom, collective action failures, and unabated externalities accumulate when federal regulation is confined to border-crossing transactions?',
    'If externality accumulation becomes severe (cross-border pollution, labor-standards collapse, product-safety races), political pressure will mount to expand federal authority through either (a) the substantial-effects reading or (b) the expansive reading, displacing the originalist constraint. If states successfully coordinate, the originalist reading is vindicated as workable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interstate_externality_management_necessity, empirical, 'Whether state-to-state coordination and dormant commerce clause doctrine are adequate substitutes for affirmative federal externality regulation.').

omega_variable(
    original_meaning_versus_originalist_method,
    'Does the originalist narrow reading accurately capture the original public meaning of ''Commerce... among the several States'' as understood at ratification, or does it misread the Framers'' likely intent to permit federal regulation of activity with substantial aggregate effects?',
    'Historical and textual scholarship analyzing founding-era usage of ''commerce,'' context of the Commerce Clause within the Constitution, Framers'' writings and state ratification debates, and comparison to alternative historical evidence.',
    'If historical scholarship establishes that original meaning permitted broader federal reach, the originalist reading loses its justificatory foundation—originalism''s internal authority depends on correctly reading the original text, not on narrow contemporary policy preferences. If scholarship confirms the narrow reading as originally correct, the reading is strengthened and competing readings lose originalist credentials.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_meaning_versus_originalist_method, conceptual, 'Whether the reading''s historical premises are sound; this omega addresses the reading''s fidelity to originalist method itself.').

omega_variable(
    federalism_value_assumption,
    'Is federalism (the allocation of authority between national and state governments) a standalone constitutional value to be preserved even when it creates coordination costs, or is federalism instrumental to other goals (liberty, democratic participation, efficient regulation) and should be overridden when those goals require national uniformity?',
    'Normative and constitutional theory: this is not resolvable by empirical data but rather by which constitutional-theory framework is adopted. However, drift signal: if the political system repeatedly rejects originalist reading and enacts broad federal regulations with bipartisan support (suggesting values have shifted toward national coordination), that signals a preference-level shift.',
    'If federalism is understood as instrumental, the originalist reading becomes vulnerable to displacement by either the expansive reading or a reformed substantial-effects reading that permits federal regulation whenever coordination problems require it. If federalism is standalone, the reading has stable normative support even when it creates coordination costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalism_value_assumption, preference, 'The normative status of federalism as a constitutional value rather than an instrumental means.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the originalist narrow reading logically foreclose the substantial-effects reading within a single constitutional framework, or can both readings coexist as live competing interpretations of the same text?',
    'Constitutional theory and doctrine: if originalism is established as the canonical interpretive method and effects-based reasoning is rejected as non-originalist, the coexistence is broken and the readings foreclose each other. If multiple interpretive methods are acknowledged as legitimate (originalism, living constitutionalism, pragmatism), coexistence persists.',
    'If the readings genuinely foreclose each other, the constraint''s classification and the sibling readings'' classifications are mutually exclusive—they cannot all be true within a single jurisprudential framework. If they coexist, all three readings remain live constraints in the corpus, modeling the actual state of constitutional dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'The logical relationship between the originalist reading and its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1787, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1787, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1787, 0.08).
narrative_ontology:measurement_basis(comm_tr_t1787, projected).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1964, 0.21).
narrative_ontology:measurement_basis(comm_tr_t1964, observed).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2012, 0.27).
narrative_ontology:measurement_basis(comm_tr_t2012, observed).
narrative_ontology:measurement(comm_tr_t2026, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(comm_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1787, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1787, 0.15).
narrative_ontology:measurement_basis(comm_be_t1787, projected).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1964, 0.52).
narrative_ontology:measurement_basis(comm_be_t1964, observed).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2012, 0.61).
narrative_ontology:measurement_basis(comm_be_t2012, observed).
narrative_ontology:measurement(comm_be_t2026, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(comm_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1787, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1787, 0.22).
narrative_ontology:measurement_basis(comm_su_t1787, projected).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1937, 0.31).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1964, 0.38).
narrative_ontology:measurement_basis(comm_su_t1964, observed).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement_basis(comm_su_t1995, observed).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2012, 0.41).
narrative_ontology:measurement_basis(comm_su_t2012, observed).
narrative_ontology:measurement(comm_su_t2026, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2026, 0.41).
narrative_ontology:measurement_basis(comm_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1787, tn=2026
narrative_ontology:measurement(comm_grid_01, commerce_clause_text__originalist_narrow_reading, accessibility_collapse(class), 1787, 0.41).
narrative_ontology:measurement(comm_grid_02, commerce_clause_text__originalist_narrow_reading, accessibility_collapse(class), 2026, 0.58).
narrative_ontology:measurement(comm_grid_03, commerce_clause_text__originalist_narrow_reading, accessibility_collapse(individual), 1787, 0.35).
narrative_ontology:measurement(comm_grid_04, commerce_clause_text__originalist_narrow_reading, accessibility_collapse(individual), 2026, 0.52).
narrative_ontology:measurement(comm_grid_05, commerce_clause_text__originalist_narrow_reading, accessibility_collapse(organizational), 1787, 0.48).
narrative_ontology:measurement(comm_grid_06, commerce_clause_text__originalist_narrow_reading, accessibility_collapse(organizational), 2026, 0.63).
narrative_ontology:measurement(comm_grid_07, commerce_clause_text__originalist_narrow_reading, accessibility_collapse(structural), 1787, 0.52).
narrative_ontology:measurement(comm_grid_08, commerce_clause_text__originalist_narrow_reading, accessibility_collapse(structural), 2026, 0.67).
narrative_ontology:measurement(comm_grid_09, commerce_clause_text__originalist_narrow_reading, resistance(class), 1787, 0.48).
narrative_ontology:measurement(comm_grid_10, commerce_clause_text__originalist_narrow_reading, resistance(class), 2026, 0.64).
narrative_ontology:measurement(comm_grid_11, commerce_clause_text__originalist_narrow_reading, resistance(individual), 1787, 0.51).
narrative_ontology:measurement(comm_grid_12, commerce_clause_text__originalist_narrow_reading, resistance(individual), 2026, 0.67).
narrative_ontology:measurement(comm_grid_13, commerce_clause_text__originalist_narrow_reading, resistance(organizational), 1787, 0.45).
narrative_ontology:measurement(comm_grid_14, commerce_clause_text__originalist_narrow_reading, resistance(organizational), 2026, 0.61).
narrative_ontology:measurement(comm_grid_15, commerce_clause_text__originalist_narrow_reading, resistance(structural), 1787, 0.42).
narrative_ontology:measurement(comm_grid_16, commerce_clause_text__originalist_narrow_reading, resistance(structural), 2026, 0.58).
narrative_ontology:measurement(comm_grid_17, commerce_clause_text__originalist_narrow_reading, stakes_inflation(class), 1787, 0.12).
narrative_ontology:measurement(comm_grid_18, commerce_clause_text__originalist_narrow_reading, stakes_inflation(class), 2026, 0.35).
narrative_ontology:measurement(comm_grid_19, commerce_clause_text__originalist_narrow_reading, stakes_inflation(individual), 1787, 0.08).
narrative_ontology:measurement(comm_grid_20, commerce_clause_text__originalist_narrow_reading, stakes_inflation(individual), 2026, 0.32).
narrative_ontology:measurement(comm_grid_21, commerce_clause_text__originalist_narrow_reading, stakes_inflation(organizational), 1787, 0.15).
narrative_ontology:measurement(comm_grid_22, commerce_clause_text__originalist_narrow_reading, stakes_inflation(organizational), 2026, 0.38).
narrative_ontology:measurement(comm_grid_23, commerce_clause_text__originalist_narrow_reading, stakes_inflation(structural), 1787, 0.18).
narrative_ontology:measurement(comm_grid_24, commerce_clause_text__originalist_narrow_reading, stakes_inflation(structural), 2026, 0.41).
narrative_ontology:measurement(comm_grid_25, commerce_clause_text__originalist_narrow_reading, suppression(class), 1787, 0.18).
narrative_ontology:measurement(comm_grid_26, commerce_clause_text__originalist_narrow_reading, suppression(class), 2026, 0.38).
narrative_ontology:measurement(comm_grid_27, commerce_clause_text__originalist_narrow_reading, suppression(individual), 1787, 0.15).
narrative_ontology:measurement(comm_grid_28, commerce_clause_text__originalist_narrow_reading, suppression(individual), 2026, 0.35).
narrative_ontology:measurement(comm_grid_29, commerce_clause_text__originalist_narrow_reading, suppression(organizational), 1787, 0.22).
narrative_ontology:measurement(comm_grid_30, commerce_clause_text__originalist_narrow_reading, suppression(organizational), 2026, 0.4).
narrative_ontology:measurement(comm_grid_31, commerce_clause_text__originalist_narrow_reading, suppression(structural), 1787, 0.25).
narrative_ontology:measurement(comm_grid_32, commerce_clause_text__originalist_narrow_reading, suppression(structural), 2026, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__originalist_narrow_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, dormant_commerce_clause_doctrine).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, state_police_power_doctrine).

% DUAL FORMULATION NOTE:
% The constraint_commerce_clause_text kernel decomposes into three structurally distinct readings, each with different ε values and beneficiary/victim structures. This story (originalist_narrow_reading) confines federal authority to text-based categories and has moderate extractiveness (0.62) because it transfers regulatory authority to states while losing coordination capacity for interstate externalities. The expansive_federal_reading interprets the same text to permit broad federal reach, yielding substantially higher extractiveness (ε ~0.75+) for intrastate-regulation mandates. The substantial_effects_limited_reading offers a middle path with moderate extractiveness. All three readings are linked as siblings; they are NOT measurable as one constraint with a measurement parameter—each has its own story, its own metrics, and its own structural data. The different ε values reflect different interpretations of what 'Commerce... among the several States' means, not different observations of a single fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__originalist_narrow_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
