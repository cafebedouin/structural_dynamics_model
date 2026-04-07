% ============================================================================
% CONSTRAINT STORY: responsibility_dilution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_responsibility_dilution, []).

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
 *   constraint_id: responsibility_dilution
 *   human_readable: The Accountability Fog
 *   domain: organizational/legal/technological
 *
 * SUMMARY:
 *   The accountability fog emerges when critical decisions affecting
 *   stakeholders are fragmented across autonomous agents, bureaucratic
 *   layers, and algorithmic filters such that no single actor can be held
 *   responsible for outcomes. This structure appears in platform moderation
 *   (algorithms filter, humans review, legal teams determine policy),
 *   corporate liability cascades (parent companies delegate to subsidiaries,
 *   subsidiaries delegate to contractors, contractors delegate to algorithmic
 *   systems), and regulatory enforcement (agencies set rules, inspectors
 *   apply rules, algorithms monitor compliance, contractors report
 *   violations). The constraint exhibits classic Tangled Rope structure:
 *   genuine coordination benefits (distributed decision-making enables scale
 *   and reduces single-point failure risk) coupled with extractive outcomes
 *   (stakeholders harmed by fragmented decisions cannot identify who to hold
 *   accountable, principals benefit from liability dispersal, intermediate
 *   layers benefit from opacity). The theater_ratio has risen from 0.40 to
 *   0.68 over the interval as legal liability evasion has become an
 *   intentional design feature rather than an accidental side effect. The
 *   base_extractiveness has increased from 0.32 to 0.58 as the fog has
 *   deepened from organizational complexity to deliberate opacity
 *   engineering.
 *
 * KEY AGENTS:
 *   - Affected Stakeholder: Primary victim (powerless/trapped) — subject to fragmented decisions with no identifiable accountable party; cannot exit the system
 *   - Delegating Principal/Organization: Primary beneficiary (institutional/arbitrage) — captures efficiency gains from distributed decision-making and disperses liability across agents; can restructure if regulatory pressure mounts
 *   - Algorithmic System Operators: Secondary beneficiary (institutional/arbitrage) — opacity of systems provides cover for decisions; limited direct accountability
 *   - Intermediate Bureaucratic Layers: Secondary beneficiary (moderate/constrained) — distributed responsibility obscures their role; but also constrained by potential liability exposure
 *   - Oversight Authority: Mixed actor (organized/constrained) — tasked with accountability enforcement but hampered by technical opacity and distributed responsibility; benefits from fog (reduced enforcement burden) but damaged by legitimacy loss
 *   - Regulatory Coalition: Organized challenger (organized/constrained) — sees fog as extraction mechanism; organizing for transparency mandates and liability piercing
 *   - Accountability Transparency: Victim-concept (powerless/trapped) — abstract good that cannot organize; collective epistemic commons contaminated by opacity
 *   - Legal Liability Clarity: Victim-concept (powerless/trapped) — traditional tort law assumes identifiable decision-maker; doctrine becomes inoperable as responsibility disperses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(responsibility_dilution, 0.58).
domain_priors:suppression_score(responsibility_dilution, 0.62).
domain_priors:theater_ratio(responsibility_dilution, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(responsibility_dilution, extractiveness, 0.58).
narrative_ontology:constraint_metric(responsibility_dilution, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(responsibility_dilution, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(responsibility_dilution, tangled_rope).
narrative_ontology:human_readable(responsibility_dilution, "The Accountability Fog").
narrative_ontology:topic_domain(responsibility_dilution, "organizational/legal/technological").

domain_priors:requires_active_enforcement(responsibility_dilution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(responsibility_dilution, delegating_principals).
narrative_ontology:constraint_beneficiary(responsibility_dilution, algorithmic_system_operators).
narrative_ontology:constraint_beneficiary(responsibility_dilution, intermediate_bureaucratic_layers).
narrative_ontology:constraint_victim(responsibility_dilution, affected_stakeholders).
narrative_ontology:constraint_victim(responsibility_dilution, accountability_transparency).
narrative_ontology:constraint_victim(responsibility_dilution, legal_liability_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED STAKEHOLDER (SNARE) — Subject to decisions fragmented across multiple agents and layers with no clear point of accountability. Cannot identify who is responsible for harm. No exit option: subject to the system's outputs. Maximum extraction because the power to affect one's fate is distributed beyond reach.
constraint_indexing:constraint_classification(responsibility_dilution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OVERSIGHT AUTHORITY (TANGLED ROPE) — Tasked with enforcing accountability but constrained by technical opacity and distributed responsibility. Benefits from the fog (reduced enforcement burden and political risk) but also bears costs (legitimacy loss, regulatory backlash). Constrained exit because withdrawing from oversight creates legal liability.
constraint_indexing:constraint_classification(responsibility_dilution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRINCIPAL / DELEGATING ORGANIZATION (ROPE) — Benefits from delegation to autonomous agents and algorithmic systems: reduces centralized decision-making risk, disperses liability, enables scale. Experiences the constraint as coordination: distributing decisions across multiple actors is a legitimate efficiency mechanism. Has arbitrage options (can restructure if needed). Net beneficiary.
constraint_indexing:constraint_classification(responsibility_dilution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (TANGLED ROPE) — Consumer advocates, legal firms, and civil rights organizations see the fog as a mechanism of extraction (institutional evasion of liability) but also as a coordination problem they can solve (through litigation, regulatory reform, transparency mandates). Organized but constrained by the legal system's own slowness. Can exit through political advocacy but at high cost.
constraint_indexing:constraint_classification(responsibility_dilution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY LEGAL DOCTRINE (PITON) — Traditional tort law assumes a clear decision-maker and causal chain. The accountability fog makes this doctrine inoperable, but law schools continue teaching it, courts maintain the framework, and legal culture defers to it. Theater ratio high because the entire legal apparatus performs accountability review while the underlying conditions make accountability impossible. The doctrine persists through institutional inertia despite structural obsolescence.
constraint_indexing:constraint_classification(responsibility_dilution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION-THEORETIC LIMIT (MOUNTAIN) — From a civilizational view, if a decision is truly distributed across N independent agents with no central coordinator, reconstructing causal responsibility requires information complexity exponential in N. Some degree of 'fog' is an irreducible consequence of distributed decision-making. However, real-world accountability fog exceeds this theoretical minimum — it is artificially amplified by deliberate opacity and liability engineering, not a natural law.
constraint_indexing:constraint_classification(responsibility_dilution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_dilution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(responsibility_dilution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_dilution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(responsibility_dilution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(responsibility_dilution, TR),
    TR >= 0.70.

:- end_tests(responsibility_dilution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts value from affected stakeholders (impossible to assign responsibility means impossible to collect damages) and transfers that value to principals and operators (liability dispersal, opacity cover, scale benefits). The extraction is not maximal because organizational legitimacy is still at stake — some oversight and narrative accountability exist as performative gestures. Suppression (0.62): High. Multiple barriers prevent stakeholders from holding actors accountable: technical opacity (algorithms are black boxes), distributed responsibility (no single decision-maker), information asymmetry (stakeholders have no access to decision logs), and legal doctrine lag (tort law assumes centralized causation). Exit is blocked: stakeholders cannot opt out of algorithmic decisions affecting them. Theater ratio (0.68): High and rising. Legal and organizational compliance systems perform accountability review (audit committees, privacy reviews, impact assessments, regulatory filings) while the structural conditions that would make accountability possible (transparency, centralized decision-making, clear causation) are absent or deliberately obscured. The performance of accountability has increased as legal pressure has mounted.
 *
 * PERSPECTIVAL GAP:
 *   The delegating principal sees Rope — they are solving the legitimate coordination problem of scaling decision-making across many agents. The affected stakeholder sees Snare — they are trapped in a system where harm is inflicted but no one is responsible. The oversight authority sees Tangled Rope — they experience both the genuine coordination benefits (distributed systems are resilient) and the extraction costs (impossible to enforce accountability). The regulatory coalition sees Tangled Rope at a generational timescale (their exit is through slow political reform). The legacy legal doctrine sees its own obsolescence but persists (Piton) — the entire judicial apparatus performs accountability review while operating with doctrines incompatible with distributed responsibility. The analytical observer risks seeing a natural law (Mountain) — that distributed systems inevitably generate accountability fog — but the structural data reveals deliberate opacity engineering: the fog exceeds what technical distribution requires.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position within the fog. The delegating principal benefits from responsibility dispersal and has high exit options (can restructure, can litigate if needed) — low d value derived from beneficiary status + arbitrage exit. The affected stakeholder bears maximum cost and has no exit (subject to the system) — high d value derived from victim status + trapped exit. The oversight authority is partly a victim (constrained by opacity) and partly a beneficiary (reduced enforcement burden) — moderate d value reflects mixed position + constrained exit. The algorithmic system operators benefit from opacity (responsibility obscured) but are also constrained by potential liability litigation — moderate d value. The regulatory coalition is organized and has real agency (political pressure, litigation) — moderate d value despite victim framing, because their constrained exit (political reform, not exit from system) provides some power. The piton classification derives from theater_ratio gate: the legal system performs accountability while lacking the structural conditions to implement it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_structure,
    'Is the accountability fog primarily a deliberate liability-avoidance strategy or an emergent property of distributed decision architectures?',
    'Documentary analysis of system design decisions; comparison of organizations with versus without deliberate opacity mechanisms; internal compliance audit trails',
    'If deliberate: classification moves toward Snare (high suppression indicates intentional mechanism). If emergent: remains Tangled Rope (coordination + unintended extraction). Affects policy response — transparency mandate vs structural redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_structure, empirical, 'Whether accountability fog is deliberate or emergent').

omega_variable(
    algorithmic_opacity_necessity,
    'Do algorithmic systems genuinely require opacity for function (proprietary models, security), or is opacity an artifact of business incentives and regulatory capture?',
    'Technical analysis of model interpretability trade-offs; comparison of open-source vs proprietary systems; audit of disclosure requirements vs actual disclosure barriers',
    'If genuine: some opacity is Mountain-like (irreducible). If artifact: opacity is entirely Snare mechanism. Affects mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_necessity, empirical, 'Whether algorithmic opacity is technically necessary').

omega_variable(
    stakeholder_remediation_access,
    'When accountability is diluted, can affected stakeholders achieve meaningful remediation through existing legal channels, or do institutional barriers make remediation impossible?',
    'Empirical study of successful liability claims against distributed decision systems; statistical analysis of litigation outcomes; access costs and timeline data',
    'If accessible: Snare classification weakens (exit through legal system exists). If blocked: Snare confirmed. Affects victim classification and exit_options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stakeholder_remediation_access, empirical, 'Whether affected stakeholders can access remediation').

omega_variable(
    delegation_legitimacy_boundary,
    'Is there a principled distinction between legitimate delegation (reducing centralized risk) and illegitimate liability evasion?',
    'Legal philosophy and organizational ethics analysis; case law evolution; stakeholder impact thresholds (how much harm justifies piercing the veil of delegation)',
    'If yes: Tangled Rope confirmed (legitimate coordination + illegitimate extraction are distinct). If no: classification may shift to Rope (delegation is purely coordination) or Snare (purely extraction). Frames mandatrophy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(delegation_legitimacy_boundary, conceptual, 'Whether legitimate delegation differs from liability evasion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(responsibility_dilution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resp_dil_tr_t0, responsibility_dilution, theater_ratio, 0, 0.4).
narrative_ontology:measurement(resp_dil_tr_t5, responsibility_dilution, theater_ratio, 5, 0.55).
narrative_ontology:measurement(resp_dil_tr_t10, responsibility_dilution, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(resp_dil_be_t0, responsibility_dilution, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(resp_dil_be_t5, responsibility_dilution, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(resp_dil_be_t10, responsibility_dilution, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(responsibility_dilution, enforcement_mechanism).
narrative_ontology:affects_constraint(responsibility_dilution, regulatory_capture).
narrative_ontology:affects_constraint(responsibility_dilution, liability_engineering).
narrative_ontology:affects_constraint(responsibility_dilution, algorithmic_opacity).

% DUAL FORMULATION NOTE:
% The accountability fog is downstream of specific technical and organizational choices (algorithmic opacity, liability dispersal structures, bureaucratic layering) but represents a distinct structural constraint on responsibility assignment. The upstream constraints model the individual mechanisms; this constraint models their interaction: how fragmentation of decision-making across multiple agents produces emergent opacity that exceeds the sum of individual components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(responsibility_dilution, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
