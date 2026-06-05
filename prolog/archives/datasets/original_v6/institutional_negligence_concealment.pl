% ============================================================================
% CONSTRAINT STORY: institutional_negligence_concealment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_negligence_concealment, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_negligence_concealment
 *   human_readable: Institutional Negligence Concealment
 *   domain: organizational_governance/accountability
 *
 * SUMMARY:
 *   Institutional negligence concealment represents a systematic extraction
 *   mechanism where organizations with authority, information control, and
 *   enforcement capacity deliberately or systematically suppress evidence of
 *   failures that harm vulnerable populations. The constraint operates by
 *   concentrating information asymmetry, suppressing alternative narratives,
 *   and maintaining retaliation risk against whistleblowers. The base
 *   properties reveal extractiveness rising from 0.45 to 0.68 and theater
 *   ratio rising from 0.42 to 0.68 — both increasing over the measurement
 *   interval, indicating that concealment deepens as the cost of exposure
 *   accumulates. The institutional leadership benefits from liability
 *   reduction and reputation protection; the affected population bears all
 *   costs with no exit mechanism; regulatory agencies experience mixed
 *   effects (coordination benefits from institutional cooperation mixed with
 *   asymmetric information extraction); potential whistleblowers face maximum
 *   suppression through career jeopardy. The constraint exhibits snare
 *   classification from most perspectives except the institutional
 *   beneficiary (rope) and the regulatory agency (tangled rope with
 *   coordination function).
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — avoids liability, maintains reputation, controls narrative
 *   - Affected Populations: Primary victim (powerless/trapped) — bear direct harms (injury, financial loss, health damage) with no exit or information access
 *   - Potential Whistleblowers: Secondary victim (moderate/constrained) — face career penalties, legal liability, isolation if they expose wrongdoing
 *   - Regulatory Agencies: Organized actor (organized/mobile) — coordinate oversight but experience asymmetric information disclosure
 *   - Internal Accountability Structures: Institutional theater (institutional/arbitrage) — maintain procedural appearance while controlling evidence
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees systematic extraction mechanism across institutional domains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_negligence_concealment, 0.68).
domain_priors:suppression_score(institutional_negligence_concealment, 0.75).
domain_priors:theater_ratio(institutional_negligence_concealment, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_negligence_concealment, extractiveness, 0.68).
narrative_ontology:constraint_metric(institutional_negligence_concealment, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(institutional_negligence_concealment, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_negligence_concealment, snare).
narrative_ontology:human_readable(institutional_negligence_concealment, "Institutional Negligence Concealment").
narrative_ontology:topic_domain(institutional_negligence_concealment, "organizational_governance/accountability").

domain_priors:requires_active_enforcement(institutional_negligence_concealment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_negligence_concealment, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_negligence_concealment, liability_avoiders).
narrative_ontology:constraint_victim(institutional_negligence_concealment, affected_populations).
narrative_ontology:constraint_victim(institutional_negligence_concealment, truth_seeking_agents).
narrative_ontology:constraint_victim(institutional_negligence_concealment, regulatory_oversight_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HARMED POPULATION (SNARE) — Victims of institutional negligence bear direct costs (injury, financial loss, health damage) while institutional leadership controls information flow. No exit mechanism; victims cannot pursue accountability without access to concealed records. Maximum experienced extraction.
constraint_indexing:constraint_classification(institutional_negligence_concealment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POTENTIAL WHISTLEBLOWERS (SNARE) — Insiders who discover negligence face severe career penalties, legal liability, and personal financial risk if they expose institutional wrongdoing. Exit options are constrained by employment dependency, regulatory retaliation, and isolation from peers. High extraction enforced through suppression mechanisms.
constraint_indexing:constraint_classification(institutional_negligence_concealment, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Benefits from concealment through liability reduction, reputation protection, and operational continuity. Experiences constraint as coordination mechanism: maintaining institutional cohesion requires information control. Net beneficiary with full exit optionality.
constraint_indexing:constraint_classification(institutional_negligence_concealment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGULATORY AGENCIES (TANGLED ROPE) — Organized agents with enforcement authority. Beneficiary from institutional cooperation and smooth operations; victim of information asymmetry and selective disclosure. Coordination function exists (monitoring safety compliance) alongside asymmetric extraction (institutions control what data regulators see). Mobile through legislative mandate but constrained by resource limits and institutional pushback.
constraint_indexing:constraint_classification(institutional_negligence_concealment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ACCOUNTABILITY RITUALS (PITON) — Formal compliance processes (internal investigations, audit committees, oversight boards) persist through institutional inertia despite low functional impact. Theater dominates: institutions produce reports and investigations that appear to investigate wrongdoing while controlling their own evidentiary base. Procedural appearance replaces actual accountability.
constraint_indexing:constraint_classification(institutional_negligence_concealment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational scope, institutional negligence concealment is a systematic extraction mechanism where information asymmetry, enforcement authority asymmetry, and retaliation risk combine to create asymmetric accountability. The constraint traps affected populations while benefiting institutional actors. Suppression is high and sustained across cycles.
constraint_indexing:constraint_classification(institutional_negligence_concealment, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_negligence_concealment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_negligence_concealment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_negligence_concealment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_negligence_concealment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_negligence_concealment, TR),
    TR >= 0.70.

:- end_tests(institutional_negligence_concealment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint extracts from affected populations (direct harm), from whistleblowers (career cost), and from regulatory capacity (information deprivation). The extraction is not total because some institutional negligence is eventually discovered through external investigation, but the systematic suppression means the institution captures years or decades of benefit before accountability occurs. The rising trajectory (0.45 → 0.68) reflects that organizations deepen concealment patterns over time as costs accumulate — initial negligence is followed by evidence suppression, then document destruction, then intimidation of witnesses, creating an extraction layer stack. Suppression (0.75): Very high. Multiple enforcement mechanisms: employment dependency (whistleblowers), legal liability threat (insiders), retaliation norms, information monopoly, and institutional reputation control. Barriers to exit are material (trapped), relational (isolation), legal (liability exposure), and epistemic (monopoly on interpretation). Theater ratio (0.68): High and increasing. Formal accountability structures (internal investigations, compliance committees, audit functions) have appearance of oversight while actual oversight is structurally compromised because the organization controls its own evidentiary base and determines what information is available for review. The rising ratio reflects that as external pressure for accountability increases, institutions add more procedural layers (external auditors, independent review boards) that maintain appearance while substance remains controlled.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between the institutional beneficiary's rope classification (constraint enables coordination and provides arbitrage benefits) and the victim's snare classification (constraint traps and extracts without alternative). This gap is not a measurement ambiguity — it reflects genuine structural difference: the beneficiary has options, information control, and enforcement authority; the victim has none. Regulatory agencies occupy an intermediate position: they perceive coordination logic (institutions cooperate with oversight) but also extraction logic (institutions control information disclosure). This tangled rope perspective is where the constraint's real mechanism appears — regulators are simultaneously enabling and unable to prevent the extraction. Whistleblowers perceive snare despite moderate power because constrained exit (career jeopardy) and retaliation risk (information suppression enforcement) combine to produce high experienced extraction. The analytical observer sees snare because the structure is inherently asymmetric across all dimensions: information, power, exit, enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Affected populations: victims + trapped → high d (≈0.95) → high f(d) → high χ. Whistleblowers: victims + constrained → high d (≈0.85) → high f(d) → high χ. Institutional leadership: beneficiaries + arbitrage → low d (≈0.10) → negative f(d) → negative χ (experienced as coordination benefit). Regulatory agencies: mixed (coordination function + information asymmetry) + mobile → moderate d (≈0.55) → moderate f(d) → moderate χ. The directionality structure explains the perspectival gap: those with trap/constrained exit experience the constraint as extractive (snare); those with arbitrage exit experience it as beneficial (rope); those with mobile exit plus mixed structural position experience mixed classification (tangled rope). No directionality overrides required — the derived values accurately reflect structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE RESOLUTION: The constraint resolves as snare (not tangled rope) because the coordination function is minimal or illusory. While the institution benefits from operational continuity, this is not coordination — it is extraction with appearance of coordination. True tangled rope requires genuine coordination function (both parties benefit from the constraint existing) plus asymmetric extraction. Here, the affected populations and whistleblowers derive no coordination benefit; their only 'benefit' is institutional stability that further enables their extraction. The regulatory agency's coordination component (mutual benefit from compliant operations) is separate from the concealment constraint itself — regulators coordinate with institutions around transparent compliance, then experience extraction when institutions selectively disclose. The snare classification is confirmed by: (1) high extractiveness (0.68) with rising trajectory; (2) high suppression (0.75) with no feedback mechanism to reduce it; (3) victims experiencing no benefit from the constraint existing; (4) exit options designed as theater (formal complaint mechanisms that don't change outcomes). The mandate is resolved by recognizing that 'institutional coordination' is not genuine coordination of the constraint but rather coordination around visible operations while negligence is concealed. The constraint's real function is extraction, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negligence_intent_threshold,
    'At what point does institutional negligence cross into intentional concealment versus passive information control?',
    'Documentary evidence of deliberate destruction, testimony of intent, comparative analysis of disclosure patterns across similar incidents, timeline reconstruction of when negligence was known versus when concealment began',
    'If primarily passive: constraint is information asymmetry problem (might shift toward tangled_rope from some perspectives). If deliberately orchestrated: constraint is pure extraction mechanism (remains snare). Affects assessment of whether sunset is possible through transparency reform versus requiring structural power redistribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negligence_intent_threshold, empirical, 'Threshold between passive and active concealment').

omega_variable(
    whistleblower_protection_sufficiency,
    'Do existing legal protections and institutional policies for whistleblowers actually reduce retaliation risk below the suppression threshold (0.75)?',
    'Comparative analysis of whistleblower outcomes across jurisdictions with varying protection levels; tracking of retaliation incidents post-disclosure; survey data on perceived safety among potential whistleblowers',
    'If protections are effective: suppression could drop to 0.40-0.50, reclassifying victim perspectives from snare toward tangled_rope. If protections are theatrical: suppression remains, confirming snare classification. Cascades to scaffold viability assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(whistleblower_protection_sufficiency, empirical, 'Effectiveness of whistleblower protection mechanisms').

omega_variable(
    third_party_verification_capacity,
    'Can independent journalists, auditors, or regulators reconstruct institutional negligence from external evidence (survivor testimony, leaked documents, pattern analysis) without institutional cooperation?',
    'Historical case studies of successful institutional accountability without internal disclosure; analysis of evidentiary sufficiency for prosecution or civil liability absent institutional cooperation; cost and time requirements for independent investigation',
    'If third-party verification is feasible: victims have an exit pathway (external accountability) that changes their directionality from trapped to constrained. If external verification is prohibitively expensive or evidentiary barriers are insurmountable: victims remain trapped. Affects whether constraint persists due to suppression intensity or due to structural power asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_verification_capacity, empirical, 'Third-party reconstruction capacity absent institutional cooperation').

omega_variable(
    successor_liability_scope,
    'Can institutional negligence concealment be transferred across organizational boundaries (new leadership, organizational restructuring, subsidiary dissolution) in ways that evade accountability?',
    'Legal analysis of successor liability doctrine; case studies of institutional negligence followed by organizational restructuring; tracking of accountability outcomes across merger/acquisition/dissolution events',
    'If liabilities can be escaped through restructuring: suppression mechanism gains a temporal dimension (delay until organizational form change). If successor liability is reliable: liabilities are inescapable, reducing institutional benefit from concealment. Affects assessment of whether constraint is purely extractive or has coordination logic (temporary delay).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_liability_scope, empirical, 'Whether institutional liability can be transferred across organizational boundaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_negligence_concealment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(negligence_tr_t0, institutional_negligence_concealment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(negligence_tr_t2, institutional_negligence_concealment, theater_ratio, 2, 0.55).
narrative_ontology:measurement(negligence_tr_t5, institutional_negligence_concealment, theater_ratio, 5, 0.65).
narrative_ontology:measurement(negligence_tr_t10, institutional_negligence_concealment, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(negligence_be_t0, institutional_negligence_concealment, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(negligence_be_t2, institutional_negligence_concealment, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(negligence_be_t5, institutional_negligence_concealment, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(negligence_be_t10, institutional_negligence_concealment, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_negligence_concealment, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_negligence_concealment, regulatory_capture).
narrative_ontology:affects_constraint(institutional_negligence_concealment, whistleblower_retaliation).
narrative_ontology:affects_constraint(institutional_negligence_concealment, information_asymmetry_suppression).

% DUAL FORMULATION NOTE:
% Institutional negligence concealment is upstream of specific domain applications (medical negligence, environmental harm, financial fraud) but represents a distinct structural constraint about information control and accountability suppression. Domain-specific stories (hospital_negligence_concealment, environmental_violation_concealment) would have higher ε values reflecting specific harms, while this general story captures the meta-constraint that enables all domain-specific realizations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
