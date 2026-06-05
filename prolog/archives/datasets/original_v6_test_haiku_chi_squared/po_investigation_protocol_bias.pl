% ============================================================================
% CONSTRAINT STORY: po_investigation_protocol_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_po_investigation_protocol_bias, []).

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
 *   constraint_id: po_investigation_protocol_bias
 *   human_readable: Post Office Investigation Protocol Bias (Presumption of Guilt)
 *   domain: legal/institutional
 *
 * SUMMARY:
 *   The Post Office investigation protocol bias represents a structural
 *   constraint that systematically presumed guilt and directed investigators
 *   to find evidence of crime in cases of financial shortfalls, rather than
 *   conducting impartial inquiry. The protocol operated within the UK Post
 *   Office institutional framework from approximately 2000-2015, affecting
 *   hundreds of subpostmasters and their employees. The constraint functioned
 *   as a Snare: extraction of liability and criminal prosecution from those
 *   trapped within the system, with suppression of alternative investigative
 *   pathways and alternative explanations (Horizon IT system failures,
 *   accounting errors, legitimate operational variance). The protocol
 *   combined institutional authority (Post Office as accuser, investigator,
 *   and primary witness) with procedural bias (presumption of guilt rather
 *   than innocence), creating compounded extraction from the most powerless
 *   actors (individual subpostmasters) who lacked independent legal
 *   representation or internal appeals mechanisms. The theater ratio (0.65)
 *   reflects that the investigation process maintained the appearance of
 *   systematic inquiry and due process while systematically excluding
 *   exculpatory evidence and alternative hypotheses. The constraint became
 *   institutionalized through bureaucratic inertia — investigators
 *   internalized the presumption-of-guilt framework as standard practice,
 *   courts accepted Post Office investigative findings with insufficient
 *   independent verification, and the system persisted despite documented
 *   failures in evidence quality and investigative logic.
 *
 * KEY AGENTS:
 *   - Subpostmasters: Primary victims (powerless/trapped) — financially liable for system errors; lack independent legal representation during investigation; face prosecution based on presumed guilt
 *   - Subpostmaster Employees: Primary victims (powerless/trapped) — face criminal prosecution based on presumed guilt within investigation; employment vulnerability compounds legal vulnerability
 *   - Post Office Management: Primary beneficiaries (institutional/arbitrage) — extracts liability from subpostmasters; resolves financial accountability through rapid closure of investigation; experiences constraint as efficient procedure
 *   - Post Office Institution: Institutional actor (institutional/arbitrage) — benefits from reputation of rigorous investigation while actually conducting biased inquiry; theater ratio shows performative aspects exceed functional truth-finding
 *   - Field Justice System: Secondary victim (moderate/constrained) — courts receive presumed-guilty findings without independent verification; judicial process becomes mechanism for legitimizing flawed investigation
 *   - Analytical Observer: Sees the constraint as violation of investigative justice principles — systematic bias extracting convictions rather than truth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(po_investigation_protocol_bias, 0.68).
domain_priors:suppression_score(po_investigation_protocol_bias, 0.82).
domain_priors:theater_ratio(po_investigation_protocol_bias, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(po_investigation_protocol_bias, extractiveness, 0.68).
narrative_ontology:constraint_metric(po_investigation_protocol_bias, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(po_investigation_protocol_bias, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(po_investigation_protocol_bias, snare).
narrative_ontology:human_readable(po_investigation_protocol_bias, "Post Office Investigation Protocol Bias (Presumption of Guilt)").
narrative_ontology:topic_domain(po_investigation_protocol_bias, "legal/institutional").

domain_priors:requires_active_enforcement(po_investigation_protocol_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(po_investigation_protocol_bias, post_office_management).
narrative_ontology:constraint_beneficiary(po_investigation_protocol_bias, post_office_institution).
narrative_ontology:constraint_victim(po_investigation_protocol_bias, subpostmasters).
narrative_ontology:constraint_victim(po_investigation_protocol_bias, subpostmaster_employees).
narrative_ontology:constraint_victim(po_investigation_protocol_bias, field_justice_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBPOSTMASTER (SNARE) — Trapped in investigative process with presumed guilt; financial liability regardless of innocence; exit via resignation means loss of livelihood and business. No alternative recourse within the institution. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.93.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUBPOSTMASTER EMPLOYEE (SNARE) — Trapped by employment relationship; faces criminal prosecution based on presumed guilt within investigation; no independent legal representation or appeal mechanism within the process. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.96.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FIELD JUSTICE SYSTEM (SNARE) — Courts receive presumed-guilty findings from Post Office investigators; judicial process legitimizes flawed investigation without independent verification of evidence; system becomes mechanism for extracting convictions rather than truth-finding. d≈0.80, f(d)≈1.25, σ=1.0 → χ≈0.85.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POST OFFICE MANAGEMENT (ROPE) — Benefits from coordination mechanism that resolves financial accountability through rapid closure of investigation; sees the protocol as efficient asset recovery and risk mitigation. Experiences constraint as functional procedure rather than coercive extraction. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: POST OFFICE INSTITUTION (PITON) — Investigation protocol persists through bureaucratic inertia despite known failures in evidence quality and justification logic; the performative aspect (appearance of investigation) exceeds the functional aspect (actual truth-finding). theater_ratio=0.65 reflects that investigation ritual maintained institutional credibility while avoiding genuine inquiry. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From universal perspective, the protocol violates core principles of investigative justice: presumption of innocence, burden of proof on accuser, independent evidence verification. The constraint extracts convictions through systematic bias rather than fact-finding. ε=0.68 reflects that institutional power converted financial accountability obligations into persecution mechanism. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.75.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(po_investigation_protocol_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(po_investigation_protocol_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(po_investigation_protocol_bias, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(po_investigation_protocol_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(po_investigation_protocol_bias, TR),
    TR >= 0.70.

:- end_tests(po_investigation_protocol_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The protocol systematically extracts liability, financial penalties, and criminal convictions from subpostmasters based on procedural bias rather than evidence. The extraction is severe but not maximum (0.95+) because some subpostmasters eventually obtained legal representation and appeals that partially mitigated the extraction. The trajectory from 0.38 to 0.68 reflects escalating institutional commitment to the biased protocol and increasing normalization of presumption-of-guilt framing. Suppression (0.82): Very high. Multiple layers suppress alternative pathways: (1) investigative protocol itself presumes guilt and excludes exculpatory evidence, (2) absence of independent legal representation for investigated subjects, (3) no internal appeals mechanism within the institution, (4) institutional authority consolidates accuser, investigator, and primary witness roles, (5) judicial system accepts Post Office findings without independent verification. Theater ratio (0.65): Moderate-high. The investigation process performs the ritual of systematic inquiry (interviews, evidence gathering, formal procedure) while systematically excluding alternative hypotheses and exculpatory evidence. The appearance of investigation legitimizes what is actually a predetermined liability extraction. The trajectory from 0.42 to 0.65 reflects increasing formalization of the procedure — more elaborate theater as the system encounters resistance and must justify its conclusions more thoroughly.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Post Office management sees a functional coordination mechanism (Rope) — the protocol efficiently resolves financial accountability and protects institutional assets. The Post Office institution sees a degraded ritual (Piton) — the performative investigation maintains credibility while avoiding genuine fact-finding. Both powerless agents (subpostmasters and employees) see pure extraction (Snare) — trapped in a system that presumes guilt and extracts convictions regardless of evidence. The field justice system sees constrained extraction (Snare) — courts become mechanisms for legitimizing flawed investigation when they should be independent arbiters. The analytical observer sees the deepest extraction (Snare) — systematic violation of investigative justice principles. The perspectival gap reveals that the 'coordination' the institution perceives (efficient liability resolution) is experienced as persecution by the powerless agents; this gap indicates the constraint is genuinely extractive rather than coordinative.
 *
 * DIRECTIONALITY LOGIC:
 *   Subpostmasters: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction from those with zero exit options. Subpostmaster employees: Victim + trapped → d≈0.95, f(d)≈1.42. Slightly higher d than subpostmasters due to employment vulnerability. Field justice system: Victim + constrained → d≈0.80, f(d)≈1.25. Constrained because courts retain formal independence but in practice accept Post Office framing. Post Office management: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with complete freedom to exit (as institution policy). Post Office institution: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Similar to management; institutional-level beneficiary status. Analytical observer: d≈0.75, f(d)≈1.10. Sees the constraint as extractive mechanism violating justice principles.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Post Office management's perception of the protocol as functional coordination (Rope) is precisely the mechanism by which the Snare operates. The protocol is NOT genuinely coordinative — it does not solve a collective action problem where all parties benefit from the solution. Rather, management's framing of the protocol as 'efficient liability resolution' naturalizes extraction. The mandatrophy resolution identifies: (1) The protocol lacks genuine coordination benefit — subpostmasters and field justice system both bear costs with no compensating benefits. (2) Suppression is extreme (0.82), excluding alternative investigative pathways and alternative evidence, which is incompatible with true coordination. (3) Theater is high (0.65), indicating the performative aspects (appearance of investigation) exceed functional aspects (actual truth-finding), which is the signature of a degraded institution using ritual to maintain legitimacy. The constraint's classification as Snare is confirmed by: (a) Extractiveness 0.68 exceeds Rope threshold (0.45), (b) Suppression 0.82 exceeds Rope threshold (minimal coercion), and (c) The perspectival gap shows institutional management and powerless agents perceive fundamentally different constraints, with the institutional perception revealing self-interested framing rather than genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_bias,
    'Was the presumption-of-guilt protocol a deliberate institutional design to extract liability from subpostmasters, or an evolving bureaucratic practice that ossified into systematic bias?',
    'Documentary analysis of investigative procedure evolution; interviews with investigators and management; comparison to equivalent investigative protocols in other UK institutions',
    'If deliberate design: Snare classification is confirmed with institutional malfeasance. If emergent inertia: Classification remains Snare but with different accountability implications (negligence vs conspiracy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_bias, empirical, 'Whether presumption-of-guilt protocol was deliberate institutional design or emergent bias').

omega_variable(
    evidence_admissibility_knowledge,
    'Did Post Office investigators knowingly present inadmissible or unreliable evidence (Horizon system failures, accounting errors) as conclusive proof, or did they operate within genuine epistemic limitations of their time?',
    'Forensic analysis of Horizon IT system failures by independent experts; timeline of when failures were documented internally vs when investigations proceeded; comparison with contemporaneous IT industry knowledge',
    'If knowing: Extraction mechanism is fraud (intentional deception). If epistemic limitation: Extraction mechanism is institutional capture by faulty systems (negligent reliance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evidence_admissibility_knowledge, empirical, 'Whether investigators knowingly presented unreliable evidence').

omega_variable(
    subpostmaster_coalition_capacity,
    'Could subpostmasters have organized coalition resistance to the investigation protocol, or were structural barriers (economic dependence, isolation, fear of legal retaliation) too severe?',
    'Historical analysis of attempted organizing; comparison with other post office unions and professional associations; assessment of economic vulnerability factors',
    'If coalition possible: Powerless classification might be upgraded to Organized at certain scales. If barriers total: Powerless/trapped classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subpostmaster_coalition_capacity, empirical, 'Whether subpostmasters could have mounted coalition resistance').

omega_variable(
    judicial_system_complicity,
    'To what extent did the judicial system exercise independent scrutiny of Post Office investigative findings, versus accepting presumed-guilt framing without adequate verification?',
    'Analysis of court records; comparison of judicial questioning patterns in PO-referred cases vs other white-collar investigations; interviews with judges and prosecutors',
    'If independent scrutiny: Field justice system escapes Snare classification (sees moderate rather than severe extraction). If systemic deference: Snare classification confirmed; field justice system is co-opted mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_system_complicity, empirical, 'Degree of judicial independence in evaluating Post Office investigative findings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(po_investigation_protocol_bias, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(poipb_tr_t0, po_investigation_protocol_bias, theater_ratio, 0, 0.42).
narrative_ontology:measurement(poipb_tr_t7, po_investigation_protocol_bias, theater_ratio, 7, 0.53).
narrative_ontology:measurement(poipb_tr_t14, po_investigation_protocol_bias, theater_ratio, 14, 0.65).

% Extraction over time
narrative_ontology:measurement(poipb_be_t0, po_investigation_protocol_bias, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(poipb_be_t7, po_investigation_protocol_bias, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(poipb_be_t14, po_investigation_protocol_bias, base_extractiveness, 14, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(po_investigation_protocol_bias, enforcement_mechanism).
narrative_ontology:affects_constraint(po_investigation_protocol_bias, judicial_system_evidentiary_standards).
narrative_ontology:affects_constraint(po_investigation_protocol_bias, institutional_accountability_mechanisms).

% DUAL FORMULATION NOTE:
% The Post Office investigation protocol bias is downstream of institutional accountability structures that require resolution of financial shortfalls, but represents a distinct constraint in how that accountability is executed. The upstream constraints (institutional accountability obligation) are genuine coordination problems; this constraint represents the extractive mechanism created by biased implementation of the accountability framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(po_investigation_protocol_bias, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
