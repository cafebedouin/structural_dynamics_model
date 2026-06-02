% ============================================================================
% CONSTRAINT STORY: po_investigation_protocol_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint in which an institutional process — investigation of financial
 *   shortfalls at subpostmaster-operated branches — was deliberately designed
 *   or negligently operated to presume guilt and extract evidence of criminal
 *   conduct rather than conduct impartial inquiry. The Horizon IT system
 *   failures created real financial discrepancies at branch level. Rather
 *   than investigate the system, Post Office leadership and investigators
 *   directed the protocol toward finding evidence of subpostmaster theft.
 *   This constraint exhibits the classical snare structure: asymmetric
 *   suppression of alternatives (investigators cannot propose system failure
 *   as the cause), high coercion (criminal prosecution), extraction
 *   benefiting institutional leadership while devastating targeted
 *   subpostmasters. The constraint operated across approximately 20 years
 *   (1999-2019), generating hundreds of false convictions before external
 *   judicial review exposed the institutional bias. The constraint's theater
 *   ratio (0.65) reflects that investigations performed the rituals of
 *   impartial inquiry (interviews, evidence gathering, written findings)
 *   while the protocol itself predetermined the conclusions — the form of due
 *   process masked the substance of presumed guilt.
 *
 * KEY AGENTS:
 *   - Subpostmasters: Primary victims (powerless/trapped) — face criminal prosecution, asset seizure, reputational destruction, and imprisonment based on investigations designed to find guilt regardless of innocence
 *   - Post Office Leadership: Primary beneficiary (institutional/arbitrage) — deflects liability for Horizon system failures onto employees; avoids systemic IT investigation and responsibility; extracts from institutional legitimacy reserves
 *   - Post Office Investigators: Institutional actor (organized/constrained) — constrained by protocol directives; career-dependent on compliance; both benefit (employment security) and suffer (complicity in false investigations)
 *   - Defense Counsel: Moderate actor (moderate/constrained) — asymmetrically disadvantaged by evidence control and withheld disclosure; no exit from institutional power imbalance
 *   - Judicial System: Abstract victim (powerless/trapped) — poisoned by false convictions; delegitimized by institutional bias; no effective mechanism to self-correct while protocol remains hidden
 *   - External Observers: Analytical position (analytical/analytical) — reveal the constraint through longitudinal investigation and appeal hearings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(po_investigation_protocol_bias, 0.68).
domain_priors:suppression_score(po_investigation_protocol_bias, 0.75).
domain_priors:theater_ratio(po_investigation_protocol_bias, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(po_investigation_protocol_bias, extractiveness, 0.68).
narrative_ontology:constraint_metric(po_investigation_protocol_bias, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(po_investigation_protocol_bias, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(po_investigation_protocol_bias, snare).
narrative_ontology:human_readable(po_investigation_protocol_bias, "Post Office Investigation Protocol Bias (Presumption of Guilt)").
narrative_ontology:topic_domain(po_investigation_protocol_bias, "legal/institutional").

domain_priors:requires_active_enforcement(po_investigation_protocol_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(po_investigation_protocol_bias, post_office_leadership).
narrative_ontology:constraint_victim(po_investigation_protocol_bias, subpostmasters).
narrative_ontology:constraint_victim(po_investigation_protocol_bias, judicial_system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCUSED SUBPOSTMASTER (SNARE) — Trapped in an institutional process designed to extract guilt regardless of innocence. No meaningful exit: refusing cooperation worsens the case; cooperation feeds predetermined conclusions. Experiences maximum extraction through legal liability, asset seizure, criminal conviction, and reputational destruction. The protocol itself guarantees victims by design.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JUDICIAL SYSTEM INTEGRITY (SNARE) — The abstract collective good of impartial fact-finding cannot defend itself or exit the protocol bias. Poisoned by false convictions. Extraction manifests as institutional delegitimacy and erosion of due process. This perspective reveals the constraint as a violation of fundamental legal norms, not merely a procedural error.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: POST OFFICE LEADERSHIP (ROPE) — Experiences the protocol as a coordination mechanism: manufacturing evidence of employee crime resolves financial shortfalls without examining systemic IT failures or management responsibility. Leadership benefits from deflection (extracting liability from themselves to subpostmasters) while perceiving the protocol as solving an organizational problem. Net beneficiary with high exit optionality — can modify protocol, suppress evidence, resist scrutiny.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POST OFFICE INVESTIGATORS (TANGLED ROPE) — Trapped between protocol directives (find guilt) and professional duty (impartial investigation). Organized actors but constrained by career risk: deviating from the presumption-of-guilt protocol risks retaliation. Benefit from career advancement and job security for complying; bear psychological cost of conducting knowingly false investigations. Neither pure snare nor pure rope — extracted from by institutional pressure while also benefiting from employment stability.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEFENSE COUNSEL (TANGLED ROPE) — Constrained by asymmetric information: Post Office withholds evidence, controls witness access, conducts closed investigations. Benefits from cases (legal fees) but also bears cost of futility — no exit from the institutional imbalance. Moderate power with constrained options. Sees both coordination function (legal representation framework) and extraction (evidentiary disadvantage).
constraint_indexing:constraint_classification(po_investigation_protocol_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — At the civilizational/universal level, some inherent tension exists between institutional self-interest and impartial investigation: large organizations always have incentive to blame subordinates rather than examine systemic failure. This perspective risks naturalizing what is actually a contingent institutional choice — the presumption-of-guilt protocol is not inherent to investigation, but rather a deliberate design. The mountain classification is a false summit.
constraint_indexing:constraint_classification(po_investigation_protocol_bias, mountain,
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

:- end_tests(po_investigation_protocol_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The protocol directly extracts criminal liability (imprisonment), asset liability (financial penalties and restitution), and reputational harm from accused subpostmasters to benefit Post Office leadership by deflecting blame. The extraction is quantifiable: 739+ convictions, hundreds of asset seizures, documented business failures, and psychological/health impacts. Not maximal (0.70+) because the extraction required victims to cooperate with investigations (creating some agency, however constrained) and because external judicial review eventually exposed the mechanism. The extractiveness increased over the interval as the protocol matured and evidence suppression became more systematic. Suppression (0.75): High. Massive barriers to alternative explanations: investigations closed after guilt-confirming findings; exculpatory evidence (Horizon system failure documentation) was withheld or deprioritized; victims had no access to technical evidence; Post Office controlled all internal investigation processes; external oversight was absent for years. Career penalties for investigators who proposed system failure explanations. Minimal formal appeals process. Theater ratio (0.65): Moderate-high. The investigative process performed the rituals of due process — formal interviews, evidence gathering, written findings, legal representation — but these procedures masked a predetermined conclusion. The theater increased over time as the protocol became more established and investigators developed more sophisticated evidence-finding techniques while remaining constrained by the presumption-of-guilt directive.
 *
 * PERSPECTIVAL GAP:
 *   Subpostmasters see snare; investigators see tangled rope (complicity + constraint); Post Office leadership sees rope (coordination mechanism); judicial system sees institutional poisoning; analytical observer sees either false mountain (natural institutional conflict) or the actual snare (deliberate/negligent design). The gap reveals how institutional procedures can appear as legitimate process to beneficiaries while operating as pure extraction to victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the structural relationship of each agent to the extraction flow. Subpostmasters: victims + trapped → d ≈ 0.95 → f(d) ≈ 1.42, producing maximum experienced extraction. Post Office leadership: beneficiaries + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12, producing negative experienced extraction (they benefit). Investigators: constrained exit + protocol-directed → d ≈ 0.50-0.60 → f(d) ≈ 0.65-0.85, tangled position. Defense counsel: moderate power + constrained options + asymmetric information → d ≈ 0.65 → f(d) ≈ 1.00, moderate extraction. The protocol itself is the mechanism that locks these directionality values in place by controlling what investigations can conclude. No override needed — the structural data directly generates the observed perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that snare classification is not victim bias — it is structural inevitability when a protocol is designed to presume guilt. The protocol does not merely fail to provide due process; it actively prevents due process by suppressing alternative explanations. The tangled rope perspectives (investigators, defense counsel) are real but secondary — they derive from the constraint's operation, not from ambiguity about whether the constraint is extraction or coordination. The underlying constraint is snare; the secondary effects on investigators and counsel are tangled. The false mountain perspective (natural institutional conflict) is exposed as naturalization: presumption of guilt is not inherent to investigation; impartial investigation is the institutional norm (UK procedure code, legal principle). The protocol deliberately inverted this norm, making the snare classification certain rather than perspectival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protocol_design_intentionality,
    'Was the presumption-of-guilt protocol a deliberate design choice to manufacture guilt findings, or an incidental consequence of cost-cutting investigation procedures?',
    'Documentary evidence: internal memos, training materials, policy justifications; testimony from protocol architects; comparative analysis with UK standard investigative procedures for financial misconduct',
    'If deliberate: snare classification is certain (intentional extraction design). If incidental: classification may shift to tangled_rope (unintended harm alongside legitimate investigation function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(protocol_design_intentionality, empirical, 'Whether presumption-of-guilt protocol was deliberate institutional design').

omega_variable(
    evidence_suppression_mechanism,
    'To what extent was exculpatory evidence systematically withheld vs. merely inadequately investigated?',
    'Forensic document review: discovery orders, FOIA requests, trial record analysis; comparison of available evidence to evidence Post Office claimed existed; expert assessment of when IT system failures should have been obvious to investigators',
    'If systematic withholding: snare classification with high suppression gate locked. If inadequate investigation: snare still holds but suppression value may decrease (shift from 0.75 to 0.60-0.65), making some perspectives tangled_rope instead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evidence_suppression_mechanism, empirical, 'Whether evidence suppression was systematic or incidental').

omega_variable(
    institutional_knowledge_of_horizon,
    'When did Post Office leadership have reasonable knowledge that Horizon IT system failures (not subpostmaster theft) were the primary cause of shortfalls, and how did this knowledge affect the protocol?',
    'Timeline of IT system failure reports, internal audits, and litigation; cross-reference with protocol modifications; testimony regarding what information was communicated to investigators and executives',
    'If knowledge existed but was suppressed: confirms deliberate snare design. If knowledge genuinely didn''t exist: mitigates snare classification toward tangled_rope (victims of systemic misunderstanding rather than institutional extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_knowledge_of_horizon, empirical, 'Timing of Post Office knowledge of system failures vs. protocol design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(po_investigation_protocol_bias, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(poinv_tr_t0, po_investigation_protocol_bias, theater_ratio, 0, 0.5).
narrative_ontology:measurement(poinv_tr_t10, po_investigation_protocol_bias, theater_ratio, 10, 0.65).
narrative_ontology:measurement(poinv_tr_t20, po_investigation_protocol_bias, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(poinv_be_t0, po_investigation_protocol_bias, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(poinv_be_t10, po_investigation_protocol_bias, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(poinv_be_t20, po_investigation_protocol_bias, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(po_investigation_protocol_bias, enforcement_mechanism).
narrative_ontology:affects_constraint(po_investigation_protocol_bias, uk_judicial_review_delays).
narrative_ontology:affects_constraint(po_investigation_protocol_bias, institutional_evidence_disclosure_asymmetry).
narrative_ontology:affects_constraint(po_investigation_protocol_bias, criminal_conviction_reversal_friction).

% DUAL FORMULATION NOTE:
% The Post Office investigation protocol bias is upstream of multiple constraint failures: judicial review delays (external mechanism cannot correct institutional bias quickly), evidence disclosure asymmetry (structural advantage retained by investigating institution), and conviction reversal friction (appeals process inadequate to systematic false convictions). Each downstream constraint has its own extractiveness but is causally dependent on the bias in this protocol.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(po_investigation_protocol_bias, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
