% ============================================================================
% CONSTRAINT STORY: organizational_accountability_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_accountability_capture, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: organizational_accountability_capture
 *   human_readable: Organizational Accountability Capture
 *   domain: organizational_governance/institutional_design
 *
 * SUMMARY:
 *   Organizational accountability capture describes the structural dynamic
 *   where mechanisms designed to hold organizations (and their leadership)
 *   accountable to stakeholders become instruments for controlling
 *   stakeholder access to accountability itself. The constraint exhibits the
 *   full spectrum of Deferential Realism types across perspectives: external
 *   stakeholders trapped in snare dynamics with no exit; executive leadership
 *   arbitraging regulatory compliance; mid-level compliance personnel
 *   experiencing mixed coordination-extraction hybrids; organized
 *   accountability advocates building sunset pathways through regulatory and
 *   technological reform; degraded compliance theater persisting through
 *   institutional inertia; and the risk of false summits that naturalize
 *   principal-agent problems as immutable logical limits. The constraint's
 *   extractiveness has increased from 0.35 to 0.58 over the measurement
 *   interval, driven by both increasing organizational complexity (making
 *   accountability mechanisms harder to operate effectively) and increasing
 *   sophistication in accountability capture techniques (regulatory
 *   arbitrage, narrative control, selective transparency). Theater ratio has
 *   risen from 0.45 to 0.68, indicating that organizations have invested
 *   heavily in accountability appearance (ethics committees, compliance
 *   certifications, diversity reporting) while functional accountability
 *   capacity has stagnated or declined.
 *
 * KEY AGENTS:
 *   - Executive Leadership: Primary beneficiary (institutional/arbitrage) — captures regulatory advantage, controls narrative, manages liability exposure, arbitrages between compliance regimes
 *   - Stakeholder Constituency: Primary victim (powerless/trapped) — employees, customers, communities, investors with no meaningful exit; face accountability mechanisms controlled by agents being held accountable
 *   - Mid-Level Compliance/HR Personnel: Secondary actor (moderate/constrained) — coordinate genuine accountability functions while being constrained by executive pressure; face career risk for aggressive investigation
 *   - Internal Auditors/Ethics Officers: Secondary actor (institutional/constrained) — appointed to ensure internal accountability but structurally embedded in power hierarchy; report to boards including agents they investigate
 *   - External Accountability Advocates: Organized reformers (organized/constrained) — worker movements, consumer advocacy, regulatory coalitions, shareholder activism building alternative pathways with sunset logic
 *   - Regulatory and Compliance Theater: Institutional mechanism (institutional/arbitrage) — formal structures (ethics committees, audit functions, certifications) that persist through degradation and inertia
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional capture as inherent principal-agent problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_accountability_capture, 0.58).
domain_priors:suppression_score(organizational_accountability_capture, 0.62).
domain_priors:theater_ratio(organizational_accountability_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_accountability_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(organizational_accountability_capture, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(organizational_accountability_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_accountability_capture, tangled_rope).
narrative_ontology:human_readable(organizational_accountability_capture, "Organizational Accountability Capture").
narrative_ontology:topic_domain(organizational_accountability_capture, "organizational_governance/institutional_design").

domain_priors:requires_active_enforcement(organizational_accountability_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_accountability_capture, executive_leadership).
narrative_ontology:constraint_beneficiary(organizational_accountability_capture, accountability_mechanism_operators).
narrative_ontology:constraint_victim(organizational_accountability_capture, stakeholder_constituency).
narrative_ontology:constraint_victim(organizational_accountability_capture, organizational_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STAKEHOLDER CONSTITUENCY (SNARE) — External stakeholders (employees, customers, communities, investors) face accountability mechanisms that appear to provide redress but are controlled by the very agents being held accountable. No meaningful exit: stakeholders cannot withdraw consent or abandon reliance on the organization. Suppression is structural — internal escalation channels route complaints back to leadership, external regulatory channels are captured or under-resourced, exit means losing livelihood/service access. Maximum experienced extraction.
constraint_indexing:constraint_classification(organizational_accountability_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EXECUTIVE LEADERSHIP (ROPE) — Experiences accountability mechanisms as coordination infrastructure enabling organizational function. Can arbitrage between compliance regimes, timing of disclosures, and regulatory jurisdictions. The accountability apparatus provides both coordination (ensuring internal reporting standards) and selective extraction (controlling what becomes public, timing sensitivity communications). Net beneficiary with significant maneuverability.
constraint_indexing:constraint_classification(organizational_accountability_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-LEVEL COMPLIANCE/HR (TANGLED ROPE) — Internal accountability operators bear dual extraction. They coordinate genuine organizational accountability functions (complaint tracking, investigation protocols, policy development) but are simultaneously constrained by executive pressure to minimize liability, contained damage, and public escalation. Career risk if they pursue complaints aggressively; structural confinement if they leave the role. Mixed function: real coordination with asymmetric extraction.
constraint_indexing:constraint_classification(organizational_accountability_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXTERNAL ACCOUNTABILITY ADVOCATES (SCAFFOLD) — Organized agents (worker movements, consumer advocacy, regulatory reform coalitions, shareholder activism, platform transparency advocates) perceive accountability capture as a temporary institutional failure with a sunset. Regulatory mandates (SEC disclosure rules, EU corporate governance, whistleblower protections), technological solutions (blockchain auditing, open-source compliance frameworks), and organizational learning are building alternative accountability pathways. Constrained by resource limitations and political opposition, but see a real exit strategy through decentralization and transparency norms.
constraint_indexing:constraint_classification(organizational_accountability_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY/COMPLIANCE THEATER (PITON) — Formal accountability structures (ethics committees, audit functions, compliance certifications) persist as degraded institutional forms. They perform accountability — generating reports, certifications, audit trails — while functional accountability mechanisms have atrophied. Theater ratio high because organizations can display compliance infrastructure without suffering meaningful constraints on leadership behavior. The mechanism survives through regulatory mandate and institutional inertia, not through functional verification capacity.
constraint_indexing:constraint_classification(organizational_accountability_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNAL AUDITORS/ETHICS OFFICERS (TANGLED ROPE) — Institutional agents appointed to ensure internal accountability face structural capture. They coordinate genuine internal control functions but are embedded in the organization's power hierarchy — they report to boards that include the agents they are supposed to investigate, face career termination risk for aggressive findings, and operate under asymmetric information (leadership controls what evidence reaches them). Beneficiaries of the accountability infrastructure (provides career role, authority framework) while simultaneously being systematically constrained from exercising accountability against powerful actors.
constraint_indexing:constraint_classification(organizational_accountability_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PRINCIPAL-AGENT NATURALIZATION (MOUNTAIN) — Risk of naturalizing a contingent institutional problem as an immutable logical limit. The principal-agent problem (delegating authority to agents whose incentives misalign with principal interests) is real, but claims that perfect accountability is 'impossible' naturalize specific institutional design choices (hierarchical reporting, board capture, asymmetric information) as inherent rather than contingent. True analytical perspective sees accountability capture as a structural failure in institutional design, not a law of nature.
constraint_indexing:constraint_classification(organizational_accountability_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_accountability_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_accountability_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_accountability_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_accountability_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_accountability_capture, TR),
    TR >= 0.70.

:- end_tests(organizational_accountability_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from stakeholders through the appearance of accountability without substance — stakeholders invest time and emotional labor in processes designed to contain rather than address their concerns. The extraction is not maximal (0.70+) because some accountability mechanisms function at least partially, and some organizations have implemented genuine reforms. The measured increase from 0.35 to 0.58 reflects accumulation of capture techniques and organizational sophistication in apparent compliance. Suppression (0.62): Moderate-high. Multiple suppression mechanisms operate: internal escalation routes complaints back to leadership; external regulatory channels are under-resourced or captured; stakeholders cannot exit without losing livelihood/service access; retaliation for complaint-filing is structurally embedded in hierarchies; information asymmetry prevents stakeholders from knowing what accountability mechanisms discovered. Theater ratio (0.68): High. Organizations have invested substantially in accountability appearance — compliance certifications, ethics committees, diversity reporting, whistleblower hotlines — that function primarily as liability management tools rather than substantive accountability mechanisms. The theater persists because it satisfies external regulatory requirements and stakeholder demand for 'something is being done' while constraining actual organizational change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival disagreement across organizational positions. Leadership perceives Rope (coordination infrastructure serving organizational legitimacy and stability). Stakeholders perceive Snare (extractive mechanism with no functional redress). Mid-level operators perceive Tangled Rope (genuine but constrained coordination work mixed with extraction pressure). Organized advocates perceive Scaffold (temporary problem being solved by transparency norms and regulatory reform). The compliance apparatus perceives itself as Piton (degraded ritual maintained through institutional requirement). The false summit risk is that analytical observers naturalize this as principal-agent inevitability rather than recognizing it as a specific institutional design failure. The perspectival gap is not measurement uncertainty — it is genuine structural disagreement about what the constraint IS and whose interests it serves.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi formula χ = ε × f(d) × σ(S) drives substantial variation in experienced extractiveness across perspectives. Stakeholder constituencies (d ≈ 0.92, f(d) ≈ 1.42) at regional scope (σ=0.9) experience χ ≈ 0.58 × 1.42 × 0.9 ≈ 0.74 — near-snare extractiveness. Executive leadership (d ≈ 0.15, f(d) ≈ -0.01) at global scope (σ=1.2) experiences χ ≈ 0.58 × (-0.01) × 1.2 ≈ -0.007 — nearly zero or negative effective extraction, consistent with Rope classification. Mid-level compliance personnel (d ≈ 0.58, f(d) ≈ 0.65) at national scope (σ=1.0) experience χ ≈ 0.58 × 0.65 × 1.0 ≈ 0.38 — consistent with Tangled Rope. The same base extractiveness (0.58) produces wildly different classifications depending on structural position (agent_power, exit_options, spatial_scope). This perspectival multiplicity is the core analytical feature of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by distinguishing between appearance and function at organizational scope. The mandatrophy question is: 'Does this constraint coordinate organizational function (legitimate accountability infrastructure) or extract from stakeholders (capture apparatus)?' The answer is: both, simultaneously, from different perspectives. Leadership benefits from coordination of internal control functions and external reputation management. Stakeholders bear extraction costs through suppressed accountability. The constraint's genuine coordination function (preventing some fraud, safety violations, and unethical behavior) is real but asymmetric — the benefits accrue to leadership through risk reduction and legitimacy, while stakeholders bear the costs of constrained accountability. The constraint cannot be classified as pure Rope (the coordination would need to distribute benefits) or pure Snare (some coordination function genuinely exists). Tangled Rope classification holds: the constraint coordinates organizational control while enforcing asymmetric extraction on stakeholders. The mandatrophy resolves through recognizing that principal-agent coordination and principal-victim extraction are the same mechanism operating at different scales.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_for_systemic_capture,
    'At what level of complaint suppression does an accountability mechanism transition from imperfect coordination to systematic extraction?',
    'Comparative analysis of complaint resolution rates, retention curves of employees filing complaints, correlation between complaint types and leadership action patterns',
    'If threshold is high (>80% suppression): many organizations misclassified as coordination systems. If threshold is low (<40% suppression): even partially-functional systems misclassified as extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_for_systemic_capture, empirical, 'Complaint suppression threshold for systemic capture classification').

omega_variable(
    external_oversight_effectiveness,
    'Do external regulatory bodies (labor boards, securities regulators, consumer protection agencies) provide independent accountability verification or are they themselves captured by the regulated industry?',
    'Analysis of regulatory penalty patterns, industry influence on regulator hiring/policy, correlation between industry presence on agency boards and enforcement rates',
    'If external oversight is independent: accountability capture is organizational-level problem (Tangled Rope/Scaffold). If regulators are captured: accountability capture is systemic across sectors (Snare from stakeholder view).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_oversight_effectiveness, empirical, 'Whether external regulatory oversight remains independent from regulated industry').

omega_variable(
    identity_capture_in_compliance_roles,
    'Do mid-level compliance and ethics personnel internalize organizational loyalty such that they cannot psychologically pursue complaints against leadership, even when procedurally authorized?',
    'Interviews with departing compliance officers; analysis of complaint patterns post-departure vs during tenure; whistleblower timing relative to role transitions',
    'If strong identity fusion: compliance personnel are identity_locked rather than constrained; suppression is partly internalized rather than purely structural. Classification shifts from external constraint to cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_capture_in_compliance_roles, empirical, 'Whether compliance role identity creates internalized suppression of accountability actions').

omega_variable(
    coordination_function_authenticity,
    'Is the accountability apparatus genuinely coordinating organizational function (real control over fraud, safety violations, ethical breaches) or is that coordination function illusory and the mechanism exists purely for extraction and theater?',
    'Analysis of organizations with disabled accountability systems: do control failures increase? Comparison of scandal patterns in organizations with strong vs weak accountability infrastructure.',
    'If coordination is real: Tangled Rope classification holds. If coordination is illusory: constraint is pure Snare regardless of beneficiary presence. This determines whether the constraint can be reformed or only dismantled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Whether accountability mechanisms perform genuine organizational control function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_accountability_capture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orgacct_tr_t0, organizational_accountability_capture, theater_ratio, 0, 0.45).
narrative_ontology:measurement(orgacct_tr_t10, organizational_accountability_capture, theater_ratio, 10, 0.62).
narrative_ontology:measurement(orgacct_tr_t20, organizational_accountability_capture, theater_ratio, 20, 0.68).
narrative_ontology:measurement(orgacct_tr_t5, organizational_accountability_capture, theater_ratio, 5, 0.54).

% Extraction over time
narrative_ontology:measurement(orgacct_be_t0, organizational_accountability_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orgacct_be_t10, organizational_accountability_capture, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(orgacct_be_t20, organizational_accountability_capture, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(orgacct_be_t5, organizational_accountability_capture, base_extractiveness, 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_accountability_capture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(organizational_accountability_capture, 0.12).
narrative_ontology:affects_constraint(organizational_accountability_capture, regulatory_capture).
narrative_ontology:affects_constraint(organizational_accountability_capture, whistleblower_retaliation).
narrative_ontology:affects_constraint(organizational_accountability_capture, organizational_opacity).

% DUAL FORMULATION NOTE:
% Organizational accountability capture is structurally upstream of regulatory_capture (organizations captured by their accountability mechanisms then capture regulators) and laterally coupled to whistleblower_retaliation (retaliation mechanisms target those who bypass captured accountability structures). Both downstream constraints are influenced by the extractiveness and suppression levels in this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_accountability_capture, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
