% ============================================================================
% CONSTRAINT STORY: executive_enforcement_congressional_oversight
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_executive_enforcement_congressional_oversight, []).

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
 *   constraint_id: executive_enforcement_congressional_oversight
 *   human_readable: Executive Enforcement vs Congressional Oversight: Asymmetric Information and Institutional Capture
 *   domain: political/institutional
 *
 * SUMMARY:
 *   The constraint between executive enforcement discretion and congressional
 *   oversight authority represents a fundamental tension in constitutional
 *   governance. The executive branch possesses operational knowledge,
 *   real-time information, and statutory authority to enforce laws and carry
 *   out policy. Congress possesses appropriations power, legislative
 *   authority, and electoral accountability. The constraint emerges from the
 *   structural gap between Congress's need for verification (to maintain its
 *   constitutional role) and the executive's informational advantages and
 *   incentives to limit disclosure. This is not a simple principal-agent
 *   problem — both branches are principals with independent constitutional
 *   authority. The constraint exhibits strong tangled rope characteristics:
 *   genuine coordination function (robust oversight prevents bureaucratic
 *   capture and illegal enforcement) coexists with asymmetric extraction
 *   (executive branch systematically exploits information asymmetries and
 *   claims executive privilege to limit accountability). The theater ratio
 *   reflects increasing performativity of oversight: congressional hearings
 *   generate constituent signaling and media coverage but face systematic
 *   non-compliance with document requests, delayed production, and redaction
 *   of information through privilege claims. The extractiveness trend line
 *   shows accumulation over the 50-year interval, driven by expansion of
 *   executive branch scope, growth of national security classification,
 *   increased assertion of executive privilege, and polarization that
 *   weaponizes oversight.
 *
 * KEY AGENTS:
 *   - Executive Branch Agencies: Primary beneficiary (institutional/arbitrage) — capture information asymmetry advantage, control pace of disclosure, exercise discretion in privilege claims
 *   - Congressional Oversight Function: Primary victim (powerless/trapped) — constitutionally mandated but structurally constrained; cannot exit obligation to provide oversight
 *   - Congressional Committees and Caucuses: Secondary victim (organized/constrained) — face resource limitations, partisan polarization, executive non-compliance; benefit from genuine verification function
 *   - Career Civil Service: Mixed position (moderate/identity_locked) — identity fused with agency mission; extracted through politicization while benefiting from oversight preventing capture
 *   - Institutional Reform Coalitions: Organized actors (organized/constrained) — see technical solutions (transparency infrastructure, document production timelines) as sunset for constraint
 *   - Formal Oversight Procedures: Degraded institution (institutional/arbitrage) — maintained through constitutional habit; theater ratio reflects performative function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional configuration as immutable constitutional law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(executive_enforcement_congressional_oversight, 0.58).
domain_priors:suppression_score(executive_enforcement_congressional_oversight, 0.62).
domain_priors:theater_ratio(executive_enforcement_congressional_oversight, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(executive_enforcement_congressional_oversight, extractiveness, 0.58).
narrative_ontology:constraint_metric(executive_enforcement_congressional_oversight, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(executive_enforcement_congressional_oversight, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(executive_enforcement_congressional_oversight, tangled_rope).
narrative_ontology:human_readable(executive_enforcement_congressional_oversight, "Executive Enforcement vs Congressional Oversight: Asymmetric Information and Institutional Capture").
narrative_ontology:topic_domain(executive_enforcement_congressional_oversight, "political/institutional").

domain_priors:requires_active_enforcement(executive_enforcement_congressional_oversight).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(executive_enforcement_congressional_oversight, executive_branch_agencies).
narrative_ontology:constraint_beneficiary(executive_enforcement_congressional_oversight, political_executive).
narrative_ontology:constraint_victim(executive_enforcement_congressional_oversight, congressional_authority).
narrative_ontology:constraint_victim(executive_enforcement_congressional_oversight, public_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONGRESSIONAL OVERSIGHT FUNCTION (SNARE) — Congress cannot exit its constitutional requirement to oversee executive action. Trapped by structural role and constitutional mandate. Faces asymmetric information, delayed disclosure, resource constraints, and executive privilege claims. Bears full extraction cost while possessing nominal authority but limited practical capacity.
constraint_indexing:constraint_classification(executive_enforcement_congressional_oversight, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL CAUCUSES AND COMMITTEES (TANGLED ROPE) — Organized but constrained by partisan polarization, budget limitations, and executive privilege doctrine. Benefit from genuine oversight coordination function (detecting fraud, waste, abuse) while simultaneously experiencing extraction through information asymmetries, delayed document production, and executive branch non-compliance with subpoenas. Constrained exit due to political costs of appearing weak on executive accountability.
constraint_indexing:constraint_classification(executive_enforcement_congressional_oversight, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH LEADERSHIP (ROPE) — Experiences the constraint as genuine coordination mechanism. Congressional oversight establishes legitimacy, prevents runaway agency capture, and creates accountability feedback loops. Net beneficiary through arbitrage: can comply with cooperative oversight or engage executive privilege strategically. Maximum agency in interpreting oversight requirements and timing of compliance.
constraint_indexing:constraint_classification(executive_enforcement_congressional_oversight, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CAREER CIVIL SERVICE (TANGLED ROPE) — Identity-locked between competing loyalty frames: fidelity to agency mission and fidelity to constitutional oversight. Cannot exit institutional role without abandoning professional identity. Experience extraction through politicized oversight, demands for partisan loyalty within executive hierarchy, and pressure to misrepresent enforcement decisions. Also benefit from coordination function where oversight maintains institutional integrity and prevents politicization. Constrained by identity fusion with agency role.
constraint_indexing:constraint_classification(executive_enforcement_congressional_oversight, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL REFORM COALITIONS (SCAFFOLD) — Good-government organizations, transparency advocates, and bipartisan reform movements see oversight capacity as a temporary problem with structural solutions: mandatory disclosure timelines, inspector general independence, technology-enabled document production, standardized compliance metrics. Low theater because the solution is technical rather than performative. Constrained exit due to political feasibility limits, but sunset logic is real — enhanced oversight infrastructure reduces the information asymmetry that enables extraction.
constraint_indexing:constraint_classification(executive_enforcement_congressional_oversight, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FORMAL OVERSIGHT PROCEDURES (PITON) — Congressional hearings, document requests, committee investigations, and inspector general reports persist as ritual performances with degraded functional capacity. Procedures are maintained through institutional habit and constitutional requirement, but their verification function has atrophied as executive privilege doctrine has expanded and information asymmetries have grown. Theater ratio reflects that much congressional oversight activity is performative — generating press coverage and constituency signaling rather than detecting or preventing enforcement abuse. The procedures carry legitimacy from tradition but diminished functional effect.
constraint_indexing:constraint_classification(executive_enforcement_congressional_oversight, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, separation of powers creates inherent information asymmetry: the executive branch must have discretionary authority to execute law, and this discretion necessarily creates a verification gap from the legislative perspective. This perspective naturalizes the constraint as an immutable structural feature of constitutional government. However, the empirical base properties (extractiveness 0.58, suppression 0.62, theater 0.68) contradict the mountain classification — the engine's false summit detector will identify this as naturalization of a contingent institutional configuration rather than a true natural law.
constraint_indexing:constraint_classification(executive_enforcement_congressional_oversight, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(executive_enforcement_congressional_oversight_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(executive_enforcement_congressional_oversight, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(executive_enforcement_congressional_oversight, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(executive_enforcement_congressional_oversight, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(executive_enforcement_congressional_oversight, TR),
    TR >= 0.70.

:- end_tests(executive_enforcement_congressional_oversight_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting substantial but incomplete extraction. The executive branch captures significant informational and temporal advantages, but Congressional access to subpoena power, appropriations leverage, and media amplification constrains total extraction. The value reflects the empirical reality that oversight is real but asymmetrically effective — Congress can force disclosure but faces systematic delays, redactions, and privilege claims. Over the 50-year interval, extractiveness has increased from 0.35 to 0.58, driven by expansion of national security classification, broader executive privilege doctrine interpretation, and growth of executive branch scope relative to congressional capacity. Suppression (0.62): High but not absolute. Barriers to effective oversight include information access limitations (classification, privilege claims), resource constraints (committee staffing and budget), time delays (documents produced after policy implementation), and partisan incentives (majority party may minimize oversight of same-party executive). Career civil servants face additional suppression through politicization pressure and loyalty conflicts. However, suppression is not total — FOIA, inspector general reports, whistleblowers, and media investigation provide alternative verification pathways. Theater ratio (0.68): High and increasing. Congressional hearings, document requests, and committee reports perform accountability functions for constituent audiences while often failing to prevent enforcement abuse. The increase from 0.42 to 0.68 reflects growing performativity: oversight activity has increased (more hearings, more transparency rhetoric) while effectiveness in preventing abuse has declined (measured by persistence of problematic enforcement actions despite oversight criticism). Theater increase correlates with partisan polarization (oversight becomes opposition party performance art) and executive privilege expansion (formal procedures face systematic non-compliance).
 *
 * PERSPECTIVAL GAP:
 *   Seven distinct classifications emerge from identical base properties, demonstrating the full range of perspectival variation. The gap between snare (powerless/trapped congressional oversight) and rope (institutional executive branch leadership) is maximal — identical structural phenomenon perceived as pure extraction vs. pure coordination. This gap reveals the fundamental asymmetry: what Congress experiences as non-compliance and information withholding, the executive experiences as legitimate discretion and necessary security. The piton classification (degraded procedures) indicates that the formal mechanisms (hearings, document requests, inspector general reports) have been partially instrumentalized or rendered performative. The scaffold classification (reform feasibility) suggests the constraint is not immutable — technical and institutional solutions exist to reduce the information asymmetry. The identity_locked classification (civil service) reveals that the constraint's damage extends beyond the institutional struggle — it creates cognitive/loyalty conflicts that degrade institutional integrity. The mountain perspective is the engine's diagnostic problem: it reveals false naturalization when the empirical base contradicts immutability.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive branch leadership occupies the position of primary beneficiary with maximum exit options (arbitrage). They control information production, timing of disclosure, and invocation of executive privilege. Their directionality d is low (toward full beneficiary), producing negative or minimal effective extraction. Their perspective (rope) reflects that they experience the constraint as solving a real coordination problem. Congressional oversight function occupies the position of primary victim with zero exit options (trapped). Congress cannot dissolve its oversight obligation and faces systematic information disadvantage. Directionality d is high (toward full victim), producing maximum experienced extraction from the powerless perspective. Organized congressional committees occupy intermediate position: they are victims of information asymmetry but possess some agency through investigatory authority and subpoena power; directionality d is moderate-high, producing high but not maximal χ. Career civil servants are victims of politicization pressure while beneficiaries of oversight preventing capture — their directionality d is closer to symmetric than either executive leadership or Congress, producing tangled rope. The piton perspective derives not from high directionality but from high theater ratio — the formal procedures have been decoupled from function. The mountain perspective at the analytical level represents maximum directionality ambiguity: from a civilizational view, the entire separation of powers creates structural information asymmetry, making d approach 0.5 (symmetric). However, the empirical base properties contradict this — the trend in extractiveness and theater both increase over time, suggesting institutional design choices rather than immutable constitutional structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that the mixed classification (tangled rope as claimed type) is correct because the constraint genuinely coordinates (Congress's oversight role is necessary for executive accountability and prevents agency capture) while simultaneously extracting (executive branch's information advantages enable systematic non-compliance with transparency expectations). The analytical false summit (mountain) is rejected because the empirical trend contradicts natural law: if the information asymmetry were immutable, extractiveness and theater should remain stable or respond to external shocks, not show monotonic increase over 50 years driven by institutional policy choices (classification expansion, privilege doctrine broadening, capacity starvation). The mandatrophy gate requires that tangled rope constraints demonstrate both genuine coordination function and asymmetric extraction with active enforcement. This constraint meets both: the coordination function is preventing executive agency from becoming autonomous (if Congress truly had no oversight, executive enforcement would drift toward agency preferences rather than presidential policy). The asymmetric extraction is demonstrated by the systematic advantage the executive obtains through information control. Active enforcement is present through executive privilege assertions, document redaction, and delay tactics. The false summit perspective (mountain) serves as a diagnostic: it reveals that policy-makers may incorrectly naturalize the oversight deficit as inherent to constitutional design rather than addressing the institutional design choices (classification scope, privilege doctrine interpretation, committee resource allocation) that could reduce extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry_scope,
    'What fraction of executive enforcement decisions are genuinely unknowable to Congress due to operational security vs. strategically concealed due to institutional incentives?',
    'Post-disclosure comparative analysis: when executive branch documents are finally released (through FOIA, whistleblower disclosure, or declassification), what percentage reveal genuine security concerns vs. political or bureaucratic self-interest?',
    'If >70% genuine security-driven: information asymmetry is partially immutable (mountain signature component). If <40%: asymmetry is strategically maintained (snare/extraction driven). If 40-70%: mixed — genuine need for secrecy intertwined with opportunistic concealment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_scope, empirical, 'Scope of information asymmetry driven by security vs. institutional incentives').

omega_variable(
    congressional_capacity_threshold,
    'At what level of staffing, technology, and budget does congressional oversight capacity transition from structurally constrained to adequate for meaningful verification?',
    'Comparative institutional analysis: oversight capacity in states with well-resourced legislative bodies vs. federal Congress; temporal analysis of oversight effectiveness vs. congressional committee budget allocation; efficiency metrics for document review and investigation completion.',
    'If threshold is achievable within current political feasibility: scaffold sunset is realistic and constraint is tangled rope. If threshold requires sustained bipartisan cooperation: constraint may be effectively mountain (asymmetry immutable under normal politics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_capacity_threshold, empirical, 'Resource threshold for effective congressional oversight capacity').

omega_variable(
    executive_privilege_doctrine_scope,
    'How expansively has executive privilege doctrine been interpreted in practice? What activities are claimed as privileged vs. actually held up in court?',
    'Legal analysis of executive privilege claims in litigation; comparison of claimed scope vs. court-upheld privilege assertions; temporal trend in privilege assertion frequency and breadth.',
    'If doctrine is narrower in practice than in executive assertion: suppression is strategic, not structural. If courts consistently uphold broad privilege: suppression has legal backing, reducing extraction component to legitimate institutional friction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(executive_privilege_doctrine_scope, empirical, 'Actual scope of executive privilege doctrine vs. claimed scope').

omega_variable(
    partisan_polarization_lock,
    'To what extent is the degraded oversight function (piton classification) driven by partisan weaponization of oversight vs. institutional capacity limits?',
    'Longitudinal analysis of oversight activity and effectiveness across periods of unified vs. divided government; comparison of oversight patterns under same-party vs. opposite-party control; analysis of committee resources and investigation completion rates by partisan alignment.',
    'If weaponization is primary driver: constraint persists due to political incentives (identity_locked institutional perspective). If capacity is primary: technical solutions (scaffold) are viable. If mixed: constraint requires both capacity and incentive reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partisan_polarization_lock, empirical, 'Contribution of partisan polarization vs. capacity limits to oversight degradation').

omega_variable(
    constitutional_vs_institutional,
    'Is the information asymmetry inherent to constitutional separation of powers (natural law) or contingent on institutional design choices (tangled rope)?',
    'Comparative constitutional analysis: oversight effectiveness under different institutional designs (parliamentary systems, legislatively-appointed executives, structured transparency requirements). Historical analysis of U.S. oversight effectiveness in different eras.',
    'If natural law: constraint is immutable mountain. If institutional design dependent: constraint is tangled rope with reform feasibility. Resolves the false summit ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_vs_institutional, conceptual, 'Whether oversight asymmetry is inherent to separation of powers or contingent on design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(executive_enforcement_congressional_oversight, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exec_cong_tr_t0, executive_enforcement_congressional_oversight, theater_ratio, 0, 0.42).
narrative_ontology:measurement(exec_cong_tr_t15, executive_enforcement_congressional_oversight, theater_ratio, 15, 0.58).
narrative_ontology:measurement(exec_cong_tr_t30, executive_enforcement_congressional_oversight, theater_ratio, 30, 0.68).
narrative_ontology:measurement(exec_cong_tr_t45, executive_enforcement_congressional_oversight, theater_ratio, 45, 0.75).

% Extraction over time
narrative_ontology:measurement(exec_cong_be_t0, executive_enforcement_congressional_oversight, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(exec_cong_be_t15, executive_enforcement_congressional_oversight, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(exec_cong_be_t30, executive_enforcement_congressional_oversight, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(exec_cong_be_t45, executive_enforcement_congressional_oversight, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(executive_enforcement_congressional_oversight, enforcement_mechanism).
narrative_ontology:affects_constraint(executive_enforcement_congressional_oversight, regulatory_capture).
narrative_ontology:affects_constraint(executive_enforcement_congressional_oversight, administrative_law_opacity).
narrative_ontology:affects_constraint(executive_enforcement_congressional_oversight, civil_service_politicization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(executive_enforcement_congressional_oversight, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
