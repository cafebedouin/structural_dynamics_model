% ============================================================================
% CONSTRAINT STORY: organizational_liability_diffusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_liability_diffusion, []).

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
 *   constraint_id: organizational_liability_diffusion
 *   human_readable: Organizational Liability Diffusion Through Hierarchical Distancing
 *   domain: organizational/legal/governance
 *
 * SUMMARY:
 *   Organizational liability diffusion through hierarchical distancing
 *   creates a structural asymmetry: senior decision-makers benefit from
 *   information opacity and hierarchical buffer zones that insulate them from
 *   personal legal liability, while frontline workers bear concentrated
 *   liability for implementing decisions made above them. The constraint
 *   exhibits features of coordination (hierarchies are necessary for
 *   distributing work in complex organizations) and extraction (the liability
 *   distribution systematically protects those with power and exposes those
 *   without). The theater ratio (0.68) reflects the substantial
 *   legal-procedural complexity used to justify liability diffusion: intent
 *   standards, knowledge requirements, and evidentiary burdens create a
 *   theater of accountability that rarely penetrates hierarchical distance.
 *   The constraint is actively enforced through organizational structures,
 *   legal doctrine, and insurance mechanisms that all presuppose and
 *   reinforce the diffusion. Regulatory reform movements (labor law, consumer
 *   protection, environmental liability) represent organized attempts to
 *   pierce hierarchical diffusion through vicarious liability, documentation
 *   requirements, and explicit knowledge standards, creating scaffold
 *   pressure with eventual sunset logic.
 *
 * KEY AGENTS:
 *   - Frontline Workers: Primary victims (powerless/trapped) — bear concentrated personal and legal liability for decisions made by others with limited information access or ability to refuse
 *   - Middle Managers: Secondary victims (moderate/constrained) — translate decisions downward while held accountable both upward and downward; exit costs are high due to professional specialization
 *   - Senior Leadership: Primary beneficiaries (institutional/arbitrage) — insulated from personal liability through hierarchical distance; experience hierarchy as enabling coordination rather than extraction
 *   - Organizational Continuity: Beneficiary (institutional/arbitrage) — organizational survival is protected by diffusion of liability that prevents single points of legal failure
 *   - Accountability Systems: Primary victim (powerless/trapped) — abstract system that cannot exit or organize; liability diffusion prevents accurate attribution of responsibility
 *   - Regulatory Reform Movements: Organized actors (organized/constrained) — unions, consumer advocates, regulatory bodies building alternative accountability standards; see constraint as temporary and solvable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_liability_diffusion, 0.58).
domain_priors:suppression_score(organizational_liability_diffusion, 0.65).
domain_priors:theater_ratio(organizational_liability_diffusion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_liability_diffusion, extractiveness, 0.58).
narrative_ontology:constraint_metric(organizational_liability_diffusion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(organizational_liability_diffusion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_liability_diffusion, tangled_rope).
narrative_ontology:human_readable(organizational_liability_diffusion, "Organizational Liability Diffusion Through Hierarchical Distancing").
narrative_ontology:topic_domain(organizational_liability_diffusion, "organizational/legal/governance").

domain_priors:requires_active_enforcement(organizational_liability_diffusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_liability_diffusion, senior_leadership).
narrative_ontology:constraint_beneficiary(organizational_liability_diffusion, organizational_continuity).
narrative_ontology:constraint_victim(organizational_liability_diffusion, frontline_workers).
narrative_ontology:constraint_victim(organizational_liability_diffusion, accountability_systems).
narrative_ontology:constraint_victim(organizational_liability_diffusion, external_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE WORKER (SNARE) — Bears personal legal and reputational liability for decisions made under organizational pressure with limited information access. Cannot exit without career termination. Suppression: institutional hierarchy prevents upward voice, documentation of orders is sparse, and whistleblower retaliation is real. Maximum extraction relative to power.
constraint_indexing:constraint_classification(organizational_liability_diffusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Translates directives downward while buffering some organizational knowledge upward. Experiences both genuine coordination (translating intent to action) and extraction (held accountable for both above and below). Exit options constrained by career lock-in and professional licensing. Sees constraint as mixed.
constraint_indexing:constraint_classification(organizational_liability_diffusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SENIOR LEADERSHIP (ROPE) — Benefits from hierarchical distance: decisions are communicated through intermediaries, creating plausible deniability. Experiences the constraint as pure coordination: the organizational hierarchy enables delegation without personal exposure. High exit optionality (can move between organizations; benefits from institutional mobility). Net beneficiary.
constraint_indexing:constraint_classification(organizational_liability_diffusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM (SCAFFOLD) — Organized actors (labor unions, consumer advocates, regulatory bodies) are building alternative accountability structures: piercing the corporate veil, imposing joint liability, requiring documentation trails, establishing vicarious liability standards. These create sunset pressure on diffusion mechanisms. See constraint as temporary misalignment being corrected.
constraint_indexing:constraint_classification(organizational_liability_diffusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZATIONAL LAW DOCTRINE (PITON) — Legal doctrine preserving liability diffusion (corporate separateness, hierarchical privilege) persists through institutional inertia despite evidence that it fails to produce legitimate coordination benefits. The doctrine's theater (complex legal arguments about intent and knowledge) is high relative to its functional verification. Maintains legitimacy through procedural complexity rather than alignment with actual organizational decision flows.
constraint_indexing:constraint_classification(organizational_liability_diffusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, hierarchies necessarily diffuse accountability: any complex organization requires delegation, and delegation inherently creates distance between decision-maker and actor. This perspective treats liability diffusion as an immutable consequence of organizational structure itself. However, the structural data reveals this as a false summit: liability diffusion is contingent on legal doctrine choices (piercing the veil, vicarious liability, documentation standards) that are changeable.
constraint_indexing:constraint_classification(organizational_liability_diffusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_liability_diffusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_liability_diffusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_liability_diffusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_liability_diffusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_liability_diffusion, TR),
    TR >= 0.70.

:- end_tests(organizational_liability_diffusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Senior leadership systematically benefits from hierarchical distance that shields them from liability while frontline workers face concentrated exposure. This is extraction, but it is not absolute — hierarchies do provide coordination benefits (specialization, information processing, decision delegation). The 0.58 value reflects that extraction is real but not maximal (maximum would be 0.75+). Suppression (0.65): High. Multiple mechanisms suppress exit and voice: professional licensing and credential lock-in make internal moves difficult; retaliation risks for whistleblowing are real; organizational power asymmetries prevent upward voice; documentation of orders is often sparse by design. Barriers to exit are substantial. Theater ratio (0.68): High. Legal doctrine around corporate intent, knowledge standards, and hierarchical privilege creates substantial procedural complexity. The theater serves to obscure actual decision flows and delay liability attribution. The ratio has increased over the 20-year measurement interval as organizations have become more complex and legal doctrine more procedurally sophisticated.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a striking perspectival divide. Senior leadership perceives pure coordination (Rope) — the hierarchy enables delegation and work distribution. The reform coalition perceives a solvable temporary problem (Scaffold) — alternative liability standards are being built. The legal doctrine perceives its own degraded function (Piton) — the complexity persists through inertia. Middle managers perceive the mixed reality (Tangled Rope) — genuine coordination with asymmetric extraction. Frontline workers perceive pure extraction (Snare) — they bear liability without control. The analytical observer risks seeing immutable necessity (Mountain) — 'all hierarchies diffuse accountability' — but the structural data reveals contingency: vicarious liability, documentation standards, and transparency reforms are changing the diffusion pattern.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position. Senior leadership with institutional power and arbitrage exit options faces low directionality (d ≈ 0.15-0.20) — the constraint flow runs toward them, they experience negative extraction. Frontline workers with powerless status and trapped exit options face high directionality (d ≈ 0.92) — the constraint flow runs away from them, maximum extraction. Middle managers at moderate power with constrained exit face intermediate directionality (d ≈ 0.55-0.65) — they experience both coordination and extraction. The reform coalition at organized power with constrained exit (but with exit path visibility through regulatory change) faces moderate-low directionality (d ≈ 0.45) — they experience extraction but see it as temporary. The canonical fallback values for institutional power at arbitrage exit would produce d ≈ 0.05-0.15, which accurately captures senior leadership's position as beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint exemplifies how mandatrophy manifests in mixed coordination-extraction systems. The mandatrophy question is 'is the hierarchy primarily coordination or primarily extraction?' The analytical observer's false summit answer is 'both, but the doctrine naturalizes extraction as unavoidable coordination.' The resolution: measurement of actual decision flows through documentation audits, liability outcomes, and comparative analysis with alternative accountability structures shows that (1) coordination benefits of hierarchy could be achieved with more transparent liability attribution, and (2) current diffusion mechanisms extract value from frontline workers disproportionately to coordination needs. The tangled_rope classification holds: genuine coordination function exists, but extraction asymmetries indicate non-justifiable suppression. The scaffold perspective indicates that this imbalance is changeable — regulatory reform is shifting the boundary. No fundamental mandatrophy remains once the contingency of the legal doctrine is recognized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_knowledge_burden,
    'Does the constraint extract value by imposing impossible knowledge/intent verification burdens on liability holders, or does it genuinely reflect the limits of organizational communication?',
    'Analysis of internal communication audit trails: comparison between documented decision hierarchies and liability outcomes. Test: do organizations with complete documentation trails show liability patterns consistent with actual decision flows, or does diffusion persist despite transparent communication?',
    'If impossible burden: constraint is purely extractive (higher snare reading). If genuine reflection: constraint has real coordination function (tangled_rope confirmed). Changes χ by 0.15-0.25.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentionality_knowledge_burden, empirical, 'Whether liability diffusion reflects communication limits or imposed impossible verification burdens').

omega_variable(
    vicarious_liability_substitution,
    'Would expanding vicarious liability standards (making organizations liable for worker actions without intent requirement) eliminate extraction or merely shift it to insurance and compliance theater?',
    'Comparative analysis of jurisdictions with vs without vicarious liability; measurement of compliance cost shifts and whether harm prevention improves or merely liability assignment changes',
    'If elimination: scaffold perspective confirmed and theater_ratio should decline post-reform. If substitution: extraction mechanism persists (moves to insurance sector, compliance documentation). Affects sunset clause validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vicarious_liability_substitution, empirical, 'Whether vicarious liability expands accountability or merely shifts extraction mechanism').

omega_variable(
    hierarchical_necessity_thesis,
    'Is hierarchical diffusion of accountability a necessary feature of all complex organizations, or is it a contingent design choice enabled by specific legal doctrines?',
    'Comparative organizational study: flat organizations, cooperative structures, stakeholder governance models that explicitly reject hierarchical diffusion. Measurement of their accountability clarity and harm outcomes.',
    'If necessary: mountain classification has merit (changes base_extractiveness interpretation). If contingent: false summit confirmed, barrier is doctrinal not structural. Affects mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hierarchical_necessity_thesis, conceptual, 'Whether hierarchical accountability diffusion is structurally necessary or doctrinally contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_liability_diffusion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orgliab_tr_t0, organizational_liability_diffusion, theater_ratio, 0, 0.52).
narrative_ontology:measurement(orgliab_tr_t10, organizational_liability_diffusion, theater_ratio, 10, 0.62).
narrative_ontology:measurement(orgliab_tr_t20, organizational_liability_diffusion, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(orgliab_be_t0, organizational_liability_diffusion, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(orgliab_be_t10, organizational_liability_diffusion, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(orgliab_be_t20, organizational_liability_diffusion, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_liability_diffusion, enforcement_mechanism).
narrative_ontology:affects_constraint(organizational_liability_diffusion, whistleblower_retaliation_suppression).
narrative_ontology:affects_constraint(organizational_liability_diffusion, documentation_asymmetry_in_organizations).
narrative_ontology:affects_constraint(organizational_liability_diffusion, corporate_veil_doctrine).

% DUAL FORMULATION NOTE:
% Organizational liability diffusion is structurally linked to three related constraints: (1) whistleblower retaliation (enforcement suppression mechanism), (2) documentation asymmetry (information control mechanism), and (3) corporate veil doctrine (legal formalization). Each has its own ε value reflecting different observable metrics. The diffusion constraint ε=0.58 reflects extractiveness at the level of accountability structures; the retaliation constraint would show higher ε (0.70+) at the suppression level; the documentation constraint would show its own extractiveness signature reflecting information control. These stories form a constraint family: diffusion is the primary mechanism, retaliation enforces it, documentation conceals it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_liability_diffusion, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
