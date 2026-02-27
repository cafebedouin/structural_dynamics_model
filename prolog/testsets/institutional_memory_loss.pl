% ============================================================================
% CONSTRAINT STORY: institutional_memory_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_memory_loss, []).

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
 *   constraint_id: institutional_memory_loss
 *   human_readable: The Amnesiac Organization: Institutional Memory Loss
 *   domain: organizational/political/technological
 *
 * SUMMARY:
 *   Organizations undergoing rapid growth, leadership turnover, or cultural
 *   transitions often experience institutional memory loss: the documented
 *   'why' behind internal constraints disappears along with the people who
 *   understood them. Senior staff retire or leave, taking tacit knowledge of
 *   unwritten rules, historical decision rationales, and subtle procedural
 *   reasoning. New cohorts inherit procedures without context, leading to
 *   either cargo-cult enforcement of incomprehensible rules or casual
 *   abandonment of safeguards whose purpose has been forgotten. The
 *   constraint exhibits all six DR types depending on observational position.
 *   From the retiring knowledge holder's view, it is pure extraction (snare):
 *   their cognitive labor has been absorbed without transfer. From new
 *   leadership's perspective, it is coordination (rope): rapid
 *   decision-making without historical friction. From the institutional
 *   system's view, it is partially performative (piton): onboarding persists
 *   but lacks function. From a knowledge management initiative's view, it is
 *   a temporary problem with a sunset (scaffold). From mid-career continuity
 *   agents, it is a hybrid (tangled rope): both constraining and enabling.
 *   From a civilizational analysis, it might appear immutable (mountain) —
 *   but this naturalization conceals organizational choices about whether to
 *   invest in documentation, mentorship, and knowledge transfer. The
 *   constraint's theater ratio has risen significantly (0.52 → 0.81) as
 *   formal procedures become increasingly divorced from their functional
 *   rationales. Extractiveness has also risen (0.28 → 0.52) as the new
 *   leadership cohort captures more benefit from rapid iteration without
 *   institutional drag.
 *
 * KEY AGENTS:
 *   - Retiring Knowledge Holders: Primary victims (powerless/trapped) — departing with undocumented expertise; bear full cost of post-exit institutional breakdown
 *   - New Leadership Cohort: Primary beneficiaries (organized/arbitrage) — benefit from rapid decision-making freed from historical constraints; can reimplement without justification to predecessors
 *   - Mid-Career Continuity Agents: Secondary victims (moderate/constrained) — caught between stability requirement and leadership pressure; experience mixed extraction and coordination
 *   - HR/Onboarding System: Institutional actor (institutional/constrained) — maintains formal knowledge transfer processes but lacks resourcing; theater is high, function degraded
 *   - Knowledge Management Initiative: Organized challenger (powerful/mobile) — sees memory loss as temporary coordination problem with explicit sunset; building documentation and transfer mechanisms
 *   - Institutional Coherence (abstract): Primary victim (powerless/trapped) — abstract collective good; no advocate; cannot organize; bears full cost of cascade failures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_memory_loss, 0.52).
domain_priors:suppression_score(institutional_memory_loss, 0.68).
domain_priors:theater_ratio(institutional_memory_loss, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_memory_loss, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_memory_loss, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_memory_loss, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_memory_loss, tangled_rope).
narrative_ontology:human_readable(institutional_memory_loss, "The Amnesiac Organization: Institutional Memory Loss").
narrative_ontology:topic_domain(institutional_memory_loss, "organizational/political/technological").

domain_priors:requires_active_enforcement(institutional_memory_loss).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_memory_loss, new_leadership_cohort).
narrative_ontology:constraint_beneficiary(institutional_memory_loss, rapid_iteration_culture).
narrative_ontology:constraint_victim(institutional_memory_loss, institutional_coherence).
narrative_ontology:constraint_victim(institutional_memory_loss, long_term_continuity_agents).
narrative_ontology:constraint_victim(institutional_memory_loss, epistemic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETIRING KNOWLEDGE HOLDER (SNARE) — Senior staff departing with undocumented tacit knowledge cannot escape the constraint. Their cognitive labor has been extracted (absorbed into institutional culture) without transfer mechanisms. Successors neither know what they don't know nor why certain procedures exist. The exiting agent bears the cost of institutional breakdown post-departure; the institution captures the short-term productivity gain from their work while losing the knowledge upon exit. Suppression is total: no mechanism exists to extract, codify, or transfer tacit knowledge. No alternative pathway available.
constraint_indexing:constraint_classification(institutional_memory_loss, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER CONTINUITY AGENT (TANGLED ROPE) — Caught between organizational inertia and leadership pressure for rapid change. Constrained by career risk (documenting 'outdated' procedures marks one as resistant to progress). Also benefits from institutional stability (their role depends on continuity). Experiences both extraction (pressure to move fast despite knowledge gaps) and coordination (their institutional knowledge is genuinely valuable to the organization's function).
constraint_indexing:constraint_classification(institutional_memory_loss, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NEW LEADERSHIP COHORT (ROPE) — Benefits from institutional memory loss in the short term. Rapid turnover enables reimplementation without justification to predecessors. 'We've always done X' is gone; now decisions can be framed as forward-looking. Leadership sees the constraint as a coordination mechanism: accelerating adoption of new tools, metrics, and strategies without nostalgic friction. High exit options (arbitrage) — they can move to new roles, companies, or projects if this organization's culture becomes too rigid. Extracted experience: none. They capture the upside.
constraint_indexing:constraint_classification(institutional_memory_loss, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HR/ONBOARDING SYSTEM (PITON) — Formal onboarding processes persist (documents, checklists, orientation modules) but are increasingly theatrical. New employees receive procedural checklists but not the reasoning behind them. Mentorship is performed but not resourced (few senior staff have time to mentor; rapid promotion of junior staff reduces mentoring pool). The system maintains the appearance of knowledge transfer while actual transfer has atrophied. Theater ratio is high because the forms are followed but the function has degraded.
constraint_indexing:constraint_classification(institutional_memory_loss, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: KNOWLEDGE MANAGEMENT INITIATIVE (SCAFFOLD) — An organized effort (internal wiki, documentation mandates, video recording of expertise) sees institutional memory loss as a temporary coordination problem with explicit sunset logic. KM initiatives frame the constraint as solvable: accumulate tacit knowledge into explicit repositories before key staff leave, establish documentation standards, create knowledge transfer bonuses. This perspective has agency and a clear exit path — as systems mature and culture shifts toward documentation, the memory loss constraint loosens. Theater is moderate because the initiative has structural accountability but faces organizational resistance.
constraint_indexing:constraint_classification(institutional_memory_loss, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a deep structural perspective, all organizations have memory loss as a limiting property: human cognitive capacity is finite, attention is scarce, and embodied knowledge dies with its bearer. No institution can preserve all tacit knowledge. This is an inherent constraint on organizational coherence — a natural law of institutional systems. However, this perspective risks naturalizing what is actually a contingent choice: whether to invest in knowledge transfer, documentation, and mentorship is organizational policy, not physics. The mountain classification may be a false summit revealing organizational abdication of continuity as 'natural law.'
constraint_indexing:constraint_classification(institutional_memory_loss, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_memory_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_memory_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_memory_loss, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_memory_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_memory_loss, TR),
    TR >= 0.70.

:- end_tests(institutional_memory_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint extracts cognitive labor from retiring staff without transfer mechanisms, but also creates real efficiency gains for new leadership in the short term through reduced friction. The score reflects net asymmetry: the knowledge is extracted (moved into institutional culture) and then lost when the bearer leaves, while the organization captured productivity gains during their tenure. Suppression (0.68): High. Barriers to knowledge transfer include: (1) organizational speed culture that devalues documentation as 'overhead,' (2) career incentives that reward forward motion over mentorship, (3) tacit nature of some knowledge (hard to articulate), (4) rapid promotion of junior staff that shrinks the mentoring pool, (5) rationalization that 'institutional memory limits innovation.' Not total suppression because knowledge management initiatives exist and some documentation occurs; but structural suppressors are strong. Theater ratio (0.81): High and rising. Onboarding checklists and orientation modules persist, but actual knowledge transfer has atrophied. Mentorship is assigned but not resourced. Documentation is mandated but not comprehensive. The formal procedures are followed (theater preserved) while their function (actual knowledge transfer) has degraded. The rise from 0.52 to 0.81 reflects increasing divergence between the appearance of knowledge transfer and its actual functioning.
 *
 * PERSPECTIVAL GAP:
 *   The retiring knowledge holder and new leadership cohort occupy opposite structural positions relative to the same constraint. For the departing senior: the constraint is a snare — they must transfer knowledge they spent decades acquiring, but the organization provides no mechanism or incentive for transfer, and upon leaving they lose all institutional capital from that knowledge. For new leadership: the constraint is a rope — they coordinate faster without historical friction, legacy systems no longer impede decision-making, and they can reimplement everything de novo according to their values. Same constraint, opposite classifications. Mid-career continuity agents see tangled rope because they benefit from institutional stability (their role depends on continuity) but are constrained by pressure to move faster than the organization can coherently sustain. The knowledge management initiative sees scaffold: a real temporary coordination problem being solved through explicit mechanisms (wikis, video recording, documentation standards) with sunset logic as culture shifts toward valuing preservation. The piton classification (HR/onboarding system) reveals that the formal machinery of knowledge transfer persists while its function has degraded — the system is inertial, maintained by habit rather than efficacy. The mountain classification at the analytical level risks naturalizing what is actually a series of organizational choices (whether to resource mentorship, whether to hire for continuity vs disruption, whether to value documented knowledge vs speed).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position within the knowledge flow. Retiring knowledge holders are trapped (no exit from the organization that preserves their institutional contribution) and their knowledge is extracted (absorbed by the institution) without compensation or transfer. The engine derives high d → high f(d) → high experienced extraction for this agent. New leadership has arbitrage options (can move to new roles/companies) and benefits from reduced institutional constraint; engine derives low d → negative χ. Mid-career continuity agents are constrained (cannot easily exit without abandoning their career investment) and experience both extraction (pressure to move fast despite knowledge gaps) and coordination (their knowledge is needed); moderate d → moderate χ. The knowledge management initiative has powerful status and mobile exit (can declare victory and move to next project); lower d → lower experienced extraction. Institutional coherence as a beneficiary group is trapped with no exit options; engine derives maximum d → maximum f(d). The piton classification derives from high theater ratio (0.81) indicating performative maintenance without functional content, not from extraction magnitude.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CANDIDATE: The constraint exhibits characteristics of natural law (immutable organizational limit) but is actually contingent institutional policy. The false summit emerges from conflating 'all organizations lose some memory' (true natural law) with 'this organization's memory loss rate and consequences are inevitable' (false). The analytical perspective's mountain classification is perspectival naturalization: it assumes that because human cognition is finite and attention is scarce, memory loss must be immutable. But memory loss magnitude depends on turnover rate, documentation investment, mentorship resources, and cultural valuation of continuity — all policy choices, not physics. Resolving the mandatrophy requires identifying that the constraint is NOT a mountain (natural limit on organizational coherence) but a tangled rope (coordination problem with asymmetric extraction costs). The organization IS solving the coordination problem through knowledge management initiatives, but doing so slowly and unevenly — benefiting new leadership while extracting from continuity agents. The mandatrophy is resolved by rejecting the naturalized mountain classification and accepting that institutional memory loss is a structural feature of organizational choice, not a law of organizational physics. The constraint can be remade through deliberate policy: prioritizing documentation, resourcing mentorship, slowing turnover, or creating incentives for knowledge transfer. The fact that these are not currently prioritized reveals that new leadership's interests (arbitrage, rapid iteration) currently dominate organizational choice, not organizational necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_criticality_threshold,
    'What fraction of tacit knowledge loss triggers institutional cascade failure (loss of organizational coherence beyond recovery)?',
    'Comparative study of organizations with varying knowledge retention rates; correlation between tacit knowledge loss percentage and institutional recovery time post-crisis',
    'If threshold < 30%: memory loss is a snare even at moderate turnover rates. If threshold > 70%: organizations have significant buffering capacity and constraint is rope-like (coordination problem). Determines whether memory loss is endemic extraction or manageable coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_criticality_threshold, empirical, 'Threshold at which knowledge loss triggers institutional cascade failure').

omega_variable(
    documentation_effectiveness,
    'Can documented procedures, wikis, and video recordings actually capture and transfer tacit knowledge, or is the transferability itself the hidden constraint?',
    'Experimental comparison: organizations with high documentation standards vs low documentation; measurement of error rates, rework cycles, and institutional coherence post-transfer',
    'If documentation effective: scaffold perspective is real and knowledge management provides genuine exit path. If ineffective: tacit knowledge is fundamentally non-transferable and the constraint is immutable (mountain). Mandatrophy resolution depends on this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(documentation_effectiveness, empirical, 'Whether documented procedures can effectively transfer tacit knowledge').

omega_variable(
    turnover_rate_causality,
    'Does rapid turnover cause memory loss, or does organizational forgetting drive out continuity-oriented staff (reverse causation)?',
    'Longitudinal analysis of turnover timing vs knowledge retention loss; exit interviews distinguishing voluntary departure vs forced/cultural pressure',
    'If turnover is cause: constraint is structural (hiring/promotion policy) and removable through staffing decisions. If forgetting drives departures: constraint is cultural and feedback-reinforcing. Affects whether new leadership benefits are real or illusory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(turnover_rate_causality, empirical, 'Whether turnover causes memory loss or memory loss drives turnover').

omega_variable(
    institutional_coherence_observability,
    'How is institutional coherence measured, and does the measurement itself change what counts as ''coherence''?',
    'Comparative analysis of implicit vs explicit coherence metrics; whether organizations optimizing for measurable coherence (e.g., process compliance) lose unmeasurable coherence (e.g., strategic continuity)',
    'If measurement defines the concept: organizations can optimize away memory loss by redefining coherence to exclude hard-to-measure dimensions. Reveals whether the constraint is structural or perspectival (artifact of how we evaluate institutions).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_coherence_observability, conceptual, 'Whether institutional coherence is measurable or measurement-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_memory_loss, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imem_tr_t0, institutional_memory_loss, theater_ratio, 0, 0.52).
narrative_ontology:measurement(imem_tr_t5, institutional_memory_loss, theater_ratio, 5, 0.68).
narrative_ontology:measurement(imem_tr_t10, institutional_memory_loss, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(imem_be_t0, institutional_memory_loss, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(imem_be_t5, institutional_memory_loss, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(imem_be_t10, institutional_memory_loss, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_memory_loss, information_standard).
narrative_ontology:affects_constraint(institutional_memory_loss, regulatory_compliance_drift).
narrative_ontology:affects_constraint(institutional_memory_loss, organizational_coherence_failure).
narrative_ontology:affects_constraint(institutional_memory_loss, institutional_legitimacy_erosion).

% DUAL FORMULATION NOTE:
% Institutional memory loss is upstream of specific policy failures (regulatory drift, compliance loss) and downstream of organizational design choices (turnover rates, documentation investment). It can be decomposed into separate stories for knowledge transfer mechanisms (higher epsilon) vs. organizational coherence (lower epsilon) vs. specific procedural failures (varies by domain). This story addresses the generic constraint; domain-specific instantiations would have distinct epsilon values reflecting empirical evidence for that domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_memory_loss, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
