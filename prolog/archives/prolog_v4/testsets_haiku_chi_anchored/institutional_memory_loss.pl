% ============================================================================
% CONSTRAINT STORY: institutional_memory_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   human_readable: The Amnesiac Organization: Institutional Memory Loss and Constraint Legitimacy Erosion
 *   domain: organizational/political/technological
 *
 * SUMMARY:
 *   Institutional memory loss occurs when an organization systematically
 *   forgets the rationale for its own constraints through rapid personnel
 *   turnover, over-reliance on ephemeral digital communications, and
 *   retirement of tacit knowledge holders. This creates a paradoxical state:
 *   the organization continues to enforce rules it no longer understands,
 *   making those rules simultaneously a coordination mechanism (standardized
 *   processes enable consistency) and an extraction mechanism (knowledge
 *   custodians are forced out, their institutional leverage neutralized by
 *   documentation of their practices). The constraint exhibits Tangled Rope
 *   characteristics at the analytical level: genuine coordination function
 *   (standardization enables scale and reduces decision friction) coexists
 *   with asymmetric extraction (organizational learning becomes proprietary
 *   IP, knowledge custodians lose bargaining power, and the organization
 *   loses adaptive capacity). The theater ratio increases over time
 *   (0.35→0.68) as the organization performs compliance with documented
 *   processes that lack living understanding, creating Goodhart drift:
 *   processes that once served real organizational needs become ritualistic,
 *   enforced not because anyone understands their purpose but because
 *   documentation and auditors require it.
 *
 * KEY AGENTS:
 *   - Knowledge Custodians: Primary victim (powerless/trapped) — hold tacit understanding of constraint rationale; forced into retirement or obsolescence
 *   - Organizational Continuity: Abstract victim (powerless/trapped) — institutional adaptive capacity diminishes as living understanding is replaced by codified procedures
 *   - Process Standardizers: Primary beneficiary (institutional/arbitrage) — external consultants and new executives capture value by codifying practices and eliminating 'legacy constraints'
 *   - Mid-Level Managers: Secondary victim (moderate/constrained) — must enforce rules they don't fully understand; constrained by documentation but unable to adapt when procedures fail
 *   - Organizational Learning Advocates: Organized agents (organized/constrained) — technologists building knowledge management systems see memory loss as a transitional problem with a sunset (documentation + AI will replace tacit knowledge)
 *   - Compliance Bureaucracy: Institutional actor (institutional/arbitrage) — auditors and regulators enforce documented procedures; maintains constraint through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_memory_loss, 0.58).
domain_priors:suppression_score(institutional_memory_loss, 0.62).
domain_priors:theater_ratio(institutional_memory_loss, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_memory_loss, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_memory_loss, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_memory_loss, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_memory_loss, tangled_rope).
narrative_ontology:human_readable(institutional_memory_loss, "The Amnesiac Organization: Institutional Memory Loss and Constraint Legitimacy Erosion").
narrative_ontology:topic_domain(institutional_memory_loss, "organizational/political/technological").

domain_priors:requires_active_enforcement(institutional_memory_loss).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_memory_loss, new_decision_makers).
narrative_ontology:constraint_beneficiary(institutional_memory_loss, external_auditors).
narrative_ontology:constraint_beneficiary(institutional_memory_loss, process_standardizers).
narrative_ontology:constraint_victim(institutional_memory_loss, institutional_continuity).
narrative_ontology:constraint_victim(institutional_memory_loss, knowledge_custodians).
narrative_ontology:constraint_victim(institutional_memory_loss, organizational_adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KNOWLEDGE CUSTODIAN (SNARE) — Long-tenured employee who holds tacit understanding of why constraints exist. Cannot exit without destroying institutional memory. Faces forced retirement or role obsolescence as org adopts 'standard processes.' d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.65.
constraint_indexing:constraint_classification(institutional_memory_loss, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MID-LEVEL MANAGER (TANGLED ROPE) — Enforces standardized processes (coordination benefit) but lacks the institutional memory to explain why those processes exist. Benefits from clarity of rules; harmed by inability to adapt when rule breaks fail. d≈0.58, f(d)≈0.75, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(institutional_memory_loss, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROCESS STANDARDIZER (ROPE) — External consultant or new executive who codifies 'best practices' and eliminates 'legacy constraints.' Experiences the transition as coordination: standardization enables scale and reduces decision friction. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(institutional_memory_loss, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZATIONAL LEARNING ADVOCATES (SCAFFOLD) — Technologists and organizational development specialists see memory loss as a transitional problem solvable by knowledge capture systems, documentation protocols, and AI-augmented institutional memory. Sunset clause: as knowledge management systems mature, explicit documentation replaces tacit knowledge retention. d≈0.35, f(d)≈0.38, σ=1.2 → χ≈0.16.
constraint_indexing:constraint_classification(institutional_memory_loss, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLIANCE BUREAUCRACY (PITON) — Auditors and regulators enforce documented procedures that may have lost their original justification. Theater_ratio=0.68 reflects performative compliance: the organization demonstrates adherence to documented processes that nobody fully understands or remembers why they exist. Inertia maintains the constraint despite functional degradation.
constraint_indexing:constraint_classification(institutional_memory_loss, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, institutional memory loss appears as both a coordination mechanism (standardization enables scale) and an extraction mechanism (organizational learning becomes proprietary IP, knowledge custodians lose bargaining power). The constraint itself is the forgetting: the organization loses the adaptive capacity to evaluate when its own rules should change. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(institutional_memory_loss, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.58): Moderate-high and rising. The organization initially captures value through standardization (legitimate coordination benefit: 0.28). But as personnel turnover accelerates and tacit knowledge is lost, the extractiveness increases (0.58) because the constraint now persists despite reduced functionality. The 'extraction' is diffuse: consultant fees, IP licensing, opportunity cost of knowledge custodian departure, and the org's own loss of adaptive capacity. Suppression (0.62): Moderate-high. Knowledge custodians are suppressed through forced retirement policies, documentation systems that displace tacit knowledge, and promotion of process standardizers who explicitly devalue 'legacy thinking.' New hires cannot access the tacit knowledge that would allow them to question inherited constraints. Alternatives to standardization (maintaining knowledge custodian networks, investing in mentorship systems) are systematically suppressed in favor of efficiency gains. Theater ratio (0.68): High and rising. Compliance with documented procedures increasingly becomes performative. The organization demonstrates adherence to rules that nobody fully understands, satisfying auditors while operational effectiveness declines. The rise from 0.35→0.68 indicates Goodhart drift: the documented procedure becomes the goal, displacing the original purpose.
 *
 * PERSPECTIVAL GAP:
 *   Knowledge custodians see Snare (d≈0.92) because they are trapped: their tacit knowledge is irreplaceable yet systematically devalued. Mid-level managers see Tangled Rope (d≈0.58) because standardization provides clarity (coordination) but undermines adaptive capacity (extraction). Process standardizers see Rope (d≈0.08) because they are solving a real coordination problem — ambiguous legacy processes do create inefficiency. The scaffold perspective sees a solvable transitional problem: if knowledge management systems work, the constraint resolves. The piton perspective observes the performance of compliance. The analytical observer sees Tangled Rope (d≈0.50) because the organization is extracting value from institutional knowledge while genuinely coordinating on standardization. The perspectival gap reflects that the same constraint (memory loss) is experienced as entrapment (custodian), compliance burden (manager), efficiency gain (standardizer), solvability (technology optimist), ritual (auditor), and paradoxical coordination-extraction (analyst).
 *
 * DIRECTIONALITY LOGIC:
 *   Knowledge custodians: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. They cannot walk away without destroying institutional memory, and their power is systematically neutralized by documentation systems. Organizational continuity: Victim + trapped → d≈0.92 (same as custodians — the org is its own victim). Process standardizers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. They can easily exit the engagement (consulting contract ends) and benefit from the work (IP, fees, reputation). Mid-level managers: Victim + constrained → d≈0.58, f(d)≈0.75. They face significant extraction (loss of adaptive capacity, forced compliance with rules they don't understand) but retain some ability to work around constraints. Organizational learning advocates: Organized + constrained → d≈0.35, f(d)≈0.38. They see agency and a path forward (technology can solve memory loss) but are constrained by adoption timelines and organizational resistance. Compliance bureaucracy: Institutional + arbitrage → d≈0.08 (but theater_ratio gate drives Piton classification, not directionality). Analytical observer: Neither pure beneficiary nor victim; observes the mixed coordination-extraction structure → d≈0.50, f(d)≈0.65.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing institutional memory loss as GENUINELY tangled: the constraint has a real coordination function (standardization enables consistency and scale) but that function is increasingly embedded in an extraction mechanism (knowledge custodians are neutralized, the org loses adaptive capacity, and value is captured by external standardizers). The danger of mislabeling it as pure coordination (Rope) is that it would naturalize the loss of institutional knowledge as an efficiency gain, when in fact the organization is becoming increasingly fragile. The danger of mislabeling it as pure extraction (Snare) is that it would dismiss the real benefits of standardization and consistency. The Tangled Rope classification captures the truth: coordination and extraction are both structurally present, growing in tension. The rising theater_ratio (0.35→0.68) indicates that the coordination function is being displaced by performance of compliance — organizational learning advocates' scaffold vision is the resolution pathway. If knowledge management systems successfully capture tacit knowledge, the constraint becomes pure coordination (Rope); the constraint resolves. If they fail, the constraint becomes pure extraction (Snare) as the org relies on increasingly brittle documented procedures with no one understanding why they exist. The analytical observer's Tangled Rope classification is stable across both outcome scenarios — it is the structural reality independent of technological optimism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_recovery_feasibility,
    'Can explicit documentation and AI-augmented knowledge capture systems genuinely replace tacit understanding that accumulated over decades?',
    'Empirical tracking of documented process efficacy vs tacit-knowledge outcomes; comparison of decision quality in operations led by knowledge custodians vs standardized procedures; identification of failure modes unique to each approach',
    'If documentation fully replaces tacit knowledge: scaffold sunset is real, constraint resolves to Rope. If tacit knowledge remains irreplaceable: organizational learning is aspirational, constraint remains Tangled Rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_recovery_feasibility, empirical, 'Whether documented processes can replace tacit institutional knowledge').

omega_variable(
    constraint_legitimacy_deterioration_rate,
    'Do standardized constraints actually lose legitimacy and adaptive capacity faster when their historical rationale is forgotten, or does the org remain functional despite the loss?',
    'Longitudinal analysis of constraint violation rates, adaptive failures, and crisis response times before and after institutional memory loss; correlation between knowledge custodian departure and organizational error rates',
    'If legitimacy deteriorates rapidly: memory loss is an extraction mechanism (Snare). If org remains functional: memory loss is coordination theater with sunk costs (Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constraint_legitimacy_deterioration_rate, empirical, 'Rate of constraint degradation after institutional memory loss').

omega_variable(
    knowledge_custodian_exit_asymmetry,
    'Is the powerless classification of knowledge custodians justified, or do they retain significant structural power through irreplaceability?',
    'Observational study of knowledge custodian leverage in retention negotiations; tracking of org crisis response times with and without custodian availability; identification of constraints that become unmaintainable post-retirement',
    'If custodians have high leverage: exit_options should be constrained or mobile (not trapped), shifting classification upward. If truly powerless: Snare classification confirmed, suggesting org is willing to suffer dysfunction to avoid knowledge-dependent personnel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_custodian_exit_asymmetry, empirical, 'Structural power retention of irreplaceable knowledge custodians').

omega_variable(
    standardization_extraction_mechanism,
    'Is standardization (process codification) primarily a coordination mechanism or a vehicle for extracting value from institutional knowledge into intellectual property and consultant fees?',
    'Financial tracking of consulting contracts, IP licensing, and process-improvement revenue during standardization periods; comparison of org efficiency gains to external value capture; analysis of whether codified processes are reusable across contexts or proprietary',
    'If extraction-dominant: beneficiary should be re-classified as high-extraction actor; χ increases. If coordination-dominant: beneficiary remains Rope; constraint softens to pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standardization_extraction_mechanism, empirical, 'Whether standardization extracts value beyond coordination benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_memory_loss, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iml_tr_t0, institutional_memory_loss, theater_ratio, 0, 0.35).
narrative_ontology:measurement(iml_tr_t5, institutional_memory_loss, theater_ratio, 5, 0.52).
narrative_ontology:measurement(iml_tr_t10, institutional_memory_loss, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(iml_be_t0, institutional_memory_loss, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(iml_be_t5, institutional_memory_loss, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(iml_be_t10, institutional_memory_loss, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_memory_loss, resource_allocation).
narrative_ontology:affects_constraint(institutional_memory_loss, organizational_brittle_recovery).
narrative_ontology:affects_constraint(institutional_memory_loss, knowledge_worker_bargaining_power).

% DUAL FORMULATION NOTE:
% Institutional memory loss is downstream of two distinct phenomena: (1) the deliberate adoption of rapid turnover as a cost-control strategy (ε=0.72, Snare-from-worker-perspective), and (2) the technological substitution of tacit knowledge with documentation systems (ε=0.35, Rope-from-standardizer-perspective). The constraint story here (ε=0.58, Tangled Rope) represents the actual outcome when both forces are present: neither pure extraction nor pure coordination, but an unstable hybrid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_memory_loss, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
