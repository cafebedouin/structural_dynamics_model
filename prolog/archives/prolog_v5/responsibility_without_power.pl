% ============================================================================
% CONSTRAINT STORY: responsibility_without_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_responsibility_without_power, []).

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
 *   constraint_id: responsibility_without_power
 *   human_readable: The Scapegoat Architecture: Responsibility Without Power
 *   domain: organizational/legal/socio-economic
 *
 * SUMMARY:
 *   The scapegoat architecture is a structural pattern in which legal or
 *   moral responsibility for maintaining system safety, ethical standards, or
 *   compliance is formally assigned to an agent (individual, middle manager,
 *   compliance officer, operational implementer) who lacks the actual power,
 *   information, or authority to control the outcome. The responsible party
 *   is selected for proximity to the failure point rather than causal
 *   authority over the decision-making process. This constraint appears as a
 *   coordination mechanism (Rope) to the authority holder who uses it to
 *   maintain governance clarity without accountability risk. It appears as a
 *   mixed coordination-extraction hybrid (Tangled Rope) to the middle manager
 *   who receives both genuine operational responsibility and artificial
 *   liability for decisions made above them. It appears as pure extraction
 *   (Snare) to the designated scapegoat who bears full reputational and
 *   financial cost while lacking control mechanisms. It appears as a
 *   temporary institutional arrangement with a sunset path (Scaffold) to
 *   organized reform movements building upstream liability doctrines. It
 *   appears as a degraded ritual maintained through inertia (Piton) to
 *   institutional observers who recognize the performative nature of
 *   individual accountability divorced from actual power. At the
 *   civilizational analytical level, there is risk of falsely naturalizing
 *   this as an inherent feature of bureaucracy (Mountain), when structural
 *   analysis reveals it as an active enforcement mechanism.
 *
 * KEY AGENTS:
 *   - Designated Scapegoat (powerless/trapped): Individual assigned liability for outcomes beyond their control — loan officer blamed for algorithmic bias, middle manager held accountable for executive directives, compliance officer responsible for vendor conduct
 *   - Authority Holder (institutional/arbitrage): Executive, regulator, or system operator who benefits from plausible deniability while maintaining governance clarity
 *   - Middle Management Layer (moderate/constrained): Occupies intermediate position receiving both genuine operational responsibility and artificial liability for system-level failures
 *   - Organizational Resilience (powerless/trapped): Collective good that cannot exit or organize; bears cost of false accountability that diverts blame from root causes
 *   - Regulatory Reform Coalition (organized/mobile): Labor unions, professional guilds, civil rights advocates, compliance councils building upstream liability doctrines
 *   - Institutional Review Structures (institutional/arbitrage): Performance management systems, compliance certifications, legal liability frameworks that maintain the ritual
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(responsibility_without_power, 0.52).
domain_priors:suppression_score(responsibility_without_power, 0.68).
domain_priors:theater_ratio(responsibility_without_power, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(responsibility_without_power, extractiveness, 0.52).
narrative_ontology:constraint_metric(responsibility_without_power, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(responsibility_without_power, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(responsibility_without_power, tangled_rope).
narrative_ontology:human_readable(responsibility_without_power, "The Scapegoat Architecture: Responsibility Without Power").
narrative_ontology:topic_domain(responsibility_without_power, "organizational/legal/socio-economic").

domain_priors:requires_active_enforcement(responsibility_without_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(responsibility_without_power, system_operators).
narrative_ontology:constraint_beneficiary(responsibility_without_power, authority_holders).
narrative_ontology:constraint_victim(responsibility_without_power, designated_responsible_parties).
narrative_ontology:constraint_victim(responsibility_without_power, organizational_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESIGNATED SCAPEGOAT (SNARE) — Individual assigned liability for outcomes they cannot control (e.g., loan officer blamed for underwriting failures driven by algorithmic bias, middle manager held accountable for execution of directives from above, supply chain compliance officer responsible for supplier conduct beyond audit scope). Trapped by employment or legal obligation; lacks authority to prevent failures; bears full reputational and financial cost of system errors. Maximum experienced extraction — no exit without catastrophic career cost.
constraint_indexing:constraint_classification(responsibility_without_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE MANAGEMENT LAYER (TANGLED ROPE) — Receives both coordination benefits (clarity of performance expectations, access to resources within constraint) and extraction costs (held liable for outcomes determined by executive strategy, budget constraints, or market conditions beyond their control). Constrained by organizational hierarchy; can negotiate within bounds but cannot exit the reporting relationship. Mixed experience: some real responsibility (local operational decisions) layered with artificial liability (system-level outcomes).
constraint_indexing:constraint_classification(responsibility_without_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AUTHORITY HOLDER (ROPE) — System operator (executive, regulator, platform operator) benefits from the scapegoat architecture by maintaining plausible deniability: the designated responsible party absorbs blame and legal liability while the actual decision-maker retains strategic autonomy. Experiences the constraint as pure coordination: assigning clear responsibility (whether earned or not) reduces ambiguity and enables governance. Can exit at will (reassign the responsibility) with minimal cost. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(responsibility_without_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Organized agents (labor unions, professional guilds, civil rights advocates, compliance councils) recognize the scapegoat architecture as a temporary institutional feature rather than a natural law. These coalitions are building sunset mechanisms: liability caps for individuals acting under institutional constraint, duty-to-delegate doctrines that shift liability upstream to decision-makers, algorithmic auditability requirements that make invisible decision-drivers auditable. High agency and clear exit path (norms change, doctrines shift). Theater_ratio is lower here because reform movements work to expose the performative nature of individual liability when power is absent.
constraint_indexing:constraint_classification(responsibility_without_power, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SCAPEGOAT RITUAL / INSTITUTIONAL INERTIA (PITON) — The formal architecture of personal responsibility persists through theater and ritual long after its functional rationale has eroded. Performance management systems, compliance certifications, and individual liability doctrines are maintained because they create the appearance of accountability, not because they reliably produce safe outcomes. The ritual persists due to institutional inertia, path dependence, and the lack of coordinated replacement structures. Observers from inside the institution see the mechanism as increasingly hollow — maintained out of habit and legal precedent rather than actual control function.
constraint_indexing:constraint_classification(responsibility_without_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing the scapegoat architecture as an inevitable feature of organizational hierarchy: 'Someone must be held responsible; it is natural that that person occupies a middle position.' This perspective treats the responsibility-power gap as inherent to bureaucracy itself — a law of organizational nature. However, the structural data contradicts this: the gap is maintained through active enforcement (contract clauses, performance metrics, legal doctrine) and is explicitly contingent on institutional design choices. Engine false summit detection will flag this as naturalization of a contingent extraction mechanism.
constraint_indexing:constraint_classification(responsibility_without_power, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_without_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(responsibility_without_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_without_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(responsibility_without_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(responsibility_without_power, TR),
    TR >= 0.70.

:- end_tests(responsibility_without_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The authority holder captures substantial benefit (risk transfer, governance clarity without accountability cost) while the scapegoat bears substantial cost (reputational damage, career risk, legal liability). However, the extraction is not total (0.70+) because some aspects are legitimate: the middle manager does exercise real operational authority and does bear appropriate responsibility for local decisions. The artificial component is the expansion of liability to system-level outcomes outside their control. The increasing trajectory (0.35→0.44→0.52) reflects organizational complexity growth — more distributed systems create more opacity, expanding the domain in which responsibility can be assigned without corresponding power. Suppression (0.68): High. Multiple barriers maintain the architecture: contractual assignment of liability, performance metrics that don't distinguish controllable from uncontrollable factors, legal doctrines that permit institutional liability to be pushed down to individuals, information asymmetries that prevent the scapegoat from understanding all relevant decision-making, and career risk that creates strong incentive to comply with the responsibility assignment rather than challenge it. Theater ratio (0.65): Moderate-high and rising. The performance management systems, compliance certifications, and individual accountability measures increasingly operate as theater — maintaining the appearance of accountability without reliably producing safer outcomes. The rising trajectory (0.45→0.58→0.65) reflects organizational emphasis shifting toward visible compliance rituals rather than root-cause control as systems become more complex.
 *
 * PERSPECTIVAL GAP:
 *   The scapegoat architecture generates a maximum perspectival gap: the authority holder sees coordination (low-cost clarity), the scapegoat sees extraction (high-cost liability for uncontrollable outcomes), and the analytical observer risks seeing a natural law (hierarchy must have accountability) when in fact it is a contingent institutional choice. The gap is not rooted in disagreement about facts but in fundamentally different structural positions: the authority holder experiences the constraint as enabling their governance; the scapegoat experiences it as constraining their career; the middle manager experiences both simultaneously. The piton classification reveals that institutional observers recognize the mechanism as increasingly hollow — the performance metrics and compliance rituals no longer reliably predict safer outcomes because they measure individual accountability divorced from actual control. The scaffold perspective identifies a real institutional feature: regulatory reform movements (duty-to-delegate doctrines, institutional negligence standards) are building alternative frameworks that shift liability upstream to decision-makers with actual power, and these reforms have a visible sunset timeline (5-10 years for doctrinal shifts in leading jurisdictions).
 *
 * DIRECTIONALITY LOGIC:
 *   The scapegoat's directionality (d ≈ 0.92) is derived from: victim status (bears full cost of failures they don't control) + trapped exit (cannot leave without catastrophic career cost). This produces maximum f(d) ≈ 1.4, driving high effective extraction chi. The authority holder's directionality (d ≈ 0.05) is derived from: beneficiary status (captures risk transfer and governance clarity) + arbitrage exit (can reassign responsibility with minimal cost). This produces negative f(d) ≈ -0.12, reducing effective extraction from their perspective to negative χ — they experience the constraint as low-cost coordination. The middle manager's directionality (d ≈ 0.55) is intermediate: partial victim (held liable for decisions above them) + constrained exit (cannot fully escape the organizational hierarchy but can negotiate bounds). This produces f(d) ≈ 0.75, producing moderate χ experience. The reform coalition's directionality (d ≈ 0.50) reflects mobile exit (can exit the current institutional form through norm change) + mixture of victim and beneficiary roles (victims of the status quo, but beneficiaries of clearer legal doctrines once reform succeeds). No directionality overrides are needed — the structural derivation captures the perspectival gaps accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY: The scapegoat architecture must be classified as Tangled Rope, not pure Snare, because it possesses a genuine coordination function alongside the extraction mechanism. The coordination function is real: assigning clear responsibility to a particular agent does reduce ambiguity and enable governance. The error would be to classify it as a Snare (pure extraction) on the grounds that the responsibility-power gap makes the arrangement unfair. Fairness is not the classification criterion; the presence or absence of a genuine coordination function is. The scapegoat architecture provides coordination (clear responsibility lines, explicit accountability chains) for the organization that employs it. That it extracts unfairly is secondary to the presence of coordination function. The classification as Tangled Rope reflects this duality: legitimate coordination benefit + asymmetric extraction cost. The mandatrophy resolution also clarifies the piton perspective: institutional actors see the mechanism as increasingly theatrical (theater_ratio rising to 0.65+) because the coordination function is not keeping pace with system complexity — responsibility assignments no longer clarify actual decision-making chains when systems are opaque and distributed. The ritual persists despite degraded functionality because the extraction benefit to authority holders is sustained. This is the classical piton signature: a former coordination mechanism (medieval hierarchies, 20th-century bureaucracy) where the primary function has atrophied but the extraction remains institutionalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liability_attribution_boundary,
    'At what level of algorithmic opacity or system complexity does individual responsibility become structurally impossible to exercise?',
    'Causal chain analysis: tracing decisions from authority holder through system architecture to designated responsible party; identification of opacity barriers that prevent actual control',
    'If boundary is clearly identifiable: liability can be reassigned upstream to decision-makers with actual control. If boundary is fuzzy: the scapegoat architecture persists by claiming all individuals have ''some agency'' and therefore bear ''some liability.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_attribution_boundary, empirical, 'Boundary at which individual responsibility becomes impossible to exercise').

omega_variable(
    exit_cost_asymmetry,
    'How much do exit costs differ between the designated scapegoat (job loss, reputational damage, legal liability) and the authority holder (reassignment, organizational continuity)?',
    'Career trajectory analysis for individuals held liable vs. authority holders in same organization; legal precedent examination for personal vs. corporate liability outcomes',
    'Large asymmetry: confirms snare classification for the scapegoat perspective. Small asymmetry: might indicate more symmetric responsibility structure (less extraction, more genuine accountability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_asymmetry, empirical, 'Asymmetry in exit costs between designated responsible parties and authority holders').

omega_variable(
    reform_doctrine_effectiveness,
    'Do shift-liability-upstream doctrines (duty to delegate, institutional negligence standards) actually reduce individual scapegoating in practice, or do they create new theatrical compliance rituals?',
    'Case law analysis pre/post doctrine adoption; comparison of individual liability frequency and severity; identification of workarounds that recreate scapegoat dynamics under new institutional forms',
    'If effective: scaffold sunset is real and measurable. If ineffective: the architecture is more entrenched than reform theory suggests, and piton classification dominates institutional views.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_doctrine_effectiveness, empirical, 'Whether upstream liability doctrines effectively reduce individual scapegoating').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(responsibility_without_power, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resp_tr_t0, responsibility_without_power, theater_ratio, 0, 0.45).
narrative_ontology:measurement(resp_tr_t3, responsibility_without_power, theater_ratio, 3, 0.58).
narrative_ontology:measurement(resp_tr_t6, responsibility_without_power, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(resp_be_t0, responsibility_without_power, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(resp_be_t3, responsibility_without_power, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(resp_be_t6, responsibility_without_power, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(responsibility_without_power, enforcement_mechanism).
narrative_ontology:affects_constraint(responsibility_without_power, algorithmic_opacity_and_audit).
narrative_ontology:affects_constraint(responsibility_without_power, liability_doctrine_upstream_shift).
narrative_ontology:affects_constraint(responsibility_without_power, performance_metric_gaming).

% DUAL FORMULATION NOTE:
% The scapegoat architecture decomposes into multiple constraint families depending on the domain: (1) organizational (responsibility for compliance officers in regulated industries), (2) legal (individual liability for corporate harms), (3) algorithmic (human operators blamed for ML system failures). Each domain has its own ε value and dynamics, but all share the core structure of responsibility assigned without corresponding power. The affects_constraints relationships indicate downstream consequences — algorithmic opacity enables scapegoating (makes responsibility assignment easier because causation is obscured), liability doctrine shifts reduce scapegoating (upstream attribution changes the responsibility assignment structure), and performance metric gaming is a secondary extraction mechanism enabled by the architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
