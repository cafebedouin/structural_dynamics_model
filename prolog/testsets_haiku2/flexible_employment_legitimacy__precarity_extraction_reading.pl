% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Precarity-Driven Platform Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   Flexible employment classification (gig worker, 1099 contractor,
 *   zero-hours contract, temporary employee) is presented in mainstream
 *   economics and policy as a market-clearing mechanism that benefits both
 *   workers (flexibility, access to income) and employers (demand matching).
 *   This constraint instantiates the precarity-extraction reading of the
 *   flexible-employment kernel: the same classification structures that
 *   enable demand matching also concentrate algorithmic control, suppress
 *   wages through surplus-labor creation, externalize employment risk to
 *   workers, shift social costs to public systems, and suppress workers'
 *   political voice through non-employment status. The reading is contested
 *   by market-efficiency and developmental-state readings (authored as
 *   sibling constraints, not this one). This story models the constraint from
 *   the extraction reading's structural frame: the coordination function is
 *   real but subordinate to extraction enabled by precarity.
 *
 * KEY AGENTS:
 *   - Platform operators (institutional, beneficiary): set terms unilaterally, control algorithmic allocation, enforce discipline through deactivation
 *   - Gig workers (powerless, identity-locked, payer): structurally precarious, no employment status, bear income volatility and uninsured risk
 *   - Contingent laborers (moderate power, constrained exit, payer): distributed across sectors, precarity normalized, face wage suppression and benefit denial
 *   - Capital holders (institutional, beneficiary): realize returns through cost reduction and risk externalization
 *   - Social safety net (organized, constrained, payer): absorbs costs of precarity (unemployment assistance, emergency healthcare, poverty support)
 *   - Labor organizations (organized, excluded, trapped): barred from organizing by classification status and algorithmic prevention of assembly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.78).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.71).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Precarity-Driven Platform Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '20a1dc34-0254-4563-a175-7145ec9472d8').
narrative_ontology:cs_kernel_codification('20a1dc34-0254-4563-a175-7145ec9472d8', formalized).
narrative_ontology:cs_authority_grounding('20a1dc34-0254-4563-a175-7145ec9472d8', extraction).
narrative_ontology:cs_interpretation_layer_present('20a1dc34-0254-4563-a175-7145ec9472d8').
narrative_ontology:cs_reading_relation('20a1dc34-0254-4563-a175-7145ec9472d8', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('20a1dc34-0254-4563-a175-7145ec9472d8', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('20a1dc34-0254-4563-a175-7145ec9472d8', foundational, precarity_enables_cost_reduction).
narrative_ontology:cs_axiom_status(precarity_enables_cost_reduction, holdable).
narrative_ontology:cs_axiom_grounding('20a1dc34-0254-4563-a175-7145ec9472d8', precarity_enables_cost_reduction, empirically_contingent).
narrative_ontology:cs_axiom('20a1dc34-0254-4563-a175-7145ec9472d8', foundational, algorithmic_control_substitutes_for_collective_power).
narrative_ontology:cs_axiom_status(algorithmic_control_substitutes_for_collective_power, holdable).
narrative_ontology:cs_axiom_grounding('20a1dc34-0254-4563-a175-7145ec9472d8', algorithmic_control_substitutes_for_collective_power, deontological).
narrative_ontology:cs_axiom('20a1dc34-0254-4563-a175-7145ec9472d8', secondary, classification_structure_legitimates_extraction).
narrative_ontology:cs_axiom_status(classification_structure_legitimates_extraction, holdable).
narrative_ontology:cs_axiom_grounding('20a1dc34-0254-4563-a175-7145ec9472d8', classification_structure_legitimates_extraction, conventional).
narrative_ontology:cs_reference_frame('20a1dc34-0254-4563-a175-7145ec9472d8', permanent_employment_with_collective_protections).
narrative_ontology:cs_drift_state('20a1dc34-0254-4563-a175-7145ec9472d8', contemporary_platform_dominance_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('20a1dc34-0254-4563-a175-7145ec9472d8', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, capital_holders).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, contingent_laborers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, traditional_employers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, traditional_employers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, social_safety_net_institutions).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, labor_flexibility_enhances_employer_control).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, risk_transfer_maximizes_capital_returns).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design classification systems (1099 contractor, gig worker, temporary employee) that shift employment risk to workers while retaining algorithmic control over labor allocation, scheduling, compensation, and termination. Set terms unilaterally, enforce compliance through deactivation, and collect surplus generated by wage suppression enabled by precarity. Justify flexibility as matching supply to demand; operate algorithmic management systems that function as labor discipline.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Accept non-employment status (independent contractor) to access the platform's customer access point. Receive no unemployment insurance, no paid leave, no health insurance, no retirement contributions, no wage guarantees. Bear full income volatility: platform can reduce available work, lower per-task compensation, or deactivate their account unilaterally without notice or recourse. Structured algorithmic discipline: acceptance rates, response times, ratings determine access and earnings. Economic dependence on platform income despite non-employment status makes exit identity-locked (professional identity, survival income, no alternative access to customer base at comparable volume).
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers, payer,
    powerless, immediate, identity_locked, local).

% Work under temporary, on-call, zero-hours, or fixed-term contracts that distribute risk to the worker: no minimum hours guaranteed, no job security, eligibility for benefits contingent on threshold work hours rarely reached, rapid termination at will. Precarity is justified as market flexibility; institutionalized as normal employment form. Can seek alternative employment, but precarity is now structural across sectors (retail, hospitality, logistics, care work), making exit to non-precarious work constrained. Suppression operates through labor-market saturation and normalized expectation that precarity is inevitable.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, contingent_laborers, payer,
    moderate, biographical, constrained, regional).

% Realize returns through cost reduction: wage suppression enabled by precarity (no benefits burden, wage undercut by surplus labor supply, no collective bargaining power), risk externalization (workers absorb income volatility, health risk, unemployment duration), and labor flexibility (rapid scaling/contraction without severance or retraining liability). Precarity is operationalized as legitimate through classification structures and market-efficiency framing; maintained through political lobbying to prevent employment reclassification or benefit extension.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, capital_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Face pressure to adopt precarious employment structures to remain competitive on cost: convert permanent roles to contingent, reduce benefit obligations, adopt gig-platform subcontracting. Benefit from reduced labor costs; also vulnerable to labor-cost competition from pure-platform operators with lower overhead. Mobile exit (can shift to traditional or precarious model) differentiates them from precarious workers, but cost pressures constrain their options upward.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, traditional_employers, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, traditional_employers, payer).

% Bear expanded social costs: unemployment assistance when gig work dries up, emergency healthcare for uninsured contingent workers, disability support for workers injured without employer liability coverage, means-tested assistance for workers earning below subsistence despite full-time precarious work. Precarity shifts insurance functions from employer to state, increasing public expenditure and implicitly subsidizing labor cost savings to platform operators and capital holders. Constrained by political resistance to tax increases; cannot easily exit these obligations.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_safety_net_institutions, payer,
    organized, generational, constrained, national).

% Structurally barred from organizing precarious workers: gig workers classified as non-employees (no legal right to organize), contingent workers scattered across employers with high turnover (collective-action coordination failure), algorithmic management systems designed to prevent in-person assembly. Would argue for employment reclassification, wage floors, benefit portability, and collective representation if admitted to the negotiation table; excluded by classification structures and right-to-work legal frameworks that treat precarity as non-negotiable market outcome.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_unions_and_workers_organizations, excluded,
    organized, generational, trapped, regional).

% Advocate for employment reclassification, mandatory benefits inclusion, wage floors, or portable benefits systems; excluded from decision-making by the political power of capital lobbies and the normalized framing of precarity as inevitable market outcome. Would reclassify the constraint's structure if they held agenda-setting power; currently constrained to advocacy and periodic legislative attempts that face strong opposition from benefiting actors.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, policy_advocates_for_employment_stability, excluded,
    moderate, generational, constrained, national).

% Observe workplace classification disputes and make determinations about whether specific worker arrangements constitute employment. In theory hold veto power over classification schemes, but in practice face resource constraints, political pressure from benefiting actors, and the complexity of adjudicating thousands of individual relationships. Some jurisdictions have begun reclassification enforcement (California AB5, UK Supreme Court ruling on Uber drivers); most remain captured or understaffed.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches highly variable labor demand to willing supply: platforms can rapidly scale and contract workforce without fixed-labor commitments; workers gain access to customer base and work flexibility. In stated form, this is genuine coordination — matching granular demand to supply without long-term commitment. In actual operation, the coordination function is secondary to cost reduction and risk transfer.
% TRANSFER_FUNCTION: Moves income from workers to platform operators and capital holders through: wage suppression (surplus labor supply created by classification system; no wage floors), risk transfer (income volatility, unemployment gaps, uninsured health costs), and social-cost externalization (state absorbs benefits shortfall, retraining, emergency services). Additionally moves political power: workers' lack of employment status blocks collective organization and political voice, concentrating agenda-setting authority with platforms and employers.
% ABSENT_VOICES: Labor unions, worker-advocacy organizations, and precarious workers themselves are structurally excluded from terms-setting: they lack legal standing (workers classified as non-employees), organizational capacity (high turnover, geographic scatter, algorithmic prevention of assembly), and political power (capital lobbies dominate policy formation). They would argue for employment reclassification, wage floors, benefit portability, and collective representation if they were seated, but the same structures that enable precarity suppress their participation.
% DISAPPEARANCE_RATIONALE: If flexible-employment precarity as a legitimated structural form vanished overnight: platform operators would face mandatory employment classification, benefit obligations, minimum-hour guarantees, and collective-bargaining exposure; capital would absorb labor-cost increases or contract workforce; contingent labor markets would collapse without the precarity option, forcing reclassification or absorption into traditional employment; social safety nets would shrink (fewer gig workers with gaps in coverage); worker income stability would rise; labor's political voice would recover through unionization and collective power. The world does not rearrange into some stable natural state — it rearranges into an employment structure dominated by collective bargaining and employer obligation rather than individual precarity and platform extraction.
% FOUNDING_PROBLEM: Matching rapid service demand (ride-sharing, delivery, task work) to labor supply without capital-intensive long-term employment commitments. Classical labor economics: demand spikes require surplus labor supply; long-term employment contracts are costly when demand is volatile and unpredictable.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and capital-market analysts attest the founding problem is live and worsening: demand volatility is increasing (seasonal, pandemic-driven, algorithmic routing), and traditional employment cannot adapt fast enough without massive underutilization. Labor economists and worker-advocacy organizations attest the founding problem is real but SOLVED by demand-management, scheduling, and temporary-employment mechanisms that preserve worker protections; they argue flexible employment persists not because the problem requires it, but because precarity delivers cost reduction and disciplinary control beyond what the problem legitimately requires. Independent economic analysis (Rosenblat, Rahman, Kalleberg) documents that precarity-based cost savings exceed any efficiency gain from matching supply to demand, supporting the extraction-reading frame.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.78 because wage suppression is substantial and systematic: (1) Classification-enabled surplus labor supply undercuts wages below what permanent employment would sustain; (2) No benefits burden shifts employer cost directly to workers and public systems; (3) Algorithmic allocation and rating systems enable individualized wage pressure (lower ratings = lower earnings) without transparency; (4) Threat of deactivation creates compliance discipline. The measurement trajectory shows extraction rising sharply in the first 12 periods (0.58→0.74) as the platform ecosystem consolidates, then plateauing (0.74→0.78) as resistance hardens and regulatory scrutiny increases — the plateau at 0.78 reflects extraction hitting a pressure ceiling, not stabilization. Suppression reaches 0.71 because: (1) Workers classified as non-employees lack legal recourse (no collective-bargaining rights, no wrongful-termination suits, limited unemployment eligibility); (2) Algorithmic management prevents in-person assembly and collective action; (3) Surplus labor supply and identity-lock (professional identity + survival income) make exit identity-locked rather than merely constrained; (4) Normalization of precarity through policy and media frames it as inevitable. Theater at 0.48 reflects the constraint's dual nature: genuine coordination function persists (demand-matching is real), but active effort goes increasingly to suppressing alternatives (preventing reclassification, blocking unionization, lobbying against benefits mandates) rather than delivering the coordination function itself. The shared measurement grid ensures every metric is authored at every time point; temporal analysis can detect when the constraint's character changes (e.g., if theater ratio rises past 0.6, coordination function has atrophied and suppression is maintaining an obsolete structure).
 *
 * PERSPECTIVAL GAP:
 *   The platform-operator and capital-holder seats compute the constraint as rope or scaffold (transitional coordination enabling market flexibility); the worker seats compute it as snare (extraction sustained by suppression and classification). The regulatory seat should detect this divergence: if agents with symmetric power and exit options experience the constraint differently, the divergence signals structural asymmetry (directionality toward extraction rather than symmetric coordination). The engine computes per-seat classifications from power, exit_options, beneficiary/victim status, and suppression: platforms with arbitrage exit and beneficiary role should compute as benefiting-coordinator; workers with identity-locked exit and payer role should compute as targets. The divergence IS the classification the story exists to document.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim status and exit options. Platform operators (beneficiary role, institutional power, arbitrage exit → d ≈ 0.1): they can exit into other business models but choose flexible employment because it maximizes extraction; their low d means effective extraction is dampened, but they collect the gains. Gig workers (payer role, powerless, identity-locked exit → d ≈ 0.95): they bear wage suppression, risk transfer, and suppression without exit options (professional identity, survival income, no alternative customer-access point at comparable volume); their high d means effective extraction is amplified. Contingent laborers (payer role, moderate power, constrained exit → d ≈ 0.75): more mobile than gig workers but precarity is now structural across sectors; higher d than platform operators but lower than gig workers. Capital holders (beneficiary role, institutional power, arbitrage exit → d ≈ 0.15): similar to platform operators; they benefit from cost reduction and can shift to traditional employment if precarity becomes unprofitable. Social safety net (payer role, organized power, constrained exit → d ≈ 0.65): bears expanded social costs but has political leverage (can lobby for benefit mandates or reclassification); constrained exit (cannot easily refuse to serve precarious workers) but lower d than workers because they hold organized power. No directionality overrides needed: the structural derivation accurately captures the per-seat relationship to this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (matching volatile demand to labor supply) was real at the constraint's origin (2008-2012, ride-sharing and task-platform emergence) and remains contested in its current status. The precarity-extraction reading argues the founding problem is substantially SOLVED by demand-management, on-call scheduling, temporary employment, and gig-platform matching — but the constraint persists and EXPANDS because extraction enabled by precarity exceeds what the problem legitimately requires. The divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges flags this as a mandatrophy candidate: if the constraint disappeared, the world rearranges (employment reclassification, benefit obligations, union organization), which means the constraint is NOT a natural feature required by the founding problem but a constructed mechanism whose persistence depends on active enforcement (classification maintenance, benefits denial, unionization prevention). The theater_ratio trajectory (0.32→0.48) shows rising performative maintenance: as regulatory pressure increases and worker awareness grows, more effort goes into defending the classification structure and suppressing alternatives (PR campaigns emphasizing worker choice, political lobbying against AB5-style reclassification, algorithmic management refinement to prevent unionization) rather than delivering the demand-matching function itself. This is the signature of mandatrophy: the constraint persists not because the founding problem requires it, but because the benefiting parties actively maintain it against mounting pressure to reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_necessity_vs_extraction_cover,
    'Is non-employment classification structurally necessary for demand-matching efficiency, or is it a cover story for cost reduction and disciplinary control that could be achieved through alternative arrangements (on-call employment, part-time positions with prorated benefits, temporary staffing agencies with portable benefits)?',
    'Comparative institutional analysis: jurisdictions that mandate employment classification (EU gig-worker directives, California AB5) or require benefit portability (portable benefits accounts, sectoral benefit funds) and measure demand-matching efficiency, wage levels, worker retention, and platform viability against comparable gig-economy platforms operating under permissive classification. If efficiency and platform viability persist under employment classification, the cover-story hypothesis is supported.',
    'If classification is unnecessary for efficiency, the constraint reclassifies from tangled_rope (coordination + extraction) to snare (pure extraction), and the founding problem is revealed as retroactively invented justification rather than genuine enabling requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_necessity_vs_extraction_cover, empirical, 'Whether non-employment classification is necessary for demand-matching or is a cost-reduction tool.').

omega_variable(
    algorithmic_control_as_labor_discipline,
    'How much of the measured suppression (0.71) is structural (legal non-employment status blocks collective action) versus internalized (workers internalize algorithmic ratings, acceptance-rate metrics, and reputation management as self-discipline, carrying the suppression even after exit)?',
    'Post-exit ethnography: interview workers who have left gig platforms and measure persistence of internalized discipline (self-metrics tracking, acceptance-rate anxiety, rating obsession) after the external algorithmic system is gone. If internalization is high, the constraint''s effective suppression is higher than the structural measure suggests — the worker carries the suppression with them.',
    'If internalization is substantial, the constraint''s grip is stronger than legal status alone indicates, and reclassification alone may not reverse worker precarity (workers would need explicit deprogramming/collective identity reconstruction alongside employment reclassification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_control_as_labor_discipline, conceptual, 'Whether suppression is structural or partially internalized through algorithmic discipline.').

omega_variable(
    wage_suppression_attribution,
    'How much of the wage suppression captured in extractiveness (0.78) is attributable to: (a) surplus labor supply created by platform growth and classification barriers; (b) algorithmic wage-setting (individualized downward pressure through ratings, acceptance rates, surge-pricing suppression); (c) social-norm shift toward precarious-wage acceptance through normalized precarity; (d) regulatory capture (prevention of minimum-wage, collective-bargaining, or benefit-floor legislation)?',
    'Econometric decomposition using regional variation: compare wage trends in jurisdictions with employment reclassification mandates (reducing surplus-labor effect and enabling collective bargaining) against permissive-classification jurisdictions, controlling for labor-market fundamentals. If reclassified workers'' wages rise substantially, (a) and (d) are primary drivers; if wages persist low despite reclassification, (b) and (c) are persistent.',
    'Attribution determines remedy design: if (a) and (d) dominate, reclassification and unionization are sufficient; if (b) and (c) dominate, wage floors, algorithmic transparency mandates, and deprogramming are necessary alongside reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_attribution, empirical, 'Which mechanisms drive the measured wage suppression.').

omega_variable(
    reading_foreclosure_contestation,
    'Does the precarity_extraction reading logically foreclose the market_efficiency_reading within a single framework, or do they coexist as genuinely alternative framings held by different parties?',
    'Analytical: the market_efficiency reading asserts flexible employment is a legitimate market-clearing mechanism; the precarity_extraction reading asserts the same mechanism is primarily a cost-reduction and risk-externalization device. These are not direct contradictions — they can coexist if one party emphasizes genuine coordination benefit (market_efficiency) and another emphasizes the extraction layered on top (precarity_extraction). The readings would foreclose only if market_efficiency asserted NO extraction or precarity_extraction asserted NO coordination — neither does. They coexist as asymmetric-access readings of the same mechanism.',
    'If coexisting, the kernel contest remains live (no theoretical resolution), and the constraint family models three genuinely held alternative interpretations. If foreclosing, one reading is logically indefensible and the constraint reduces to the surviving reading(s).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_contestation, conceptual, 'Whether the sibling readings logically eliminate each other or coexist as live alternatives.').

omega_variable(
    gig_worker_identity_lock_mechanism,
    'Is gig-worker identity-lock driven by: (a) economic necessity (no alternative income source, cannot afford identity transition costs); (b) professional identity fusion (self-concept as gig worker, reputation invested in platform ratings, career narrative locked to gig-platform success); (c) informational isolation (unaware of alternative work arrangements, normalization of gig work as only available option); (d) technological lock-in (data, ratings, customer relationships on platform are non-portable)?',
    'Mixed-methods: survey gig workers on exit barriers, conduct ethnography on professional identity formation, measure information diffusion about alternative arrangements, and examine data portability and rating transferability across platforms. High emphasis on (b) suggests identity-fusion suppression persists after exit; high emphasis on (d) suggests regulatory intervention on data portability could enable exit; high emphasis on (c) suggests information campaigns could shift perceived options.',
    'Identity-lock mechanism determines remedy: if (b) dominates, organizing and collective identity-building is necessary to break internalized suppression; if (d) dominates, data-portability and rating-transfer mandates could lower exit costs; if (c) dominates, information intervention could shift perceived choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gig_worker_identity_lock_mechanism, conceptual, 'What mechanisms bind gig-worker identity to platform precarity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(flex_tr_t0, observed).
narrative_ontology:measurement(flex_tr_t3, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 3, 0.36).
narrative_ontology:measurement_basis(flex_tr_t3, observed).
narrative_ontology:measurement(flex_tr_t6, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement_basis(flex_tr_t6, observed).
narrative_ontology:measurement(flex_tr_t9, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 9, 0.44).
narrative_ontology:measurement_basis(flex_tr_t9, observed).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement_basis(flex_tr_t12, observed).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement_basis(flex_tr_t15, observed).
narrative_ontology:measurement(flex_tr_t18, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 18, 0.48).
narrative_ontology:measurement_basis(flex_tr_t18, observed).
narrative_ontology:measurement(flex_tr_t21, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 21, 0.48).
narrative_ontology:measurement_basis(flex_tr_t21, observed).
narrative_ontology:measurement(flex_tr_t25, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(flex_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(flex_be_t0, observed).
narrative_ontology:measurement(flex_be_t3, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement_basis(flex_be_t3, observed).
narrative_ontology:measurement(flex_be_t6, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 6, 0.66).
narrative_ontology:measurement_basis(flex_be_t6, observed).
narrative_ontology:measurement(flex_be_t9, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 9, 0.71).
narrative_ontology:measurement_basis(flex_be_t9, observed).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement_basis(flex_be_t12, observed).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement_basis(flex_be_t15, observed).
narrative_ontology:measurement(flex_be_t18, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 18, 0.77).
narrative_ontology:measurement_basis(flex_be_t18, observed).
narrative_ontology:measurement(flex_be_t21, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 21, 0.78).
narrative_ontology:measurement_basis(flex_be_t21, observed).
narrative_ontology:measurement(flex_be_t25, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement_basis(flex_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(flex_su_t0, observed).
narrative_ontology:measurement(flex_su_t3, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 3, 0.59).
narrative_ontology:measurement_basis(flex_su_t3, observed).
narrative_ontology:measurement(flex_su_t6, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement_basis(flex_su_t6, observed).
narrative_ontology:measurement(flex_su_t9, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 9, 0.66).
narrative_ontology:measurement_basis(flex_su_t9, observed).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(flex_su_t12, observed).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(flex_su_t15, observed).
narrative_ontology:measurement(flex_su_t18, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 18, 0.71).
narrative_ontology:measurement_basis(flex_su_t18, observed).
narrative_ontology:measurement(flex_su_t21, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 21, 0.71).
narrative_ontology:measurement_basis(flex_su_t21, observed).
narrative_ontology:measurement(flex_su_t25, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(flex_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__precarity_extraction_reading, 0.18).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_management_labor_control).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, social_safety_net_adequacy_gap).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, labor_unionization_suppression_through_classification).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the flexible-employment-legitimacy kernel. The market_efficiency_reading instantiates the same standing arrangement (flexible employment classification) but from the benefiting parties' frame (genuine coordination); the developmental_state_reading occupies a middle position (coordination requiring state management). All three share the same referent (standing arrangement) but differ in ε (extractiveness), beneficiary/victim structure, and classification consequences. The three stories form a constraint family linked by network edges: precarity_extraction reading AFFECTS market_efficiency and developmental_state (the precarity reading argues for reclassification that would eliminate the need for both alternative readings); market_efficiency reading INFLUENCES developmental_state (if market mechanisms are truly efficient, state management is unnecessary). Each story must be authored independently with its own metrics, beneficiary/victim structure, and commentary; the engine computes per-seat classification from the structural data — divergence across readings is expected and is the measurement the kernel-contest family exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
