% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy via Qualitative Development (Innovation & Efficiency Framing)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint embodies a state legitimacy strategy that replaced raw
 *   GDP growth targets with innovation and efficiency metrics as the primary
 *   measure of state performance. Rather than justifying rule through
 *   year-over-year growth, the state claims to deliver structural
 *   transformation: a modernized, high-tech, sustainable productive base. The
 *   strategy benefits high-tech sectors and urban professional classes
 *   through preferential credit and industrial policy. It extracts from
 *   traditional manufacturing workers, property-dependent local governments,
 *   and agricultural sectors, whose interests are invisible in the quality
 *   metrics and whose displacement is justified as necessary modernization.
 *   The constraint operates as both genuine coordination (states genuinely
 *   benefit from upgrading their productive base) and asymmetric extraction
 *   (the burden of transition and the gains from transformation are unequally
 *   distributed). The claim is tangled_rope; the metrics describe a
 *   constraint moving toward snare characteristics as extraction rises and
 *   theater increases without corresponding employment or livelihood gains.
 *
 * KEY AGENTS:
 *   - State planning authority — sets and enforces development metrics (agenda_setter)
 *   - High-tech sectors and innovation ecosystem — receive preferential state resources (beneficiaries)
 *   - Urban professional class — benefit from innovation-focused development and rising wages (secondary beneficiaries)
 *   - Traditional manufacturing workers — face factory closures and suppressed wages, trapped by geography and skills (primary victims)
 *   - Property-dependent local governments — lose revenue and directionality as central state reallocates resources (secondary victims)
 *   - Agricultural sector and low-skill service workers — deprioritized in resource allocation, wage growth suppressed (victims)
 *   - Central bank and financial authorities — execute credit allocation enforcing the constraint (co-agenda-setters)
 *   - International standards bodies — provide external validation and constrain/enable the reading (observers)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.71).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy via Qualitative Development (Innovation & Efficiency Framing)").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '2c9b3a47-f731-4aa1-abc2-cbe68b89eca9').
narrative_ontology:cs_kernel_codification('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', formalized).
narrative_ontology:cs_authority_grounding('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', extraction).
narrative_ontology:cs_interpretation_layer_present('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9').
narrative_ontology:cs_reading_relation('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', foundational, innovation_structural_necessity).
narrative_ontology:cs_axiom_status(innovation_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', innovation_structural_necessity, empirically_contingent).
narrative_ontology:cs_axiom('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', foundational, quality_metrics_objective_measure).
narrative_ontology:cs_axiom_status(quality_metrics_objective_measure, holdable).
narrative_ontology:cs_axiom_grounding('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', quality_metrics_objective_measure, instrumental).
narrative_ontology:cs_reference_frame('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', innovation_driven_modernization_baseline).
narrative_ontology:cs_drift_state('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', contemporary_displacement_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c9b3a47-f731-4aa1-abc2-cbe68b89eca9', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, urban_professional_class).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_workers).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, agricultural_sector).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, low_skill_service_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, central_bank_and_financial_authorities).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, urban_professional_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces development metrics that elevate 'quality' (innovation index, patent filings, venture capital deployment, energy efficiency ratios, sustainable sourcing compliance) above raw GDP expansion. Justifies the shift as modernizing the economy and meeting global environmental standards. Allocates state-directed credit, industrial policy resources, and regulatory approval to sectors meeting the quality metrics. Claims legitimacy from the transformation itself, not from worker outcomes.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_planning_authority, agenda_setter,
    institutional, generational, analytical, national).

% Receive preferential access to state credit, industrial policy subsidies, R&D tax credits, and regulatory fast-tracking. Their growth is tracked as evidence of the legitimacy strategy's success. They can exit into global supply chains and offshore operations if domestic conditions deteriorate. They benefit from the quality-metrics framing because it justifies state resources flowing to their sectors regardless of employment impact.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    organized, generational, arbitrage, global).

% Universities, national labs, and state-owned research institutes receive sustained funding tied to patent production and commercialization metrics. Their legitimacy and career advancement depend on demonstrating innovation leadership. They benefit from the quality reading because it institutionalizes their sector as the measure of national progress.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    organized, generational, mobile, national).

% Engineers, scientists, managers, and tech workers in the innovation ecosystem receive rising wages and global career paths. They also indirectly benefit from the narrative that their labor represents the nation's future. They bear some cost through higher consumer prices and uneven public investment, but exit options and rising purchasing power offset this substantially.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, urban_professional_class, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, urban_professional_class, payer).

% Face factory closures and wage suppression as state credit flows to higher-tech sectors. Retraining programs promised by the state are underfunded and misaligned with actual job markets. Their displacement is justified as a necessary transition cost of modernization. They cannot exit the region or sector easily; retraining often fails. Their interests in stable employment are invisible in the quality-development metrics.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_workers, payer,
    powerless, biographical, trapped, regional).

% Historically funded through property taxes and land-sale revenues, they lose both as manufacturing contracts and urban real-estate cycles cool. Central state directs resources to innovation hubs (major cities) via new metrics. Local officials must comply with national quality-development reporting or lose discretionary transfers. They are caught between shrinking tax bases and mandated spending on innovation infrastructure they cannot build.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    moderate, biographical, constrained, local).

% Receives lower priority in resource allocation as innovation metrics favor high-tech manufacturing and services. Sustainability metrics imposed from above (carbon, chemical reduction) raise production costs without corresponding price support. Farmer incomes stagnate. They lack the political voice and organizational capacity to contest the metrics, but cannot easily shift sectors.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, agricultural_sector, payer,
    moderate, generational, constrained, regional).

% Benefit minimally from innovation gains; quality-development framing creates no metric for their job stability or wage growth. They experience the constraint as an abstraction — growth legitimacy shifted to innovation legitimacy, but their wages and employment remain volatile. They are neither counted in nor beneficiaries of the transformation narrative.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, low_skill_service_workers, payer,
    powerless, immediate, trapped, local).

% Execute the state's credit allocation and regulatory strategy. They benefit from the quality-development framing because it provides technical language (innovation index, efficiency ratio, sustainability scoring) that makes credit decisions appear objective and depoliticized. They actively enforce the constraint by directing credit and regulatory approval to sectors meeting quality metrics.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_bank_and_financial_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, central_bank_and_financial_authorities, beneficiary).

% Labor unions, rural development advocates, and livelihood-security advocates would contest the prioritization of innovation over employment or agricultural support. They are excluded from agenda-setting but retain some voice through political pressure and international advocacy. Their exclusion is structural: the quality-development metrics produce no input for their concerns.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, competing_development_advocates, excluded,
    moderate, biographical, constrained, national).

% Environmental, labor, and development bodies produce standards that both legitimize and constrain the quality-development reading. The state uses favorable standards (sustainability rankings, innovation indices) to validate the strategy and avoids scrutiny of labor and livelihood outcomes. Their role is as external validator, not enforcer.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, international_standards_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates research funding, industrial policy, and credit allocation around innovation and efficiency metrics, replacing allocation decisions based on growth-rate targets or employment outcomes. Solves the problem of state developmental priority-setting by providing technical language (innovation index, patent density, venture deployment) that appears objective and future-oriented.
% TRANSFER_FUNCTION: Moves state-directed credit, R&D subsidies, industrial policy resources, and regulatory approval from sectors measured as low-quality (traditional manufacturing, agriculture, low-skill services) to sectors measured as high-quality (high-tech, biotechnology, green energy). Also transfers risk: workers in displaced sectors bear closure risk while innovation sectors receive state backstops for commercial failure.
% ABSENT_VOICES: Displaced manufacturing workers, rural farmers, and livelihood-focused labor advocates would contest the priority of innovation metrics over employment and wage outcomes. They are excluded from the technical standards-setting process that defines 'quality development' and cannot shape the metrics used to allocate resources. Their objection would reframe the constraint as extraction rather than modernization.
% DISAPPEARANCE_RATIONALE: If this legitimacy reading and its accompanying metrics vanished, resource allocation would shift away from innovation hubs toward broader-based employment, wage, and livelihood measures. Central governments would face renewed pressure to justify growth in terms workers directly experience. The innovation ecosystem would lose state priority and dedicated credit access. Regional inequality would likely decrease but investment in frontier technologies would contract unless private capital fully compensated.
% FOUNDING_PROBLEM: Post-1980s development evidence showed diminishing returns to raw growth in large economies: GDP expansion no longer translated into proportional job creation or wage growth. Simultaneously, environmental constraints became binding and technological disruption accelerated. The reading framed a new legitimacy: instead of growth for its own sake, deliver modernized productive capacity, energy efficiency, and innovation capacity — outcomes states could claim to measure and deliver regardless of employment outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Development economists and technology advocates outside benefiting sectors attest that structural transformation is necessary and that quality metrics capture genuine productive upgrading. However, labor economists, livelihood advocates, and affected regional governments contest whether innovation gains are broadly distributed and whether the metrics accurately measure what citizens actually experience. The empirical question — whether innovation-focused development ultimately improves worker outcomes — remains unresolved in the literature.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the constraint transfers substantial state resources to innovation sectors while offering displaced workers minimal compensation or genuine retraining. The rise from 0.48 to 0.68 over the interval reflects the state's increasing commitment to quality metrics and deepening prioritization of innovation funding. Suppression is high (0.71) because alternative narratives — livelihood-security readings, growth-focused readings, livelihood advocacy — are actively excluded from technical standards-setting and budget deliberation. Theater_ratio rises from 0.28 to 0.42 as the state increasingly performs innovation leadership (R&D announcements, patent targets, venture funds) while actual worker transitions remain underfunded and poorly managed. The measurements reflect a constraint hardening over time: the innovation reading's institutional capture deepens, theater increases as the gap widens between announced transformation and actual worker outcomes, and suppression intensifies as excluded voices find fewer channels. Accessibility_collapse is moderate (0.64) because alternatives — shifting back to growth-focused metrics, emphasizing livelihood, or pursuing technological nationalism — remain conceptually available but institutionally suppressed and politically costly to advocate. Resistance is moderate (0.58) because displaced workers and livelihood advocates maintain some political voice and international pressure, but lack the institutional power to contest the metrics.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state planning authority) and beneficiaries (high-tech sectors, urban professionals) should classify this constraint very differently from the victims (displaced workers, property-dependent local governments). From the beneficiary seat, the constraint appears as genuine modernization coordination: investing in innovation and efficiency is necessary for long-term prosperity and global competitiveness, a shared project benefiting all. From the victim seat, the constraint appears as coercive extraction: displacement without real alternatives, suppressed wages, and false promises of retraining. The state uses technical language (innovation index, efficiency ratio) to make resource reallocation appear objective and depoliticized, but the beneficiary selection is deeply political. The engine should compute this divergence from the structural data: beneficiaries see coordination; victims see extraction. The theater_ratio captures the rhetorical gap: the state performs transformation while delivery lags.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives cleanly from beneficiary/victim declarations and exit options. High-tech sectors and the innovation ecosystem are near d=0.0 (full beneficiary): they collect preferential credit, regulatory approval, and R&D funding; their exit options are excellent (global supply chains, offshore operations) so structural dependence is low. The urban professional class is near d=0.2 (weak beneficiary): they benefit from rising wages and career opportunities, but partially bear transition costs through taxes and uneven public investment; their exit options are mobile so they are not trapped. Traditional manufacturing workers and property-dependent local governments are near d=0.85 (near full target): they bear the extraction (displaced wages, suppressed local revenue) with minimal offset; exit options are trapped (geography, skills, institutional dependence). Agricultural workers and low-skill service workers are near d=0.9 (near full target): they are deprioritized and experience suppressed wage growth; their exit options are trapped (low skill, regional dependence). No directionality override needed; the structural derivation aligns with the narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — diminishing returns to raw growth, technological disruption, environmental constraints — was live when the quality-development reading emerged (~2000-2010). The reading offered a coherent answer: pivot to innovation and efficiency metrics, modernize the productive base, decouple growth from employment as the primary metric. However, the founding problem's status is now contested. Labor economists and livelihood advocates argue that the founding problem (how to deliver broad-based prosperity in a high-disruption environment) is NOT solved by the quality-development reading. Growth has remained volatile; automation has deepened displacement; innovation gains have concentrated. The quality-development reading has persisted despite contested founding-problem status because it serves the state's institutional interests (provides technical language, justifies preferential resource allocation) and benefits entrenched coalitions (high-tech sectors, central financial authorities). This is mandatrophy territory: the arrangement persists because beneficiaries have institutional power, not because it solves the founding problem for broad constituencies. The constraint should carry a mandatrophy_resolved: true flag with commentary noting the status contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_trickle_down_mechanism,
    'Do innovation gains in high-tech sectors actually trickle down to displaced workers through retraining, higher consumer surplus, or second-order employment, or do the gains concentrate in the innovation ecosystem itself?',
    'Longitudinal wage and employment tracking of workers displaced from traditional manufacturing relative to innovation-sector wage growth; consumer surplus measurement; retraining outcomes analysis.',
    'If trickle-down is substantial and durable, the constraint operates more as genuine modernization (rope-like coordination with shared benefit). If minimal, the extraction is concentrated and the constraint is structurally closer to snare — innovation benefits are captured while transition costs are externalized to victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_trickle_down_mechanism, empirical, 'Whether innovation gains are broadly distributed or concentrated in beneficiary sectors.').

omega_variable(
    quality_metrics_validity,
    'Do the adopted quality metrics (innovation index, patent density, venture deployment, efficiency ratios) actually measure structural transformation that improves long-term productive capacity and citizen welfare, or do they measure intermediate outputs that can be gamed without corresponding real improvement?',
    'Retrospective analysis: correlate quality-metric improvements with actual productivity growth, export performance, and wage outcomes in comparator economies; examine whether quality metrics correlate with Goodhart drift (metric optimization decoupled from real outcomes).',
    'If metrics measure genuine transformation, the constraint is a coordination tool with valid feedback. If metrics are primarily performative, theater_ratio should rise further and the constraint should reclassify toward piton or snare with high theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_metrics_validity, empirical, 'Whether quality-development metrics measure real transformation or are performative proxies.').

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the performance_legitimacy kernel. The sibling readings — quantitative_growth_reading, livelihood_security_reading, techno_nationalist_reading — compete for institutional priority. Which reading the state adopts shapes resource allocation, worker outcomes, and regional inequality. What determines the reading''s persistence or displacement?',
    'Observe institutional signals: does state policy rhetoric emphasize innovation metrics, growth rates, livelihood outcomes, or national technology leadership? Do budget allocations and regulatory decisions align with one reading? Do subordinate agencies implement one reading consistently? A displaced reading would show weakening institutional enforcement and rising theater_ratio as the old metrics persist without resource backing.',
    'If this qualitative-development reading loses institutional priority (other readings dominate state practice), the constraint should downgrade: enforcement would weaken, theater_ratio would rise further, and the extracted rents would appear increasingly performative. Alternatively, if a different reading captures the kernel, a new constraint emerges (sibling reading, different ε) and this one becomes dormant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel reading stability: whether this reading maintains institutional primacy or is displaced by a sibling reading.').

omega_variable(
    suppression_internalization_vs_structural,
    'Is the suppression of alternative development narratives primarily structural (excluded from budget deliberation, regulatory frameworks, technical standards bodies) or internalized (affected parties accept that innovation is the legitimate path and their displacement is necessary)?',
    'Post-exit suppression trajectory: if labor advocates or regional governments exit innovation-focused planning (form separate development coalitions), does suppression persist? Do they carry the belief that innovation is the legitimate criterion even after exit, or do they reframe the constraint as extractive?',
    'If suppression is primarily structural, relief would come from including excluded voices in metrics-setting. If partly internalized, the constraint''s effective suppression is higher than the structural measure alone suggests, and recovery requires cognitive reframing work, not just institutional inclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Suppression mechanism: structural exclusion versus internalized acceptance of innovation-priority narrative.').

omega_variable(
    kernel_reading_framing,
    'This reading instantiates ONE interpretation of what ''performance legitimacy'' means in a state-capitalist system. The kernel is contested: different institutional actors, political coalitions, and theoretical traditions hold different readings. This reading privileges innovation and efficiency as the primary measures; siblings privilege growth, livelihood, or technological nationalism. Are these readings genuinely distinct constraints, or competing framings of a single underlying constraint?',
    'Per ε-invariance principle: measure the standing arrangement (state resource allocation for development) under each reading''s own lights. If different readings produce materially different ε values (because they measure different extraction mechanisms), they are distinct constraints. If ε stays similar across readings but interpretation differs, they are the same constraint under contention.',
    'If distinct constraints: author separate constraint stories for each reading, linked via network.affects_constraints. If one constraint with multiple readings: model the reading-level variation through omegas and cs_structure, not through separate stories. The evidence suggests genuine distinction (each reading predicts different beneficiary/victim sets and different resource allocation), supporting the separate-constraints approach already in use.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether reading-level variation represents genuinely distinct constraints or contending framings of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__qualitative_development_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__qualitative_development_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__qualitative_development_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__qualitative_development_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__qualitative_development_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__qualitative_development_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(perf_tr_t35, performance_legitimacy__qualitative_development_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__qualitative_development_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__qualitative_development_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__qualitative_development_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__qualitative_development_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__qualitative_development_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__qualitative_development_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(perf_be_t35, performance_legitimacy__qualitative_development_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__qualitative_development_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__qualitative_development_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__qualitative_development_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__qualitative_development_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__qualitative_development_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__qualitative_development_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(perf_su_t35, performance_legitimacy__qualitative_development_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__qualitative_development_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the performance_legitimacy kernel. All four readings operate in the same domain (state legitimacy strategy in state-capitalist development) but instantiate different constraints because they (1) measure different standing arrangements (innovation-focused allocation vs. growth-focused vs. livelihood-focused vs. techno-nationalist priority), (2) have different beneficiary/victim sets, and (3) carry different ε values derived from their respective observables. The kernel is CONTESTED: different state institutions, political coalitions, and theoretical traditions hold different readings simultaneously. Each reading-as-constraint is linked to its siblings via network.affects_constraints to enable contamination analysis (if one reading's institutional support erodes, which siblings expand?). The readings are NOT alternative measurements of one constraint — they are genuinely distinct constraints unified at the kernel level (same contestation, different operationalizations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__qualitative_development_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
