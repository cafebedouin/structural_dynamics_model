% ============================================================================
% CONSTRAINT STORY: garment_supplychain_audit_theater
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_garment_supplychain_audit_theater, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: garment_supplychain_audit_theater
 *   human_readable: Garment Supplier Code-of-Conduct Audit Theater Regime
 *   domain: labor/supply_chain/corporate_responsibility
 *
 * SUMMARY:
 *   The garment supply chain audit-theater regime represents a sophisticated
 *   extraction mechanism that disguises itself as coordination through
 *   procedural legitimacy. Global brands implement code-of-conduct audits and
 *   third-party certifications to address labor rights and safety, creating
 *   the appearance of accountability while actual working conditions
 *   deteriorate or stagnate. The regime operates through a structural
 *   paradox: individual-level visible coercion (audit compliance cycles,
 *   speed-ups, wage suppression) appears to lower over the interval as audit
 *   theater increases (0.52 → 0.78), but structural and class-level
 *   suppression rises simultaneously (0.54 → 0.68 at class level). Workers
 *   experience this as a substitution: direct supervisory coercion gives way
 *   to opaque audit-driven production pressure and restricted collective
 *   organizing. The audit regime coordinates global supply networks and
 *   legitimizes brand sourcing while systematically obscuring who bears
 *   extraction costs and preventing organized worker response.
 *
 * KEY AGENTS:
 *   - Garment Workers: Primary victims (powerless/trapped) — bear extraction through wage suppression, speed-ups, audit compliance cycles; economic necessity prevents exit; individual visible coercion lowers as audit theater rises, obscuring the mechanism
 *   - Factory Management: Secondary institutional actor (organized/constrained) — coordinate production and pass audit costs downstream to workers; constrained by audit schedules but can arbitrage compliance costs
 *   - Brand Corporations: Primary beneficiary (institutional/arbitrage) — extract legitimacy, risk management, and margin protection; high arbitrage option; experience regime as coordination
 *   - Audit Intermediary Firms: Secondary beneficiary (institutional/arbitrage) — revenue model tied to audit volume, not worker outcomes; arbitrage options allow exit from individual brand relationships; experience as pure coordination
 *   - Factory Compliance Systems: Engineered institutional actor — the apparatus through which audit performance is staged; enables gaming and cost transfer; maintained through institutional inertia (piton characteristic)
 *   - Worker Collective Organizing: Suppressed actor (organized/constrained) — union attempts, worker committees, independent grievance mechanisms face rising organizational-level suppression (0.62 → 0.71) as individual audit theater substitutes
 *   - Analytical Observer: Civilizational seat (analytical/analytical) — identifies the regime as tangled rope: genuine coordination function (global supply logistics) paired with systematic extraction (asymmetric cost, informational, and political power distribution)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(garment_supplychain_audit_theater, 0.68).
domain_priors:suppression_score(garment_supplychain_audit_theater, 0.62).
domain_priors:theater_ratio(garment_supplychain_audit_theater, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(garment_supplychain_audit_theater, extractiveness, 0.68).
narrative_ontology:constraint_metric(garment_supplychain_audit_theater, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(garment_supplychain_audit_theater, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(garment_supplychain_audit_theater, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(garment_supplychain_audit_theater, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(garment_supplychain_audit_theater, tangled_rope).
narrative_ontology:human_readable(garment_supplychain_audit_theater, "Garment Supplier Code-of-Conduct Audit Theater Regime").
narrative_ontology:topic_domain(garment_supplychain_audit_theater, "labor/supply_chain/corporate_responsibility").

domain_priors:requires_active_enforcement(garment_supplychain_audit_theater).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(garment_supplychain_audit_theater, brand_corporations).
narrative_ontology:constraint_beneficiary(garment_supplychain_audit_theater, audit_intermediaries).
narrative_ontology:constraint_victim(garment_supplychain_audit_theater, garment_workers).
narrative_ontology:constraint_victim(garment_supplychain_audit_theater, factory_compliance_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(garment_supplychain_audit_theater, audit_intermediary_firms).
narrative_ontology:constraint_victim(garment_supplychain_audit_theater, factory_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers in garment factories in South and Southeast Asia, primarily women, aged 18-45, earn $3-8 USD daily. Their situation is defined by the audit regime as follows: (1) Production speed increases when audits are scheduled to show 'efficiency improvements' to brands; (2) Wages remain suppressed even when audits report 'compliance'; (3) Audit documentation and brand responses are opaque to workers; (4) Attempting to organize independent unions or worker committees risks factory blacklisting; (5) Economic necessity (supporting families, lack of alternative employment) prevents exit from the industry or factory; (6) Visible supervisory coercion (yelling, threatening termination) has decreased as audit theater increases, but structural coercion (throughput quotas, piece-rate wage systems) has intensified. Workers bear the extraction through unpaid compliance labor, wage suppression, speed-ups, and exclusion from the governance of the system that controls their work.
narrative_ontology:constraint_stakeholder(garment_supplychain_audit_theater, garment_workers, payer,
    powerless, biographical, trapped, global).

% Factory managers operate within the audit regime's constraints: they must maintain certifications to retain brand contracts. Their situation includes: (1) Scheduling production and compliance cycles around audit timelines; (2) Passing cost pressures from brands downstream to workers via wage suppression and speed-ups; (3) Managing the appearance of compliance while maintaining cost competitiveness; (4) Constrained by audit requirements but able to arbitrage compliance costs (hiring temporary workers pre-audit, then laying them off; preparing documents that do not reflect daily practice); (5) Dependent on brand contracts for revenue, so cannot refuse audit demands. Factory management coordinates the supply logistics (matching orders to production capacity, coordinating across shifts and product lines) while simultaneously extracting from workers by enforcing audit-driven production cycles that benefit brands.
narrative_ontology:constraint_stakeholder(garment_supplychain_audit_theater, factory_management, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(garment_supplychain_audit_theater, factory_management, payer).

% Multinational apparel brands (Nike, Adidas, H&M, etc.) benefit from the audit regime through: (1) Certified supply chains that provide reputational protection against labor-rights criticism; (2) Cost reduction enabled by audits that pressure factories to suppress wages and speed up production; (3) Risk transfer: audits create liability shielding if labor violations occur (brands can claim they audited and found compliance); (4) Arbitrage options: brands can switch factories, audit firms, or production regions if a factory loses certification or audit costs rise; (5) Market legitimacy: certifications enable premium positioning (sustainability claims, ethical sourcing) that command higher margins. Brands experience the audit regime as coordination: it solves the problem of verifying supply chain conditions across global production networks. They are net beneficiaries with exit capacity.
narrative_ontology:constraint_stakeholder(garment_supplychain_audit_theater, brand_corporations, beneficiary,
    institutional, immediate, arbitrage, global).

% Third-party audit companies (SGS, Bureau Veritas, Intertek, etc.) and certification bodies profit from the audit regime through: (1) Audit fees per factory inspection ($500-5000 per audit, typically 2-4 audits per factory annually); (2) Certification and training fees (initial certification, recertification, remediation consultation); (3) Revenue model incentivizes audit volume, not rigor (more audits = more revenue); (4) Can exit individual brand relationships if demands become too costly; (5) Market competition among auditors reduces pressure for substantive investigation (brands choose firms willing to pass more factories). Audit intermediaries experience the regime as pure coordination: they solve brands' verification problem. They are net beneficiaries.
narrative_ontology:constraint_stakeholder(garment_supplychain_audit_theater, audit_intermediary_firms, beneficiary,
    institutional, biographical, arbitrage, global).

% The institutional apparatus of audit protocols, checklists, scoring rubrics, certification standards, and compliance documentation systems. This is not an agent but a structured system: it embodies the regime's design choices about what gets measured, how factories demonstrate compliance, and what counts as 'passing.' The compliance system's situation is institutional reproduction: it persists through habit, regulatory requirement, and brand demand, even as its effectiveness at protecting workers declines. It enables gaming (factories prepare for audits, then revert to non-compliant conditions) and cost transfer (workers absorb the labor of compliance preparation).
narrative_ontology:constraint_stakeholder(garment_supplychain_audit_theater, factory_compliance_systems, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(garment_supplychain_audit_theater, factory_compliance_systems).

% Independent worker unions, worker committees, and collective bargaining mechanisms that could organize workers to demand higher wages, safer conditions, and participation in factory governance. These are not present in the audit regime or are actively suppressed. The situation is absence: workers lack an organized voice in the system that controls their labor. Union organizers face factory retaliation, visa restrictions, and political suppression in many garment-producing countries. Worker committees formed during audits are often dissolved post-audit. This excluded voice represents the mechanism by which workers could challenge extraction if they had collective organizing capacity.
narrative_ontology:constraint_stakeholder(garment_supplychain_audit_theater, worker_organizing_potential, excluded,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(garment_supplychain_audit_theater, worker_organizing_potential).

% Brand consumers in wealthy countries who could, in principle, demand accountability if they understood the gap between audit theater and actual conditions. Currently excluded from the regime because audit reports are not transparent, brands control the narrative, and the media coverage of garment labor is episodic rather than structural. Consumers have the purchasing power and informational access to challenge the regime, but are systematically positioned outside it.
narrative_ontology:constraint_stakeholder(garment_supplychain_audit_theater, consumer_awareness, excluded,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_non_agent(garment_supplychain_audit_theater, consumer_awareness).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(garment_supplychain_audit_theater, brand_corporations).
narrative_ontology:fixing_cost_class(garment_supplychain_audit_theater, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The regime coordinates global garment supply networks by creating standardized audit protocols that allow brands to verify (or claim to verify) labor practices across hundreds of factories in dozens of countries. Without this coordination mechanism, brands would face fragmented, non-comparable factory information, making it difficult to manage scale. The coordination solves a real problem: global supply chains require synchronization of product specifications, quality standards, delivery timelines, and (ostensibly) labor standards. The audit regime provides a common language and verification structure for this coordination.
% TRANSFER_FUNCTION: The regime transfers: (1) Labor value from workers to brands and auditors via wage suppression, unpaid compliance labor, and opportunity cost of restricted organizing; (2) Risk from brands to factories and workers (liability for violations is pushed to factory management, worker protection responsibility is pushed onto audits); (3) Legitimacy and market premium to brands (certified supply chains enable sustainability claims and premium pricing). The flow is asymmetric: workers provide labor under suppressed terms; factories provide management labor and compliance administration; brands extract profit margin and reputational value; auditors extract fees.
% ABSENT_VOICES: Independent worker voices are absent: no workers sit on brand governance committees, no worker representatives review audit findings, no worker unions are consulted on audit protocol design. Garment-worker organizing efforts in producing countries are either excluded by design (not invited to participate in brand decisions) or suppressed (union organizers face retaliation). Consumer voices are absent: brand customers in wealthy countries are not informed about the gap between audit theater and actual conditions; they are not invited to demand transparency. Independent labor-rights NGOs are present but operate in an advisory capacity without decision-making power. The excluded voices represent power positions that could contest the extraction: workers could demand higher wages and collective bargaining; consumers could demand transparency and accountability; unions could enforce labor standards.
% DISAPPEARANCE_RATIONALE: If the audit regime disappeared overnight, global garment supply networks would face a coordination crisis: brands would lack a common verification mechanism, factory relationships would rely on direct negotiation, and labor standards would revert to bilateral agreement without third-party oversight. Factories would no longer face audit compliance cycles, allowing faster production and (potentially) higher wages if competitive pressure forced wage increases. Workers' ability to organize would increase because audit suppression would be removed. Brands would lose their certification legitimacy and would face immediate pressure to establish alternative verification mechanisms or retreat from ethical-sourcing claims. The market would not remain unchanged: new coordination mechanisms would emerge (brands might develop internal monitoring, rely on factory reputation, or form direct partnerships), but the current regime's specific structure would be gone.
% FOUNDING_PROBLEM: The founding problem was legitimate: garment supply chains in the 1990s-2000s had severe labor violations (child labor, forced overtime, wage theft, unsafe conditions) that were largely invisible to brands and consumers. Brands faced activist pressure and reputational risk. Third-party audits emerged to address the problem: creating transparency, documenting conditions, and providing accountability for code-of-conduct violations. The founding mandate was worker protection through verified compliance.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested by different stakeholders: (1) Brands claim the problem has been substantially addressed (audits work, conditions have improved); (2) Worker-rights organizations report that serious violations persist in many audited factories and that audit theater masks continued exploitation; (3) Independent researchers find mixed evidence: some conditions have improved in some factories, but wage suppression and speed-ups have intensified; (4) Workers themselves report that visible coercion has decreased but structural pressure has increased. The attestation most independent from the benefiting parties comes from labor-rights NGOs and academic research: both indicate that the founding problem (systematic labor violations) remains live, but that audits are insufficient to address it and increasingly function as a legitimacy mechanism for brands rather than a protection mechanism for workers. The regime itself claims the problem is 'dead' (audits prove compliance), but this claim is self-interested because audits are the beneficiary of that narrative.
narrative_ontology:disappearance_verdict(garment_supplychain_audit_theater, world_rearranges).
narrative_ontology:founding_problem_status(garment_supplychain_audit_theater, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GARMENT WORKER (SNARE) — Trapped in the compliance regime by economic necessity. Visible coercion (audit performance, wage suppression, speed-up cycles) paired with opaque beneficiary accountability. No exit capacity; bears full cost of extraction while audit theater masks the mechanism from brand consumers. Maximum experienced extraction.
constraint_indexing:constraint_classification(garment_supplychain_audit_theater, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FACTORY MANAGEMENT (TANGLED ROPE) — Coordinates production scale and supply chain logistics (genuine coordination function) while simultaneously extracting from workers via audit-theater compliance cycles. Constrained by audit schedules and certification requirements but can pass costs downstream. Mixed experience: coordination necessary, extraction embedded in the mechanism.
constraint_indexing:constraint_classification(garment_supplychain_audit_theater, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAND CORPORATION (ROPE) — Experiences the audit regime as coordination: certified supply chain legitimizes brand claims and manages reputational risk. Net beneficiary with high arbitrage options (can switch suppliers, regions, audit firms). The regime coordinates their sourcing while protecting their margin. Low experienced extraction.
constraint_indexing:constraint_classification(garment_supplychain_audit_theater, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AUDIT INTERMEDIARY FIRMS (ROPE) — Benefit from sustained audit demand and certification revenue. Arbitrage option: can exit individual brand relationships. Experience the regime as pure coordination — they solve the brand's verification problem. Revenue tied to audit volume, not to worker outcomes, creating structural misalignment. Low experienced extraction.
constraint_indexing:constraint_classification(garment_supplychain_audit_theater, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLIANCE CERTIFICATION SYSTEM (PITON) — The audit ritual persists through institutional inertia despite atrophying function. Theater ratio (0.78) reveals that compliance audits have become primarily performative: standardized checklists, scheduled inspections, favorable scoring pressure, audit avoidance gaming. The original function (protecting workers) has degraded; the ritual continues to extract resources and legitimacy claims. Maintained because alternatives haven't fully displaced it, not because it works.
constraint_indexing:constraint_classification(garment_supplychain_audit_theater, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical/civilizational vantage, the regime genuinely coordinates global supply chains (legitimate function) while simultaneously extracting from workers through audit gaming, cost pressure transfer, and informational asymmetry. The constraint cannot be described as pure extraction (Snare) because coordination function is real; cannot be described as pure coordination (Rope) because asymmetric extraction is structural. The theatrical dimension (0.78) indicates the extractive mechanisms are deliberately obscured by the audit performance.
constraint_indexing:constraint_classification(garment_supplychain_audit_theater, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(garment_supplychain_audit_theater_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(garment_supplychain_audit_theater, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(garment_supplychain_audit_theater, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(garment_supplychain_audit_theater, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(garment_supplychain_audit_theater, TR),
    TR >= 0.70.

:- end_tests(garment_supplychain_audit_theater_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, rising. The regime extracts through multiple channels: (1) wage suppression enforced by brand pressure for cost reduction; (2) unpaid audit-preparation labor cycles; (3) speed-ups disguised as production targets needed for audit compliance; (4) information asymmetry (workers lack access to audit reports and brand compliance decisions). The metric rises from 0.58 to 0.68 over the interval because extraction becomes more sophisticated: as visible individual-level suppression decreases (audit theater's cosmetic function), structural and organizational extraction intensifies. The core extraction — profit margin transfer to brands and auditors — remains constant; the obscuration improves. Suppression (0.62): Stable-to-rising. This constraint declares `suppression` as the raw structural property (unscaled), not modulated by context. Individual-level suppression decreases (0.58 → 0.48) as audit theater aestheticizes the regime, but organizational-level suppression increases (0.62 → 0.71) as factory management hardens compliance mechanisms and restricts collective worker voice. Class-level suppression rises (0.54 → 0.68) as independent worker organizing becomes harder. The net effect is suppression substitution, not reduction: visible individual coercion is replaced with invisible structural coercion. Theater ratio (0.78): High, rising. Audit protocols operate primarily through performance of legitimacy rather than substantive worker condition improvement. Standardized checklists cannot capture factory-floor realities; audit schedules allow advance preparation; favorable scoring pressure creates audit-pass incentives unaligned with worker welfare; post-audit reversions are common. Theater increases as brand demand for certification grows and auditors optimize for scalability over rigor. Accessibility collapse (0.71): High. Workers in global garment supply chains face nearly complete closure of exit alternatives: geographic mobility requires capital; alternative employment in garment-producing regions is structurally similar; relocation out of industry requires asset accumulation impossible at garment wages; political boundaries and visa restrictions prevent arbitrage. The audit regime further collapses alternatives by making non-compliant factories difficult to join (audit blacklisting). Resistance (0.55): Moderate, declining. Worker resistance takes multiple forms: individual departure, silent non-compliance, informal organizing, occasional strikes. The metric declines (0.62 → 0.58 at class level) because audit theater suppresses collective organizing while individual exit capacity remains bounded. Organized resistance requires class consciousness and collective action capacity; the regime systematically undermines both.
 *
 * PERSPECTIVAL GAP:
 *   This constraint displays maximum perspectival divergence. The garment worker sees a snare: economic entrapment, visible coercion cycles, no exit, no accountability. Factory management sees tangled rope: they coordinate genuine production and supply logistics while simultaneously extracting from workers; constrained but not powerless. The brand corporation sees rope: the audit regime solves their certification and reputational risk problem; they are net beneficiaries with arbitrage options. Audit intermediaries see rope: pure coordination of verification services; revenue streams with exit options. The compliance system itself appears as piton: the audit ritual persists through inertia despite degraded function; theater ratio (0.78) indicates the mechanism is primarily performative. The analytical observer sees tangled rope at civilizational scale: real coordination function (global supply networks genuinely need synchronization) paired with real extraction (asymmetric cost, information, and political power distribution). The perspectival gap opens because directionality differs sharply: workers have high d (victims, trapped) yielding high χ (experienced extraction); brands have low d (beneficiaries, arbitrage) yielding negative χ (experienced subsidy); factory management has moderate d, experiencing both coordination benefits and extraction costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from structural position: (1) beneficiary status and exit options determine baseline d for each agent; (2) the engine's sigmoid f(d) transforms d into experienced extractiveness (χ). Workers: d ≈ 1.0 (victims, trapped exit) → f(d) ≈ high χ. Brands: d ≈ 0.1 (beneficiaries, arbitrage exit) → f(d) ≈ negative χ (subsidy). Factory management: d ≈ 0.55 (secondary to both coordination and extraction) → f(d) ≈ moderate χ. Audit intermediaries: d ≈ 0.2 (beneficiaries, arbitrage exit) → f(d) ≈ low χ. The directionality divergence is the mechanism: the same structural constraint produces radically different experienced extractiveness depending on which seat the agent occupies. No directionality override is needed here because the base derivation captures the true asymmetry: workers are trapped victims; brands are mobile beneficiaries; factory management is constrained secondary actors. The beneficiary/victim declarations in base_properties correctly identify the flow direction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy unresolved. The constraint's mandate (protect garment worker rights and safety through verified compliance) has partially degraded but formally persists. The original problem the regime was built to solve — unsafe conditions, wage theft, child labor — remains live in many factories, but the audit mechanism has become insufficient to address it. Theater rising (0.52 → 0.78) while structural extraction steady (0.68) or rises suggests the mandate is being displaced by a new function: legitimacy provision for brands. The regime's primary function has shifted from worker protection to brand risk management without formal acknowledgment. This is mandatrophy in motion: the original mandate is theoretically active (audits still claim to address safety and rights), but the operative mandate has become certification provision. Resolution requires either (1) restoration of the mechanism to genuine worker protection (high cost, high resistance from brands and auditors), or (2) formal transition to a new mandate (reframing audits as supply-chain legitimacy theater, not worker-protection mechanism — this would complete the mandatrophy but acknowledge it). The constraint remains unresolved because institutions invest in the old mandate's language while operating under the new mandate's incentives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    audit_avoidance_gaming_extent,
    'How much of measured ''compliance'' reflects actual workplace condition improvement versus sophisticated audit evasion and gaming?',
    'Longitudinal worker interview data; comparison of audit findings vs. independent worker-directed assessment; analysis of post-audit condition reversions',
    'If gaming dominates (>70%): suppression and theater ratio should be reclassified higher; the regime is primarily extractive masquerading as coordination. If improvement is genuine (>50%): extraction remains but coordination function is more substantive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(audit_avoidance_gaming_extent, empirical, 'Extent of audit gaming versus genuine compliance').

omega_variable(
    worker_visibility_of_extraction_mechanism,
    'Can workers distinguish between legitimate production coordination demands and engineered audit-theater compliance cycles that extract unpaid labor?',
    'Worker perception studies; analysis of which audit requirements correlate with worker complaints vs. production efficiency gains; exit-survey data from workers who leave the industry',
    'If workers perceive mechanism clearly: suppression should be lower (workers have epistemic clarity); resistance should be higher (organized response possible). If mechanism is opaque: suppression is higher; the theatrical component obscures the extraction, trapping workers cognitively as well as materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_visibility_of_extraction_mechanism, empirical, 'Worker transparency regarding extraction mechanism').

omega_variable(
    audit_schedule_versus_production_needs,
    'Are audit inspection schedules genuinely random and unannounced, or do they predictably align with garment seasons when factories can absorb preparation costs?',
    'Statistical analysis of audit timing across factories, seasons, and order cycles; interviews with audit schedulers and factory management about planning pressure',
    'If schedules are strategically timed to low-impact periods: the regime''s suppression and theater ratio are intentionally managed by the intermediaries (institutional coordination of an extraction mechanism). If schedules are random: timing advantage is happenstance, not designed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_schedule_versus_production_needs, empirical, 'Audit schedule timing relative to production cycles').

omega_variable(
    beneficiary_accountability_asymmetry,
    'What enforceable accountability exists for brands if audit data proves false, factories violate findings, or workers report conditions contradicting certified reports?',
    'Legal analysis of audit contract terms; case study review of brand response to audit failure disclosures; comparison of worker restitution clauses vs. brand financial penalties',
    'If accountability is asymmetric (workers have no recourse, brands face only reputational risk): the extraction mechanism is protected by institutional design. If accountability is symmetric (enforceable standards apply equally): the regime''s extractive character is partially constrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_accountability_asymmetry, conceptual, 'Asymmetric accountability protecting extraction').

omega_variable(
    theater_ratio_measurement_basis,
    'How much of theater ratio (0.78) reflects genuine performance of procedural legitimacy versus measurement artifact of standardized audit protocols themselves?',
    'Deconstruction of audit protocols: what proportion of checklist items address worker-identified concerns vs. brand risk categories vs. auditor operational convenience? Comparison of auditor training focus (worker welfare vs. certification maintenance)',
    'If protocols are worker-centered but execution is theatrical: theater is implementation drift, fixable. If protocols themselves privilege theater over substance: the regime is designed as extractive masquerading as coordination. The claimed_type (tangled_rope) survives either way, but the mandatrophy mechanism differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_measurement_basis, conceptual, 'Sources of theater in audit protocol design').

omega_variable(
    individual_suppression_lowering_mechanism,
    'As individual-level visible coercion decreases (per audit-theater design), does suppression of collective worker organizing increase proportionally?',
    'Time-series analysis of union organizing attempts, worker committees, collective grievance mechanisms pre- vs. post-certification regimes; correlation between audit introduction and suppression of independent worker voice institutions',
    'If collective suppression rises as individual suppression theaters down: the regime is substituting invisible structural suppression for visible individual coercion (sophisticated extraction mechanism). If both decrease: suppression itself is declining. If both increase: the regime compounds extraction at multiple levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_suppression_lowering_mechanism, empirical, 'Trade-off between individual and collective suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(garment_supplychain_audit_theater, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(garment_audit_tr_t0, garment_supplychain_audit_theater, theater_ratio, 0, 0.52).
narrative_ontology:measurement_basis(garment_audit_tr_t0, observed).
narrative_ontology:measurement(garment_audit_tr_t5, garment_supplychain_audit_theater, theater_ratio, 5, 0.68).
narrative_ontology:measurement_basis(garment_audit_tr_t5, observed).
narrative_ontology:measurement(garment_audit_tr_t10, garment_supplychain_audit_theater, theater_ratio, 10, 0.78).
narrative_ontology:measurement_basis(garment_audit_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(garment_audit_be_t0, garment_supplychain_audit_theater, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(garment_audit_be_t0, observed).
narrative_ontology:measurement(garment_audit_be_t5, garment_supplychain_audit_theater, base_extractiveness, 5, 0.63).
narrative_ontology:measurement_basis(garment_audit_be_t5, observed).
narrative_ontology:measurement(garment_audit_be_t10, garment_supplychain_audit_theater, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(garment_audit_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(garment_audit_su_t0, garment_supplychain_audit_theater, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(garment_audit_su_t0, observed).
narrative_ontology:measurement(garment_audit_su_t5, garment_supplychain_audit_theater, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(garment_audit_su_t5, observed).
narrative_ontology:measurement(garment_audit_su_t10, garment_supplychain_audit_theater, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(garment_audit_su_t10, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=10
narrative_ontology:measurement(garment_audit_grid_01, garment_supplychain_audit_theater, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(garment_audit_grid_02, garment_supplychain_audit_theater, accessibility_collapse(class), 10, 0.68).
narrative_ontology:measurement(garment_audit_grid_03, garment_supplychain_audit_theater, accessibility_collapse(individual), 0, 0.78).
narrative_ontology:measurement(garment_audit_grid_04, garment_supplychain_audit_theater, accessibility_collapse(individual), 10, 0.82).
narrative_ontology:measurement(garment_audit_grid_05, garment_supplychain_audit_theater, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(garment_audit_grid_06, garment_supplychain_audit_theater, accessibility_collapse(organizational), 10, 0.71).
narrative_ontology:measurement(garment_audit_grid_07, garment_supplychain_audit_theater, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(garment_audit_grid_08, garment_supplychain_audit_theater, accessibility_collapse(structural), 10, 0.76).
narrative_ontology:measurement(garment_audit_grid_09, garment_supplychain_audit_theater, resistance(class), 0, 0.62).
narrative_ontology:measurement(garment_audit_grid_10, garment_supplychain_audit_theater, resistance(class), 10, 0.58).
narrative_ontology:measurement(garment_audit_grid_11, garment_supplychain_audit_theater, resistance(individual), 0, 0.42).
narrative_ontology:measurement(garment_audit_grid_12, garment_supplychain_audit_theater, resistance(individual), 10, 0.38).
narrative_ontology:measurement(garment_audit_grid_13, garment_supplychain_audit_theater, resistance(organizational), 0, 0.51).
narrative_ontology:measurement(garment_audit_grid_14, garment_supplychain_audit_theater, resistance(organizational), 10, 0.48).
narrative_ontology:measurement(garment_audit_grid_15, garment_supplychain_audit_theater, resistance(structural), 0, 0.55).
narrative_ontology:measurement(garment_audit_grid_16, garment_supplychain_audit_theater, resistance(structural), 10, 0.62).
narrative_ontology:measurement(garment_audit_grid_17, garment_supplychain_audit_theater, stakes_inflation(class), 0, 0.51).
narrative_ontology:measurement(garment_audit_grid_18, garment_supplychain_audit_theater, stakes_inflation(class), 10, 0.58).
narrative_ontology:measurement(garment_audit_grid_19, garment_supplychain_audit_theater, stakes_inflation(individual), 0, 0.68).
narrative_ontology:measurement(garment_audit_grid_20, garment_supplychain_audit_theater, stakes_inflation(individual), 10, 0.72).
narrative_ontology:measurement(garment_audit_grid_21, garment_supplychain_audit_theater, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(garment_audit_grid_22, garment_supplychain_audit_theater, stakes_inflation(organizational), 10, 0.71).
narrative_ontology:measurement(garment_audit_grid_23, garment_supplychain_audit_theater, stakes_inflation(structural), 0, 0.64).
narrative_ontology:measurement(garment_audit_grid_24, garment_supplychain_audit_theater, stakes_inflation(structural), 10, 0.75).
narrative_ontology:measurement(garment_audit_grid_25, garment_supplychain_audit_theater, suppression(class), 0, 0.54).
narrative_ontology:measurement(garment_audit_grid_26, garment_supplychain_audit_theater, suppression(class), 10, 0.68).
narrative_ontology:measurement(garment_audit_grid_27, garment_supplychain_audit_theater, suppression(individual), 0, 0.58).
narrative_ontology:measurement(garment_audit_grid_28, garment_supplychain_audit_theater, suppression(individual), 10, 0.48).
narrative_ontology:measurement(garment_audit_grid_29, garment_supplychain_audit_theater, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(garment_audit_grid_30, garment_supplychain_audit_theater, suppression(organizational), 10, 0.71).
narrative_ontology:measurement(garment_audit_grid_31, garment_supplychain_audit_theater, suppression(structural), 0, 0.61).
narrative_ontology:measurement(garment_audit_grid_32, garment_supplychain_audit_theater, suppression(structural), 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(garment_supplychain_audit_theater, resource_allocation).
narrative_ontology:affects_constraint(garment_supplychain_audit_theater, fast_fashion_cost_pressure).
narrative_ontology:affects_constraint(garment_supplychain_audit_theater, worker_collective_bargaining_suppression).
narrative_ontology:affects_constraint(garment_supplychain_audit_theater, audit_intermediary_capture).

% DUAL FORMULATION NOTE:
% The garment supply chain audit theater is downstream of brand demand for cost reduction (fast_fashion_cost_pressure) and upstream of worker organizing suppression (worker_collective_bargaining_suppression). The audit mechanism mediates both: it translates cost pressure into worker-level speed-ups while obscuring the mechanism; it suppresses organizing by making independent worker action appear as audit violation. The audit intermediary capture constraint (audit_firms increasingly dependent on brand budgets, incentivized toward favorable audits) is concurrent, not upstream/downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
