% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__competitive_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__competitive_moat_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article17_erasure_right__competitive_moat_reading
 *   human_readable: Article 17 Erasure Right as Competitive Moat (Right to Be Forgotten)
 *   domain: technology_governance/data_protection/competition_policy
 *
 * SUMMARY:
 *   Article 17 of the GDPR establishes the 'right to be
 *   forgotten'—individuals can request erasure of their personal data from
 *   controllers. This story instantiates ONE reading of this contested
 *   kernel: Article 17 functions as incumbent protection via compliance cost
 *   asymmetry and technical infrastructure requirements. In this reading, the
 *   nominal privacy right is genuine but operates as a competitive moat.
 *   Incumbents with distributed cost structures absorb compliance costs;
 *   challengers cannot replicate the infrastructure at sub-billion-user
 *   scale. The constraint coordinates privacy rights (real coordination
 *   function) AND asymmetrically extracts from challengers via barrier
 *   amplification. This is a Tangled Rope: it solves a genuine coordination
 *   problem (data retention accountability) AND operates as enforced
 *   asymmetric extraction (compliance cost barrier). The claim/metric
 *   divergence is structural, not error: the regulation CLAIMS to be privacy
 *   protection; the authored metrics describe substantial extractive
 *   operation (0.68 extractiveness, rising over time from 0.48 to 0.68 as
 *   incumbents optimize moat strategies).
 *
 * KEY AGENTS:
 *   - incumbent_digital_platforms: Institutional power, arbitrage-level exit, globally distributed, benefit from compliance moat — d near beneficiary end (~0.2)
 *   - challenger_platforms: Moderate power, constrained exit, regional scope, bear fixed compliance costs — d near target end (~0.75)
 *   - startups_building_data_services: Powerless, trapped exit, national scope, unable to afford compliance infrastructure at scale — d at full target (~0.95)
 *   - eu_regulators: Institutional power, analytical exit, continental scope, set and enforce the rules — agenda-setter seat
 *   - individuals_in_eu: Organized power, constrained exit, benefit from nominal right but unequally exercise it — beneficiary seat
 *   - competition_authorities: Institutional power, analytical exit, observe and potentially intervene on competition grounds — observer seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, 0.68).
domain_priors:suppression_score(article17_erasure_right__competitive_moat_reading, 0.42).
domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__competitive_moat_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__competitive_moat_reading, "Article 17 Erasure Right as Competitive Moat (Right to Be Forgotten)").
narrative_ontology:topic_domain(article17_erasure_right__competitive_moat_reading, "technology_governance/data_protection/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__competitive_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__competitive_moat_reading, '04c385dc-3e2e-4619-a352-a67fbc7f207f').
narrative_ontology:cs_kernel_codification('04c385dc-3e2e-4619-a352-a67fbc7f207f', fixed_text).
narrative_ontology:cs_authority_grounding('04c385dc-3e2e-4619-a352-a67fbc7f207f', extraction).
narrative_ontology:cs_interpretation_layer_present('04c385dc-3e2e-4619-a352-a67fbc7f207f').
narrative_ontology:cs_reading_relation('04c385dc-3e2e-4619-a352-a67fbc7f207f', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('04c385dc-3e2e-4619-a352-a67fbc7f207f', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('04c385dc-3e2e-4619-a352-a67fbc7f207f', foundational, compliance_cost_asymmetry_competitively_significant).
narrative_ontology:cs_axiom_status(compliance_cost_asymmetry_competitively_significant, holdable).
narrative_ontology:cs_axiom_grounding('04c385dc-3e2e-4619-a352-a67fbc7f207f', compliance_cost_asymmetry_competitively_significant, empirically_contingent).
narrative_ontology:cs_axiom('04c385dc-3e2e-4619-a352-a67fbc7f207f', secondary, deletion_infrastructure_as_barrier_to_entry).
narrative_ontology:cs_axiom_status(deletion_infrastructure_as_barrier_to_entry, holdable).
narrative_ontology:cs_axiom_grounding('04c385dc-3e2e-4619-a352-a67fbc7f207f', deletion_infrastructure_as_barrier_to_entry, instrumental).
narrative_ontology:cs_reference_frame('04c385dc-3e2e-4619-a352-a67fbc7f207f', privacy_protection_through_data_sovereignty).
narrative_ontology:cs_drift_state('04c385dc-3e2e-4619-a352-a67fbc7f207f', contemporary_post_2023, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('04c385dc-3e2e-4619-a352-a67fbc7f207f', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__competitive_moat_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, incumbent_digital_platforms).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, large_data_controllers).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, challenger_platforms).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, startups_building_data_services).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, emerging_market_entrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, individuals_in_eu).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, data_brokers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large established platforms (Google, Facebook, Amazon) have existing compliance infrastructure, dedicated legal teams, and distributed data architectures already in place. They absorb Article 17 compliance costs as a fixed institutional overhead. Their size allows them to spread compliance and technical costs across billions of users and vast revenue streams, reducing per-user friction. They additionally benefit from the barrier this creates: competitors cannot easily replicate their compliance engineering or legal infrastructure, making market entry prohibitively expensive.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, incumbent_digital_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, incumbent_digital_platforms, agenda_setter).

% Smaller competitors and regional platforms must build Article 17 compliance infrastructure from scratch: legal interpretation expertise, technical systems for data location and deletion, cross-border coordination, audit trails, and proof systems. These are fixed costs that do not scale with user base until the platform reaches billions-user scale. A regional social network or emerging search engine cannot afford a dedicated privacy compliance team; the cost is prohibitive relative to revenue.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, challenger_platforms, payer,
    moderate, biographical, constrained, regional).

% Early-stage data-science startups and analytics firms serving SME customers face the same Article 17 infrastructure burden as incumbents but lack the user base or revenue to distribute costs. A startup offering ML-powered customer analytics or HR data services must implement erasure-request handling, data location mapping, technical deletion verification, and potentially GDPR-compliant audit trails. Each user erasure request may require manual intervention or custom engineering. The cost per request is high; scaling to automated compliance is prohibitively expensive for a sub-100-person team.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, startups_building_data_services, payer,
    powerless, biographical, trapped, national).

% Data brokers and data aggregation services accumulate and sell datasets. Article 17 compliance requires them to maintain accurate data-lineage maps (knowing exactly where every data point sits), respond to erasure requests within 30 days, and prove deletion. For a company managing billions of data points across multiple sources and customers, this infrastructure is substantial. Compliance is often outsourced to specialized vendors, raising operational costs and reducing profit margins.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, data_brokers, payer,
    moderate, biographical, constrained, global).

% EU regulators (DPAs, national regulators, the European Commission) set and enforce the rules. They define what 'erasure' means in practice, accept compliance frameworks, and issue enforcement actions against non-compliant controllers. They face a competing mandate: protecting individual privacy rights while avoiding economic burden so severe that it distorts competition.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, eu_regulators, agenda_setter,
    institutional, generational, analytical, continental).

% EU residents have a legal right to erasure but depend on platforms to honor it. They benefit from the nominal right and the platforms' compliance machinery. In practice, those with resources to hire lawyers or advocacy organizations can more effectively exercise the right; those without must rely on platform responsiveness and voluntariness. The right exists; its exercise is unequally distributed.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, individuals_in_eu, beneficiary,
    organized, biographical, constrained, continental).

% Non-EU data controllers (U.S., Chinese, other jurisdictions) face Article 17 obligations only if they process EU residents' data. They can avoid compliance by not operating in the EU or by not storing EU residents' data in systems subject to GDPR. This is a structural choice unavailable to EU-based competitors, who cannot opt out of GDPR. Non-EU competitors can compete in the EU market without bearing the same compliance burden if they use different technical approaches (e.g., not storing persistent profiles of EU users).
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, non_eu_competitors, excluded,
    powerful, generational, constrained, global).

% Privacy advocacy groups, civil-society organizations, and academic researchers study the right to erasure. They monitor whether platforms honor requests, document selective compliance, and push for stronger enforcement. They do not collect from the constraint but observe its operation and advocate for interpretations aligned with privacy protection.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, privacy_advocates, observer,
    organized, generational, analytical, continental).

% Competition authorities (EU national authorities, DGCOMP, national merger authorities) assess whether Article 17 compliance creates or reinforces market dominance. They can use competition law to challenge platform practices that use compliance costs as anti-competitive barriers.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, competition_authorities, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__competitive_moat_reading, incumbent_digital_platforms).
narrative_ontology:fixing_cost_class(article17_erasure_right__competitive_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform legal obligation across EU-regulated entities to honor individual requests to delete personal data, preventing indefinite retention and creating accountability mechanisms. Solves the collective-action problem of fragmented data retention policies by enforcing a shared standard: individuals have a delete right, controllers must respond within 30 days, and deletion must be verifiable.
% TRANSFER_FUNCTION: Transfers compliance costs (legal expertise, technical infrastructure, operational labor) from data subjects (who would otherwise lack leverage to compel deletion) to data controllers (who bear the obligation). Also transfers a competitive advantage from smaller, leaner entrants to established players with distributed cost structures.
% ABSENT_VOICES: Non-EU competitors operating globally can structure their operations to avoid GDPR applicability; they are not absent from the policy conversation but are structurally exempted. EU-based startups and challengers who lack the resources to mount effective advocacy are underrepresented in regulatory consultations dominated by incumbent platforms' legal teams. They cannot afford to participate in rulemaking.
% DISAPPEARANCE_RATIONALE: If Article 17 and its enforcement disappeared overnight, EU residents would lose the nominal right to deletion; data controllers would no longer face uniform compliance obligations; smaller competitors would face lower infrastructure barriers to entry (though they would compete in a market with less data privacy protection). Incumbent platforms would lose the compliance moat protecting their market position but would also no longer bear the compliance costs. The competitive landscape would shift substantially.
% FOUNDING_PROBLEM: Unchecked data retention by platforms created lasting digital records and privacy harms; individuals had no effective mechanism to prevent indefinite persistence of their data in corporate systems.
% FOUNDING_PROBLEM_CORROBORATION: Privacy advocates and individuals attest the founding problem remains live: platforms retain data far longer than users expect, and deletion requests are often ignored or delayed (documented in NGO reports and academic studies, e.g., Norwegian Consumer Council investigations). Incumbents attest the problem is solved through Article 17 compliance. Competition authorities and emerging competitors attest the founding problem's solution has been weaponized as an entry barrier, shifting the constraint from privacy protection to market protection.
narrative_ontology:disappearance_verdict(article17_erasure_right__competitive_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__competitive_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__competitive_moat_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__competitive_moat_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__competitive_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__competitive_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 (at interval end) because the compliance cost burden falls asymmetrically on challengers; incumbents absorb it as institutional overhead and use it as a moat. Suppression is moderate (0.42) because the constraint does not require coercive silencing of alternatives—it is a regulatory cost that makes alternatives unaffordable. Theater ratio is 0.48 (approaching 0.5) because the compliance operation is half genuine privacy protection (real coordination function) and half moat maintenance (exclusionary effect): incumbents genuinely delete data AND strategically maintain the infrastructure as a barrier. Accessibility collapse is 0.71 because challengers cannot realistically exit GDPR or avoid compliance; alternatives (structured data in non-EU bases, minimal profiling) are technically constrained and competitive disadvantages. Resistance is 0.59 because startups and challengers resist the cost burden but lack the power to change the regulatory framework; incumbents defend it; the regulation is stable despite opposition from below. The measurement trajectory shows extractiveness rising from 0.48 to 0.68 over the interval, indicating that as incumbents mature their compliance infrastructure and optimize cost distribution, the moat deepens—initial chaos (early compliance, high per-unit costs for all) gives way to entrenched advantage (large platforms achieve economies of scale, smaller entrants remain priced out). Theater ratio rises from 0.32 to 0.48, indicating that while real data-deletion activity continues, an increasing share of the infrastructure is devoted to proving compliance and maintaining the barrier rather than to enabling user control.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent and challenger seats compute radically different constraint types. From the incumbent seat (institutional power, arbitrage exit, beneficiary): Article 17 is a well-behaved Rope—it coordinates privacy rights, they comply, the infrastructure spreads over billions of users, cost per user is negligible, they benefit from the legitimate right. From the challenger seat (moderate-to-powerless power, trapped exit, payer): Article 17 is a Snare—it imposes a fixed compliance cost that is impossible to escape, it prices them out of the market, and the regulation persists because incumbents defend it and regulation-makers cannot see the asymmetry (they only see the beneficiary side: privacy protection is real). The engine computes BOTH seats' perspectives from the structural data and surfaces the divergence. The agenda-setter (regulator) and observer (competition authority) seats add additional readings: regulators see coordination (privacy protection is the goal), competition authorities see competitive harm (the moat effect is a side effect they may remedy). This multi-seat structure is the core of the tangled-rope diagnosis: the constraint genuinely coordinates AND asymmetrically extracts, and different seats experience fundamentally different constraint types.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents derive d near 0.15-0.25 (beneficiary end): they hold institutional power, arbitrage-level exit options (they can restructure data architectures if needed, operate globally, negotiate with regulators), and clearly benefit from the compliance moat. Challengers derive d near 0.75-0.95 (target end): they hold powerless-to-moderate power, trapped exit (they cannot leave GDPR or avoid compliance, they cannot restructure as globally as incumbents), and bear the asymmetric cost. Individuals derive d near 0.5 (symmetric): they genuinely benefit from the privacy right (low d) but also bear diffuse costs (higher d from compliance-driven pricing and reduced competitive choice in data services). Regulators and competition authorities are analytical seats with d at 0.5 (neutral observation). The derivation is driven by (1) beneficiary/victim declarations (incumbents explicitly benefit, challengers explicitly pay), (2) exit options (incumbents have arbitrage, challengers have trapped or identity-locked), and (3) power asymmetry (institutional vs. moderate/powerless). No directionality overrides needed; the derivation captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unchecked data retention, privacy harms from indefinite persistence) was live and real in 2014-2017; Article 17 addressed it coordinately. By 2023-2025, the problem has been substantially solved—major platforms honor deletion requests at scale, deletion mechanisms are implemented and audited, individual privacy protection is credibly offered. However, the regulation persists and has acquired a second function: incumbent protection via compliance moat. The founding-problem-status is contested: privacy advocates say the problem remains live (platforms still retain more data than users realize, deletion enforcement is incomplete); incumbents say it is solved; competition authorities say the original coordination function is solved but the constraint has accumulated a new extractive function (the moat). This is a textbook mandatrophy scenario: the original mandate (privacy protection) has been achieved; the constraint persists for a different reason (incumbent competitive protection). The classification prevents misreading this as a failed Rope or a pure Snare. It is a Tangled Rope specifically because BOTH the coordination and extraction components are active: deletion rights are genuinely exercised and enforced, AND the compliance cost is genuinely asymmetric. Removing Article 17 would harm privacy (coordination loss); leaving it unchanged perpetuates competitive distortion (extraction persistence). The remedy is not abolition but restructuring: equivalent-treatment technical standards that lower compliance costs for challengers, or exemptions for small-scale processors, or baseline compatibility layers that let challengers plug into compliance infrastructure. Recognizing mandatrophy does not eliminate the constraint; it clarifies what kind of remedy is needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_cost_measurement_ambiguity,
    'What is the true marginal cost for a challenger platform to build and maintain Article 17 compliance infrastructure, versus the average cost distributed across an incumbent''s installed base?',
    'Detailed cost-accounting studies from challenger platforms and startups, regulatory data-call responses (e.g., from competition authorities), or contractor quotes for compliance-as-a-service solutions serving SME platforms.',
    'A measured cost ratio of >10:1 (challenger per-user cost to incumbent per-user cost) would strongly support the moat diagnosis; a ratio <2:1 would suggest the asymmetry is weaker and the constraint may be closer to a Rope. The extractiveness metric depends on this ratio; the higher the cost asymmetry, the more extractive the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_cost_measurement_ambiguity, empirical, 'Scale and cost asymmetry between incumbent and challenger compliance infrastructure.').

omega_variable(
    reading_boundary_ambiguity,
    'Does Article 17 compliance primarily enable privacy protection (fundamental reading) or primarily function as a competitive moat (competitive-moat reading), or are both effects equally present?',
    'Comparative analysis of privacy outcomes and competitive structure before and after Article 17 implementation; regulatory assessments of whether Article 17 enforcement correlates more strongly with privacy protection or with competitive disadvantage for challengers; survey of affected parties'' primary motivations for compliance.',
    'If evidence shows privacy protection dominates and moat is incidental, reclassify toward Rope (coordination-dominant). If evidence shows moat dominates and privacy is theater, reclassify toward Snare (extraction-dominant). If both effects are structurally inseparable (deletion compliance requires the same infrastructure that creates the moat), the Tangled Rope classification is confirmed and remedies must address both effects simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether the privacy protection and competitive moat functions are separable or structurally coupled.').

omega_variable(
    accessibility_collapse_mechanism,
    'Is the high accessibility collapse (0.71) driven by legal barriers (GDPR is mandatory and cannot be opted out of) or by technical barriers (the infrastructure cost is prohibitive) or by market barriers (challengers cannot fundraise adequately to cover compliance costs)?',
    'Analysis of alternative market strategies (non-EU bases, minimal profiling, federated infrastructure) and their competitive viability; case studies of startups that attempted to compete under Article 17 and the barriers they hit.',
    'If legal barriers dominate, the constraint is inescapable and accessibility collapse is accurately high. If technical barriers dominate, solutions may emerge from compliance-as-a-service or open-source infrastructure. If market barriers dominate (limited venture capital for compliance-heavy startups), the issue is venture-capital structure, not the constraint itself. This distinction informs remedy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_collapse_mechanism, empirical, 'Mechanism driving alternatives'' collapse: legal mandate, technical difficulty, or market structure.').

omega_variable(
    incumbent_strategic_moat_maintenance,
    'Do incumbents actively lobby for stricter Article 17 enforcement, fund compliance infrastructure as a service to smaller players to neutralize the moat, or treat the moat as a competitive advantage they defend?',
    'Analysis of regulatory comments filed by incumbents vs. challengers; documentation of compliance-as-a-service offerings (whether incumbents actively market them to competitors); lobbying records and positions on Article 17 amendment or clarification proposals.',
    'If incumbents actively lobby for stricter enforcement and defend the moat, that confirms the extraction diagnosis and suggests the moat is intentional. If incumbents share compliance infrastructure or support standard-setting that lowers barriers, the moat may be incidental and the constraint may be moving toward Rope. The theater ratio measurement reflects this: rising theater ratio suggests increasing strategic moat maintenance over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_strategic_moat_maintenance, empirical, 'Whether incumbents actively defend the compliance-cost moat as a competitive advantage.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the competitive-moat reading''s core premise (compliance cost asymmetry is a primary function of Article 17) logically foreclose the privacy-fundamental reading (privacy protection is the primary function), or do both premises remain coherent within different frameworks?',
    'Logical analysis: if one party claims ''Article 17 exists to protect privacy'' and another claims ''Article 17 functions to protect incumbents,'' can both be true within the SAME commitment framework (e.g., GDPR''s founding documents and legislative intent)? Or does one reading require denying the other?',
    'If the readings foreclose each other, they represent a genuine logical conflict and the regulation is being read in contradictory ways (one is wrong). If they coexist, both effects are real and the regulation is doing both things simultaneously—privacy protection AND moat creation. This distinction determines whether the constraint is a straightforward Rope (privacy) or a Tangled Rope (both effects), and affects remedy design (can you keep privacy protection while removing the moat, or are they coupled?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Logical relationship between the privacy-fundamental and competitive-moat readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__competitive_moat_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__competitive_moat_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t3, article17_erasure_right__competitive_moat_reading, theater_ratio, 3, 0.38).
narrative_ontology:measurement_basis(arti_tr_t3, observed).
narrative_ontology:measurement(arti_tr_t6, article17_erasure_right__competitive_moat_reading, theater_ratio, 6, 0.43).
narrative_ontology:measurement_basis(arti_tr_t6, observed).
narrative_ontology:measurement(arti_tr_t9, article17_erasure_right__competitive_moat_reading, theater_ratio, 9, 0.46).
narrative_ontology:measurement_basis(arti_tr_t9, observed).
narrative_ontology:measurement(arti_tr_t12, article17_erasure_right__competitive_moat_reading, theater_ratio, 12, 0.47).
narrative_ontology:measurement_basis(arti_tr_t12, observed).
narrative_ontology:measurement(arti_tr_t15, article17_erasure_right__competitive_moat_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(arti_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__competitive_moat_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t3, article17_erasure_right__competitive_moat_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement_basis(arti_be_t3, observed).
narrative_ontology:measurement(arti_be_t6, article17_erasure_right__competitive_moat_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(arti_be_t6, observed).
narrative_ontology:measurement(arti_be_t9, article17_erasure_right__competitive_moat_reading, base_extractiveness, 9, 0.65).
narrative_ontology:measurement_basis(arti_be_t9, observed).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__competitive_moat_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(arti_be_t12, observed).
narrative_ontology:measurement(arti_be_t15, article17_erasure_right__competitive_moat_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(arti_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__competitive_moat_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t3, article17_erasure_right__competitive_moat_reading, suppression_requirement, 3, 0.37).
narrative_ontology:measurement_basis(arti_su_t3, observed).
narrative_ontology:measurement(arti_su_t6, article17_erasure_right__competitive_moat_reading, suppression_requirement, 6, 0.39).
narrative_ontology:measurement_basis(arti_su_t6, observed).
narrative_ontology:measurement(arti_su_t9, article17_erasure_right__competitive_moat_reading, suppression_requirement, 9, 0.41).
narrative_ontology:measurement_basis(arti_su_t9, observed).
narrative_ontology:measurement(arti_su_t12, article17_erasure_right__competitive_moat_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(arti_su_t12, observed).
narrative_ontology:measurement(arti_su_t15, article17_erasure_right__competitive_moat_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(arti_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__competitive_moat_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__competitive_moat_reading, 0.14).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, article17_erasure_right__censorship_mechanism_reading).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, gdpr_data_controller_liability).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, data_portability_competitive_dynamics).

% DUAL FORMULATION NOTE:
% This is one of three readings of the article17_erasure_right kernel. The privacy_fundamental_reading treats Article 17 as privacy protection with minimal extraction (Mountains/Rope); this reading (competitive_moat) treats it as Tangled Rope with substantial extraction via barrier amplification. The censorship_mechanism_reading treats it as a Snare enabling suppression. All three readings coexist; they are not alternatives but represent different parties' structural positions and interests. The privacy reading is upstream (GDPR's stated intent); the competitive-moat reading influences it (documents how a well-intentioned right becomes a moat); the censorship reading is partially downstream (depends on the right existing to weaponize it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
