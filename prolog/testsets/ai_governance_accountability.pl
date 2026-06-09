% ============================================================================
% CONSTRAINT STORY: ai_governance_accountability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_accountability, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_governance_accountability
 *   human_readable: AI Governance Accountability Gap
 *   domain: political_theology/technology_ethics/governance
 *
 * SUMMARY:
 *   The AI governance accountability gap describes the structural asymmetry
 *   between the locus of decision-making authority over AI systems
 *   (concentrated in private transnational corporations) and the locus of
 *   accountability mechanisms (fragmented across under-resourced national
 *   regulators and voluntary industry frameworks). This constraint emerges
 *   from the interaction of three structural features: (1) technical
 *   complexity that creates information asymmetry between developers and
 *   regulators, (2) transnational deployment that fragments regulatory
 *   jurisdiction, and (3) market concentration that gives dominant firms
 *   agenda-setting power in governance forums. The encyclical Antiqua et Nova
 *   identifies this gap as a failure of 'sound politics' (§73) and calls for
 *   governance structures that 'ensure that the development and use of
 *   technology serve the common good' (§75). The constraint exhibits genuine
 *   coordination function — international frameworks (OECD AI Principles,
 *   UNESCO Recommendation, Partnership on AI) enable dialogue and
 *   standard-setting — but embeds asymmetric extraction: affected populations
 *   bear algorithmic harms with no meaningful recourse, while tech monopolies
 *   capture the benefits of innovation without corresponding accountability.
 *   The theater_ratio (0.58) reflects that much governance activity is
 *   performative: multi-stakeholder forums provide legitimation without
 *   binding enforcement, ethics boards issue recommendations without
 *   implementation mechanisms, and transparency initiatives disclose process
 *   without enabling contestation. The EU regulatory coalition represents a
 *   structural alternative (scaffold perspective) with sunset logic: hard law
 *   (AI Act, DSA) is replacing soft governance, creating binding
 *   accountability mechanisms with enforcement teeth. Whether this sunset is
 *   real or aspirational depends on implementation — the omega on
 *   participation theater addresses this uncertainty.
 *
 * KEY AGENTS:
 *   - Tech Monopolies: Primary beneficiary (institutional/arbitrage) — capture decision authority over AI deployment, arbitrage across jurisdictions, shape governance frameworks through industry forums and regulatory capture
 *   - Affected Populations (Global South): Primary victim (powerless/trapped) — subject to algorithmic decisions with no recourse, trapped by digital dependency and lack of alternative infrastructure
 *   - Civil Society Organizations: Secondary victim (moderate/constrained) — participate in governance forums but lack resources and information access to compel accountability; mixed coordination and extraction
 *   - States Lacking Governance Capacity: Victim (institutional/constrained) — benefit from international coordination frameworks but cannot enforce domestically due to technical complexity and resource asymmetry
 *   - States with Regulatory Capture: Mixed beneficiary/victim (institutional/constrained) — benefit from industry tax revenue and innovation narrative but bear legitimacy costs of capture; constrained by political economy
 *   - EU Regulatory Coalition: Organized agents (organized/mobile) — building binding accountability mechanisms (AI Act, GDPR, DSA) with sunset logic; can exit voluntary frameworks and impose requirements
 *   - CST Analytical Position: Analytical observer (analytical/analytical) — recognizes both coordination function (international dialogue, ethical frameworks) and asymmetric extraction (private capture of authority over universal human goods)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_accountability, 0.62).
domain_priors:suppression_score(ai_governance_accountability, 0.68).
domain_priors:theater_ratio(ai_governance_accountability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_accountability, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_governance_accountability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_governance_accountability, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_accountability, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_governance_accountability, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_accountability, tangled_rope).
narrative_ontology:human_readable(ai_governance_accountability, "AI Governance Accountability Gap").
narrative_ontology:topic_domain(ai_governance_accountability, "political_theology/technology_ethics/governance").

domain_priors:requires_active_enforcement(ai_governance_accountability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_accountability, '0f6a73e2-0919-426f-800d-96e33fb52a1a').
narrative_ontology:cs_kernel_codification('0f6a73e2-0919-426f-800d-96e33fb52a1a', formalized).
narrative_ontology:cs_authority_grounding('0f6a73e2-0919-426f-800d-96e33fb52a1a', lineage).
narrative_ontology:cs_interpretation_layer_present('0f6a73e2-0919-426f-800d-96e33fb52a1a').
narrative_ontology:cs_created_at('0f6a73e2-0919-426f-800d-96e33fb52a1a', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_accountability, tech_monopolies).
narrative_ontology:constraint_beneficiary(ai_governance_accountability, states_with_regulatory_capture).
narrative_ontology:constraint_victim(ai_governance_accountability, states_lacking_governance_capacity).
narrative_ontology:constraint_victim(ai_governance_accountability, civil_society_organizations).
narrative_ontology:constraint_victim(ai_governance_accountability, affected_populations_global_south).
narrative_ontology:constraint_vindicates(ai_governance_accountability, technological_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_accountability, market_efficiency_in_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: International frameworks (OECD AI Principles, UNESCO Recommendation, Partnership on AI, Global Partnership on AI) coordinate standard-setting, information-sharing, and norm development across jurisdictions. Multi-stakeholder forums enable dialogue between industry, civil society, and governments. Transparency initiatives and ethics review processes provide procedural coordination for AI deployment decisions.
% TRANSFER_FUNCTION: Decision authority over AI systems flows from affected populations and national governments to transnational tech corporations. Algorithmic harms (discriminatory outcomes, privacy violations, manipulative design) flow from corporations to affected populations. Legitimation flows from multi-stakeholder participation to industry self-regulation. Resources (regulatory capacity, technical expertise, agenda-setting power) concentrate in dominant firms.
% ABSENT_VOICES: Affected populations in the Global South are systematically excluded from governance forums due to resource constraints, language barriers, and lack of technical expertise. Workers subject to algorithmic management are absent from labor governance discussions. Indigenous communities affected by data extraction and surveillance lack representation. Small states without regulatory capacity are present in name but lack meaningful voice. The excluded would object to: concentration of decision authority in private hands, lack of binding enforcement, performative participation without remedy mechanisms, and suppression of alternative governance models (public utility, cooperative ownership).
% DISAPPEARANCE_RATIONALE: If the governance gap disappeared overnight — if binding accountability mechanisms with enforcement teeth replaced voluntary frameworks — the world would rearrange substantially. Tech monopolies would face constraints on deployment decisions, affected populations would gain remedy mechanisms, regulatory agencies would require technical capacity-building, and market concentration would face structural challenge. The arrangements (corporate self-regulation, multi-stakeholder forums, voluntary ethics review) depend on the gap's persistence. This is not a natural fact — it is a constructed institutional arrangement that serves identifiable beneficiaries.
% FOUNDING_PROBLEM: The founding problem was coordination failure in early AI governance (circa 2015-2017): rapid AI deployment across jurisdictions with no international standards, no shared ethical frameworks, and no mechanisms for information-sharing between researchers, industry, and policymakers. The initial governance initiatives (Partnership on AI 2016, OECD AI Principles 2019) addressed genuine coordination needs: establishing common terminology, identifying shared risks, building trust between stakeholders, and creating forums for dialogue.
% FOUNDING_PROBLEM_CORROBORATION: Industry actors and captured regulators attest the founding problem remains live: they argue voluntary frameworks are maturing, binding regulation would stifle innovation, and coordination is improving incrementally. Civil society organizations and EU regulators attest the founding problem is dead: the coordination need has been met (standards exist, dialogue forums function), but the structure now serves extraction (industry captures governance to avoid accountability). Academic researchers are divided: some see ongoing coordination needs (technical complexity requires expert input), others see regulatory capture (industry expertise becomes veto power). The CST analytical position (encyclical §73-75) implicitly attests the founding problem is dead: the call for 'sound politics' and binding accountability mechanisms presumes current voluntary coordination is insufficient, and the structure now serves private power rather than common good.
narrative_ontology:disappearance_verdict(ai_governance_accountability, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_accountability, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED POPULATIONS (SNARE) — Communities subject to algorithmic decisions (credit scoring, content moderation, surveillance) with no meaningful recourse. Trapped by digital dependency and lack of alternative infrastructure. Maximum extraction: decisions made by distant actors with no accountability mechanism.
constraint_indexing:constraint_classification(ai_governance_accountability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVIL SOCIETY (TANGLED ROPE) — NGOs and advocacy groups benefit from coordination mechanisms (multi-stakeholder forums, transparency initiatives) but bear costs of asymmetric information access and resource constraints. Can participate in governance theater but cannot compel accountability. Mixed experience: some voice, limited power.
constraint_indexing:constraint_classification(ai_governance_accountability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNDER-RESOURCED STATES (TANGLED ROPE) — National governments without technical expertise or regulatory infrastructure. Benefit from international coordination frameworks (OECD AI Principles, UNESCO recommendations) but cannot enforce them domestically. Constrained by technical complexity and resource asymmetry. The coordination function is real but extraction dominates.
constraint_indexing:constraint_classification(ai_governance_accountability, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TECH MONOPOLIES (ROPE) — Primary beneficiaries. Experience governance gap as coordination: voluntary frameworks (Partnership on AI, industry standards) enable market expansion while avoiding binding regulation. Arbitrage across jurisdictions. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(ai_governance_accountability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CAPTURED REGULATORS (TANGLED ROPE) — Advanced economies (US, UK) where regulatory agencies are structurally dependent on industry expertise and revolving-door employment. Benefit from industry tax revenue and innovation narrative but bear legitimacy costs. Constrained by political economy of capture. Mixed coordination and extraction.
constraint_indexing:constraint_classification(ai_governance_accountability, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: EU REGULATORY COALITION (SCAFFOLD) — Organized actors (EU AI Act, GDPR enforcement, Digital Services Act) building binding accountability mechanisms. See current gap as temporary coordination failure with sunset logic: hard law is replacing soft governance. Mobile — can exit voluntary frameworks and impose requirements. Transitional structure with declared endpoint.
constraint_indexing:constraint_classification(ai_governance_accountability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: CST ANALYTICAL (TANGLED ROPE) — From civilizational perspective grounded in human dignity and common good, current governance structures provide genuine coordination (international dialogue, ethical frameworks) but embed asymmetric extraction (private actors capture decision authority that affects universal human goods). The encyclical's call for 'sound politics' and participatory governance recognizes both functions. Analytical classification aligns with claimed type.
constraint_indexing:constraint_classification(ai_governance_accountability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_accountability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_governance_accountability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_governance_accountability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_governance_accountability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_governance_accountability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Substantial. Tech monopolies capture decision authority over systems affecting billions while bearing minimal accountability costs. Affected populations experience algorithmic harms (discriminatory credit scoring, biased content moderation, invasive surveillance) with no meaningful remedy. The extraction is not total — some coordination mechanisms exist (transparency requirements, ethics review) — but the asymmetry is severe and growing. The value reflects that much of the 'governance' activity is theater rather than binding constraint on corporate power. Suppression (0.68): High. Multiple mechanisms suppress alternatives: technical complexity creates information asymmetry that excludes non-expert participation; transnational deployment fragments regulatory jurisdiction; market concentration gives dominant firms veto power over governance frameworks; revolving-door employment captures regulatory agencies; intellectual property regimes prevent independent audit. Resistance is substantial (civil society advocacy, EU regulation, academic criticism) but structurally disadvantaged. Theater ratio (0.58): Moderate-high. Multi-stakeholder forums, ethics boards, and transparency initiatives provide legitimation without binding enforcement. Much governance activity is performative: principles are declared, commitments are made, but implementation mechanisms are absent or voluntary. The theater has increased over the interval (0.35 → 0.58) as governance activity proliferated without corresponding accountability. Accessibility collapse (0.42): Moderate. Alternatives to current governance structure are visible and contested: public utility models, cooperative ownership, mandatory algorithmic impact assessment, independent oversight boards. The gap is not naturalized as inevitable — resistance is high (0.71) — but alternatives face structural barriers (market concentration, regulatory capture, technical complexity). Resistance (0.71): High. Substantial organized opposition from civil society, academic researchers, EU regulators, and affected communities. The encyclical itself is an act of resistance — magisterial teaching authority contesting the technocratic paradigm. Resistance is structurally disadvantaged but not absent.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates indexical classification across power and exit dimensions. Affected populations (powerless/trapped) experience pure extraction (Snare) — algorithmic decisions with no recourse. Civil society organizations (moderate/constrained) experience mixed coordination and extraction (Tangled Rope) — can participate in forums but cannot compel accountability. Under-resourced states (institutional/constrained) experience the same mixed structure from a different seat — benefit from international frameworks but cannot enforce domestically. Tech monopolies (institutional/arbitrage) experience coordination (Rope) — voluntary frameworks enable market expansion without binding constraint. Captured regulators (institutional/constrained) experience Tangled Rope from yet another angle — benefit from industry relationship but bear legitimacy costs. The EU coalition (organized/mobile) sees a temporary problem with sunset (Scaffold) — hard law is replacing soft governance. The CST analytical position (analytical/analytical) recognizes the mixed structure at civilizational scale — genuine coordination function embedded with asymmetric extraction. The perspectival gap is not about disagreement over facts but about structural position: the same governance arrangements extract from some agents while coordinating for others. The analytical classification (Tangled Rope) aligns with the claimed type because the coordination function is real (international dialogue, standard-setting, ethical frameworks) but so is the extraction (private capture of authority over universal goods).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Tech monopolies are declared beneficiaries with arbitrage exit — they capture decision authority and can exit unfavorable jurisdictions. This produces low d (near beneficiary end) and low or negative chi (experienced as coordination). Affected populations are declared victims with trapped exit — they bear algorithmic harms with no recourse and cannot exit digital dependency. This produces high d (near target end) and high chi (experienced as extraction). Civil society organizations are victims with constrained exit — they participate but lack power to compel accountability. This produces moderate-high d and moderate chi (mixed experience). Under-resourced states are victims with constrained exit — they benefit from frameworks but cannot enforce. Moderate-high d, moderate chi. Captured regulators are beneficiaries (industry revenue, innovation narrative) with constrained exit (political economy of capture) — this produces moderate d and moderate chi, but the mixed beneficiary/victim status creates the Tangled Rope classification. The EU coalition is neither pure beneficiary nor victim — they are organized agents building alternatives — with mobile exit (can impose requirements). This produces low-moderate d and low chi (experienced as temporary coordination problem). The analytical position has analytical exit and recognizes both functions — moderate d reflecting the mixed structure. No directionality overrides are needed because the structural declarations (beneficiary/victim + exit options) produce the correct d values for each perspective's classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that Tangled Rope classification captures the structural reality: genuine coordination function (international dialogue, standard-setting, ethical frameworks enable collective action on AI governance) coexists with asymmetric extraction (private actors capture decision authority over systems affecting universal human goods, while affected populations bear harms with no recourse). The coordination is not mere cover story — the frameworks do enable information-sharing, norm-setting, and capacity-building. But the extraction is also real — the governance gap concentrates power in private hands while fragmenting accountability. The CST analytical position recognizes both: the encyclical calls for 'sound politics' that can 'ensure technology serves the common good' (§75), acknowledging that current structures provide some coordination (the encyclical itself participates in international dialogue) while demanding transformation to address extraction (binding accountability, participatory governance, subsidiarity). The mandate (coordinate AI governance for common good) persists, but the structure embeds extraction alongside coordination. This is the definitional structure of Tangled Rope: both functions are real, both are necessary for classification, and neither reduces to the other. The perspectival gap (Snare from powerless/trapped, Rope from institutional/arbitrage, Scaffold from organized/mobile) demonstrates that the mixed structure is experienced differently depending on structural position, but the analytical classification integrates both functions at civilizational scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_enforcement_threshold,
    'At what point does voluntary industry self-regulation transition from coordination to extraction — when does lack of binding enforcement constitute structural suppression rather than coordination lag?',
    'Comparative analysis of voluntary vs. mandatory frameworks: compliance rates, enforcement actions, remedy availability for affected parties. Historical precedent from other industries (finance, pharmaceuticals) transitioning from self-regulation to public oversight.',
    'If voluntary frameworks show high compliance and effective remedy: coordination function dominates, Rope from more perspectives. If compliance is performative and remedy unavailable: extraction dominates, Snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_enforcement_threshold, empirical, 'Threshold distinguishing coordination lag from structural extraction in voluntary governance').

omega_variable(
    subsidiarity_implementation_ambiguity,
    'Does CST principle of subsidiarity support decentralized governance (local/national AI regulation) or require coordinated global frameworks to address transnational corporate power?',
    'Doctrinal analysis of subsidiarity in contexts of asymmetric power (Quadragesimo Anno §79-80, Centesimus Annus §48). Does subsidiarity require capacity-building at lower levels before devolving authority, or does it permit higher-level intervention when lower levels are structurally incapable?',
    'If subsidiarity requires local primacy: current global governance initiatives may violate CST principles. If subsidiarity permits intervention under incapacity: global frameworks are doctrinally warranted. Affects whether CST supports or contests current governance trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidiarity_implementation_ambiguity, conceptual, 'Whether subsidiarity principle supports decentralized or coordinated global AI governance').

omega_variable(
    participation_theater_measurement,
    'Do multi-stakeholder governance forums (Partnership on AI, Global Partnership on AI) constitute genuine participatory governance or performative inclusion that legitimates pre-determined industry positions?',
    'Process tracing of forum decisions: correlation between civil society input and policy outcomes; resource asymmetries in participation (who can afford sustained engagement); veto points and agenda-setting power. Comparison with historical cases of captured multi-stakeholder processes (Internet governance, climate negotiations).',
    'If forums show genuine influence: theater_ratio should be lower, coordination function stronger. If forums are performative: current theater_ratio (0.58) may underestimate extraction, and the scaffold perspective''s sunset logic is aspirational rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(participation_theater_measurement, empirical, 'Whether multi-stakeholder forums provide genuine participation or performative legitimation').

omega_variable(
    doctrinal_development_necessity,
    'Does AI governance require NEW doctrinal development in CST (novel principles for algorithmic dignity, data rights, automated decision-making) or APPLICATION of existing principles (dignity, common good, justice) to new context?',
    'Magisterial interpretation: does the encyclical''s language (''add my own voice,'' ''res nova'') signal doctrinal development or emphatic application? Comparison with historical doctrinal development (Dignitatis Humanae on religious freedom, Laudato Si'' on ecological conversion) vs. application (Rerum Novarum applying justice to industrial labor).',
    'If doctrinal development: CST authority structure must absorb new content, creating interpretive instability during development period. If application: existing doctrine suffices, and the encyclical''s authority is continuous with tradition. Affects cs_structure classification and interpretation_layer stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_development_necessity, conceptual, 'Whether AI requires doctrinal development or application of existing CST principles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_accountability, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_gov_theater_2015, ai_governance_accountability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ai_gov_theater_2018, ai_governance_accountability, theater_ratio, 3, 0.48).
narrative_ontology:measurement(ai_gov_theater_2021, ai_governance_accountability, theater_ratio, 6, 0.58).
narrative_ontology:measurement(ai_gov_theater_2024, ai_governance_accountability, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(ai_gov_extract_2015, ai_governance_accountability, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_gov_extract_2018, ai_governance_accountability, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(ai_gov_extract_2021, ai_governance_accountability, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(ai_gov_extract_2024, ai_governance_accountability, base_extractiveness, 9, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_gov_suppress_2015, ai_governance_accountability, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(ai_gov_suppress_2018, ai_governance_accountability, suppression_requirement, 3, 0.61).
narrative_ontology:measurement(ai_gov_suppress_2021, ai_governance_accountability, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(ai_gov_suppress_2024, ai_governance_accountability, suppression_requirement, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_accountability, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of technocratic_paradigm_vs_human_primacy. The upstream constraint describes the ideological/cultural contest between technocratic rationality and human-centered values; this constraint describes the institutional/governance structure that embeds that contest. The upstream constraint's extractiveness reflects the cultural-epistemic asymmetry (technocratic framing suppresses alternative rationalities); this constraint's extractiveness reflects the institutional-political asymmetry (private capture of public authority). Both are Tangled Rope but at different levels of analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
