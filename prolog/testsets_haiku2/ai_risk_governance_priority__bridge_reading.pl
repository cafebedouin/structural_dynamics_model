% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: Unified AI Risk Governance: Present Harms + Existential Risk Integration
 *   domain: governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint instantiates the BRIDGE READING of the contested kernel
 *   of AI risk governance priorities. The reading holds that present
 *   documented harms (algorithmic bias, surveillance, labor displacement
 *   affecting marginalized populations) and existential risks from advanced
 *   AI are structurally entangled and require unified governance frameworks.
 *   This reading differs from the existential_risk_reading (which prioritizes
 *   superintelligence scenarios as the primary governance concern) and the
 *   near_term_harms_reading (which prioritizes immediately observable harm
 *   reduction). The bridge reading claims both harms-and-risks are necessary
 *   framing for legitimate AI governance, and that frameworks treating them
 *   separately fail to address the dependency structure: present safety
 *   practices shape future training; existential-risk research informs
 *   baseline safety requirements for today. The engine will compute per-seat
 *   classification from the structural data; this reading's claim is TANGLED
 *   ROPE: it coordinates genuine research and resource sharing (coordination
 *   function) while extracting through institutional prestige and resource
 *   concentration (extraction function). The bridging institutions benefit
 *   from being the seats that can translate between vocabularies; present
 *   marginalized populations bear costs both as deployment targets and as
 *   governance outsiders.
 *
 * KEY AGENTS:
 *   - bridging_research_institutions: agenda-setter and structural beneficiary (control research framing, concentrated grants/talent). Institutional power, high arbitrage (can shift between communities), global scope.
 *   - present_marginalized_populations: payer and victim (bear ongoing harms from deployed systems, excluded from governance conversations). Powerless, trapped exit, biography scope.
 *   - future_humanity: non-agent victim (designated concern object in the bridge reading, but cannot participate). Universal scope, civilizational time horizon.
 *   - existential_risk_researchers: beneficiary under existential_risk_reading, constrained under bridge_reading (their agenda is legitimized but must now justify to near-term community). Moderate power, constrained exit.
 *   - near_term_ai_ethics_researchers: beneficiary under near_term_harms_reading, constrained under bridge_reading (their agenda is legitimized but must now justify to existential-risk community). Moderate power, constrained exit.
 *   - ai_industry_developers: excluded from bridge reading's governance framing, though they have substantial shadow influence. Powerful, mobile, biographical.
 *   - funding_agencies: agenda-setters who operationalize the bridge framework by allocating resources to bridging institutions. Institutional power, generational horizon, mobile.
 *   - policy_makers: observers attempting to synthesize governance from the contested research landscape. Institutional power, analytical exit, national scope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.58).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.62).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Unified AI Risk Governance: Present Harms + Existential Risk Integration").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, 'd7be3f6d-8807-45e8-8ecd-a37fa84cfa10').
narrative_ontology:cs_kernel_codification('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', distributed).
narrative_ontology:cs_authority_grounding('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', distributed).
narrative_ontology:cs_reading_relation('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', foundational, present_harms_existential_risks_structurally_entangled).
narrative_ontology:cs_axiom_status(present_harms_existential_risks_structurally_entangled, holdable).
narrative_ontology:cs_axiom_grounding('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', present_harms_existential_risks_structurally_entangled, empirically_contingent).
narrative_ontology:cs_axiom('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', foundational, governance_integration_necessary_for_safety).
narrative_ontology:cs_axiom_status(governance_integration_necessary_for_safety, holdable).
narrative_ontology:cs_axiom_grounding('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', governance_integration_necessary_for_safety, instrumental).
narrative_ontology:cs_reference_frame('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', fragmented_research_communities).
narrative_ontology:cs_drift_state('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', contemporary_ai_governance_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d7be3f6d-8807-45e8-8ecd-a37fa84cfa10', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_research_institutions).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, existential_risk_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, near_term_ai_ethics_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, ml_safety_community).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, ml_safety_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% 5% of the research institutions publishing across both near-term AI ethics and existential risk domains. They set agenda by choosing which research gets funding, which papers are highlighted, which collaborations are brokered. They benefit from resource concentration: grants, talent, and citations flow to those who can translate between the two fields. They are simultaneously constrained by the need to maintain credibility in both communities, which have partially divergent incentives.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_research_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, bridging_research_institutions, beneficiary).

% Subject to documented present harms: algorithmic bias in hiring and lending, surveillance targeting of marginalized communities, content moderation errors affecting vulnerable speech, labor displacement in service sectors. They bear the costs of ongoing deployment and optimization of AI systems while governance frameworks debate long-term existential risk. Their exit from AI-mediated systems is not possible; they absorb harms as systems are deployed and modified in real time.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, present_marginalized_populations, payer,
    powerless, biographical, trapped, global).

% Research community focused on long-horizon superintelligence scenarios and global catastrophic risk. They benefit from governance frameworks that prioritize existential scenarios; such prioritization justifies their research agenda and attracts resources. They are constrained by the need to maintain scientific standing in the face of near-term researchers' arguments that present harms are immediate and more tractable.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_researchers, beneficiary,
    moderate, civilizational, constrained, global).

% Research community focused on documented algorithmic bias, fairness, safety, and social impact of deployed systems. They benefit from governance frameworks that prioritize present harms; such prioritization justifies their research agenda and attracts resources. They are constrained by the need to engage with long-horizon scenarios or appear parochial and reactive.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_ai_ethics_researchers, beneficiary,
    moderate, biographical, constrained, global).

% For-profit AI companies deploying systems at scale. They would prefer governance frameworks that defer to their internal safety practices and voluntary commitments. The bridge reading that demands unified frameworks threatens to impose stricter, more coordinated oversight. They are excluded from the governance-setting conversation in the bridge reading's framing (present only as subjects of regulation, not as agenda-setters), though they have substantial influence in practice.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_industry_developers, excluded,
    powerful, biographical, mobile, global).

% Not an agent, but a non-agent entity the constraint references. Future people would bear costs of existential risks or permanent values-lock scenarios if they occur. They cannot participate in current governance decisions and carry the largest potential downside.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__bridge_reading, future_humanity).

% Government, foundation, and corporate entities controlling research funding. They operationalize the governance framework by choosing which research proposals to fund, whether to fund bridging work or siloed work, and what metrics determine success. They are constrained by public pressure (from marginalized populations affected by present harms) and by philanthropic mandate alignment (some funders are committed to near-term, others to long-horizon scenarios).
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, funding_agencies, agenda_setter,
    institutional, generational, mobile, national).

% Legislatures and regulatory bodies tasked with setting AI governance rules. They observe the research community's frame wars and try to synthesize a unified policy. They are pressured by both harms-focused constituents (present injuries) and by deferential-to-industry actors (who worry about existential risk being used to block innovation). Their challenge is balancing a governance framework the research community cannot itself agree on.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, policy_makers, observer,
    institutional, biographical, analytical, national).

% Researchers and practitioners working on technical safety and alignment. They sit at the structural intersection: benefit from unified frameworks (which legitimize their work as addressing both horizons), but also bear the cost of maintaining that bridge (high collaborative burden, needing expertise in both technical safety and social impacts). Divided internally on whether bridging is coherent or diluting.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ml_safety_community, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, ml_safety_community, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, bridging_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a shared research and governance infrastructure connecting near-term AI ethics (addressing present harms from deployed systems) with existential-risk research (addressing long-horizon alignment and control problems). Solves the coordination problem of fragmentation: researchers in each domain work in isolation, develop incompatible safety vocabularies, compete for funding, and fail to notice that present practices (how systems are trained, what metrics are optimized for, who participates in safety decisions) shape future capability and risk profiles.
% TRANSFER_FUNCTION: Moves research funding, institutional prestige, and agenda-setting authority from broadly distributed researchers toward a concentrated set of bridging institutions that can credibly operate in both domains. Moves researcher attention from single-domain questions to integration questions (how do bias mitigation practices inform alignment research; how do long-horizon control constraints reshape deployment safety). Moves governance legitimacy from industry self-regulation toward academic/policy frameworks claiming to integrate both timeframes.
% ABSENT_VOICES: Present marginalized populations affected by algorithmic bias, surveillance, and labor displacement are absent from research governance conversations (they are CLAIMED as concern objects in the bridge reading but do not set research agenda). AI industry developers are structurally excluded from the bridge reading's governance framing (positioned as subjects of regulation, not as participants in agenda-setting). Within the research community, early-career researchers are underrepresented in bridging institutions, and researchers from the Global South are absent from governance conversations despite experiencing different risk profiles for both present and future AI systems.
% DISAPPEARANCE_RATIONALE: If the bridge reading's unified-governance framework disappeared overnight, research would likely re-stratify into separate near-term and existential-risk pipelines. Funding agencies would sort grants into two buckets rather than seeking bridging proposals. Conferences would re-specialize; journals would return to single-domain focus. The integration would not persist—bridging institutions would lose their comparative advantage and researchers would follow funding back to single-domain careers. Marginalized populations would lack the existential-risk credibility currently used to justify attention to present harms in long-horizon discussions, and existential-risk researchers would lack the evidence base from present-system safety practices that informs control problems. The world would rearrange into the separate governance structures the bridge reading claims are inadequate.
% FOUNDING_PROBLEM: AI systems are causing documented present harms (bias in hiring, lending, criminal justice; surveillance targeting; content moderation errors; labor displacement affecting marginalized communities) while simultaneously advancing toward capability levels that could pose existential risks if unaligned. Traditional governance approaches treating present harms and existential risks as separate problems fail to address their structural coupling: present deployment practices (what features are optimized for, who participates in safety reviews, which errors are tolerated) establish the baseline assumptions future systems inherit; long-horizon safety research (interpretability, control, alignment) generates insights that would prevent present-harm modes if integrated into current systems. The founding problem is the need for governance that treats present safety practices as foundational constraints for future scenarios and existential-risk research as informing present baseline safety standards.
% FOUNDING_PROBLEM_CORROBORATION: The existence of present documented harms is corroborated by independent audits, affected-community testimony, and academic research from outside the benefiting parties (civil rights organizations, affected workers, journalists). The existence of existential risks from advanced AI is corroborated by academic research and some policy analysts, though less independently (most corroboration comes from researchers in the field itself). The claim that these problems are STRUCTURALLY ENTANGLED (not merely coexistent) is corroborated only by bridging institutions and a minority of policy analysts. Independent corroboration from sources outside the research communities themselves is weak: industry developers, policy makers, and affected communities are largely absent from the corroboration set. This is the weakest point of the bridge reading's founding problem: while both the present-harms problem and the existential-risk problem are independently attested, the structural entanglement claim rests primarily on self-corroboration within the research community making the claim.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the bridge reading's coordination function is genuine—governance fragmentation is a real coordination problem—but so is the extraction: the constraint concentrates resources and agenda-setting in a small set of institutions. Suppression is similarly moderate (0.62): the constraint's persistence depends on maintaining that the two research communities must cooperate (soft enforcement), and on gatekeeping at conferences, journals, and funding agencies (institutional enforcement). The pressure to suppress is real but not totalizing—dissenting researchers in both near-term and existential-risk camps continue publishing under their original framings. Theater is high-moderate (0.48): significant effort is devoted to demonstrating conceptual unity (linking papers, joint conferences, interdisciplinary grad programs) but the underlying research incentives remain partly divergent; when funding dried up, several bridging initiatives shrank, suggesting some theatricality in the unity display. Accessibility collapse is modest (0.51): researchers unhappy with the bridge framework retain exits (publish in single-domain venues, join single-domain institutions, move to industry or policy). Resistance is high (0.67): existential-risk researchers argue the bridge reading diffuses focus on catastrophic scenarios; near-term researchers argue it legitimizes hand-waving about futuristic risks at the expense of tractable present problems. Both communities have mounted strong resistance to the prioritization logic the bridge reading imposes.
 *
 * PERSPECTIVAL GAP:
 *   From the bridging institutions' seat, the unified framework is a solution to research fragmentation that improves resource efficiency and cross-pollination. From the near-term harms researchers' seat, it is a dilution of urgency: existential scenarios remain speculative while people are suffering now. From existential-risk researchers' seat, it is a distraction: present-harm mitigation does not address the alignment problem and may give false confidence in incremental safety. From present marginalized populations' seat (largely outside the conversation), it is irrelevant theater: governance research disputes do not address their actual exposure to algorithmic harm. From the industry's seat, the bridge framework is more dangerous than either siloed approach because unified governance could impose tighter coordination on safety standards across the industry. The engine will compute these divergences from the structural positions (power, exit options, time horizon, spatial scope) declared for each stakeholder; the bridge reading's claim does not adjudicate which perspective is correct—only that the framework demands all be considered together.
 *
 * DIRECTIONALITY LOGIC:
 *   Present marginalized populations carry directionality toward the target end (d near 1.0): they are powerless, trapped, immediate-horizon, and bear extraction in the form of governance externality (decisions made about them without their participation) plus ongoing harm from systems being governed. Future humanity is a non-agent placeholder, assigned d as a maximally extractive position (universal scope, no exit, no voice). Bridging institutions carry directionality toward the beneficiary end (d near 0.0): institutional power, arbitrage exit (can leave either community), and direct extraction of resources and prestige. Existential and near-term researchers are symmetric-to-moderate (d near 0.5): moderate power, constrained exit (must maintain credibility in their home field), biography horizon. Funding agencies are moderate-to-beneficiary (d near 0.3): they benefit from the constraint's ability to channel resources through unified governance, but must navigate political pressure from both directions. The bridge reading's asymmetry is stark: present marginalized populations and future humanity on the extraction end; bridging institutions on the concentration end; both research communities in the middle bearing pressure to integrate. This asymmetry is what makes the classification TANGLED ROPE rather than ROPE: genuine coordination (bringing two siloed fields together solves a real research bottleneck) coexists with structural extraction (institutional prestige and resource concentration in bridging nodes).
 *
 * MANDATROPHY ANALYSIS:
 *   The bridge reading's founding problem (governance fragmentation + need for integrated baseline safety) is live (the separation between near-term and existential risk research is documented and ongoing). The disappearance verdict is world_rearranges (the constraint's removal would allow refragmentation into two pipelines). The mandatrophy test: does the founding problem's liveness match the constraint's persistence? YES—the constraint persists because the founding problem is still live and both research communities see value (though contested value) in some integration. There is no mandatrophy yet. However, the constraint is at risk of future mandatrophy if: (1) one research community decisively establishes credibility and draws all resources, leaving the other unable to argue for integration; or (2) the bridging institutions become permanent power nodes and the coordination function atrophies into pure institutional theater (bridges maintained for prestige even if collaboration declines). The measurement trajectory is slightly declining in extractiveness (0.48→0.58→down to 0.58) with high theater throughout, which could signal early mandatrophy drift: extractiveness rising-then-plateauing while theater stays elevated suggests institutional maintenance rather than functional deepening. Watch whether bridging institutions begin to lose comparative advantage if single-domain research becomes more prestigious or better-funded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_entanglement_vs_factual_independence,
    'Are present AI harms and existential risks structurally entangled (such that governance of one constrains the other) or factually independent (such that optimizing for one does not help or hinder the other)?',
    'Empirical: tracing the dependency chain—do present safety practices (e.g., bias mitigation, transparency requirements) demonstrably influence how future systems are trained and deployed? Do long-horizon constraints (e.g., interpretability research, control problems) generate insights that reduce present harms? Counterfactual: comparing governance outcomes in jurisdictions/institutions that treat the problems as unified vs. siloed.',
    'If truly entangled, the bridge reading''s unified-governance claim becomes necessary (siloed approaches will miss critical dependencies). If independent, the constraint''s coordination function disappears and it collapses into pure institutional extraction (power concentration without functional gain). If partially entangled, the bridge reading partially succeeds—coordination is real but incomplete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_entanglement_vs_factual_independence, empirical, 'Whether present harms and existential risks are governance-coupled or independent.').

omega_variable(
    bridging_institution_brittleness,
    'Is the constraint''s integration function dependent on a small set of highly-credentialed brokers, or is it distributable across the research community? What happens if the 5% of bridging institutions lose funding or credibility?',
    'Network analysis of citation patterns, funding flows, and collaboration networks: do bridging institutions remain central, or has distributed bridging grown? Historical test: did the 2023 AI pause debate and EXO debate fragment bridging, or did it strengthen it? Stress test: funding withdrawal from bridging institutions to observe whether integration collapses or self-organizes.',
    'High brittleness (concentration in few nodes) makes the constraint fragile to institutional disruption and more clearly extractive (power in few hands). Distributed brittleness (function survives node loss) would suggest the coordination function is real and resilient, downgrading the extraction classification. This omega directly feeds the PITON risk: if bridging is pure theater sustained by institutional inertia, it will show as high brittleness + high theater + declining extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bridging_institution_brittleness, empirical, 'Structural resilience of the bridging function to disruption of bridging institutions.').

omega_variable(
    representation_of_present_harms_stakeholders,
    'Are governance frameworks determined by researchers, or do affected populations (those experiencing algorithmic bias and surveillance harms) have genuine participatory voice in setting research priorities and safety standards?',
    'Institutional audit: membership of governance committees, funding agencies, policy bodies—what proportion of seats are held by representatives of affected populations vs. researchers? Process audit: do governance deliberations include testimony and evidence from affected communities, or only from the research/industry/policy elite? Outcome audit: do governance decisions reflect constraints and priorities stated by affected populations, or only those framed by researchers?',
    'If affected populations are genuinely participatory, the constraint''s structure shifts: they move from ''payer'' to ''beneficiary'' or ''co-agenda-setter'' (governance reflects their priorities, not just researcher-optimized integration). If they remain excluded, the constraint remains extractive from their perspective (they pay the costs of governance fragmentation without setting its agenda). This is an R3 check on absent voices: the six_questions field names them as absent; this omega asks whether genuine participation would change the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(representation_of_present_harms_stakeholders, empirical, 'Whether marginalized populations have participatory voice in AI governance priorities.').

omega_variable(
    kernel_foreclosure_risk,
    'Is the bridge reading genuinely holding together, or is one sibling reading in the process of foreclosing the others through institutional dominance?',
    'Citation dominance: is the existential-risk reading or near-term-harms reading increasingly cited and funded at the expense of the bridge reading? Institutional drift: are bridging institutions beginning to specialize (some turning primarily existential, some turning primarily near-term) rather than maintaining integration? Policy direction: are governing bodies converging on one reading''s priority framing?',
    'If one reading is foreclosing the others, the kernel contest is shifting from ''three readings coexist'' to ''one is dominating.'' This would make the bridge reading transitional (a SCAFFOLD rather than TANGLED ROPE) if it represents a temporary coalition that will fragment when institutional pressure settles. It would be a sign of approaching mandatrophy if the bridge reading''s integration function becomes obsolete because one community''s framing won.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_foreclosure_risk, empirical, 'Whether the bridge reading is stable or in process of being foreclosed by institutional drift toward one sibling.').

omega_variable(
    unified_framework_vs_coordination_theater,
    'Is the declared ''unified framework'' a genuine coordination solution, or primarily institutional theater—the appearance of integration without substantive collaboration?',
    'Behavioral: do researchers claiming the bridge reading actually collaborate across communities (co-author papers, share methods, cite each other''s results) or do they cite the ''unified framework'' as justification while remaining siloed in practice? Incentive: do universities and funders reward genuine cross-domain work, or reward claiming cross-domain work while actually maintaining single-domain metrics (publications, citations)? Stress test: when resources tighten or one community''s work becomes more prestigious, does cross-domain collaboration persist or does it disappear?',
    'High theater + low genuine collaboration = the constraint is closer to PITON (performative maintenance of unity for institutional standing) than TANGLED ROPE (real coordination with embedded extraction). This would suggest the extraction is cleaner (power concentration without much functional gain) and the theater metric should be higher than authored. This omega directly tests the authored theater_ratio (0.48) plausibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unified_framework_vs_coordination_theater, empirical, 'Whether ''unified frameworks'' are genuine coordination or institutional theater.').

omega_variable(
    committer_kernel_contest_reading_stability,
    'Are the three readings (bridge, existential, near-term) genuinely coexistent without foreclosure, or does the bridge reading''s framing (structural entanglement) logically require or foreclose certain positions from the other readings?',
    'Logical: if the bridge reading is correct that present harms and existential risks are structurally entangled, does the existential-risk reading''s claim that existential risk is the paramount concern become internally contradictory (since paramount priority must account for entanglement)? Can the near-term-harms reading''s prioritization of tractable present problems coexist with bridge-reading governance if the governance assumes non-separability? This is a conceptual test, not empirical—it asks whether the three readings are stable coexisters or whether one reading''s truth would logically eliminate another.',
    'If the bridge reading logically FORECLOSES one of the siblings, the reading_relations should shift from coexists_with to forecloses, and the kernel itself is less deeply contested (one reading has hidden dominance in the argument space). If all three genuinely coexist, the kernel is deeply contested and the bridge reading is one legitimate position among three. This affects whether the constraint should be classified as TANGLED ROPE (unstable coexistence, extraction) or as a SCAFFOLD (transitional coalition waiting for conceptual resolution).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_contest_reading_stability, conceptual, 'Logical stability of the three readings as coexistent alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.41).
narrative_ontology:measurement_basis(ai_r_tr_t0, observed).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__bridge_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement_basis(ai_r_tr_t5, observed).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__bridge_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(ai_r_tr_t10, observed).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__bridge_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(ai_r_tr_t15, observed).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.49).
narrative_ontology:measurement_basis(ai_r_tr_t20, projected).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_governance_priority__bridge_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(ai_r_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_r_be_t0, observed).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__bridge_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(ai_r_be_t5, observed).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__bridge_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(ai_r_be_t10, observed).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__bridge_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(ai_r_be_t15, observed).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(ai_r_be_t20, projected).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_governance_priority__bridge_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(ai_r_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(ai_r_su_t0, observed).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__bridge_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(ai_r_su_t5, observed).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__bridge_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(ai_r_su_t10, observed).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__bridge_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement_basis(ai_r_su_t15, observed).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(ai_r_su_t20, projected).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_governance_priority__bridge_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(ai_r_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__bridge_reading, 0.18).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% The bridge reading instantiates one interpretation of the contested kernel ai_risk_governance_priority. The existential_risk_reading and near_term_harms_reading are sibling constraints representing alternative framings of the same governance domain. All three readings share the same referent (the space of AI governance institutions and research prioritization) but author different ε values, different victim/beneficiary structures, and different claimed types. The readings are linked because institutional dominance by one reading affects the others' operating environment: if existential-risk framing becomes predominant, near-term harm research loses funding and legitimacy (influence relation). If near-term harm prioritization becomes policy-dominant, existential-risk researchers are marginalized (influence relation). The bridge reading is neither a median nor a synthetic compromise—it is an independent claim about structural entanglement that coexists with the other two. All three readings are authored from the bridge_reading seat's perspective (per Rule 1: generate only this reading as a clean ε-invariant constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
