% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated AI Alignment (Capability + Deployment Harms)
 *   domain: technology/governance/ethics
 *
 * SUMMARY:
 *   The integrated reading of AI alignment treats catastrophic-risk
 *   prevention and present-harm prevention as complementary rather than
 *   competing priorities. This reading emerges from civil-rights, AI ethics,
 *   and inclusive-governance communities alongside traditional AI-safety
 *   institutions. The constraint operates as a research and governance
 *   framework: it allocates resources, sets methodologies (dual red-teaming
 *   and deployment audits), and establishes legitimacy structures that
 *   require both types of alignment work. The extractiveness is
 *   moderate-to-high (0.62) because the constraint asymmetrically privileges
 *   those with expertise and funding authority (research institutions,
 *   deployment companies) over those bearing harms (marginalized groups,
 *   resource-poor contexts). The suppression is substantial (0.71) because
 *   advocates for nearterm-harm priority and present-affected communities are
 *   structurally suppressed from equal voice in alignment governance. The
 *   theater ratio is moderate (0.42) and rising: as the integrated reading
 *   gains institutional acceptance, performative balance-talk increases while
 *   resource allocation remains skewed toward existential risk.
 *
 * KEY AGENTS:
 *   - AI safety research community (institutional beneficiary; sets research agenda)
 *   - AI deployment companies (powerful agenda-setter; absorbs compliance costs)
 *   - Marginalized groups present (powerless payers; trapped by structural exclusion)
 *   - Future populations (powerless beneficiaries; represented proxy-wise)
 *   - Present-harm advocacy (excluded; would reorder priorities)
 *   - Longtermist funding (organized beneficiary; controls resource allocation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.62).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated AI Alignment (Capability + Deployment Harms)").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "technology/governance/ethics").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '4e2c937b-74a8-4e64-84ee-b8abfaa29ac1').
narrative_ontology:cs_kernel_codification('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', distributed).
narrative_ontology:cs_authority_grounding('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', distributed).
narrative_ontology:cs_reading_relation('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', foundational, both_catastrophic_and_present_harms_require_alignment).
narrative_ontology:cs_axiom_status(both_catastrophic_and_present_harms_require_alignment, holdable).
narrative_ontology:cs_axiom_grounding('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', both_catastrophic_and_present_harms_require_alignment, deontological).
narrative_ontology:cs_axiom('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', foundational, complementary_rather_than_competing_methodology).
narrative_ontology:cs_axiom_status(complementary_rather_than_competing_methodology, holdable).
narrative_ontology:cs_axiom_grounding('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', complementary_rather_than_competing_methodology, instrumental).
narrative_ontology:cs_reference_frame('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', unified_alignment_research_agenda).
narrative_ontology:cs_drift_state('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', contemporary_institutional_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e2c937b-74a8-4e64-84ee-b8abfaa29ac1', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, affected_populations_present).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, future_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, ai_safety_research_community).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, marginalized_groups_present).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, resource_constrained_contexts).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, underrepresented_voices_governance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, longtermist_advocacy).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, affected_populations_present).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets research priorities and funding allocations; defines what 'alignment' means in academic and policy discourse. Instantiates the integrated reading through methodological choices (red-teaming plus deployment audits, not either/or). Benefits from legitimacy as the authoritative interpretive community but operates under constraint that resources allocated to capability-risk research are unavailable for nearterm-harm mitigation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_safety_research_community, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, ai_safety_research_community, beneficiary).

% Experience discrimination, manipulation, and extraction from deployed AI systems today (credit scoring, hiring, content curation, predictive policing, loan access). Benefit from governance frameworks and audits that catch discriminatory systems pre-deployment. Pay through delayed deployment of beneficial AI capabilities when resources flow to catastrophic-risk work, and through continued exposure during the research-to-deployment lag.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, affected_populations_present, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, affected_populations_present, beneficiary).

% Disproportionately harmed by deployed AI discrimination and extraction (facial recognition misidentification, algorithmic discrimination in criminal justice, predatory targeting). Trapped by systemic exclusion from AI governance discussions; their voice in priority-setting is minimal. Depend on external advocacy and regulatory mechanisms to defend their interests, but those mechanisms are often underfunded relative to existential-risk research.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, marginalized_groups_present, payer,
    powerless, biographical, trapped, global).

% Protected by alignment research that prevents catastrophic loss of control over advanced AI systems. Cannot advocate for themselves; their interests are represented proxy-wise through longtermism frameworks and civilizational-risk reasoning. Benefit structurally from both capability-risk work and deployment-harm prevention, as either failure mode (uncontrolled capability or normalized extraction) creates catastrophic paths.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations, beneficiary,
    powerless, civilizational, analytical, universal).

% Deploy AI systems for profit; operate under regulatory and reputational pressure to address nearterm harms. Resist capability-risk priorities that slow deployment; absorb compliance costs from deployment audits. Have significant influence over which alignment research gets funded (through partnerships, grants, hiring) and which gets deprioritized.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_deployment_companies, agenda_setter,
    powerful, biographical, mobile, global).

% Non-wealthy jurisdictions and organizations that cannot afford duplicate AI auditing/governance infrastructure; depend on global standards and research infrastructure. When research resources concentrate on existential risk, nearterm-harm governance in these contexts remains underfunded, leaving populations vulnerable to imported harms. When deployment work concentrates on wealthy-market use cases, resource-poor contexts receive less scrutiny.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, resource_constrained_contexts, payer,
    moderate, biographical, constrained, national).

% Funding institutions and researchers who prioritize civilizational-scale risk. Benefit from the integrated reading's inclusion of both existential and nearterm concerns (legitimacy boost: 'not ignoring present harms'). Have substantial budget control and can allocate resources toward their interpretation of alignment research.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, longtermist_advocacy, beneficiary,
    organized, civilizational, mobile, global).

% Activists and organizations defending marginalized groups against AI discrimination, extraction, and surveillance. Often excluded from technical AI-safety governance conversations; their priority-setting voice is marginalized. Would argue for reallocation of safety resources toward deployment audits, fairness evaluation, and participatory governance if they had commensurate seat at the table.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, present_harm_advocacy, excluded,
    moderate, biographical, constrained, global).

% Enforce alignment standards and governance across jurisdictions; depend on research community to define what alignment means and how to measure it. Operate under pressure from both deployment companies (regulatory capture risk) and civil-society organizations (transparency/accountability demands). Must navigate the capability-vs-deployment priority contest without clear technical consensus.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, regulatory_bodies, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, ai_safety_research_community).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces unified research and governance methodology that addresses catastrophic misalignment risks AND present-harm prevention simultaneously, preventing siloed approaches that treat them as competing rather than complementary priorities. Enables resource allocation and standard-setting that serve both futures and presents.
% TRANSFER_FUNCTION: Moves research attention, funding allocation, and governance authority away from pure existential-risk frames toward integrated priority-setting; simultaneously moves resources from present-harm communities (who lose funding for long-term research) and from deployment companies (who face dual-auditing requirements). Establishes shared legitimacy structure where both capability and deployment risks must be addressed in every major AI project.
% ABSENT_VOICES: Present-harm advocates, marginalized communities bearing harms, resource-poor jurisdictions, and nearterm-affected populations are underrepresented in the research institutions and funding bodies that adjudicate alignment priorities. Their absence from governance tables means the integrated reading is set by those with existential-risk expertise rather than those living present harms.
% DISAPPEARANCE_RATIONALE: If the integrated reading vanished and alignment reverted to pure existential-risk framing, resources would flow more heavily toward technical capability research (red-teaming, interpretability, scalable oversight) and away from deployment auditing and fairness evaluation. Marginalized populations would face higher near-term discrimination risk; resource-poor jurisdictions would receive less governance infrastructure. If it reverted to pure nearterm-harms framing, existential-risk research would be defunded, and the civilizational-scale threats would receive minimal attention—a different rearrangement of the same world.
% FOUNDING_PROBLEM: Early AI deployment (2015–2025) revealed severe present harms (algorithmic discrimination, extractive data practices, surveillance systems harming marginalized groups) while existential-risk research institutions and funding operated in parallel, with little structural connection. Neither community adequately addressed the other's priorities, creating a false choice between addressing catastrophic risks and preventing present injustice.
% FOUNDING_PROBLEM_CORROBORATION: Deployment-harm researchers and civil-rights advocates attest the founding problem is live and the present-only reading is insufficient. Existential-risk researchers and longtermists attest the founding problem is real but subordinate to civilizational risks. Independent audits of deployed AI systems (from outside both advocacy camps) confirm both present harms are real and widespread. The integrated reading is endorsed by researchers (e.g., FAccT community, some longtermists) who argue both are necessary; it is contested by purists on both sides (existential-risk isolationists who see nearterm work as distraction; justice advocates who see longtermism as resource-stealing).
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.62 reflects asymmetric resource allocation: the integrated reading legitimizes work on both types of harms, but funding and governance authority remain concentrated among those with existential-risk expertise, not among those bearing present harms. Present-harm research receives proportionally less funding, and marginalized communities pay through continued exposure to deployed AI discrimination while resources flow to long-term research. Suppression at 0.71 is high because the present-harm advocacy community is structurally excluded from governance conversations—their voice in priority-setting is mediated through research institutions that may not represent their interests. Theater at 0.42 and rising reflects institutional adoption: the integrated reading now appears in funding priorities and governance frameworks, but actual resource allocation remains skewed, and much of the visible commitment to 'addressing both' is rhetorical cover for continued existential-risk prioritization. Accessibility collapse at 0.58 is moderate: the integrated reading presents itself as a unified framework, making alternatives (pure existential-risk or pure nearterm-harms readings) seem untenable once the integrated logic is accepted; however, the underlying priority contest remains live and alternatives are still advocated. Resistance at 0.73 is substantial: existential-risk isolationists resist the constraint as diluting focus on civilizational threats; deployment-first advocates resist it as legitimizing existential-risk prioritization; the constraint is actively contested by both flanks.
 *
 * PERSPECTIVAL GAP:
 *   From the research-community and longtermist seat, the integrated reading is genuine coordination that prevents false choices and optimizes civilization-scale outcome. From the marginalized-groups and present-harm-advocacy seats, the same reading is extractive cover: it legitimizes resource allocation to existential work while nominally acknowledging present harms, without proportionally funding the present-harm side. From the deployment-companies seat, it creates dual-compliance burdens (both red-teaming and deployment audits) that are economically expensive and operationally disruptive. The engine computes these divergent positions from the structural data—research institutions and longtermism funding have mobility and arbitrage exits (they can fund other work, migrate to other domains); marginalized communities and resource-poor contexts have trapped or identity-locked exits (they cannot exit the systems that harm them); deployment companies have mobile exits (they can relocate to lighter-regulation jurisdictions).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the research community and longtermist funding is near-beneficiary (d ≈ 0.2): they benefit from legitimacy and funding flow, with high exit mobility. Directionality for marginalized groups and resource-poor contexts is near-target (d ≈ 0.85): they bear costs (continued exposure to discrimination, underfunded governance), with trapped or identity-locked exits—they cannot exit the AI systems that govern their lives. Directionality for affected populations (present but not marginalized) is more symmetric (d ≈ 0.55): they benefit from governance that catches discriminatory systems, but pay through deployment delays and continued lag periods. Directionality for deployment companies is moderate (d ≈ 0.5): they benefit from the legitimacy boost of 'addressing both,' but pay compliance costs and face dual scrutiny. Present-harm advocates are excluded, so their directionality is high (d ≈ 0.9): they would pay through continued resource deprioritization and are not at the table to negotiate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested: present-harm advocates and deployment-harm researchers assert it is live (AI discrimination is happening now, governance is underfunded); existential-risk purists assert it is subordinate (the founding problem is existential risk, nearterm work is triage). The integrated reading asserts both are live and complementary, but the constraint's operation diverges from this claim: extractiveness and theater-ratio measurements show that resource allocation and governance authority remain skewed toward existential risk. The constraint is not mandatrophy'd—the founding problem (choosing between existential and nearterm safety) has not been solved—but the measured operation (skewed allocation despite integrated rhetoric) suggests the constraint may be decaying into a hybrid snare/piton: it coordinates a unified research agenda (rope function) while extracting from nearterm-harm communities (snare function) and theatrically maintains balance-talk while actual allocation drifts (piton function). The mandatrophy flag does not apply because the founding problem remains active and contested, not dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    priority_contest_under_scarcity,
    'Is the apparent resource competition between existential-risk and nearterm-harm research genuine (both compete for scarce research attention and funding), or is it manufactured by institutional structures that could accommodate both if reordered?',
    'Counterfactual: if a new funding mechanism existed with no budget constraint, what would integrated research look like? Do the competing communities identify the same methodological priorities, or are the priorities structurally opposed (requiring trade-off choices even with adequate resources)?',
    'If genuine structural opposition, the integrated reading is a false synthesis that must eventually choose; if manufactured by scarcity, the integrated reading is viable and requires only resource reallocation. Classification consequence: integrated rope vs. disguised snare/rope hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priority_contest_under_scarcity, empirical, 'Whether resource competition reflects genuine methodological opposition or institutional constraint.').

omega_variable(
    representation_and_extraction_asymmetry,
    'Does the integrated reading genuinely address present-harm communities (they have voice in priority-setting) or does it extract from them (it uses their concerns for legitimacy while maintaining their exclusion from resource allocation)?',
    'Governance audit: track decision-making authority (who votes, who funds) vs. rhetoric (what alignment claims to include). Compare resource allocation to present-harm communities with their stated priorities. If allocation tracks rhetoric, the reading is genuine coordination; if rhetoric diverges from allocation, the reading is extractive cover.',
    'If extractive cover, the constraint is snare (extraction from marginalized groups) riding a rope coordination function (unified research agenda). If genuine inclusion, the constraint approaches rope (coordination) with some asymmetry (those with expertise gain more authority than those bearing harms). Classification consequence: tangled-rope with high suppression vs. snare with coordination framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(representation_and_extraction_asymmetry, empirical, 'Whether the constraint genuinely includes marginalized voices or extracts their concerns for legitimacy.').

omega_variable(
    temporal_coupling_between_readings,
    'Does working on nearterm harms (deployment audits, fairness research) methodologically improve or hinder work on existential risks? Are the readings truly complementary in practice, or do they create different skill requirements and attention demands that make genuine integration difficult?',
    'Research community survey or publication analysis: do researchers who work on nearterm harms also publish on existential risk, or do they form separate career paths? Does investment in fairness evaluation infrastructure improve or delay existential-risk infrastructure?',
    'If genuinely complementary (dual focus strengthens both), the integrated reading is robust rope; if they compete for researcher attention and create separate communities, the integrated reading is aspirational narrative covering partial coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_coupling_between_readings, empirical, 'Whether nearterm and existential-risk research are methodologically complementary or competing.').

omega_variable(
    reading_contest_kernel_ambiguity,
    'Is the ai_alignment_priority kernel a genuine strategic disagreement (different communities prioritize different risks for defensible reasons), or is it a displacement of a deeper disagreement about whose interests count (existential-risk research privileges civilizational futures; nearterm-harm research privileges present marginalized groups)?',
    'Discourse analysis: when communities contest priority, do they dispute empirical facts about risk magnitude (existential vs. nearterm), or do they dispute whether present suffering and future catastrophe are commensurable goods to weigh? If they dispute incommensurability, the kernel is not really about priority—it is about value frameworks.',
    'If genuine strategic disagreement, the integrated reading can mediate by allocating resources to both; if dispute is about incommensurable values, the integrated reading is false synthesis and must eventually choose. Classification consequence: tangled-rope with real coordination vs. false-synthesis snare masquerading as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel_ambiguity, conceptual, 'Whether the priority contest reflects strategic disagreement or value-framework incommensurability.').

omega_variable(
    institutional_capture_in_integrated_framing,
    'Does the integrated reading enable or prevent capture by deployment companies? If companies adopt ''we address both existential and nearterm risks,'' can they use this framing to defuse regulatory pressure while continuing extractive practices?',
    'Policy and market study: track deployment-company commitments to fairness/auditing vs. actual resource allocation post-integrated-reading adoption. Does public commitment to ''addressing both'' correlate with increased or decreased fairness investment?',
    'If integrated reading is captured (rhetoric without resource follow-through), the constraint becomes snare with deployment-company extraction. If it enables enforcement (companies held to dual standards), it remains tangled-rope with meaningful asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_in_integrated_framing, empirical, 'Whether integrated-reading framing enables deployment-company regulatory capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__integrated_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__integrated_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__integrated_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_priority__integrated_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__integrated_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__integrated_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__integrated_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_priority__integrated_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__integrated_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__integrated_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__integrated_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_priority__integrated_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__integrated_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_alignment_priority kernel. The existential_risk_reading and nearterm_harms_reading are sibling constraints, not alternative framings of this one. All three share the same referent (the standing AI-governance arrangements they contest) but author different ε values reflecting different readings of what 'alignment' requires and what harms matter most. The network link indicates these are not independent—existential-risk work and nearterm-harm work structurally influence each other's legitimacy, funding, and institutional position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
