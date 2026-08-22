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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: AI Alignment: Integrated Catastrophic and Present-Harm Priority
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   The integrated reading of AI alignment treats both existential
 *   catastrophic risk AND present discriminatory harms as equally valid
 *   safety concerns requiring unified governance, methodology, and resource
 *   allocation. The constraint operates at the institutional level: research
 *   communities, funding mechanisms, review boards, and red-team/audit
 *   practices are organized around a premise that catastrophic-risk work and
 *   fairness-audit work are complementary rather than competing. This
 *   constraint is ONE READING of the contested kernel
 *   'ai_alignment_priority'—alongside the existential_risk_reading (which
 *   prioritizes tail-scenario work) and the nearterm_harms_reading (which
 *   prioritizes justice for present victims). The integrated reading sits
 *   between these, claiming both are necessary and codependent. The
 *   extractiveness and suppression metrics reflect the constraint's
 *   operation: it coordinates genuine safety work across communities but does
 *   so through asymmetric resource flows, exclusionary governance structures,
 *   and pressure on dissenting researchers to conform to dual-methodology
 *   framing.
 *
 * KEY AGENTS:
 *   - integrated_alignment_advocates: institutional agenda-setters managing dual-priority research and funding
 *   - marginalized_populations_present: powerless beneficiaries nominally included but resource-starved
 *   - existential_safety_research_community: organized researchers paying via resource constraints and scope pressure
 *   - nearterm_deployment_audit_community: moderate-power researchers paying via instrumentalization of their work
 *   - ai_capability_labs: powerful agenda-setters subject to audit/red-team requirements but able to arbitrage
 *   - institutional_research_funders: institutional agenda-setters enforcing the dual-methodology gate
 *   - existential_risk_skeptics: excluded researchers unable to participate without accepting the premise
 *   - deployment_moratorium_advocates: excluded moderate-power actors whose alternative framings are incompatible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.62).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "AI Alignment: Integrated Catastrophic and Present-Harm Priority").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, 'd0f90ca7-0821-41c4-bb83-d7cff464fe87').
narrative_ontology:cs_kernel_codification('d0f90ca7-0821-41c4-bb83-d7cff464fe87', distributed).
narrative_ontology:cs_authority_grounding('d0f90ca7-0821-41c4-bb83-d7cff464fe87', extraction).
narrative_ontology:cs_reading_relation('d0f90ca7-0821-41c4-bb83-d7cff464fe87', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0f90ca7-0821-41c4-bb83-d7cff464fe87', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('d0f90ca7-0821-41c4-bb83-d7cff464fe87', foundational, dual_harm_parity_axiom).
narrative_ontology:cs_axiom_status(dual_harm_parity_axiom, holdable).
narrative_ontology:cs_axiom_grounding('d0f90ca7-0821-41c4-bb83-d7cff464fe87', dual_harm_parity_axiom, deontological).
narrative_ontology:cs_axiom('d0f90ca7-0821-41c4-bb83-d7cff464fe87', foundational, integrated_methodology_axiom).
narrative_ontology:cs_axiom_status(integrated_methodology_axiom, holdable).
narrative_ontology:cs_axiom_grounding('d0f90ca7-0821-41c4-bb83-d7cff464fe87', integrated_methodology_axiom, instrumental).
narrative_ontology:cs_reference_frame('d0f90ca7-0821-41c4-bb83-d7cff464fe87', pre_integration_fragmented_communities).
narrative_ontology:cs_drift_state('d0f90ca7-0821-41c4-bb83-d7cff464fe87', contemporary_institutional_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0f90ca7-0821-41c4-bb83-d7cff464fe87', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, marginalized_populations_present).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, future_populations_catastrophic_scenario).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, capability_safety_researchers_deferred).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, deployment_audit_capacity_constrained).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, resource_starved_nearterm_initiatives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, existential_safety_research_community).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, nearterm_deployment_audit_community).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, existential_safety_research_community).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, nearterm_deployment_audit_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research communities and policy advocates arguing that AI safety MUST serve both existential risk reduction AND present-harm prevention simultaneously. They set research agendas, allocate grant funding, shape institutional priorities, and enforce dual-methodology requirements (red-teaming + fairness audits in parallel). They face constant pressure to arbitrate resource tradeoffs between long-horizon and immediate-impact work.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, integrated_alignment_advocates, agenda_setter,
    organized, generational, constrained, global).

% Communities experiencing discriminatory deployment harms NOW: algorithmic bias in hiring, credit, policing, content moderation affecting marginalized groups disproportionately. The integrated reading nominally includes them as beneficiaries because it asserts their present suffering is a safety concern equal to existential risk. But resource allocation remains skewed toward long-horizon work; their participation in governance is minimal; their harms are instrumentalized as justification for dual-method research rather than driving resource decisions.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, marginalized_populations_present, beneficiary,
    powerless, immediate, trapped, global).

% A non-agent cohort representing potential future persons whose existence and welfare depend on successful existential-risk mitigation. The integrated reading treats them as beneficiaries of dual methodology (catastrophic risk reduction increases their probability of existence). But they have no voice in resource allocation—no testimony, no representation, no capacity to contest being bundled with present-harm prevention.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations_catastrophic_scenario, beneficiary,
    powerless, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, future_populations_catastrophic_scenario, excluded).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__integrated_reading, future_populations_catastrophic_scenario).

% Researchers working on long-horizon AI control, interpretability, and catastrophic-scenario prevention. Under the integrated reading, they are tasked to share resource pools, research teams, and institutional prioritization with near-term harm initiatives. Many report reduced funding, longer timelines, and institutional pressure to justify work in terms of present-benefit metrics rather than tail-risk reduction. Their methodologies (red-teaming, alignment research) are disciplined by dual-methodology requirements.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, existential_safety_research_community, payer,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, existential_safety_research_community, beneficiary).

% Researchers and practitioners working on fairness audits, bias detection, and immediate-harm mitigation in deployed systems. They gain legitimacy and resource access from integration into the alignment frame, but face constant epistemic pressure to map their findings onto tail-risk language or lose institutional priority. Their work on present harms is reframed as 'building safety culture' or 'identifying failure modes'—instrumentalized rather than autonomous.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, nearterm_deployment_audit_community, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, nearterm_deployment_audit_community, beneficiary).

% The organizations deploying large language models and frontier systems in production. The integrated reading constrains them: they must submit to fairness audits (nearterm) and participate in red-teaming (catastrophic risk). They can exit some constraints (choose jurisdictions with lighter governance) but not all (reputation/regulatory pressure). They have formal seats on governance boards but operate on deployment timelines that treat both categories of oversight as friction.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_capability_labs, agenda_setter,
    powerful, biographical, arbitrage, global).

% Government, foundation, and corporate entities allocating research funding. They enforce the integrated reading by requiring dual-methodology work for funding; this gives them power to shape research priorities and enforce the constraint's boundary maintenance. They justify allocation by claiming both categories must be served; in practice, catastrophic-risk work retains prestige while present-harm work competes for remainder.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, institutional_research_funders, agenda_setter,
    institutional, generational, arbitrage, global).

% Researchers and advocates who dispute that AI poses civilizational extinction risk and argue that resources devoted to tail-scenario planning are opportunity costs stealing from evidence-based present-harm work. They are structurally excluded from the integrated reading's framework: they cannot participate in governance without accepting the dual-harm premise, so their objections remain external commentary rather than internal pressure.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, existential_risk_skeptics, excluded,
    organized, biographical, constrained, global).

% Communities arguing that rapid deployment should stop until both categories of harm can be credibly mitigated. The integrated reading excludes them by framing deployment as continuous and optimization-driven (red-teaming happens in deployment, audits post-deployment). Their alternative—pausing systems until confidence rises—is incompatible with the integrated reading's incremental-refinement logic.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, deployment_moratorium_advocates, excluded,
    moderate, immediate, constrained, global).

% External researchers analyzing the governance structures of AI risk prioritization. They document the constraint's operation, measure resource flows, track outcome metrics, and publish comparative analyses of different institutional framings.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, integrated_alignment_advocates).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates two historically separate safety communities (existential-risk and fairness/bias researchers) into a shared governance structure with unified funding, institutional review, and red-team/audit methodology. Solves the collective-action problem of fragmented safety work and creates accountability pathways that span both risk timescales.
% TRANSFER_FUNCTION: Moves research capacity, institutional prestige, funding allocation, and governance authority from single-priority frameworks (pure existential risk OR pure near-term justice) toward joint methodologies and shared resource pools. Moves research timelines from long-horizon-only or immediate-only toward dual-track work. Moves decision-making authority from separate communities toward integrated institutional structures.
% ABSENT_VOICES: Researchers who reject the existential-risk premise are structurally excluded; marginalized communities whose present suffering is most acute have no seat at governance tables, only tokenized inclusion as beneficiaries; deployment-pause advocates who view continuous-optimization as incompatible with safety guarantees cannot access the framework without endorsing it; technology workers directly responsible for discriminatory outcomes are named as 'affected parties' rather than decision-makers.
% DISAPPEARANCE_RATIONALE: If the integrated reading vanished, AI safety governance would bifurcate into separate institutions, funding streams, and methodologies again: existential-risk work would return to pure control/interpretability focus; deployment auditing would revert to compliance-driven, post-hoc assessment disconnected from systemic risk framing. The resource pools, grant mechanisms, and institutional review structures currently unified would split. Present harms would lose their claimed articulation to tail-risk frameworks and might regain autonomy (or disappear from safety discourse entirely).
% FOUNDING_PROBLEM: Two legitimate safety imperatives—preventing civilizational extinction from AI and preventing discriminatory AI harms now—were operating in separate research communities with separate funding mechanisms, institutional pathways, and priority hierarchies. This fragmentation created two failure modes: existential-risk work was accused of neglecting present suffering; present-harm work was accused of missing systemic failure modes. The integrated reading was built to unify these into a single governance frame where both are 'alignment' problems.
% FOUNDING_PROBLEM_CORROBORATION: Integrated-reading advocates argue the fragmentation problem is still live: separate communities remain siloed, funding competes, methodologies diverge. Existential-risk researchers attest the founding problem is overstated—the two communities already collaborate; forcing integration creates resource pressure and scope creep. Near-term harm advocates testify the founding problem was real but integration has subordinated their work to tail-risk language rather than solving the fragmentation. Independent observers document persistent separate funding streams and parallel institutional structures despite integration rhetoric.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.62) because the integrated reading extracts research capacity, reorients timelines, and forces resource sharing without establishing credible priority mechanisms—researchers experience this as diffuse tax on their work. Suppression is elevated (0.71) because the constraint operates through exclusionary governance: you either accept the dual-harm premise or you are outside the institutional frame. Theater is moderate (0.48) because genuine red-teaming and fairness auditing occur, but a growing fraction of the ceremonial work involves demonstrating compliance with integration language rather than advancing safety. Accessibility of alternatives is moderate (0.58): researchers CAN pursue single-priority work outside institutional funding, but career pressure, legitimacy, and resource access drive conformity. Resistance is high (0.74): existential-risk skeptics publish critiques, near-term advocates chafe at instrumentalization, and existential researchers document resource constraints—active contestation is visible and persistent. The measurement series shows gradual hardening: extractiveness and suppression trend upward as institutional enforcement tightens; theater rises as governance machinery becomes more elaborate. This is NOT a convergence toward natural stability (which would flatten); it is extraction accumulation driven by institutional lock-in.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is claimed as tangled_rope (genuine coordination + asymmetric extraction) because it solves a real collective-action problem (integrating separate safety communities) while simultaneously extracting from researchers whose work is subordinated to the dual-harm frame. The existential_risk_reading sits nearby but forecloses the integrated reading's core axiom: if catastrophic risk dominates all other considerations, then treating present harms as equal-priority is a resource misallocation. The nearterm_harms_reading influences the integrated reading but identifies a structural exploitation: present victims are instrumentalized as beneficiaries to justify the institutional arrangement, but governance remains insulated from their actual demands. The integrated reading claims to coexist with both siblings (each party can hold its reading independently), but the resource and governance dynamics create pressure: to stay funded and legitimate, researchers must accept integrated framing, which forecloses skepticism about the dual-harm premise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (marginalized_populations, future_populations): inclusion as beneficiaries is nominal—resource allocation, governance participation, and actual research direction remain controlled by integrated_alignment_advocates. The derivation should yield d = low (beneficiary-coded) but the resource evidence suggests d = moderate (they benefit nominally but are constrained). No override needed—the structural data (powerless power, trapped exit, excluded governance role) already produces the right d. Victims (capability_safety_researchers_deferred, deployment_audit_capacity_constrained, resource_starved_nearterm_initiatives): these are researcher cohorts experiencing reduced resource share, longer timelines, and pressure to conform. Derivation yields d = high (targets of extraction). Payers: existential_safety_research_community (organized, constrained exit) and nearterm_deployment_audit_community (moderate power, constrained exit) are both payers—they contribute capacity and accept disciplined work without proportional resource return.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (fragmented safety communities operating in silos) is contested in status—integrated-reading advocates say it remains live; existential researchers say it is overstated; near-term advocates say the 'solution' made it worse by subordinating their work. The disappearance_verdict is world_rearranges: AI safety governance would bifurcate again if the integrated reading vanished. This mismatch (contested status + world_rearranges) triggers the mandatrophy flag: the founding problem may have outlived the arrangement that claims to solve it. The arrangement persists because institutional funders (who benefit from unified governance) enforce it; research communities (who pay the cost) cannot exit without reputational loss. This is a zombie coordination—it looks like it solved a problem, but the problem is either solved (so the arrangement should simplify) or unsolved (so the arrangement should change). The integrated reading's mandatrophy status is: CONTESTED. The arrangement exhibits theater_ratio trend upward (ceremonial governance replacing functional integration) and theater_ratio baseline moderate (half the work is real, half is compliance theater). This suggests the founding problem may be dead but the arrangement persists as institutional ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_mechanism_opacity,
    'Is the claimed dual-priority resource allocation genuinely balanced, or do budgets continue to favor existential-risk work despite integration rhetoric?',
    'Transparent accounting of research funding flows by risk category (existential vs. near-term) across funders, institutions, and time periods. Comparison with pre-integration baseline to measure actual resource reallocation.',
    'If budgets remain skewed toward existential risk despite integration claims, the constraint is pure extraction (present-harm researchers subsidize existential-risk prestige) dressed in coordination language. If budgets genuinely rebalance, the constraint is tangled_rope with more genuine coordination than measured extractiveness suggests. The measurement moves the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_allocation_mechanism_opacity, empirical, 'Whether integrated resource allocation reflects its dual-priority claim or maintains historical prestige hierarchy.').

omega_variable(
    existential_risk_empirical_dispute,
    'Is the foundational disagreement about whether catastrophic AI risk is plausible a conceptual/preference disagreement, or does it rest on empirical claims about AI capability trajectories that could be tested?',
    'Decompose the existential-risk premise into testable empirical sub-claims (alignment difficulty, scaling dynamics, capability emergence timelines) and measure them against evidence. Identify which parts are conceptual (e.g., moral weight of future persons) vs. empirical.',
    'If existential risk is primarily empirical, the integrated reading can accommodate skepticism by updating the resource split based on evidence. If it is primarily conceptual/axiological, skepticism cannot be accommodated within the same framework—the kernel admits genuinely incommensurable readings and the integrated reading is choosing a middle ground between logically disjoint positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_risk_empirical_dispute, conceptual, 'Whether the existential/nearterm disagreement is empirically resolvable or preference-dependent.').

omega_variable(
    marginalized_populations_governance_exclusion,
    'Is the powerlessness of marginalized_populations_present in AI alignment governance a necessary consequence of their technical expertise constraints, or a structural choice to keep governance insulated from redistribution demands?',
    'Examine governance board composition, decision authority, and budget allocation votes over time. Test whether increased participation by affected communities changes priorities. Audit whether ''community input'' processes are decision-making or theater.',
    'If exclusion is choice-driven, the constraint extracts legitimacy from marginalized communities (claiming to serve them) while keeping them powerless. If it is expertise-driven, the constraint''s structure is correct but its beneficiary list should exclude nominal-only beneficiaries. Either way, the classification clarifies: if the first, it is snare; if the second, it is tangled_rope with one beneficiary set smaller than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_populations_governance_exclusion, empirical, 'Whether marginalized communities'' governance exclusion is structural design or expertise artifact.').

omega_variable(
    kernel_incommensurability_reading_relations,
    'Do the three readings (existential, near-term, integrated) occupy genuinely coexisting positions held by different parties, or does the existential reading logically foreclose the integrated reading''s core axiom that both risk categories are equally valid?',
    'Formalize the existential-risk reading''s core premise (e.g., ''tail-scenario probability × impact dominates near-term harms in expected utility'') and test whether it logically entails that integrated dual-priority is suboptimal. Test whether a party can hold existential premises while endorsing integrated resource allocation.',
    'If existential risk entails that integration is suboptimal, the reading_relation from integrated to existential should be ''coexists_with'' (each party holds its own) rather than ''influences'' (integration creates pressure on existential work). If the existential axiom logically forecloses integrated priority, the relation is ''forecloses'' and the three readings are not truly stable coexistents but rather mutually exclusive possibilities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_incommensurability_reading_relations, conceptual, 'Whether the three readings are logically independent (coexisting) or logically dependent (one forecloses another).').

omega_variable(
    suppression_internalization_in_research_communities,
    'Is the measured suppression (0.71) structural—external enforcement via funding conditions and institutional review—or internalized—researchers have adopted the integrated frame as their own epistemic stance?',
    'Post-constraint-removal / exit-option-expansion analysis: if suppression persists after funding pressure is removed (e.g., researchers with independent funding, researchers who leave the field), it is partially internalized. Compare researcher rhetoric when funding-accountable vs. anonymous surveys to measure the structural vs. internalized split.',
    'If suppression is mostly structural, removing the funding gate would reduce extraction. If mostly internalized, researchers carry the constraint''s logic with them even without enforcement. If partially both (the likely case), the constraint''s effective suppression is higher than the structural measure suggests—it shapes researcher identity and not just behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_research_communities, empirical, 'Whether suppression is structural (external enforcement) or internalized (adopted as researcher identity).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__integrated_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__integrated_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__integrated_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__integrated_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__integrated_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__integrated_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__integrated_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__integrated_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__integrated_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__integrated_reading, 0.18).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel 'ai_alignment_priority'. The integrated reading claims both existential catastrophic risk AND present discriminatory harms are equally valid safety concerns requiring unified governance. The existential_risk_reading prioritizes tail-scenario work and treats near-term harms as secondary; it has high ε on catastrophic-risk extraction, low ε on deployment-audit efficiency. The nearterm_harms_reading prioritizes justice for present victims and treats catastrophic speculation as resource drain; it has high ε on instrumentalization of marginalized communities, low ε on existential-risk work's legitimacy. These are three structurally distinct constraints with different beneficiary/victim sets, different ε values, and different enforcement mechanisms, all instantiating the same kernel. They are linked here because their resource competition and institutional pressure are interdependent: the integrated reading creates pressure on both siblings by claiming to subsume them; the siblings influence back by challenging the dual-priority axiom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
