% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety Dual Priority Mandate
 *   domain: technology_governance/ai_safety
 *
 * SUMMARY:
 *   The dual_priority_reading instantiates the claim that AI safety
 *   governance must address existential risk from future superintelligent
 *   systems AND documented near-term harms from current deployed systems as
 *   non-competing, simultaneously prioritized objectives. This reading
 *   emerged from the ai_safety_commitment kernel as a synthesis position
 *   attempting to unify the field's fractured risk portfolio. Its structural
 *   signature: it names the union of both risk populations as its beneficiary
 *   set (everyone affected by AI risk across timescales) but extracts from
 *   both specialist communities by demanding they accept resource dilution
 *   and methodological compromise. The constraint requires active enforcement
 *   through funding mandates, institutional charters, and peer review norms
 *   that penalize single-priority work. Over the 2015-2023 interval,
 *   extraction has risen as the field professionalized and the 'both/and'
 *   framing became a gatekeeping credential — researchers and organizations
 *   must perform dual-priority commitment to access resources, regardless of
 *   their actual comparative advantage.
 *
 * KEY AGENTS:
 *   - ai_safety_research_community: Primary beneficiary (institutional/moderate) — gains unified field legitimacy and funding access through the dual-priority frame
 *   - existential_risk_specialists: Primary victim (organized/constrained) — pressured to dilute long-horizon work with near-term deliverables to maintain funding
 *   - near_term_harms_advocates: Primary victim (organized/constrained) — pressured to frame present-day harms as 'existential risk relevant' to satisfy the mandate
 *   - resource_constrained_organizations: Secondary victim (moderate/trapped) — small labs and global south actors bear highest coordination cost for dual-priority compliance
 *   - policy_makers_seeking_comprehensive_regulation: Secondary beneficiary (institutional/arbitrage) — gain a single governance object that claims to cover all AI risk
 *   - public_interest_advocates: Tertiary beneficiary (organized/mobile) — gain rhetorical leverage from the comprehensive frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.35).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.25).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety Dual Priority Mandate").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technology_governance/ai_safety").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, 'f163b596-81c9-446e-aaee-bc219edddbed').
narrative_ontology:cs_kernel_codification('f163b596-81c9-446e-aaee-bc219edddbed', distributed).
narrative_ontology:cs_authority_grounding('f163b596-81c9-446e-aaee-bc219edddbed', distributed).
narrative_ontology:cs_reading_relation('f163b596-81c9-446e-aaee-bc219edddbed', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('f163b596-81c9-446e-aaee-bc219edddbed', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('f163b596-81c9-446e-aaee-bc219edddbed', foundational, risk_timescales_non_competing).
narrative_ontology:cs_axiom_status(risk_timescales_non_competing, holdable).
narrative_ontology:cs_axiom_grounding('f163b596-81c9-446e-aaee-bc219edddbed', risk_timescales_non_competing, instrumental).
narrative_ontology:cs_axiom('f163b596-81c9-446e-aaee-bc219edddbed', foundational, unified_governance_object_necessary).
narrative_ontology:cs_axiom_status(unified_governance_object_necessary, holdable).
narrative_ontology:cs_axiom_grounding('f163b596-81c9-446e-aaee-bc219edddbed', unified_governance_object_necessary, conventional).
narrative_ontology:cs_reference_frame('f163b596-81c9-446e-aaee-bc219edddbed', unified_ai_safety_field).
narrative_ontology:cs_drift_state('f163b596-81c9-446e-aaee-bc219edddbed', post_chatgpt_governance_surge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f163b596-81c9-446e-aaee-bc219edddbed', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_research_community).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, policy_makers_seeking_comprehensive_regulation).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, public_interest_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, existential_risk_specialists).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harms_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, resource_constrained_organizations).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, comprehensive_risk_governance).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, intergenerational_justice_in_ai).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The field-as-a-whole gains unified legitimacy, pooled funding streams, and a single governance object from the dual-priority frame. Individual researchers can move between sub-communities, and the field's collective bargaining power with funders and policymakers is strengthened. Exit is near-arbitrage: researchers can pivot to adjacent fields (ML safety, ethics, policy) with transferable capital.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_research_community, beneficiary,
    institutional, generational, arbitrage, global).

% Researchers focused on long-horizon alignment theory and existential risk modeling must allocate increasing fractions of their output to near-term benchmarks, interpretability demos, and policy-relevant deliverables to maintain funding. Their comparative advantage (deep theoretical work on speculative futures) is taxed to prove dual-priority relevance. Exit is constrained: pivoting to pure near-term work loses their epistemic identity; leaving the field loses their community and mission.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_specialists, payer,
    organized, generational, constrained, global).

% Researchers and advocates documenting bias, discrimination, labor exploitation, and misinformation in deployed systems must frame their work as 'relevant to existential risk' or 'building foundations for long-term safety' to access AI safety funding streams. Their comparative advantage (empirical rigor on present harms) is taxed to satisfy a long-horizon relevance criterion. Exit is constrained: the dual-priority frame controls the most prestigious funding; pure near-term work is relegated to 'AI ethics' with lower status and resources.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harms_advocates, payer,
    organized, biographical, constrained, global).

% Small labs, independent researchers, and organizations in the Global South lack the personnel to simultaneously pursue deep alignment theory and rigorous empirical auditing of deployed systems. The dual-priority mandate effectively requires a team size and budget that only well-resourced institutions can sustain. They are trapped: they cannot comply without diluting both priorities, and they cannot access the field's core resources without performing compliance.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, resource_constrained_organizations, payer,
    moderate, biographical, trapped, global).

% Governments and international bodies gain a single 'AI safety' governance object that claims to cover all risk timescales, simplifying legislative design and inter-agency coordination. They can fund one portfolio instead of adjudicating between competing risk framings. Exit is arbitrage-grade: they can adopt alternative governance frameworks (risk-tiered, sector-specific) if the dual-priority model proves unworkable.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_makers_seeking_comprehensive_regulation, beneficiary,
    institutional, generational, arbitrage, national).

% Civil society organizations gain rhetorical leverage from the comprehensive frame: they can demand action on both present harms and future risks without being forced to choose. They are not directly taxed by the mandate but benefit from its political momentum. Exit is mobile: they can shift to single-issue campaigns if the dual-priority frame stalls.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, public_interest_advocates, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, public_interest_advocates, observer).

% NSF, EU Horizon, private foundations, and university hiring committees enforce the dual-priority frame through grant calls, fellowship criteria, and tenure standards that require 'broad AI safety relevance.' They are the distributed enforcement mechanism — no single institution owns the mandate, but collectively they make single-priority work structurally disadvantaged. Their situation is analytical: they observe the field's dynamics and adjust incentives, but they also bear reputational risk if the frame produces incoherent outputs.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, funding_agencies_and_hiring_committees, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified AI safety field with a single governance object, pooled funding streams, and shared legitimacy that enables coordinated action across risk timescales — solving the fragmentation that previously left policymakers without a clear interlocutor and researchers without a shared identity.
% TRANSFER_FUNCTION: Moves research autonomy and methodological purity from both specialist communities (existential risk theorists, near-term harm empiricists) toward the field-level institution (funding agencies, conference organizers, hiring committees) in the form of compliance with dual-priority framing requirements. Resources flow to actors who can credibly perform both priorities; actors specialized in one pay a coordination tax.
% ABSENT_VOICES: Researchers who believe the risk categories are fundamentally incommensurable and that a portfolio approach (separate well-funded programs for each) would produce better outcomes than a forced synthesis. Also absent: Global South AI practitioners for whom the dual-priority mandate's resource requirements are exclusionary, and critics who argue the frame obscures power dynamics by treating all 'AI safety' as a unified technical project.
% DISAPPEARANCE_RATIONALE: If the dual-priority mandate vanished overnight, funding agencies would likely revert to portfolio approaches (separate existential risk and near-term harm programs), specialist communities would reclaim methodological autonomy, and the field would re-fragment — but with higher total research output on each priority. The coordination infrastructure (conferences, journals, talent pipelines) would persist but reorganize around distinct risk categories.
% FOUNDING_PROBLEM: By 2015, the AI safety field had fractured into mutually unintelligible sub-communities: one focused on speculative existential risk from future superintelligence, another on documented harms from deployed systems. Policymakers faced a fragmented field with no unified governance object; funders couldn't evaluate competing claims; talent pipelines split. The dual-priority mandate was proposed as a synthesis: a single 'AI safety' commitment covering both timescales.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (field fragmentation preventing coherent governance) is attested by early field-builders (e.g., Future of Life Institute 2015 Puerto Rico conference organizers, early AI safety grantmakers). However, by 2023, substantial infrastructure exists on both sides — the fragmentation problem is substantially solved, but the dual-priority frame persists as a credential. Critics from both specialist communities (e.g., MIRI-affiliated researchers on the existential risk side; DAIR Institute and algorithmic justice advocates on the near-term side) attest the mandate now extracts more conformity than it coordinates.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).
:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores moderate extractiveness (0.35) because it extracts methodological conformity and resource allocation compliance from both specialist communities in exchange for field-level legitimacy and pooled funding — a genuine coordination function (unified AI safety governance) with asymmetric extraction (specialists pay coordination tax). Suppression (0.25) is present but not extreme: single-priority work isn't banned, but it faces structural penalties in funding, hiring, and publication. Theater ratio (0.28) reflects that the coordination function is real (the field does need some bridge between timescales) but a growing share of dual-priority performance is credential-maintenance rather than substantive integration. Accessibility collapse (0.38) is moderate: alternative framings (single-priority, portfolio approaches) remain thinkable but are institutionally disadvantaged. Resistance (0.55) is significant: both specialist communities have pushed back against dilution of their core missions.
 *
 * PERSPECTIVAL GAP:
 *   From the field-level beneficiary seat, this is a rope: it solves the fragmentation problem that previously left AI safety without a unified governance object. From the specialist victim seats, it is a snare: their comparative advantage is taxed to maintain a coalition they didn't choose. From the resource-constrained seat, it is a piton: the mandate persists because no one has enough power to change it, but no one benefits enough to maintain it enthusiastically. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The dual-priority frame creates a coordination benefit for the field-as-a-whole (ai_safety_research_community, policy_makers) by pooling legitimacy and funding streams. But the coordination tax falls disproportionately on the two specialist communities (existential_risk_specialists, near_term_harms_advocates) who must reshape their research agendas to satisfy the mandate. Resource-constrained organizations lack the slack to absorb this tax and are effectively trapped. The agenda_setter role is distributed across funding agencies, conference organizers, and hiring committees who enforce the frame — no single institution owns it, creating a diffused but potent enforcement network.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (field fragmentation preventing coherent AI safety governance) was live in 2015-2018. By 2023, the field has substantial infrastructure, but the dual-priority mandate has become a credential that persists independent of whether it still solves the coordination problem. The mandate now extracts conformity from specialists who would otherwise pursue more targeted, higher-leverage work. Mandatrophy is not resolved: the constraint's coordination function has atrophied into a membership signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct structural reading of the ai_safety_commitment kernel, or a pragmatic compromise between sibling readings?',
    'Trace resource allocation decisions: if dual-priority frameworks produce distinct institutional arrangements and budget lines not reducible to either sibling''s preferred portfolio, it is a distinct reading.',
    'If a distinct reading, it carries its own ε and victim structure; if a compromise, its extraction profile should be modeled as a convex combination of the siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether dual_priority_reading is a third reading or a mixture of the other two.').

omega_variable(
    resource_allocation_coherence,
    'Does the dual-priority mandate produce coherent resource allocation under scarcity, or does it systematically favor one intervention type while claiming to serve both?',
    'Longitudinal analysis of funding flows, talent distribution, and policy outputs in jurisdictions adopting dual-priority frameworks vs. single-priority frameworks.',
    'If coherence fails systematically, the constraint operates as extraction from the disfavored population toward the favored one, reclassifying toward snare for that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_allocation_coherence, empirical, 'Whether dual-priority resource allocation is structurally coherent or covertly extractive.').

omega_variable(
    suppression_via_frame_capture,
    'Does the dual-priority framing suppress dissent by making single-priority advocacy appear irresponsible or incomplete?',
    'Discourse analysis of funding decisions, hiring, and publication norms: do actors who prioritize one risk type face structural penalties framed as ''neglecting the other''?',
    'If frame capture operates, suppression is higher than the scalar metric captures — the constraint suppresses not just alternatives but the articulation of single-priority positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_via_frame_capture, conceptual, 'Whether the dual-priority frame functions as a suppression mechanism against single-priority advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(ai_s_tr_t0, observed).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__dual_priority_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(ai_s_tr_t5, observed).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__dual_priority_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(ai_s_tr_t10, observed).
narrative_ontology:measurement(ai_s_tr_t15, ai_safety_commitment__dual_priority_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(ai_s_tr_t15, observed).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__dual_priority_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(ai_s_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(ai_s_be_t0, observed).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__dual_priority_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement_basis(ai_s_be_t5, observed).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__dual_priority_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(ai_s_be_t10, observed).
narrative_ontology:measurement(ai_s_be_t15, ai_safety_commitment__dual_priority_reading, base_extractiveness, 15, 0.33).
narrative_ontology:measurement_basis(ai_s_be_t15, observed).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__dual_priority_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(ai_s_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(ai_s_su_t0, observed).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__dual_priority_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement_basis(ai_s_su_t5, observed).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__dual_priority_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement_basis(ai_s_su_t10, observed).
narrative_ontology:measurement(ai_s_su_t15, ai_safety_commitment__dual_priority_reading, suppression_requirement, 15, 0.23).
narrative_ontology:measurement_basis(ai_s_su_t15, observed).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__dual_priority_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement_basis(ai_s_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__dual_priority_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_governance_funding_mandates).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_talent_pipeline).

% DUAL FORMULATION NOTE:
% The ai_safety_commitment kernel decomposes into three readings with distinct ε profiles: existential_risk_reading (low ε, mountain-like for its community), near_term_harms_reading (moderate ε, rope-like for its community), and dual_priority_reading (higher ε, tangled_rope — coordinates across communities but extracts conformity from both). The dual_priority_reading influences both siblings by setting the field-level legitimacy conditions they must meet for funding and recognition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, organized, 0.72).
constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
