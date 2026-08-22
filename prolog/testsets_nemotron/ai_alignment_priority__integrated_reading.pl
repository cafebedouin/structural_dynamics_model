% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated AI Alignment Priority (Catastrophic + Present Harms)
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   The AI alignment field has fractured into two dominant readings of what
 *   'alignment' means: one prioritizing catastrophic risk from advanced
 *   systems (existential_risk_reading), the other prioritizing present
 *   discriminatory and extractive harms from deployed systems
 *   (nearterm_harms_reading). This constraint story captures a third
 *   reading—the integrated reading—which claims these are complementary, not
 *   competing, priorities. The integrated reading asserts a dual methodology
 *   (red-teaming for capability risks + audits for deployment harms) and a
 *   resource allocation that balances both. The constraint is the
 *   institutionalized expectation that alignment work must address both
 *   tracks simultaneously. It extracts moderately from single-focus actors
 *   (pure existential risk funders, pure near-term justice funders,
 *   single-focus labs) while coordinating a broader coalition. The
 *   ε-invariance principle applies: this is ONE reading with ONE ε, not an
 *   average of the two siblings. The sibling readings are separate
 *   constraints (other files) linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - ai_safety_researchers: Primary beneficiaries (institutional/biographical) — gain legitimacy and funding for dual-track work
 *   - civil_society_auditors: Beneficiaries (organized/biographical) — gain institutional recognition for audit methodology
 *   - policy_makers_balanced: Agenda setters (institutional/generational) — set governance agendas requiring both tracks
 *   - affected_marginalized_communities: Victims of present harms, beneficiaries of integrated approach (powerless/biographical)
 *   - future_generations_advocates: Beneficiaries (analytical/civilizational) — gain standing for long-term risk in governance
 *   - pure_existential_risk_funders: Victims of resource diversion (powerful/biographical) — see funding split toward near-term work
 *   - pure_nearterm_justice_funders: Victims of resource diversion (organized/biographical) — see funding split toward long-term risk
 *   - single_focus_research_labs: Victims (moderate/biographical) — pressured to adopt dual methodology or lose legitimacy
 *   - resource_constrained_civil_society: Victims (powerless/biographical) — cannot afford dual-track capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.38).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.22).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated AI Alignment Priority (Catastrophic + Present Harms)").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '5dace697-bf6f-4602-9246-301e54030ab7').
narrative_ontology:cs_kernel_codification('5dace697-bf6f-4602-9246-301e54030ab7', distributed).
narrative_ontology:cs_authority_grounding('5dace697-bf6f-4602-9246-301e54030ab7', distributed).
narrative_ontology:cs_reading_relation('5dace697-bf6f-4602-9246-301e54030ab7', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('5dace697-bf6f-4602-9246-301e54030ab7', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('5dace697-bf6f-4602-9246-301e54030ab7', foundational, harm_complementarity_thesis).
narrative_ontology:cs_axiom_status(harm_complementarity_thesis, holdable).
narrative_ontology:cs_axiom_grounding('5dace697-bf6f-4602-9246-301e54030ab7', harm_complementarity_thesis, empirically_contingent).
narrative_ontology:cs_axiom('5dace697-bf6f-4602-9246-301e54030ab7', foundational, dual_methodology_necessity).
narrative_ontology:cs_axiom_status(dual_methodology_necessity, holdable).
narrative_ontology:cs_axiom_grounding('5dace697-bf6f-4602-9246-301e54030ab7', dual_methodology_necessity, instrumental).
narrative_ontology:cs_reference_frame('5dace697-bf6f-4602-9246-301e54030ab7', fragmented_alignment_field_2018).
narrative_ontology:cs_drift_state('5dace697-bf6f-4602-9246-301e54030ab7', post_chatgpt_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5dace697-bf6f-4602-9246-301e54030ab7', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, ai_safety_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, civil_society_auditors).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, policy_makers_balanced).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, affected_marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, future_generations_advocates).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, pure_existential_risk_funders).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, pure_nearterm_justice_funders).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, single_focus_research_labs).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, resource_constrained_civil_society).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, affected_marginalized_communities).
narrative_ontology:constraint_vindicates(ai_alignment_priority__integrated_reading, complementary_harm_prevention_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_priority__integrated_reading, dual_methodology_necessity).
narrative_ontology:constraint_vindicates(ai_alignment_priority__integrated_reading, intergenerational_justice_in_alignment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers working on both capability risk mitigation (red-teaming, interpretability, scalable oversight) and deployment harm prevention (fairness audits, impact assessments). Gain funding access and professional legitimacy from the integrated frame. Can exit to pure-existential or pure-nearterm positions but lose the integrated coalition's resources.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_safety_researchers, beneficiary,
    institutional, biographical, mobile, global).

% Organizations conducting algorithmic audits, bias assessments, and deployment impact evaluations. Gain institutional recognition and funding streams from the integrated frame's dual methodology mandate. Constrained exit: specialized audit expertise is not easily transferable to red-teaming or theoretical safety work.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, civil_society_auditors, beneficiary,
    organized, biographical, constrained, global).

% Government officials and legislators crafting AI governance frameworks (EU AI Act, US Executive Orders, UK AI Safety Institute mandates). Set agendas requiring both pre-deployment risk assessment and post-deployment audit. Can arbitrage between sibling reading constituencies for political support.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, policy_makers_balanced, agenda_setter,
    institutional, generational, arbitrage, national).

% Communities experiencing present algorithmic harms (discriminatory hiring, lending, policing, healthcare). Bear the costs of deployed misaligned systems. Are nominal beneficiaries of the integrated frame's near-term track but structurally trapped—cannot exit the systems harming them and lack power to shape governance.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, affected_marginalized_communities, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, affected_marginalized_communities, beneficiary).

% Researchers and philosophers arguing for long-term catastrophic risk prevention. Gain standing in governance through the integrated frame's catastrophic track. Analytical seat: their constituency does not yet exist and cannot exit or organize.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_generations_advocates, beneficiary,
    analytical, civilizational, analytical, universal).

% Philanthropic and government funders (e.g., Open Philanthropy, LTFF, government AI safety institutes) historically focused on existential risk. See integrated mandates as diverting resources from what they view as the overwhelming priority. Can exit by restricting funding to pure-existential work but lose influence over mainstream governance.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, pure_existential_risk_funders, payer,
    powerful, biographical, mobile, global).

% Civil rights foundations, algorithmic justice orgs, regulatory advocates focused on present harms. See integrated mandates as diluting urgency of justice work. Constrained exit: their funding base and mission lock them to near-term work; cannot easily pivot to speculative long-term risk.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, pure_nearterm_justice_funders, payer,
    organized, biographical, constrained, national).

% Academic and industrial labs specializing in either theoretical safety (mechanistic interpretability, alignment theory) or deployment audits (fairness, robustness). Pressured to adopt dual methodology or lose hiring/funding legitimacy. Constrained exit: deep specialization makes pivoting costly; reputational capital is track-specific.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, single_focus_research_labs, payer,
    moderate, biographical, constrained, global).

% Small NGOs, community groups, Global South practitioners working on AI harms. Cannot afford dual-track capacity (red-teaming requires compute/expertise they lack; audits require legal/technical capacity they lack). Trapped: excluded from integrated funding streams but harmed by both risk categories.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, resource_constrained_civil_society, payer,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation of AI governance into two mutually blind tracks: one that sees only catastrophic futures and ignores present suffering, and one that sees only present injustice and ignores existential stakes. The integrated frame creates a shared governance vocabulary, dual methodology standards (red-teaming + audits), and funding structures that require both.
% TRANSFER_FUNCTION: Moves funding, talent, and institutional legitimacy from single-focus actors (pure existential risk funders, pure near-term justice funders, single-focus labs) toward integrated coalitions and dual-track practitioners. Also moves epistemic authority: the integrated frame becomes the default for 'serious' alignment work, marginalizing pure specialists.
% ABSENT_VOICES: Global South AI practitioners who lack compute infrastructure for red-teaming and legal capacity for audits. Pure theoretical alignment researchers who view empirical audit work as irrelevant. Pure activist organizers who view red-teaming as legitimizing harmful systems. Single-issue funders who refuse dual-track mandates. These voices are excluded from the integrated coalition's governance tables because their institutional form cannot satisfy the dual methodology requirement.
% DISAPPEARANCE_RATIONALE: If the integrated frame vanished overnight, AI governance would revert to the two fragmented tracks: existential risk institutes would defund audit work; justice orgs would defund red-teaming; policy would lose the dual-track mandate; funding would polarize. The field would reorganize around the two sibling readings as competing paradigms rather than a unified field.
% FOUNDING_PROBLEM: By 2018-2020, the AI alignment field had fractured into two communities with mutually unintelligible vocabularies, funding streams, and success metrics: one focused on speculative catastrophic risks from future superintelligent systems, the other on documented discriminatory harms from deployed systems. Governance bodies faced contradictory expert advice. The integrated reading was built to solve this fragmentation by asserting complementarity.
% FOUNDING_PROBLEM_CORROBORATION: The fragmentation problem is attested by multiple independent sources outside the integrated coalition's beneficiaries: (1) The 2021-2023 UNESCO AI ethics recommendations explicitly note the 'disconnect between long-term safety and near-term ethics communities'; (2) The 2023 UK AI Safety Summit framing papers documented 'two epistemic communities talking past each other'; (3) Independent bibliometric studies (e.g., arXiv category co-citation analysis 2019-2024) show near-zero citation flow between 'AI safety' and 'FAccT' literature until 2022. No single beneficiary group authored this corroboration.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).
:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate because the constraint imposes real resource allocation demands on single-focus actors while delivering coordination value to the integrated coalition. Suppression (0.22) is low-moderate: the constraint operates through legitimacy and funding pressure rather than hard coercion, but single-focus labs face real professional sanctions for non-compliance. Theater ratio (0.18) reflects that some actors perform integration (adding audit language to existential risk grants, or red-teaming language to justice grants) without structural change. Accessibility collapse (0.35) is moderate: alternatives (pure focus) remain intellectually and organizationally viable but lose institutional legitimacy. Resistance (0.45) is significant: both sibling reading communities actively contest the integrated frame as dilution of their priority. The claimed_type is tangled_rope because the constraint has a genuine coordination function (dual methodology solves a real governance gap) AND asymmetric extraction (single-focus actors pay for the integration). Active enforcement is required: funding agencies, journals, and hiring committees enforce the dual-track expectation.
 *
 * PERSPECTIVAL GAP:
 *   From the integrated coalition's seat (ai_safety_researchers, civil_society_auditors, policy_makers_balanced), this is a rope: genuine coordination solving a real problem of fragmented governance. From the single-focus seats (pure_existential_risk_funders, pure_nearterm_justice_funders, single_focus_research_labs), this is a snare: their specialized expertise and funding streams are being taxed to support a methodology they view as dilution. From resource_constrained_civil_society, it is a snare with high extraction: they cannot afford dual-track capacity and are effectively excluded. The engine computes this divergence from the structural data; the authored claim (tangled_rope) captures the structural truth that BOTH coordination and extraction are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ai_safety_researchers, civil_society_auditors, policy_makers_balanced, affected_marginalized_communities, future_generations_advocates) gain legitimacy, funding access, and governance standing from the integrated frame. Their directionality d is low (near beneficiary end). Victims (pure_existential_risk_funders, pure_nearterm_justice_funders, single_focus_research_labs, resource_constrained_civil_society) bear the cost of dual-track demands: redirected funding, expanded scope requirements, loss of specialized legitimacy. Their d is high (near target end). The excluded voices (pure single-focus theorists, Global South practitioners without dual-track capacity) are structurally absent from the integrated coalition's governance tables. The dual victim set (present marginalized + future populations) is the reading's structural signature—it is not a compromise but a distinct theoretical commitment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented alignment field unable to govern AI systems across the full risk spectrum) remains live—AI capabilities continue expanding faster than governance. The integrated reading prevents mislabeling the coordination of dual methodology as pure extraction (it solves a real fragmentation problem) AND prevents mislabeling the extraction from single-focus actors as mere coordination overhead (it structurally disadvantages them). The mandatrophy risk is that if AI development pauses or governance matures, the dual-track requirement could persist as a piton—theatrical integration without the fragmentation problem that justified it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_integrated,
    'Is this constraint a distinct structural reading of the ai_alignment_priority kernel, or a mere rhetorical synthesis of existential_risk_reading and nearterm_harms_reading?',
    'Trace resource allocation patterns in major AI governance institutions: if funding, staffing, and metric adoption show genuine dual-track structure (separate red-teaming AND audit programs with independent budgets), the integrated reading is structurally distinct; if budgets are zero-sum reallocations with shared personnel, it is a synthesis.',
    'If synthesis, ε collapses toward the dominant sibling reading''s value and victim set narrows; if distinct, the dual victim set and moderate ε on both tracks are validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_integrated, empirical, 'Whether the integrated reading instantiates a genuinely separate constraint with its own ε and victim structure').

omega_variable(
    complementary_vs_zero_sum,
    'Do catastrophic risk mitigation and present harm prevention operate as complementary (synergistic) or zero-sum (competing) in actual institutional practice?',
    'Longitudinal study of AI safety orgs adopting integrated frameworks: measure whether red-teaming capabilities improve audit effectiveness (and vice versa) or whether staff/time budgeting shows strict trade-offs.',
    'If complementary, the integrated reading''s claimed_type (tangled_rope) holds with moderate ε; if zero-sum, the constraint fractures into two snares each extracting from the other''s constituency, raising ε for both victim sets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complementary_vs_zero_sum, empirical, 'Whether the dual methodology produces synergy or competition in practice').

omega_variable(
    future_generations_standing,
    'Do future populations constitute a structural victim class with standing in current governance, or are they a rhetorical device used by present actors?',
    'Examine whether governance mechanisms (liability regimes, compute governance, model licensing) create enforceable obligations to non-existent parties, or whether all enforceable obligations resolve to present stakeholders.',
    'If future generations have no structural standing, the victim set reduces to present marginalized groups only, collapsing this reading toward nearterm_harms_reading structurally; if they do, the dual victim set is real and ε reflects extraction from both temporal directions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_standing, conceptual, 'Whether future populations are a real structural victim class or a projected one').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 2018, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_tr_t2018, ai_alignment_priority__integrated_reading, theater_ratio, 2018, 0.05).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_tr_t2020, ai_alignment_priority__integrated_reading, theater_ratio, 2020, 0.08).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_tr_t2022, ai_alignment_priority__integrated_reading, theater_ratio, 2022, 0.12).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_tr_t2023, ai_alignment_priority__integrated_reading, theater_ratio, 2023, 0.15).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_tr_t2024, ai_alignment_priority__integrated_reading, theater_ratio, 2024, 0.17).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_tr_t2025, ai_alignment_priority__integrated_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_be_t2018, ai_alignment_priority__integrated_reading, base_extractiveness, 2018, 0.15).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_be_t2020, ai_alignment_priority__integrated_reading, base_extractiveness, 2020, 0.22).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_be_t2022, ai_alignment_priority__integrated_reading, base_extractiveness, 2022, 0.28).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_be_t2023, ai_alignment_priority__integrated_reading, base_extractiveness, 2023, 0.32).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_be_t2024, ai_alignment_priority__integrated_reading, base_extractiveness, 2024, 0.35).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_be_t2025, ai_alignment_priority__integrated_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_su_t2018, ai_alignment_priority__integrated_reading, suppression_requirement, 2018, 0.08).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_su_t2020, ai_alignment_priority__integrated_reading, suppression_requirement, 2020, 0.12).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_su_t2022, ai_alignment_priority__integrated_reading, suppression_requirement, 2022, 0.18).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_su_t2023, ai_alignment_priority__integrated_reading, suppression_requirement, 2023, 0.2).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_su_t2024, ai_alignment_priority__integrated_reading, suppression_requirement, 2024, 0.21).
narrative_ontology:measurement(ai_alignment_priority__integrated_reading_su_t2025, ai_alignment_priority__integrated_reading, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__integrated_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_governance_funding_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_audit_standardization).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_red_teaming_infrastructure).

% DUAL FORMULATION NOTE:
% This is the integrated_reading of the ai_alignment_priority kernel. The existential_risk_reading and nearterm_harms_reading are sibling constraints with distinct ε, victim sets, and claimed types. All three form a constraint family linked by network.affects_constraints. The integrated reading claims complementary coordination; the siblings claim priority of one track over the other. The ε values differ: existential_risk_reading has low ε for present harms (treated as distraction) but high ε for capability risks if neglected; nearterm_harms_reading has the inverse; integrated_reading has moderate ε on both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, powerful, 0.75).
constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, organized, 0.35).
constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, moderate, 0.65).
constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, powerless, 0.55).
constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, analytical, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
