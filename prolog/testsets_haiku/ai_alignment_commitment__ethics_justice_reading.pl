% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment as Prevention of Bias Reproduction and Present-Day Harm
 *   domain: technology/ethics/AI governance
 *
 * SUMMARY:
 *   The ethics-justice reading of AI alignment defines the central alignment
 *   problem as preventing AI systems from reproducing social bias and causing
 *   present-day harms to marginalized communities. This reading prioritizes
 *   documented current failures (facial recognition bias, algorithmic
 *   discrimination in employment and credit, disparate impact in criminal
 *   justice) over speculative future control problems. It positions bias
 *   prevention as the urgent alignment task and redirects research attention,
 *   compute resources, and institutional authority toward ethics researchers
 *   and civil rights advocates. The constraint extracts from long-term AI
 *   safety research by reframing what counts as an alignment problem and
 *   reallocating limited institutional attention. Marginalized communities
 *   are named as beneficiaries, though their own voice in defining alignment
 *   priorities is often constrained. This is ONE READING of a contested
 *   kernel — the AI alignment commitment — distinguished from the
 *   safety-control reading (which prioritizes catastrophic risk prevention)
 *   and the integrated reading (which treats both as non-exclusive). The
 *   claim and metrics are independent by design: this reading is CLAIMED as
 *   tangled_rope (both coordination and extraction present), and the metrics
 *   reflect high suppression and rising theater ratio, indicating increasing
 *   performance of bias-prevention narratives while actual community voice
 *   may be constrained.
 *
 * KEY AGENTS:
 *   - marginalized_communities_subject_to_bias: primary beneficiary in principle, structurally trapped (cannot exit AI systems governing their opportunities)
 *   - ai_ethics_researchers: agenda-setter controlling research priorities and definitions of alignment
 *   - civil_rights_advocacy_groups: organized voice for beneficiaries, influence is constrained
 *   - long_term_ai_safety_research_agenda: payer (non-agent entity whose priority is redirected)
 *   - capability_research_institutions: payer (face bias-mitigation burden and resource reallocation)
 *   - futures_research_communities: excluded (long-term safety concerns marginalized from alignment discourse)
 *   - affected_communities_themselves: excluded (structurally absent from decision-making despite nominal protection)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment as Prevention of Bias Reproduction and Present-Day Harm").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "technology/ethics/AI governance").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, '055def23-d390-4893-bd4c-01b04079450c').
narrative_ontology:cs_kernel_codification('055def23-d390-4893-bd4c-01b04079450c', distributed).
narrative_ontology:cs_authority_grounding('055def23-d390-4893-bd4c-01b04079450c', extraction).
narrative_ontology:cs_interpretation_layer_present('055def23-d390-4893-bd4c-01b04079450c').
narrative_ontology:cs_reading_relation('055def23-d390-4893-bd4c-01b04079450c', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('055def23-d390-4893-bd4c-01b04079450c', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('055def23-d390-4893-bd4c-01b04079450c', foundational, present_harms_take_priority_over_speculative_risks).
narrative_ontology:cs_axiom_status(present_harms_take_priority_over_speculative_risks, holdable).
narrative_ontology:cs_axiom_grounding('055def23-d390-4893-bd4c-01b04079450c', present_harms_take_priority_over_speculative_risks, deontological).
narrative_ontology:cs_axiom('055def23-d390-4893-bd4c-01b04079450c', foundational, marginalized_communities_are_alignment_stakeholders).
narrative_ontology:cs_axiom_status(marginalized_communities_are_alignment_stakeholders, holdable).
narrative_ontology:cs_axiom_grounding('055def23-d390-4893-bd4c-01b04079450c', marginalized_communities_are_alignment_stakeholders, conventional).
narrative_ontology:cs_reference_frame('055def23-d390-4893-bd4c-01b04079450c', historical_discrimination_continuity).
narrative_ontology:cs_drift_state('055def23-d390-4893-bd4c-01b04079450c', contemporary_algorithmic_deployment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('055def23-d390-4893-bd4c-01b04079450c', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_subject_to_bias).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ai_ethics_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, civil_rights_advocacy_groups).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_ai_safety_research_agenda).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, compute_resource_allocation).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, capability_research_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, compute_resource_allocation_committee).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face documented immediate harms from deployed AI systems in employment screening, credit decisions, criminal justice algorithms, and social benefits allocation. Their protection from algorithmic discrimination is the stated goal of this constraint. They have no practical exit: these systems govern consequential access regardless of their preferences. This reading frames their protection as the urgent alignment problem.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_subject_to_bias, beneficiary,
    powerless, biographical, trapped, global).

% Control the research agenda by defining what 'alignment' means, what counts as an alignment problem, and what success looks like. Conduct fairness audits and bias impact evaluations. Set metrics focused on disparate impact and community protection. Secure research funding and publication venues that validate bias-prevention framings. Can shift to different institutions or funding models if their framing becomes untenable in one setting, but face real pressure to maintain the bias-prevention priority.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_ethics_researchers, agenda_setter,
    institutional, generational, mobile, global).

% Advocate for regulatory and technical intervention to prevent AI discrimination. Benefit from having their concerns elevated to the status of alignment problems and embedded in technical requirements. Their influence is constrained: they can shape discourse and demand accountability, but cannot directly control AI development or institutional research priorities. Often work in coalition with ethics researchers to amplify their voice.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, civil_rights_advocacy_groups, beneficiary,
    organized, generational, constrained, national).

% A research program studying existential and catastrophic risks from advanced AI systems — control failures, specification gaming, deceptive alignment in superhuman systems. Pays the cost of this constraint through redirected attention, compute resources, and institutional priority. Researchers in this agenda find their work characterized as less urgent or less central to 'real' alignment, even when control research would prevent failures in long-term advanced systems.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_ai_safety_research_agenda, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(ai_alignment_commitment__ethics_justice_reading, long_term_ai_safety_research_agenda).

% Develop AI systems under capability and performance pressures. Face requirements to conduct fairness audits, bias impact assessments, and community engagement processes. These requirements impose development friction, slow capability milestones, and redirect engineering resources toward bias evaluation. Exit options are limited: regulatory pressure, customer demand, and reputational risk make ignoring bias requirements costly.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, capability_research_institutions, payer,
    institutional, biographical, constrained, global).

% Controls allocation of finite compute resources between safety research, bias evaluation, and capability development. This reading shifts compute budgets toward fairness evaluation and bias auditing, away from long-term safety research infrastructure. Makes relative-priority decisions that affect which research programs can scale.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, compute_resource_allocation_committee, payer,
    institutional, generational, constrained, global).

% Conduct research on long-term AI development, advanced capability risks, deceptive alignment, and specification gaming in superintelligent systems. Are systematically excluded from alignment priority-setting: their concern set (control problems in advanced AI) is treated as speculative or orthogonal to 'real' alignment. Their voice in defining what alignment means is suppressed in discourse and funding allocation.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, futures_research_communities, excluded,
    moderate, civilizational, constrained, global).

% Communities experiencing AI bias are nominally protected but excluded from decision-making about what counts as harm, what success looks like, and how to prioritize solutions. Their own definitions of harm and protection are mediated through ethics researchers and advocacy institutions rather than being heard directly. Structural power asymmetries and technical expertise barriers limit their voice in technical problem-definition.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, affected_communities_themselves, excluded,
    powerless, biographical, trapped, global).

% Seek to investigate whether bias prevention and long-term safety can be addressed as complementary rather than competitive alignment problems. Observe the resource allocation disputes and institutional competition between ethics-justice and safety-control framings. Currently marginal in institutional authority; positioned to detect whether the two readings can be integrated or whether they are fundamentally in tension.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, integration_research_programs, observer,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__ethics_justice_reading, ai_ethics_researchers).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__ethics_justice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a collective understanding that preventing reproduction of historical discrimination in AI systems and protecting marginalized communities from algorithmic bias is the central alignment problem. Coordinates ethics researchers, capability developers, civil rights advocates, and (in principle) affected communities around a shared definition of what alignment requires. Creates common standards for fairness evaluation and bias auditing.
% TRANSFER_FUNCTION: Transfers research agenda authority, institutional credibility, and compute resources from long-term AI safety research toward bias-prevention research. Transfers regulatory and public attention from capability development toward fairness concerns. Transfers the burden of bias mitigation from society-at-large onto AI developers and ethics review processes. Moves institutional power to define 'alignment' from safety researchers toward ethics researchers and advocacy groups.
% ABSENT_VOICES: Futures research communities studying long-term control problems are structurally excluded from alignment priority-setting despite their direct stake in how the term is defined. Affected communities themselves — who would articulate their own harm definitions and preferred solutions — are often nominally included but actually absent from technical decision-making. AI researchers in developing nations whose systems face different bias patterns are typically not in the room when global alignment standards are set.
% DISAPPEARANCE_RATIONALE: If this constraint and its enforcement mechanisms disappeared, institutional pressure on bias-prevention research would collapse, compute budgets would reflow toward capability and long-term safety research, fairness audit requirements would become optional, and the authority to define 'alignment' would revert to different framings. The protection infrastructure around marginalized communities would dissolve. Research communities studying long-term control problems would see resource and attention reallocation in their favor. But the very real problem of present-day AI bias harms would persist and accelerate in systems no longer subject to fairness scrutiny.
% FOUNDING_PROBLEM: AI systems trained on historical data or optimized against biased proxies reproduce and amplify discrimination patterns that already disadvantage marginalized populations. These harms manifest in real consequential decisions: employment screening, credit allocation, criminal justice risk assessment, social benefits allocation. Marginalized communities face compounded disadvantage when algorithmic systems layer discrimination on top of existing structural inequalities.
% FOUNDING_PROBLEM_CORROBORATION: Extensive empirical evidence from academic research (Buolamwini & Buolamwini on facial recognition accuracy disparities across race and gender, ProPublica's COMPAS recidivism analysis showing racial disparity in false positive rates, Obermeyer et al. on racial bias in healthcare algorithm, Jackson et al. on employment discrimination in algorithmic screening). Documented harms reported by civil rights organizations including NAACP, Algorithmic Justice League, AI Now Institute, and grassroots communities. Long-term safety researchers do not dispute the existence of present-day bias — they dispute whether it should dominate alignment definitions.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 because this reading achieves coordination around bias prevention (real problem, genuine research function) while simultaneously extracting research attention and institutional priority from long-term safety research. The extraction mechanism is not coercive in the crude sense — it operates through framing and priority-setting. Theater ratio rises from 0.28 to 0.44 over the interval because the proportion of alignment activity devoted to public bias audits, fairness narratives, and community engagement (performative relative to core functionality) increases even as the core bias-prevention function persists. Suppression is high (0.71) because the constraint requires active enforcement to exclude competing framings: futures researchers must be deprioritized, long-term safety research must be recharacterized as less urgent, and alternative definitions of alignment must be suppressed in discourse and funding allocation. The measurement series traces a 2015–2025 window (t=0 to t=20 as rough year-markers) showing extractiveness and theater both rising, indicating the reading is becoming institutionalized while simultaneously becoming more performative.
 *
 * PERSPECTIVAL GAP:
 *   From the ethics-justice researcher seat, the constraint is genuine coordination: preventing demonstrated harms to real communities. From the long-term safety research seat, the same constraint is extractive: finite attention redirected toward present-day problems away from future control risks. From the marginalized-community seat, the constraint is both protective (in principle) and extractive (in practice — their own voice is mediated). The engine computes these divergent types from the structural data: the agenda-setter (ethics researchers) will compute rope or tangled_rope from their seat, while the long-term safety community will compute snare. The beneficiary set (marginalized communities, ethics researchers) and victim set (long-term safety research, capability institutions) are explicitly named, enabling per-seat directionality derivation.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities (powerless, trapped, biographical horizon) face directionality near d=1.0 (full targets of bias in current systems) but would benefit from protection, making their effective extraction negative if the constraint functioned without suppression. However, the suppression vector (their voices excluded from technical definition-making, their needs mediated through proxy institutions) inverts this: they are beneficiaries in name but targets in structure. Ethics researchers (institutional, mobile) are near d=0.0 beneficiary end: they control the constraint's framing and benefit from institutional authority and research attention. Long-term safety research (analytical, civilizational) is the victim: its priority is extracted regardless of its merits by the reframing of alignment. Capability institutions (institutional, constrained) face d=0.7 or higher: they must invest in bias mitigation and fairness auditing, reducing their autonomy and speed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (AI bias harms to marginalized communities) has an UNAMBIGUOUSLY live status — empirical evidence from deployed systems is extensive. The disappearance verdict is world_rearranges: if this constraint's enforcement (bias audit requirements, fairness metrics, resource allocation) disappeared, institutional pressure on capability research would relax and long-term safety research would see resource reallocation. However, the real coordination function (preventing documented AI discrimination) would also vanish. This is not a mandatrophy case (where founding problems are dead but constraints persist), but rather a live-function case where both coordination and extraction are active. The theater rising (0.28 → 0.44) while extractiveness also rises suggests increasing performativity around bias prevention as the constraint becomes institutionalized, but the performativity is layered ON TOP of real coordination, not replacing it (unlike piton degradation). This remains tangled_rope: genuine coordination for bias prevention + genuine extraction from long-term safety research agenda.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_vs_control_constraint_independence,
    'Are bias prevention in current systems and control safety in advanced systems structurally separable alignment problems, or is one fundamentally prerequisite or entangled with the other?',
    'Empirical investigation of whether systems that are robustly aligned against bias are meaningfully more or less aligned against control failures. Theoretical analysis of whether bias-prevention constraints interact with or corrupt control mechanisms. Natural experiments from jurisdictions with different bias-regulation intensity comparing control alignment outcomes.',
    'If the problems are independent, this reading''s autonomy claim is sound and both can be treated as legitimate alignment targets without necessary trade-off. If they are entangled (e.g., bias in training data corrupts learned goals in ways that amplify control risks, or control mechanisms worsen bias problems), then the separation premise breaks and the integrated reading becomes structurally more sound.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bias_vs_control_constraint_independence, empirical, 'Whether bias prevention and long-term control safety are independent alignment problems or entangled.').

omega_variable(
    marginalized_community_voice_capture,
    'Are the voices and preferences of marginalized communities genuinely centered in this constraint''s definition and enforcement, or has their protection been reinterpreted by proxy-holders (ethics researchers, advocates, institutions)?',
    'Direct deliberation with affected communities on alignment priorities. Participatory design in which communities author their own harm definitions and preferred solutions. Longitudinal tracking of whether community-identified harms match the harms addressed by ethics-driven alignment research. Community audits of whether fairness metrics actually reduce the harms communities experience.',
    'If communities'' own articulations diverge significantly from the constraint''s framing, the constraint risks becoming an extractive apparatus that benefits ethics institutions while marginalizing communities (victim-role capture). The theoretical beneficiary set would require revision from ''marginalized communities'' to ''ethics researchers'' and the constraint would reclassify toward snare. If alignment is achieved through community voice, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_community_voice_capture, empirical, 'Whether marginalized communities genuinely drive alignment priorities or are subordinated within the constraint''s structure.').

omega_variable(
    reading_kernel_distinction,
    'Is ''AI alignment'' a single contested kernel admitting multiple coherent readings of what the term means, or are the ethics-justice and safety-control framings incommensurable uses of the same word that should be disaggregated into separate concepts?',
    'Linguistic and conceptual analysis of whether ethics-justice and safety-control readings share sufficient common reference to count as readings of one kernel. Examination of whether advocates of each reading accept that the others are offering legitimate alternative framings of alignment or whether they treat alternatives as confusion/misdefinition.',
    'If readings are incommensurable, this constraint should be retitled ''present_day_bias_prevention'' rather than claiming ''alignment'', and the constraint family logic would disaggregate. If they are genuinely readings of a shared kernel, the competition is a legitimate kernel contest and the reading structure is sound. The kernel verdict affects whether the three stories form a constraint family or are independent concepts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether ethics-justice and safety-control framings are readings of one contested kernel or incommensurable uses of ''alignment''.').

omega_variable(
    research_agenda_extraction_vs_capability_effect,
    'Does this reading extract from long-term safety research by redirecting attention and resources, or does bias-prevention requirement actually slow risky capability deployment and thereby reduce long-term risks?',
    'Track institutional attention allocation and resource flow over time. Model the causal pathways: does bias prevention slow capability timelines in ways that reduce advanced-AI deployment risk? Does it constrain which capability research is pursued, filtering for less dangerous directions? Or does it operate independently and purely redirect resources?',
    'If pure extraction, the victim designation for long-term safety research is accurate and this is snare-like. If the relationship is complex (bias prevention slows risky capability deployment, thereby serving long-term safety), then the constraint''s function is complementary and extractiveness estimate should decrease. The effective victim set might shrink or the extraction might become net-positive for long-term safety interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_agenda_extraction_vs_capability_effect, empirical, 'Whether the constraint extracts from long-term safety research or has complex downstream safety effects.').

omega_variable(
    reading_instantiation_ambiguity,
    'What is the minimal commitment required to instantiate this ethics-justice reading of alignment, and what is separable elaboration or reinterpretation?',
    'Specification of the core axioms (present harms take priority; marginalized communities are alignment stakeholders) and determination of which institutional and technical practices are entailed by those axioms versus which are contingent elaborations. This grounds the reading''s identity separately from institutional implementations.',
    'If the core reading is thin and the elaborate practices that extract resources are contingent, the reading could be implemented differently (by communities themselves, with less institutional gatekeeping) and the extraction would be an artifact of current institutional configurations rather than inherent to the reading. If the reading is thick and entails the current extraction structures, then the reading itself is extractive-by-design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_instantiation_ambiguity, conceptual, 'Whether the ethics-justice reading is inherently extractive or whether extraction is contingent to current institutional implementations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 16, 0.43).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 20, 0.44).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 4, 0.61).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__ethics_justice_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family decomposing the contested term 'AI alignment' into three structurally distinct readings of what alignment requires. The ethics-justice reading prioritizes present-day bias harms to marginalized communities. The safety-control reading prioritizes catastrophic risk from advanced systems. The integrated reading denies the priority trade-off and seeks both simultaneously. Each reading has different ε values, beneficiary/victim structures, and institutional implications. The three readings compete for research agenda authority and resource allocation; all three remain live positions in the AI governance discourse despite their tensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__ethics_justice_reading, powerless, 0.95).
constraint_indexing:directionality_override(ai_alignment_commitment__ethics_justice_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
