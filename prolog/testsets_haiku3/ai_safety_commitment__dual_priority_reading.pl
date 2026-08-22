% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: Dual-Priority AI Safety Commitment (Existential + Near-Term)
 *   domain: technology_governance/risk_assessment
 *
 * SUMMARY:
 *   The dual-priority reading holds that AI safety requires addressing both
 *   existential risk (misaligned superintelligent systems) and near-term
 *   harms (algorithmic bias, labor displacement, misinformation) as
 *   non-competing priorities within a unified institutional framework. This
 *   reading was built to prevent institutional fission between two research
 *   communities with different timescales and audiences. The constraint
 *   operates as a tangled rope: it genuinely coordinates fragmented research,
 *   preventing complete institutional separation; it simultaneously extracts
 *   from affected present-harm communities by subordinating their urgent
 *   needs to speculative long-term risks, and from near-term researchers by
 *   requiring them to justify research within an existential-risk-dominant
 *   paradigm. The claim/metric gap is deliberate: the constraint is CLAIMED
 *   as rope (coordination without extraction) while metrics reflect
 *   substantial extraction (0.62) and suppression (0.71), with rising theater
 *   (0.48) suggesting the commitment's coordinative function is increasingly
 *   performative.
 *
 * KEY AGENTS:
 *   - AI safety research community: agenda-setters and beneficiaries — maintain institutional coherence and capture funding
 *   - Existential-risk researchers: powerful beneficiaries but constrained by dual-priority — can operate independently but benefit from legitimacy
 *   - Near-term harms researchers: moderate, constrained — dependent on dual-priority framing for institutional recognition
 *   - Populations exposed to present harms: powerless payers — experience documented extraction without agency
 *   - Future human civilization: non-agent analytical entity through which existential risk is claimed
 *   - AI development companies: excluded from agenda-setting despite operating the harmful systems
 *   - Funding bodies and regulators: co-agenda-setters who enforce the dual-priority through allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.62).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.71).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "Dual-Priority AI Safety Commitment (Existential + Near-Term)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '54729c18-a270-474d-aa2b-dca58470a9dd').
narrative_ontology:cs_kernel_codification('54729c18-a270-474d-aa2b-dca58470a9dd', distributed).
narrative_ontology:cs_authority_grounding('54729c18-a270-474d-aa2b-dca58470a9dd', extraction).
narrative_ontology:cs_interpretation_layer_present('54729c18-a270-474d-aa2b-dca58470a9dd').
narrative_ontology:cs_reading_relation('54729c18-a270-474d-aa2b-dca58470a9dd', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('54729c18-a270-474d-aa2b-dca58470a9dd', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('54729c18-a270-474d-aa2b-dca58470a9dd', foundational, dual_priority_non_competing).
narrative_ontology:cs_axiom_status(dual_priority_non_competing, holdable).
narrative_ontology:cs_axiom_grounding('54729c18-a270-474d-aa2b-dca58470a9dd', dual_priority_non_competing, conventional).
narrative_ontology:cs_axiom('54729c18-a270-474d-aa2b-dca58470a9dd', secondary, institutional_unity_prerequisite).
narrative_ontology:cs_axiom_status(institutional_unity_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('54729c18-a270-474d-aa2b-dca58470a9dd', institutional_unity_prerequisite, instrumental).
narrative_ontology:cs_reference_frame('54729c18-a270-474d-aa2b-dca58470a9dd', unified_ai_safety_mandate).
narrative_ontology:cs_drift_state('54729c18-a270-474d-aa2b-dca58470a9dd', contemporary_resource_scarcity_period, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('54729c18-a270-474d-aa2b-dca58470a9dd', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_research_community).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, safety_governance_institutions).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, populations_exposed_to_present_harms).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, future_human_civilization).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, existential_risk_researchers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, funding_bodies_and_philanthropists).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, policy_makers_and_regulators).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, affected_labor_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, marginalized_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic and independent research institutions that frame AI safety as a coordinated research problem spanning both existential-risk and near-term-harm domains. They set research agendas, allocate grant funding, define terminology and problem boundaries, and adjudicate which harms count as 'safety-relevant.' They benefit from the commitment by maintaining coherent institutional legitimacy and capturing resources directed toward 'AI safety' broadly.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_research_community, agenda_setter,
    institutional, civilizational, constrained, global).

% Researchers focused on long-term, low-probability extinction scenarios from advanced AI systems. They benefit from the dual-priority framing because it legitimizes their research within a broader safety mandate, capturing institutional and philanthropic funding. They also incur costs: they must engage with competing research agendas, defend resource allocation against near-term harm researchers, and maintain coherence with a commitment to both branches.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_researchers, beneficiary,
    powerful, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, existential_risk_researchers, payer).

% Researchers focused on documented present-day harms: algorithmic bias, labor displacement, misinformation amplification, privacy violations. They benefit from the dual-priority frame because it brings their concerns into the safety mandate. They incur costs: their research must justify urgency against existential timescales, they share limited funding pools, and the commitment structure often treats near-term harms as derivative or subordinate.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers, payer).

% Communities experiencing documented harms now: workers in algorithmic management systems, people subject to discriminatory AI decisions, populations targeted by AI-amplified misinformation, marginalized groups bearing disproportionate harms from deployment. They pay through direct harm exposure and lack agency in how the dual-priority commitment allocates resources between their immediate needs and speculative future risks.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, populations_exposed_to_present_harms, payer,
    powerless, biographical, trapped, global).

% A non-agent entity representing humanity across future time horizons. Listed for narrative completeness as the population bearing existential risk. Its interests are declared through present institutions and researchers who claim to speak for long-term flourishing.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, future_human_civilization, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_safety_commitment__dual_priority_reading, future_human_civilization).

% Commercial AI developers operate within a dual-priority safety commitment they did not author and have limited voice in. They would argue that research resources should prioritize deployment-relevant safety (near-term) and feasible mitigation (not speculative long-term). Their exclusion from the commitment's agenda-setting means the constraint operates largely without their input on what constitutes 'safety' in their operational context.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_development_companies, excluded,
    institutional, biographical, mobile, global).

% Major funders (OpenPhil, US government agencies, tech company foundations) enforce the dual-priority commitment through grant-making. They benefit by positioning themselves as funding a comprehensive safety mandate; they enforce by requiring funded research to address both branches or justify neglect of either. They set the boundary of what counts as 'safety' through their allocation decisions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, funding_bodies_and_philanthropists, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, funding_bodies_and_philanthropists, beneficiary).

% Government agencies tasked with AI regulation and oversight. They benefit from the dual-priority frame because it legitimizes comprehensive regulatory action spanning both immediate harms (algorithmic accountability) and long-term risks (AI governance structures). They face tension between addressing documented harms and preparing for speculative futures with limited regulatory machinery.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_makers_and_regulators, beneficiary,
    powerful, generational, mobile, national).

% Workers displaced or harmed by AI deployment in labor contexts. They experience a present-day extraction (job loss, wage pressure, unsafe conditions) that the dual-priority commitment frames as a component of a broader safety mandate rather than a distinct harm requiring urgent redress. Their ability to mobilize is constrained by the dispersion of their harm across multiple sectors.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, affected_labor_communities, payer,
    organized, biographical, constrained, global).

% Communities bearing disproportionate harms from biased or discriminatory AI systems. They are identity-locked into the categories the systems target (race, gender, disability status, immigration status), making exit impossible. The dual-priority commitment treats their harms as safety-relevant but often secondary to existential-risk research, creating a coherence gap: their urgent, documented harm is subordinated to speculative future risk.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, marginalized_populations, payer,
    powerless, biographical, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, ai_safety_research_community).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies fragmented AI safety research (existential-risk and near-term-harms branches) under a single institutional mandate, enabling cross-disciplinary dialogue, shared funding streams, and integrated governance frameworks. Prevents the two research communities from becoming entirely separate institutions with no common language or legitimacy bridge.
% TRANSFER_FUNCTION: Moves research resources, institutional legitimacy, and policy-making authority from other domains (and from near-present interventions) into a unified 'AI safety' framework. The transfer flows upward to high-level research agendas and funding bodies; downward it distributes (unevenly) to both existential-risk and near-term-harms researchers, but in proportions that favor the more institutionalized existential-risk branch.
% ABSENT_VOICES: Affected communities (workers, marginalized populations) have no seat in setting the dual-priority commitment; they would object that their present harms are treated as secondary and that the commitment's resource allocation favors speculative future risks over documented urgency. AI development companies are excluded from the safety research agenda, though they execute the systems that cause both categories of harm.
% DISAPPEARANCE_RATIONALE: If the dual-priority commitment evaporated overnight, the AI safety research community would fragment into two competing institutional ecosystems (existential-risk and near-term-harms tracks), with separate funding streams, journals, conferences, and regulatory pathways. Governance structures would shift: near-term harms would be reassigned to consumer protection, labor law, and anti-discrimination regimes (where enforcement currently lags); existential-risk work would either disappear (lacks mainstream legitimacy alone) or consolidate into specialized high-level institutions. The present coherence is enforced; removal would rearrange the landscape significantly.
% FOUNDING_PROBLEM: When AI systems began showing documented present-day harms (bias in hiring algorithms, discrimination in credit decisions, labor displacement) AND theoretical work identified long-term risks from advanced AI, the field faced a choice: treat them as separate problems or as branches of a unified 'AI safety' mandate. The dual-priority commitment was built to prevent institutional fission — to hold both in a single framework so neither dominates and both can claim legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: The safety research community and funding institutions attest the founding problem remains live: both harms exist, institutional integration remains necessary. Affected communities and near-term harm researchers counter that the founding problem has shifted: the real problem is now that the dual-priority commitment subordinates present harms to existential speculation. Regulatory bodies and policy makers (independent seats) attest the commitment helps them coherently address both near-term and long-term risks, but note that resource allocation remains skewed toward long-term concerns.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the commitment transfers resources and institutional legitimacy from other domains into AI safety, concentrating power in research institutions and funding bodies, and because it subordinates present-harm communities' urgent needs to long-term speculation. Suppression is substantial (0.71) because maintaining the dual-priority frame requires actively suppressing voices that would argue for resource reallocation toward present harms: near-term harm researchers cannot argue their case at full strength without threatening the unified mandate; affected communities have no formal voice in the research agenda. Theater is rising (0.25→0.48 over the interval, then plateauing) because the commitment increasingly performs unity while actual resource allocation diverges — funding bodies speak the language of dual priority while allocating disproportionately to existential-risk research, and both branches engage in rhetorical claims about integration that don't map to operational practice. Accessibility collapse (0.51) is moderate because alternative institutional arrangements exist (separate research tracks, siloed regulation) but require overcoming the invested coherence of the present system. Resistance (0.74) is high because affected communities, labor advocates, and some near-term harm researchers actively contest the priority structure, even if their resistance doesn't translate to institutional power.
 *
 * PERSPECTIVAL GAP:
 *   The existential-risk researchers and funding bodies should compute as experiencing a rope (genuine coordination enabling their research), while powerless affected communities should compute as trapped in a snare (subordinated harms, no exit, suppressed voice). The dual-priority research community should compute as beneficiaries experiencing moderate extraction (constrained by the commitment but advantaged by institutional legitimacy). The engine's per-seat classification from the structural data will reveal this divergence: high directionality (d near 1.0) for powerless affected communities; low directionality (d near 0.0) for existential-risk researchers and funders; moderate d for near-term harm researchers caught between benefit and constraint. This is the intended measurement: the claim asserts rope, the metrics describe tangled rope, and the per-seat computation reveals why different actors experience it so differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Existential-risk researchers: d near 0.2 (beneficiary end) — they benefit substantially from the dual-priority frame, have institutional power and mobile exit options, face minimal extraction beyond having to acknowledge near-term research. Near-term harm researchers: d near 0.5 (symmetric) — they benefit from institutional recognition but are constrained by the existential-risk-dominant paradigm and compete for limited funding; moderate extraction, moderate benefit. Affected communities (labor, marginalized populations): d near 0.85 (target end) — powerless, identity-locked or trapped in the harms the systems cause, no voice in research agenda, subordinated to speculative risks, high extraction. Funding bodies: d near 0.15 (beneficiary end) — they benefit from controlling a comprehensive safety mandate and capture resource flow, face minimal extraction. The structural asymmetry is the story's point: the constraint benefits those with power to set agendas and harms those with no voice in them.
 *
 * MANDATROPHY ANALYSIS:
 *   The dual-priority commitment risks mandatrophy on the near-term-harms branch: the founding problem ('prevent institutional fission') was real in the early phase but has been substantially solved — the research community now coexists (if unequally) within a single institutional framework. The problem is whether the mandate has outlived its function. If the dual-priority frame is now simply a device for subordinating present-harm research to existential-risk research, the mandate should be classified as mandatrophy (a zombie commitment). The measurement series and commentary show theater rising (performative coordination without resource alignment), extraction plateauing (the subordination is stable, not intensifying), and resistance high (the arrangement is contested but stable). This suggests early mandatrophy: the unified framework is maintained for institutional coherence, not because it serves its founding function of preventing fission. The tangled-rope classification is defensible because genuine coordination still occurs (existential-risk and near-term research do interact, funding streams do blend), but the rising theater_ratio flags the hypothesis that extraction is becoming the primary function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_coherence,
    'Can a unified AI safety commitment genuinely allocate resources equitably between existential-risk and near-term-harms research, or is the commitment structurally biased toward long-term speculation?',
    'Multi-year institutional audit of funding allocation across both branches, controlling for researcher power and institutional entrenchment. Track whether near-term harm research receives proportional resources relative to workforce displaced, discrimination incidents documented, or population affected.',
    'If allocation is demonstrably biased toward existential risk despite equal harm metrics, the dual-priority reading becomes mandatrophy (zombie commitment) and reclassifies to snare (the ''dual priority'' is rhetorical cover for existential-risk dominance). If allocation tracks harm metrics, the tangled-rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_allocation_coherence, empirical, 'Whether the dual-priority commitment can operationally support both branches or is structurally biased.').

omega_variable(
    suppression_internalization,
    'Is the suppression of near-term-harm researcher advocacy structural (institutional barriers to funding and publication) or internalized (researchers self-suppress to maintain institutional legitimacy)?',
    'Post-separation trajectory: if near-term harm research flourishes under independent institutional support (separate funding stream, journals, conferences), suppression was internalized; if research stalls due to loss of legitimacy spillover from existential-risk institutions, suppression is structural.',
    'Internalized suppression indicates identity-lock and higher effective extraction (targets carry the suppression with them even after exit). Structural suppression is higher in magnitude but allows clearer separation. If internalized, the constraint''s effective extraction is worse than the authored 0.62 suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of near-term harm advocacy is structural or internalized.').

omega_variable(
    mandate_obsolescence,
    'Has the dual-priority commitment''s founding function (prevent institutional fission between existential-risk and near-term-harms research) been achieved to the point that the mandate is now obsolete, persisting only through institutional inertia?',
    'Counterfactual: would existential-risk and near-term-harms research separate completely if the dual-priority commitment dissolved? If yes, mandate is live; if no, mandate is dead (mandatrophy).',
    'If mandatrophy is confirmed, the constraint reclassifies from tangled-rope to piton (persists through institutional theater, not functional need). If mandate is live, tangled-rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence, conceptual, 'Whether the dual-priority commitment has outlived its founding function.').

omega_variable(
    reading_foreclosure_test,
    'Is the dual-priority reading logically foreclosed by either sibling reading (existential-risk-only or near-term-harms-only), or do all three coexist as live positions in the field?',
    'Institutional census: identify major research institutions, funding bodies, and regulatory agencies by their declared AI-safety position. Are there pure existential-risk-only institutions? Pure near-term-harms-only institutions? Institutions claiming dual priority? If all three types exist and are stable, the readings coexist. If one has been eliminated by institutional pressure or philosophical argument, note the foreclosure.',
    'If readings coexist, the network edges should all be ''coexists_with''. If one reading forecloses another (e.g., if a philosophical argument convinced the field that existential risk necessarily dominates near-term concerns), the edge becomes ''forecloses'' and the network structure changes to capture the argument''s force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, empirical, 'Whether the dual-priority reading forecloses, is foreclosed by, or coexists with its sibling readings.').

omega_variable(
    kernel_referent_stability,
    'What is the kernel (the contested claim) stable referent for this reading? Is it ''the institutional arrangements governing AI safety research,'' or ''the actual trajectory of risks from AI systems,'' or ''the normative commitment of the safety field''?',
    'Frame analysis: ask stakeholders (existential-risk researchers, near-term harm researchers, affected communities, funders, regulators) what they believe the dual-priority commitment is about. Are they answering the same question? If not, they inhabit different kernels and the reading is under-determined.',
    'If the kernel is unstable (different stakeholders believe the commitment addresses different referents), the reading cannot be ε-invariant and should decompose into separate stories. If stable, the reading is coherent but may be extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_referent_stability, conceptual, 'Whether the kernel referent is stable across stakeholder interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__dual_priority_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__dual_priority_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement(ai_s_tr_t15, ai_safety_commitment__dual_priority_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__dual_priority_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(ai_s_tr_t25, ai_safety_commitment__dual_priority_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__dual_priority_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__dual_priority_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(ai_s_be_t15, ai_safety_commitment__dual_priority_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__dual_priority_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(ai_s_be_t25, ai_safety_commitment__dual_priority_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__dual_priority_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__dual_priority_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(ai_s_su_t15, ai_safety_commitment__dual_priority_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__dual_priority_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ai_s_su_t25, ai_safety_commitment__dual_priority_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__dual_priority_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is the DUAL_PRIORITY reading of the AI_SAFETY_COMMITMENT kernel. Two sibling readings are authored separately: (1) EXISTENTIAL_RISK reading — claims existential-risk prevention is the core of AI safety, treats near-term harms as deployment-specific rather than safety-domain issues. (2) NEAR_TERM_HARMS reading — claims near-term harms are the core of AI safety, treats existential speculation as institutional distraction. The three readings decompose because their ε values diverge substantially: dual-priority reading authors ε=0.62 (high extraction from affected communities due to resource subordination); existential-risk reading authors lower ε for the existential-concern domain but higher for exclusion of near-term voices; near-term-harms reading authors high ε for present-harm subordination. Each reading has a different victim set and beneficiary structure. The network edges (both siblings are affected by this reading) capture the structural competition for institutional resources and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, institutional, 0.18).
constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, powerful, 0.25).
constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
