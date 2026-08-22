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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment as Justice Prevention: Bias Reproduction Constraint (Ethics-Justice Reading)
 *   domain: AI governance / technology ethics / social justice
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested 'AI alignment'
 *   kernel—the ethics-justice reading. The kernel itself is the stabilized
 *   commitment that AI systems should be aligned with human values and social
 *   good. The reading reframes alignment to mean: preventing reproduction of
 *   social bias and present-day harm, with specific focus on marginalized
 *   populations and concrete, documented discrimination. This reading
 *   COEXISTS with a safety-control reading (alignment as preventing
 *   catastrophic loss of control over advanced AI) and an integrated reading
 *   (both control and justice are essential and non-exclusive). The three
 *   readings share the kernel (AI alignment commitment) but differ on what
 *   alignment MEANS, who counts as a victim, what counts as evidence of
 *   success, and how resources should be allocated. This story describes the
 *   ethics-justice reading's structure: who benefits (marginalized
 *   communities, affected populations), who bears costs (long-term safety
 *   researchers, control-risk funding streams), how enforcement operates
 *   (governance institutions operationalizing fairness metrics and bias
 *   auditing), and why the constraint persists (genuine coordination function
 *   around present harms, layered with extraction from safety research
 *   authority).
 *
 * KEY AGENTS:
 *   - marginalized_communities: Specific populations experiencing documented bias in deployed AI systems (hiring, credit, criminal justice, moderation); trapped exit — benefit from bias prevention focus.
 *   - ai_safety_research_community: Long-term control and specification risk researchers; their agenda is deprioritized, funding diverted, narrative authority challenged — they are the victims of this reading's extraction.
 *   - ai_governance_institutions: Regulatory bodies, corporate boards, NGO coalitions; they are the agenda-setters, deciding what 'alignment' means operationally and allocating enforcement machinery.
 *   - ai_developers: Powerful but constrained; they pay compliance costs (bias auditing) and opportunity costs (safety research diverted), though they gain reputational protection.
 *   - control_risk_advocates: Excluded from this reading's scope; their long-term risk focus is not denied but deprioritized; they represent the contestation point.
 *   - alignment_researchers__safety_focused: Observer seat; they see how the constraint structures what gets measured, funded, and legitimized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.72).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment as Justice Prevention: Bias Reproduction Constraint (Ethics-Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "AI governance / technology ethics / social justice").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, 'e95457b3-57ee-456a-a6d2-cca14cb34595').
narrative_ontology:cs_kernel_codification('e95457b3-57ee-456a-a6d2-cca14cb34595', distributed).
narrative_ontology:cs_authority_grounding('e95457b3-57ee-456a-a6d2-cca14cb34595', distributed).
narrative_ontology:cs_reading_relation('e95457b3-57ee-456a-a6d2-cca14cb34595', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('e95457b3-57ee-456a-a6d2-cca14cb34595', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('e95457b3-57ee-456a-a6d2-cca14cb34595', foundational, present_harm_prioritization).
narrative_ontology:cs_axiom_status(present_harm_prioritization, holdable).
narrative_ontology:cs_axiom_grounding('e95457b3-57ee-456a-a6d2-cca14cb34595', present_harm_prioritization, empirically_contingent).
narrative_ontology:cs_axiom('e95457b3-57ee-456a-a6d2-cca14cb34595', foundational, marginalized_populations_epistemic_authority).
narrative_ontology:cs_axiom_status(marginalized_populations_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('e95457b3-57ee-456a-a6d2-cca14cb34595', marginalized_populations_epistemic_authority, deontological).
narrative_ontology:cs_reference_frame('e95457b3-57ee-456a-a6d2-cca14cb34595', justice_centered_alignment).
narrative_ontology:cs_drift_state('e95457b3-57ee-456a-a6d2-cca14cb34595', contemporary_ai_deployment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e95457b3-57ee-456a-a6d2-cca14cb34595', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, affected_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_ai_safety_research).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, control_risk_funding_streams).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ai_developers).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_safety_research_community).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations historically subject to discriminatory automated decision-making (hiring systems, credit scoring, criminal justice algorithms, content moderation). Benefit from alignment commitments that center bias prevention and demand transparent, auditable systems that do not reproduce existing harms. Their exit from algorithmic systems is constrained by integration into core institutional services.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_communities, beneficiary,
    powerless, biographical, trapped, global).

% Researchers focused on long-term control problems, scalable oversight, and superintelligence alignment. Under this reading, their research agenda is reframed as extractive or secondary to immediate justice concerns. Funding, institutional support, and narrative authority shift toward bias prevention and fairness audits. Their alternative framing (that long-term safety is essential precondition) is treated as displacement of current-harm focus.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_safety_research_community, payer,
    moderate, generational, constrained, global).

% Regulatory bodies, corporate ethics boards, NGO coalitions that define and enforce alignment standards. Under this reading, they commit enforcement machinery to bias auditing, fairness metrics, community impact assessments, and transparency requirements. They allocate resources—funding, certification authority, compliance review—to operationalize the justice-centered definition of alignment.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_governance_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Corporations and labs building deployed AI systems. They bear compliance costs (fairness testing, bias audits, documentation requirements) and opportunity costs (engineering time diverted from capability development). They also have secondary benefit: reputational protection and reduced liability exposure from proactive bias mitigation. Exit is constrained by regulatory requirements and stakeholder pressure.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_developers, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, ai_developers, beneficiary).

% Researchers and policy actors focused on long-term control and specification risks. Their concerns are structurally excluded from the ethics-justice reading's scope: the reading does not deny their concerns but deprioritizes them as not-immediate-harm. They argue that justice without control is incoherent (an unaligned superintelligence will impose its own harm structure regardless of fairness intentions). Their exclusion is not elimination but institutional marginalization.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, control_risk_advocates, excluded,
    moderate, civilizational, constrained, global).

% Communities and advocates organized around specific harms: workers displaced by automation, borrowers denied credit by biased scoring, defendants subjected to biased risk assessment, content creators suppressed by moderation algorithms. Benefit from alignment definitions that center their lived experience and demand remedy. Their exit is constrained by dependence on services that employ the systems.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, affected_populations, beneficiary,
    organized, biographical, constrained, global).

% Analytical seat representing the epistemic community attempting to define alignment rigorously across competing framings. They observe the constraint's operation: how justice prioritization structures research allocation, how the measurement apparatus privileges current-harm observables over long-term-risk modeling, how the reading's enforcement selects for certain research questions and away from others.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, alignment_researchers__safety_focused, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__ethics_justice_reading, ai_governance_institutions).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__ethics_justice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns AI system deployment with justice and equity principles: coordinates developers, governance bodies, and affected communities around a shared commitment that AI systems must not reproduce or amplify present-day discrimination and that marginalized populations must be explicitly centered in alignment work.
% TRANSFER_FUNCTION: Redirects research funding, institutional authority, and engineering effort from long-term safety/control research toward immediate bias prevention, fairness testing, and justice impact assessment. Moves political legitimacy from abstract risk models toward concrete community testimony about present harms.
% ABSENT_VOICES: Long-term AI safety researchers focused on control and specification risks; futurist communities concerned with far-future coordination problems; researchers modeling s-risk and existential harm whose work sits outside the present-harm measurement frame. They would argue that justice without control is unstable and that deprioritizing safety research creates a different class of harm.
% DISAPPEARANCE_RATIONALE: If this alignment commitment (justice-centered, bias-prevention-focused) disappeared overnight, AI governance would revert to capability-acceleration-first or abstract safety-focused framings; community participation in alignment work would collapse; bias auditing and fairness certification would lose institutional backing; resources would flow back to long-term safety research. The distribution of harms would shift from concentrated on marginalized communities (under the current constraint) to diffuse or future-concentrated (under the counterfactual).
% FOUNDING_PROBLEM: AI systems are being deployed into high-stakes decision-making domains where they reproduce and amplify historical discrimination: hiring algorithms that replicate gender and racial bias, credit scoring that locks out marginalized borrowers, criminal justice systems that entrench racial disparities in sentencing recommendations, content moderation that disproportionately suppresses marginalized voices. These are present, measurable, documented harms affecting specific populations now.
% FOUNDING_PROBLEM_CORROBORATION: Documented by independent researchers (Buolamwini & Gebru on facial recognition bias, Obermeyer et al. on healthcare algorithm discrimination, ProPublica on COMPAS recidivism scoring), affected communities testifying before regulatory bodies, and audit reports from civil rights organizations. Corroboration comes from outside the governance institutions and research communities that benefit from justice-centered alignment framing.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.68) reflects a substantive asymmetry: this reading coordinates genuine justice concerns (centering present harms is a real coordination function) while simultaneously extracting authority and resources from control-risk research. The coordination function is real—marginalized communities benefit from explicit centering—but it is entwined with what amounts to epistemic suppression of an alternative research agenda. Suppression (0.72) captures the enforcement machinery required to maintain this reading's primacy: institutional selection for justice-framed research, funding barriers for safety-focused work, narrative framing that treats control research as displacement or delay tactics. Theater (0.48) reflects that the constraint carries genuine operative function (bias audits are real, fairness testing happens) but an increasing share of institutional activity is performative—community consultation that does not alter system design, certification that does not prevent deployment, audits that document but do not remediate. The measurement series shows accumulating extraction as safety research faces sustained deprioritization (extractiveness rising 0.52→0.68) and as performative elements expand (theater 0.38→0.48). Accessibility collapse (0.61) is moderate: alternatives to bias-prevention focus exist (the safety-control reading, the integrated reading) and remain live in academic and some policy circles, but institutional closure makes them less accessible. Resistance (0.58) is substantial: safety researchers push back, argue that control and justice are non-exclusive, and some policy actors maintain parallel funding for long-term work—but the structural tide favors the justice-centered reading under current governance conditions.
 *
 * PERSPECTIVAL GAP:
 *   The governance-institution seat (agenda-setter) experiences this constraint as legitimate and necessary coordination: centering justice is morally right, bias prevention is operationally clear, affected communities must be heard. From this seat, the constraint is rope-like (genuine coordination function). The marginalized-communities seat experiences it as protection: the constraint centers their documented harms, allocates institutional power to their testimony, and demands remediation. From this seat, the constraint is also beneficiary-aligned and legitimately enforced. The long-term-safety-research seat experiences it as extraction: their research agenda is deprioritized, their funding is redirected, their core concerns (control risk, specification hazard) are treated as secondary or displacement tactics. From this seat, the constraint looks like snare—enforced suppression of an alternative research program. The developer seat sits between: they pay compliance costs (payer) but gain reputational protection and regulatory favor (secondary benefit). The engine computes all of these asymmetries from the structural data (power, exit_options, declared roles, beneficiary/victim status). The authored claim (tangled_rope) reflects the reading's own internal logic: genuine coordination (justice centering) + asymmetric extraction (safety research deprioritization) + active enforcement (institutional machinery maintaining the framing's dominance).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for marginalized_communities: low d (~0.1-0.2) — they are beneficiaries (centered in policy, their testimony is demanded, harms are explicitly the measure of success), trapped in exit (dependence on systems they cannot exit), organized (advocacy groups, community coalitions amplify individual power). Beneficiary status drives d down sharply. Directionality for ai_safety_research_community: high d (~0.8-0.9) — they are victims (deprioritized, defunded, reframed as obstacles), moderate power (institutional presence, journal authority, some funding streams), but constrained exit (reframing yourself into a different research domain is slow and costly; intellectual investment is sunk). Victim status drives d up; moderate power and constrained exit keep it below full target. Directionality for ai_governance_institutions: low d (~0.0-0.1) — they are agenda-setters (they define the constraint and enforce it), institutional power (they command resources and authority), arbitrage exit (they can shift institutional meaning, redefine metrics, reallocate resources if the reading's legitimacy erodes). Agenda-setter status and arbitrage exit drive d near the beneficiary end. Directionality for ai_developers: mid-high d (~0.55-0.65) — they are payers (compliance costs, opportunity costs), powerful but constrained exit (regulatory requirements and reputational pressure make exit from bias-mitigation compliance costly, though their fundamental business model is not threatened). Payer status and constrained exit keep d in the mid-target range; power keeps it from reaching full target. Directionality for control_risk_advocates: not formally authored in stakeholder roles (they are excluded), but structurally they experience d ~0.7-0.8 — excluded from the official conversation, their research receives no mandate from this constraint, their voice is kept out of governance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits tension between its founding problem and its operational function. The founding problem is live and specific: AI systems DO reproduce present-day bias, and this causes documented harm to marginalized communities. That function remains operative. However, the constraint increasingly operates as a reading-dominance enforcement mechanism: it allocates institutional power to one interpretation of alignment (ethics-justice) at the expense of another (safety-control). If the mandate-vs-function test applies: the founding problem (bias prevention) remains valid, but the constraint's persistence is now sustained by institutional and narrative machinery that maintains the ethics-justice reading's primacy. This is different from mandatrophy (where the founding problem is dead but the constraint persists theatrically)—it is more like a tangled rope in which coordination function (bias prevention) is entwined with extraction function (safety research deprioritization). The theater_ratio rising to 0.48 suggests growing performative elements: governance institutions holding consultations that do not alter policy, auditing practices that are ceremonial, community testimony that is heard but not acted upon. But this is not yet full piton status because the coordination function remains real. The constraint is best described as tangled_rope trending toward higher theater, which the measurement trajectory captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_reading_vs_empirical_bias,
    'Is bias prevention genuinely foundational to alignment (essential precondition for any safe AI system), or is it one important concern among others including long-term control risk?',
    'The empirical question is whether AI systems can be simultaneously biased (against present stakeholders) and safely controlled (in the long-term sense). If a system can be controlled but biased, the concerns are separable; if control failure enables systematic bias amplification, they are entangled.',
    'If separable, this reading''s extraction from control research is justified (prioritize the more urgent concern). If entangled, deprioritizing control research risks enabling a different class of harm (uncontrolled systems impose their own bias regardless of fairness intentions), and the constraint''s classification shifts from tangled_rope toward snare (pure extraction disguised as justice prioritization).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_reading_vs_empirical_bias, empirical, 'Whether bias prevention and control are conceptually separable or empirically entangled concerns.').

omega_variable(
    research_funding_zero_sum_assumption,
    'Is funding for justice-centered AI alignment zero-sum with funding for control-risk research, or could both agendas expand simultaneously?',
    'Observe whether recent AI alignment funding growth flows primarily to ethics-justice work, primarily to control work, or both. Historical data: 2018-2022 showed control research expanding; 2022-2024 shows ethics-justice work expanding faster. The empirical question is whether the latter displaced the former or whether total alignment funding expanded.',
    'If zero-sum (total alignment budget fixed, ethics work grows while control work shrinks), the constraint operates as pure extraction wrapped in justice language. If non-zero-sum (both expand), the constraint is genuine tangled rope without the extraction element dominating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(research_funding_zero_sum_assumption, empirical, 'Whether ethics-justice alignment funding growth represents reallocation or net growth in AI alignment resources.').

omega_variable(
    marginalized_population_definition_scope,
    'Who counts as marginalized populations entitled to protection under this alignment constraint? Current discourse centers race, gender, disability, income in present systems; does it include future generations affected by long-term AI risks?',
    'Examine governance documents, funding criteria, and impact assessments: do they include future-affected populations in the scope of justice concerns, or only present-population harms?',
    'If future populations are excluded from the justice scope, the constraint''s beneficiary set is defined by temporal proximity (only present sufferers count). This strengthens the deprioritization of control research and makes the constraint''s extraction function more apparent. If future populations are included, the boundary between ethics-justice and safety-control readings becomes less clean, and the integrated reading gains force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_population_definition_scope, conceptual, 'The temporal scope of who counts as a marginalized population entitled to alignment protection.').

omega_variable(
    suppression_mechanism_structural_vs_ideological,
    'Is the measured suppression (0.72) of control-risk research structural (budget constraints, institutional incentives, grant-making priority shifts) or ideological (narrative that control work is displacement or less urgent)?',
    'Post-exit analysis: if a researcher leaves the ethics-justice alignment space and continues control-risk work, do they experience reduction in suppression? If so, suppression is structural (tied to institutional position). If narratives persist and suppression continues, it is partially ideological (carried in worldview).',
    'Structural suppression is reversible by institutional change; ideological suppression persists even after structural incentives change. High ideological suppression suggests the constraint''s extraction component is internalized and durable. This affects the classification trajectory and the likelihood of remediation through policy change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_ideological, empirical, 'Whether suppression of control research is structural or internalized.').

omega_variable(
    kernel_reading_necessity,
    'Are these three readings genuinely three stable readings of a single contested kernel, or is one of them (safety_control_reading) actually a distinct constraint that happens to use similar vocabulary?',
    'Check whether all three readings share the same core commitment: that AI systems should be aligned with human values and social good. If yes, they are readings of a shared kernel. If the safety-control reading rejects the kernel (argues alignment is not about values or social good but only about maintaining control), they are separate constraints, not readings.',
    'If truly three readings of one kernel: the constraint story is correct, and cs_structure relationships apply. If the safety-control position is a separate constraint that merely looks like a reading: the kernel decomposition is wrong, and the network should treat them as affects_constraints neighbors, not sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_necessity, conceptual, 'Whether the three declared positions are genuinely readings of a shared kernel or separate constraints using overlapping language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ai_a_tr_t3, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 3, 0.41).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 6, 0.44).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement(ai_a_tr_t18, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 18, 0.47).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ai_a_be_t3, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 3, 0.56).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(ai_a_be_t18, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_a_su_t3, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 3, 0.63).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 6, 0.67).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(ai_a_su_t18, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 18, 0.71).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__ethics_justice_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested ai_alignment_commitment kernel. The ethics-justice reading prioritizes present-day bias prevention and centers marginalized communities as the primary victim set. The safety-control reading prioritizes long-term catastrophic-risk prevention and centers humanity-scale risks. The integrated reading asserts both concerns are essential and non-exclusive. All three readings share the stabilized commitment that 'AI systems should be aligned with human values and social good,' but diverge on what alignment MEANS operationally, what counts as evidence of success, and how resources should be allocated. The three constraint stories are linked via network.affects_constraints and are distinguished by their cs_structure.reading_relations and axioms. Do not attempt to merge them into one constraint: each reading has its own ε (differently scoped extraction), its own victim set, its own enforcement machinery, and its own classification. The decomposition is justified by ε-invariance (OQ-26 / DP-001): a single constraint cannot have multiple structurally distinct referents and multiple victim sets without losing ε's meaning as an intrinsic property.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
