% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

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
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: Integrated Alignment Commitment: Control and Justice as Non-Exclusive Priorities
 *   domain: AI governance/technology ethics/risk assessment
 *
 * SUMMARY:
 *   This story instantiates the integrated reading of the AI alignment
 *   commitment kernel: the claim that alignment work must attend
 *   simultaneously to control problems (preventing catastrophic loss of
 *   oversight over advanced systems) and justice problems (preventing
 *   reproduction of bias and present-day harm) as non-exclusive, mutually
 *   necessary components of a single unified effort. This is distinct from
 *   the safety_control_reading (which treats catastrophic loss of control as
 *   the alignment problem) and the ethics_justice_reading (which treats
 *   present-day bias and harm as the alignment problem) — those are separate
 *   constraint stories with their own ε values, not alternative measurements
 *   of this one. The integrated reading's own claim to legitimacy is that
 *   fragmentation into separate silos wastes finite institutional attention
 *   and produces a false dichotomy; this story evaluates whether the
 *   integrated commitment itself, as an institutional arrangement, delivers
 *   on that promise or produces a new extraction pattern where the appearance
 *   of comprehensive coverage substitutes for depth on either front.
 *
 * KEY AGENTS:
 *   - integrated_alignment_researchers: agenda_setter (organized/mobile) — administers the integrated framing and captures institutional prestige from its adoption
 *   - cross_coalition_funders: beneficiary (institutional/mobile) — satisfies multiple donor constituencies with one funding line
 *   - future_humanity: beneficiary and payer (powerless/trapped) — cannot participate but bears both the upside of genuine control attention and the downside of diluted focus
 *   - present_marginalized_populations: payer (powerless/trapped) — bears present harm that may be deprioritized relative to speculative future scenarios under integration
 *   - control_only_research_programs and justice_only_research_programs: payer (moderate/constrained) — bear framing-labor costs and compete for the same diluted pool
 *   - ai_developers: observer and agenda_setter (institutional/arbitrage) — can use the integrated commitment as reputational cover without binding obligation on either dimension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.52).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.44).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Integrated Alignment Commitment: Control and Justice as Non-Exclusive Priorities").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "AI governance/technology ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, 'a96fa55c-e0e3-4e95-a0cf-10a02114cd79').
narrative_ontology:cs_kernel_codification('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', distributed).
narrative_ontology:cs_authority_grounding('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', distributed).
narrative_ontology:cs_reading_relation('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', ai_alignment_commitment__ethics_justice_reading, influences).
narrative_ontology:cs_axiom('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', foundational, control_justice_non_exclusivity_thesis).
narrative_ontology:cs_axiom_status(control_justice_non_exclusivity_thesis, holdable).
narrative_ontology:cs_axiom_grounding('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', control_justice_non_exclusivity_thesis, conventional).
narrative_ontology:cs_axiom('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', secondary, siloed_alignment_effort_is_self_defeating).
narrative_ontology:cs_axiom_status(siloed_alignment_effort_is_self_defeating, holdable).
narrative_ontology:cs_axiom_grounding('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', siloed_alignment_effort_is_self_defeating, instrumental).
narrative_ontology:cs_reference_frame('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', pre_fragmentation_unified_alignment_ideal).
narrative_ontology:cs_drift_state('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', contemporary_institutional_alignment_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a96fa55c-e0e3-4e95-a0cf-10a02114cd79', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, cross_coalition_funders).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, control_only_research_programs).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, justice_only_research_programs).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__integrated_reading, false_dichotomy_rejection_doctrine).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__integrated_reading, unified_effort_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate that control problems (loss of oversight over powerful systems) and justice problems (bias, present-day harm reproduction) must be addressed as a single integrated research and policy agenda rather than as competing priorities. They set conference agendas, shape funding calls, and administer institutional review criteria that reward integrated framing. Their institutional standing and funding access grow as the integrated frame gains adoption.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers, beneficiary).

% Philanthropic and corporate funders who can present a single integrated portfolio as addressing both existential risk and social harm concerns simultaneously, satisfying multiple donor and stakeholder constituencies with one funding line rather than defending two separately contested allocations.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, cross_coalition_funders, beneficiary,
    institutional, generational, mobile, global).

% Has no voice in current resource allocation but stands to benefit if genuine attention to control problems prevents catastrophic loss of oversight over advanced systems. Simultaneously bears cost if integration dilutes control-specific research rigor by spreading finite technical attention across justice concerns that, however legitimate, do not address the loss-of-control failure mode. Cannot exit the arrangement being made on its behalf now.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, future_humanity, payer).

% Communities currently experiencing algorithmic bias, discriminatory automated decisions, and surveillance harms. Under the integrated frame, their concrete present injuries compete for research and policy attention with abstract future catastrophic scenarios; when integration functions as rhetorical cover rather than genuine dual-attention, their harms are deprioritized relative to a dedicated justice-only agenda while being nominally represented as 'covered.'
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, present_marginalized_populations, payer,
    powerless, immediate, trapped, global).

% Technical safety researchers focused narrowly on preventing catastrophic loss of control (interpretability, corrigibility, capability control). Under the integrated commitment, they face pressure to incorporate justice framing into grant applications and institutional messaging even when their specific technical contribution has no natural connection to present-day bias harms, diluting focus and forcing costly framing labor.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, control_only_research_programs, payer,
    moderate, biographical, constrained, global).

% Researchers and advocates focused on algorithmic fairness, bias audits, and present-day accountability. Under the integrated commitment, they compete for the same funding pools and institutional attention as existential-risk-framed control work, and their concrete, immediately actionable harms are sometimes deprioritized relative to speculative catastrophic scenarios that carry more institutional prestige and funding weight in AI governance discourse.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, justice_only_research_programs, payer,
    moderate, biographical, constrained, global).

% Frontier AI labs can point to an integrated alignment commitment as evidence they are addressing both concerns, which can function as reputational cover that reduces pressure for concrete measures on either front. They observe and sometimes fund the discourse without being bound by enforceable standards on either control or justice dimensions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_developers, observer,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, ai_developers, agenda_setter).

% Would need clear, separable, actionable standards to regulate either control risks or justice harms effectively, but the integrated framing's emphasis on non-exclusivity can produce policy language too diffuse to generate enforceable rules for either problem, leaving regulators without a clean lever.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, policy_regulators, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a resource-and-attention war between control-focused and justice-focused AI safety communities by establishing that both are legitimate, simultaneously necessary components of alignment, avoiding a zero-sum framing that would let institutions dismiss one concern to fund the other exclusively.
% TRANSFER_FUNCTION: Moves institutional attention, funding, and research prestige between control-focused and justice-focused communities under a shared banner; also transfers moral legitimacy to actors (funders, developers) who can claim comprehensive coverage without necessarily funding either concern at the depth a dedicated single-focus program would require.
% ABSENT_VOICES: Present marginalized populations experiencing concrete algorithmic harm right now are formally 'included' in the integrated frame but structurally absent from the rooms where integration's actual resource splits are decided; future humanity has no representative at all and its interests are asserted by proxy by whichever coalition currently controls the integrated framing's emphasis.
% DISAPPEARANCE_RATIONALE: If the integrated commitment vanished, control-focused and justice-focused research communities would likely re-fragment into separately funded, separately institutionalized camps — some in the field argue this would sharpen focus and increase rigor for both; others argue it would recreate a zero-sum funding war in which justice concerns, having less institutional prestige and shorter time horizons attached to their urgency claims, would lose ground to control-focused existential-risk framing. Both outcomes are defended by credible parties, hence contested.
% FOUNDING_PROBLEM: Early AI safety discourse treated 'alignment' as synonymous with preventing catastrophic loss of control, marginalizing critiques from fairness, accountability, and transparency researchers who argued that present-day discriminatory harms were being erased from the alignment conversation entirely; the integrated reading was built to stop this exclusionary framing from becoming institutionally permanent.
% FOUNDING_PROBLEM_CORROBORATION: Independent science-and-technology-studies scholars and AI policy historians outside both the control and justice research communities corroborate that the exclusionary framing problem was real and that the integrated commitment emerged partly in response to documented institutional capture of 'alignment' by control-only framings (e.g., critiques published by STS researchers not funded by either coalition). However, whether the integrated commitment has actually resolved the founding problem, versus merely relabeling the same resource competition under shared vocabulary, remains disputed even among those corroborating sources.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.52 rather than high, because the integrated commitment has a genuine coordination function (preventing a zero-sum framing war between two legitimate concerns) that is not purely cover — but the rising trajectory across the measurement grid (0.32 to 0.52) reflects a real risk that comprehensive-sounding integration becomes a vehicle for diffusing accountability on both fronts simultaneously, benefiting whoever administers the integrated frame's emphasis at any given moment. Theater ratio rises from 0.18 to 0.40 because as the integrated commitment becomes institutionally standard language, an increasing share of its invocation is rhetorical positioning (grant boilerplate, conference framing) rather than genuine dual-track technical and justice work. Suppression is authored moderate (0.44) and rising modestly (0.30 to 0.44) because the commitment does not suppress by force, but by making it institutionally costly to argue for single-focus depth — a control researcher or justice researcher who declines the integrated frame risks appearing to dismiss a legitimate concern, which is a real if soft form of suppression on unmodified single-focus advocacy.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (integrated_alignment_researchers), the arrangement is a well-reasoned rejection of a false and costly dichotomy. From the present_marginalized_populations and control_only/justice_only payer seats, the same arrangement can register as a mechanism that defers depth on their specific urgent concern in favor of a vaguer, more fundable, more institutionally comfortable comprehensive-sounding claim. The engine computes these divergent per-seat readings from the declared structural data; the story does not adjudicate which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Integrated_alignment_researchers and cross_coalition_funders sit near the beneficiary end: they administer or fund under a frame that expands their legitimacy and reduces the reputational risk of appearing to have chosen sides. Future_humanity is dual-positioned — genuinely served if integration produces real control attention, genuinely harmed if integration dilutes it, and structurally powerless to correct course either way, which is why it appears as both beneficiary and payer with trapped exit and civilizational time horizon. Present_marginalized_populations bear the clearest present-tense cost: their immediate, documentable harms compete against speculative future catastrophic scenarios that often carry more institutional prestige in AI governance discourse, and their formal 'inclusion' in the integrated frame does not guarantee resource parity. The two single-focus research communities are structurally payers under this reading because the integrated commitment imposes framing costs on them regardless of whether integration serves their specific technical or advocacy goals.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that 'alignment' had become synonymous with control-only concerns, erasing justice critiques — was real and is corroborated by researchers outside either coalition. But founding_problem_status is authored as contested rather than resolved: the integrated commitment may have replaced exclusionary control-only framing with a differently-shaped extraction, where the appearance of comprehensive coverage under one banner substitutes for the harder institutional work of funding both programs at the depth each requires. This is precisely the tangled_rope signature: genuine coordination function (avoiding a false, resource-wasting dichotomy) coexisting with asymmetric extraction (present-tense marginalized harm and single-focus research rigor both pay a diffusion cost that primarily benefits whoever administers the integrated frame's current emphasis).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_as_genuine_synthesis_or_diffusion_cover,
    'Does the integrated reading actually produce deeper simultaneous attention to both control and justice problems, or does it function primarily as institutional cover that allows funders and developers to claim comprehensive coverage while under-resourcing both relative to what dedicated single-focus programs would achieve?',
    'Longitudinal tracking of funding allocation and research output depth (technical rigor metrics, publication density, concrete policy impact) for control-specific and justice-specific subcomponents within integrated-framed institutions, compared against equivalent metrics from single-focus institutions over the same interval.',
    'If integration produces genuine depth gains on both fronts, the coordination function dominates and the constraint reads closer to rope. If integration systematically under-resources both relative to single-focus baselines, the extraction function dominates and the constraint reads closer to snare wearing tangled_rope''s coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_as_genuine_synthesis_or_diffusion_cover, empirical, 'Whether integrated framing genuinely deepens dual attention or diffuses accountability for both concerns.').

omega_variable(
    kernel_reading_relationship_ai_alignment_commitment,
    'How does the integrated_reading relate structurally to the safety_control_reading and ethics_justice_reading of the ai_alignment_commitment kernel — are these three genuinely non-exclusive framings, or does the integrated reading''s claim to non-exclusivity itself foreclose the possibility that either single-focus reading could be structurally correct (i.e., that in a given resource-constrained institutional moment, one concern genuinely should take priority)?',
    'Comparative analysis across the three linked constraint stories: examine whether historical episodes exist where prioritizing control work over justice work (or vice versa) produced better real-world outcomes than integrated allocation would have, which would falsify the integrated reading''s core non-exclusivity premise as a general claim.',
    'If genuine resource-constrained tradeoff moments exist where single-focus prioritization outperforms integration, the integrated reading''s founding claim (that fragmentation is always the error) is itself a contestable normative position, not a structural fact — which would shift this story''s claimed_type consideration and its axiom status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relationship_ai_alignment_commitment, conceptual, 'Whether non-exclusivity is a structural fact about alignment or a contestable normative commitment that itself has costs.').

omega_variable(
    future_humanity_representation_ambiguity,
    'Who legitimately speaks for future humanity''s interest in genuine control-risk attention within the integrated frame, given that future humanity cannot participate in current resource allocation and its interests are asserted by proxy by whichever coalition currently holds institutional influence?',
    'Examine whether integrated-framed institutions have established any accountability mechanism (e.g., red-team review, external audit) specifically for whether control-relevant technical work maintains rigor comparable to dedicated control-only programs, as opposed to relying on self-report from the integrated coalition.',
    'Absence of such a mechanism would support classifying future_humanity''s beneficiary status as largely rhetorical rather than structurally secured, strengthening the payer characterization already declared for this stakeholder.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_humanity_representation_ambiguity, conceptual, 'Whether future humanity''s stated benefit under integration is structurally secured or merely asserted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__integrated_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__integrated_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__integrated_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__integrated_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__integrated_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_commitment__integrated_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__integrated_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__integrated_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__integrated_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__integrated_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__integrated_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_commitment__integrated_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__integrated_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__integrated_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__integrated_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__integrated_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__integrated_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__integrated_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_commitment__integrated_reading, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__integrated_reading, 0.1).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__ethics_justice_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the ai_alignment_commitment kernel. safety_control_reading treats catastrophic loss of control as the alignment problem (higher ε from control-focused institutional capture, victim set: future humanity and displaced justice concerns). ethics_justice_reading treats present-day bias and harm reproduction as the alignment problem (victim set: present marginalized populations under a differently-shaped institutional extraction). integrated_reading (this story) rejects the exclusivity of either framing but is authored with its own non-trivial ε (0.52) reflecting the risk that comprehensive-sounding synthesis becomes a diffusion mechanism rather than a genuine solution to either sub-problem. The three stories share no metric values by design — each has independently authored extractiveness, victim sets, and claimed_type, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
