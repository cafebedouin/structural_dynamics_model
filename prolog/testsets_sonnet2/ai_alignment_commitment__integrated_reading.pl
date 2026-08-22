% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Integrated Alignment Commitment: Control and Justice as Non-Exclusive Obligations
 *   domain: AI governance/technology ethics/risk assessment
 *
 * SUMMARY:
 *   The integrated reading holds that control-risk (preventing loss of
 *   control over advanced AI) and justice-risk (preventing reproduction of
 *   bias and present-day harm) are not competing priorities but components of
 *   a single alignment problem, and that treating them as exclusive is itself
 *   the source of extraction: institutions and researchers who force a choice
 *   between them waste effort, misallocate funding, and leave both present
 *   victims and future humanity worse off than a unified effort would. This
 *   story authors that reading as its own constraint, distinct from the
 *   safety_control_reading and ethics_justice_reading siblings, per the
 *   ε-invariance principle — the integrated reading's ε is about the cost of
 *   false dichotomization and the governance overhead the integration project
 *   itself introduces, not about either sibling's substantive risk domain.
 *
 * KEY AGENTS:
 *   - integrated_safety_researchers: agenda-setters who administer and benefit from the integrated framing's institutional rise
 *   - present_marginalized_populations: bear ongoing concrete harm and have no voice in resource allocation under the integrated mandate
 *   - future_humanity: powerless proxy-represented victim of any allocation error the integration project makes
 *   - single_lens_researchers_deprioritized: pay a career cost when their specialized work is relabeled incomplete
 *   - cross_disciplinary_governance_bodies: institutional beneficiaries who gain expanded jurisdiction by claiming both domains
 *   - ai_developers: excluded from the framing debate but bear whatever compliance structure it eventually produces
 *   - policy_analysts: analytical observers assessing whether integration delivers or merely rhetorically expands authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.58).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.52).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Integrated Alignment Commitment: Control and Justice as Non-Exclusive Obligations").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "AI governance/technology ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, 'dc661a86-94ff-49e4-9145-bae9f0caa782').
narrative_ontology:cs_kernel_codification('dc661a86-94ff-49e4-9145-bae9f0caa782', distributed).
narrative_ontology:cs_authority_grounding('dc661a86-94ff-49e4-9145-bae9f0caa782', distributed).
narrative_ontology:cs_reading_relation('dc661a86-94ff-49e4-9145-bae9f0caa782', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('dc661a86-94ff-49e4-9145-bae9f0caa782', ai_alignment_commitment__ethics_justice_reading, influences).
narrative_ontology:cs_axiom('dc661a86-94ff-49e4-9145-bae9f0caa782', foundational, control_and_justice_harms_are_non_exclusive_priorities).
narrative_ontology:cs_axiom_status(control_and_justice_harms_are_non_exclusive_priorities, holdable).
narrative_ontology:cs_axiom_grounding('dc661a86-94ff-49e4-9145-bae9f0caa782', control_and_justice_harms_are_non_exclusive_priorities, conventional).
narrative_ontology:cs_axiom('dc661a86-94ff-49e4-9145-bae9f0caa782', secondary, siloed_alignment_effort_is_itself_a_harm_source).
narrative_ontology:cs_axiom_status(siloed_alignment_effort_is_itself_a_harm_source, holdable).
narrative_ontology:cs_axiom_grounding('dc661a86-94ff-49e4-9145-bae9f0caa782', siloed_alignment_effort_is_itself_a_harm_source, instrumental).
narrative_ontology:cs_reference_frame('dc661a86-94ff-49e4-9145-bae9f0caa782', unified_alignment_research_agenda).
narrative_ontology:cs_drift_state('dc661a86-94ff-49e4-9145-bae9f0caa782', post_ai_safety_ai_ethics_public_dispute_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc661a86-94ff-49e4-9145-bae9f0caa782', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, integrated_safety_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, cross_disciplinary_governance_bodies).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, single_lens_researchers_deprioritized).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__integrated_reading, alignment_is_not_a_zero_sum_resource_allocation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Argue that treating control risk and justice harm as competing priorities is a structural error that starves both agendas; they administer the integrated framing in funding proposals, conference programming, and institutional mandates, and their professional standing rises as the integrated view gains institutional traction.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, integrated_safety_researchers, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, integrated_safety_researchers, beneficiary).

% Bear the concrete, documented harms of biased and discriminatory AI systems today — denied loans, misidentified by surveillance, excluded by hiring algorithms. Under an integrated framing, their urgent present harm competes for attention and resources with speculative future catastrophic-risk work; they have no seat in setting the allocation and no exit from systems already deployed against them.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, present_marginalized_populations, payer,
    powerless, immediate, trapped, global).

% Cannot advocate for itself; bears the entire cost if control problems are neglected in favor of tractable present-day justice work, or if resources are diffused across an integrated agenda that under-resources the hardest control problems. Represented only through proxies who have their own institutional incentives.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Career researchers who built expertise and institutional position within a single-lens frame (pure control theory or pure fairness/bias auditing) now find their specialized work relabeled as incomplete or siloed by the ascendant integrated framing, affecting hiring, funding, and publication venues regardless of the quality of their specific contributions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, single_lens_researchers_deprioritized, payer,
    moderate, biographical, constrained, national).

% New and existing institutions (governance councils, standards bodies, funding agencies) that adopt the integrated mandate expand their jurisdiction and legitimacy by claiming authority over both control and justice questions simultaneously, positioning themselves as indispensable coordinators rather than deferring to either specialized community.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, cross_disciplinary_governance_bodies, beneficiary,
    institutional, generational, arbitrage, global).

% Building the systems that both control-risk and justice-harm concerns are about. Have resources to engage with whichever framing minimizes their compliance burden and often prefer siloed treatment (technical safety teams handling control, separate policy teams handling bias) over an integrated mandate that would require restructuring how they allocate engineering and governance resources internally. Rarely consulted on which framing should govern their obligations.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_developers, excluded,
    powerful, biographical, mobile, global).

% Study whether integrated mandates produce better outcomes than specialized ones, or whether integration functions mainly as a rhetorical move that lets institutions claim broader authority without delivering more on either front.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__integrated_reading, cross_disciplinary_governance_bodies).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the real coordination failure where control-risk and justice-harm communities compete for the same scarce funding, attention, and regulatory bandwidth by framing both as facets of one alignment problem rather than rival claims on a fixed pool of resources.
% TRANSFER_FUNCTION: Moves institutional legitimacy, funding discretion, and agenda-setting authority toward actors and bodies who can credibly claim integrated expertise, while diffusing accountability for concrete near-term harms and concrete long-term catastrophic risks across a broader, less specifically answerable mandate.
% ABSENT_VOICES: Present marginalized populations affected by deployed systems today, and no representative of future humanity exists at all — both bear the cost of any resource-allocation error the integrated framing produces, but neither is present in the rooms where the framing is adopted or the allocation is actually made.
% DISAPPEARANCE_RATIONALE: Integrated-reading proponents argue the world would rearrange badly — control and justice work would immediately resume competing for the same grants and conference slots, reproducing the fragmentation the reading exists to prevent. Single-lens researchers and some governance skeptics argue the opposite: that specialized work would simply proceed more efficiently without an added coordination layer, and that the integrated framing's disappearance would mainly cost the cross-disciplinary bodies their claimed jurisdiction.
% FOUNDING_PROBLEM: Alignment research and AI ethics/justice research developed as separate communities with separate funding streams, conferences, and vocabularies, leading to public disputes (often characterized as 'AI safety vs. AI ethics') that made policymakers treat the two agendas as competitors for the same limited attention rather than complementary components of one governance problem.
% FOUNDING_PROBLEM_CORROBORATION: Some funders and policy staff outside either research community corroborate that the perceived rivalry did waste coordination effort and confuse legislative staffers drafting AI regulation. But other outside observers — including science-and-technology-studies scholars not affiliated with either camp — report that the 'false dichotomy' framing itself is sometimes used by newly formed integrated-governance institutions to justify expanding their own scope, and that the underlying resource competition persists under new integrated vocabulary rather than being resolved by it.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) sits at a moderate-high level because the coordination claim is real (fragmentation genuinely wastes effort) but the extraction is also real: authority and funding discretion flow disproportionately to actors who can claim integrated expertise, while the two victim populations — present-day harmed groups and unrepresentable future humanity — have no mechanism to contest how the 'unified' resources are actually split. Theater ratio rises over the measured interval (0.20 to 0.42) as more institutions adopt integrated language in mission statements and grant calls without necessarily changing resource allocation, consistent with a coordination claim being used increasingly as legitimacy cover. Suppression (0.52) reflects that single-lens work faces real institutional friction (grant rejection, panel exclusion) once the integrated framing becomes dominant, though this is milder than outright suppression since specialized work can still be reframed to fit.
 *
 * PERSPECTIVAL GAP:
 *   From the integrated_safety_researchers' seat, the constraint reads as pure coordination correcting a costly false dichotomy. From present_marginalized_populations' and future_humanity's seats, the same arrangement reads as an extraction structure: their urgent, asymmetric claims get diluted into a 'both matter equally' frame that in practice defers to whichever concern the currently dominant institutional actors find more tractable or fundable — often control risk, given its association with well-resourced technical labs. The engine should compute these seats differently given the sharp power and exit-option asymmetries authored above.
 *
 * DIRECTIONALITY LOGIC:
 *   integrated_safety_researchers and cross_disciplinary_governance_bodies are beneficiaries: they gain institutional legitimacy, expanded jurisdiction, and reduced inter-community conflict from the integrated framing succeeding, so they sit near the beneficiary end of directionality. present_marginalized_populations and future_humanity are victims with trapped exit options and no capacity to contest allocation, pushing them toward the full-target end. single_lens_researchers_deprioritized is a moderate-power victim whose career exit options are constrained but not fully trapped — they can retrain or reframe their work, at cost. ai_developers are excluded rather than coordinated or extracted from directly by this specific constraint; their mobile exit options keep them structurally outside the directionality calculation for this story even though they are the ultimate object of both control and justice concerns.
 *
 * MANDATROPHY ANALYSIS:
 *   The integrated reading is a live candidate for mandatrophy in either direction: if the founding problem (harmful false dichotomy fragmenting effort) is genuinely dead — i.e., the two communities were never as opposed as claimed, or have already substantively merged — then the integration project persists mainly to justify governance bodies' expanded jurisdiction, which is exactly the tangled_rope signature this story claims. If the founding problem is still live, the integration commitment functions as necessary coordination and the extraction is closer to overhead than to capture. The founding_problem_status is authored as contested because the corroborating evidence cuts both ways, which is itself the honest empirical state rather than a resolved verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_as_genuine_synthesis_or_rhetorical_expansion,
    'Does the integrated framing genuinely produce better resource allocation across control and justice concerns, or does it primarily function as a legitimacy claim that lets new governance bodies expand jurisdiction over both domains without improving outcomes on either?',
    'Compare funding and outcome trajectories in jurisdictions/institutions that adopted integrated mandates against matched institutions that maintained separate control-risk and justice-risk tracks, measuring whether integration correlates with better outcomes on both dimensions or with resource dilution and jurisdictional capture by coordinating bodies.',
    'If integration produces measurably better joint outcomes, the constraint is closer to a genuine rope with limited extraction overhead. If it primarily expands coordinating institutions'' authority without improving either domain''s outcomes, the tangled_rope classification is confirmed and possibly understates the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(integration_as_genuine_synthesis_or_rhetorical_expansion, empirical, 'Whether integration delivers substantive gains or mainly institutional jurisdiction.').

omega_variable(
    false_dichotomy_reality_check,
    'Was the control-vs-justice dichotomy the integrated reading rejects ever as sharp and zero-sum in practice as its founding narrative claims, or is the ''false dichotomy'' framing itself a rhetorical construction that overstates prior fragmentation to justify the integration project?',
    'Historical analysis of actual funding allocation, conference co-location, and joint publication rates between control-risk and justice-risk research communities prior to the integrated framing''s emergence, to establish whether the fragmentation was as severe as claimed.',
    'If the prior fragmentation was overstated, the founding_problem_status leans toward already-largely-resolved-or-exaggerated, weakening the coordination justification and strengthening the reading of cross_disciplinary_governance_bodies as primarily extraction-seeking. If fragmentation was severe, the coordination claim is substantially vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_dichotomy_reality_check, conceptual, 'Whether the founding fragmentation problem was as severe as the integrated reading''s origin story claims.').

omega_variable(
    committer_framing_disagreement_locus,
    'Where exactly do the safety_control_reading, ethics_justice_reading, and integrated_reading disagree — is it about which harms matter more (a values dispute), about what alignment research should be funded (a resource dispute), or about whether the two harm types share a common technical root cause (an empirical dispute)?',
    'This is the committer-structure question routed to omega per Rule 2: a sibling reading would change the beneficiary/victim structure by picking a single referent harm (either catastrophic loss of control OR present bias reproduction) rather than treating both as co-equal referents, which changes who counts as the primary victim and correspondingly which extraction the constraint''s ε is measuring.',
    'If the disagreement is fundamentally a values dispute about which harm matters more, the three readings are irreducibly plural and coexist as genuinely different constraints (as authored). If it is actually a resolvable empirical dispute about shared technical root causes, the three readings could in principle converge, which would undermine the case for treating integrated_reading as a stable, distinct constraint rather than a temporary bridging position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_disagreement_locus, conceptual, 'The structural location of disagreement between the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__integrated_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__integrated_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__integrated_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__integrated_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__integrated_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_commitment__integrated_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__integrated_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__integrated_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__integrated_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__integrated_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__integrated_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_commitment__integrated_reading, base_extractiveness, 24, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_alignment_commitment__integrated_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__integrated_reading, 0.1).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ethics_justice_reading).

% DUAL FORMULATION NOTE:
% This constraint is the third member of the ai_alignment_commitment kernel family, alongside safety_control_reading (referent: catastrophic loss of control) and ethics_justice_reading (referent: present-day bias and social harm reproduction). Unlike its siblings, integrated_reading's ε is not about either substantive harm domain directly but about the cost of treating the two as exclusive — its distinctive victim set (present_marginalized_populations AND future_humanity, jointly, plus researchers penalized by the framing shift) and its distinctive extraction mechanism (institutional jurisdiction capture by coordinating bodies) make it a structurally separate constraint rather than a synthesis or average of the other two. All three link to each other via affects_constraints because adoption of any one reading by policymakers or funders directly changes resource availability and legitimacy conditions for the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
