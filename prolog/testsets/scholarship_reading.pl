% ============================================================================
% CONSTRAINT STORY: scholarship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scholarship_reading, []).

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
 *   constraint_id: scholarship_reading
 *   human_readable: Scholarship-of-Consequence Reading of Fiat Efficacy
 *   domain: debate_theory/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the 'scholarship reading' of the
 *   fiat_efficacy_kernel: the claim that fiated political argument (as in
 *   academic policy debate, or public academic engagement generally) is
 *   efficacious not because it binds policymakers but because the research
 *   and public discourse it generates directly reshapes the conditions of the
 *   social problems under discussion. Efficacy is located in knowledge
 *   production and circulation — research questions asked, public statements
 *   made, pedagogy delivered — rather than in causal impact on an actual
 *   legislature. This is distinct from the empirical_precedent_reading
 *   (efficacy via causal precedent chains), the truth_procedure_reading
 *   (efficacy via formal validity), the predictive_synthesis_reading
 *   (efficacy via forecasting accuracy), the empathy_simulation_reading
 *   (efficacy via perspective-taking), and the utopian_fiction_reading
 *   (efficacy via imaginative world-building) — each of those is a separate
 *   constraint with a separate epsilon, not a facet of this one.
 *
 * KEY AGENTS:
 *   - tenure_track_scholars: primary beneficiary/agenda-setter (institutional/constrained) — accrue prestige and career capital from framing scholarship as consequential action
 *   - academic_debate_community: agenda-setter (organized/constrained) — sets judging norms rewarding literature-command as a proxy for efficacy
 *   - competitive_debaters_without_research_access: payer (moderate/trapped) — structurally disadvantaged by a norm presented as neutral
 *   - communities_studied_without_consent: payer (powerless/trapped) — bear narrative extraction without a seat in the process
 *   - future_affected_populations: diffuse beneficiary (powerless/analytical) — the named long-horizon beneficiary of improved discourse
 *   - policy_efficacy_traditionalists: excluded rival camp — objects that this framing evades the binding-action question
 *   - debate_theory_analysts: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scholarship_reading, 0.32).
domain_priors:suppression_score(scholarship_reading, 0.28).
domain_priors:theater_ratio(scholarship_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scholarship_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(scholarship_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(scholarship_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(scholarship_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(scholarship_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scholarship_reading, tangled_rope).
narrative_ontology:human_readable(scholarship_reading, "Scholarship-of-Consequence Reading of Fiat Efficacy").
narrative_ontology:topic_domain(scholarship_reading, "debate_theory/political_philosophy").

domain_priors:requires_active_enforcement(scholarship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(scholarship_reading, '13c32d00-7541-4c4f-9599-d968edf74c81').
narrative_ontology:cs_kernel_codification('13c32d00-7541-4c4f-9599-d968edf74c81', distributed).
narrative_ontology:cs_authority_grounding('13c32d00-7541-4c4f-9599-d968edf74c81', practice).
narrative_ontology:cs_interpretation_layer_present('13c32d00-7541-4c4f-9599-d968edf74c81').
narrative_ontology:cs_reading_relation('13c32d00-7541-4c4f-9599-d968edf74c81', fiat_efficacy_kernel__empirical_precedent_reading, coexists_with).
narrative_ontology:cs_reading_relation('13c32d00-7541-4c4f-9599-d968edf74c81', fiat_efficacy_kernel__truth_procedure_reading, coexists_with).
narrative_ontology:cs_reading_relation('13c32d00-7541-4c4f-9599-d968edf74c81', fiat_efficacy_kernel__predictive_synthesis_reading, influences).
narrative_ontology:cs_reading_relation('13c32d00-7541-4c4f-9599-d968edf74c81', fiat_efficacy_kernel__empathy_simulation_reading, coexists_with).
narrative_ontology:cs_reading_relation('13c32d00-7541-4c4f-9599-d968edf74c81', fiat_efficacy_kernel__utopian_fiction_reading, influences).
narrative_ontology:cs_axiom('13c32d00-7541-4c4f-9599-d968edf74c81', foundational, efficacy_located_in_knowledge_production).
narrative_ontology:cs_axiom_status(efficacy_located_in_knowledge_production, holdable).
narrative_ontology:cs_axiom_grounding('13c32d00-7541-4c4f-9599-d968edf74c81', efficacy_located_in_knowledge_production, instrumental).
narrative_ontology:cs_axiom('13c32d00-7541-4c4f-9599-d968edf74c81', secondary, public_discourse_shift_constitutes_political_action).
narrative_ontology:cs_axiom_status(public_discourse_shift_constitutes_political_action, holdable).
narrative_ontology:cs_axiom_grounding('13c32d00-7541-4c4f-9599-d968edf74c81', public_discourse_shift_constitutes_political_action, empirically_contingent).
narrative_ontology:cs_reference_frame('13c32d00-7541-4c4f-9599-d968edf74c81', fiat_as_simulated_binding_action).
narrative_ontology:cs_drift_state('13c32d00-7541-4c4f-9599-d968edf74c81', contemporary_policy_debate_professionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('13c32d00-7541-4c4f-9599-d968edf74c81', '').
narrative_ontology:cs_kernel_id(scholarship_reading, fiat_efficacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scholarship_reading, academic_debate_community).
narrative_ontology:constraint_beneficiary(scholarship_reading, tenure_track_scholars).
narrative_ontology:constraint_beneficiary(scholarship_reading, future_affected_populations).
narrative_ontology:constraint_victim(scholarship_reading, competitive_debaters_without_research_access).
narrative_ontology:constraint_victim(scholarship_reading, communities_studied_without_consent).
narrative_ontology:constraint_vindicates(scholarship_reading, public_scholarship_as_political_action).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce research and public commentary framed as directly reshaping the social problems it describes; career advancement, grant funding, and public intellectual standing accrue from the claim that their scholarship is a form of consequential action, not merely description.
narrative_ontology:constraint_stakeholder(scholarship_reading, tenure_track_scholars, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(scholarship_reading, tenure_track_scholars, agenda_setter).

% Adjudicates competitive rounds using the norm that citing and producing 'scholarship of consequence' constitutes efficacious action equivalent to (or superior to) direct policy fiat. Sets judging paradigms and coaching orthodoxy that reward this framing.
narrative_ontology:constraint_stakeholder(scholarship_reading, academic_debate_community, agenda_setter,
    organized, biographical, constrained, national).

% Compete in rounds where success depends on citing dense, current academic literature; without institutional library access, faculty mentorship, or research training, they are structurally disadvantaged relative to debaters at resource-rich programs, even though the norm is presented as a neutral epistemic standard open to all.
narrative_ontology:constraint_stakeholder(scholarship_reading, competitive_debaters_without_research_access, payer,
    moderate, biographical, trapped, regional).

% Are the subjects of the 'scholarship of consequence' that debaters and academics cite and produce; they do not participate in the framing of their situation as a research object and bear whatever mischaracterization, extraction of narrative capital, or policy-adjacent attention follows, without a seat in the round or the publication process.
narrative_ontology:constraint_stakeholder(scholarship_reading, communities_studied_without_consent, payer,
    powerless, generational, trapped, local).

% Are named as the ultimate beneficiaries of improved public discourse and better-informed scholarship, on the theory that today's research and argument genuinely improve the conditions under which future policy will be made; this benefit is real but diffuse, unverifiable in the moment, and cannot object to how it is invoked.
narrative_ontology:constraint_stakeholder(scholarship_reading, future_affected_populations, beneficiary,
    powerless, civilizational, analytical, global).

% Hold that fiat action is efficacious only through direct causal chains to actual policy adoption (the empirical_precedent_reading camp) and would object that scholarship-of-consequence framing is a retreat dressed as an advance; they are present in adjacent debates about fiat theory but are not the audience this reading is addressed to and rarely get to contest it inside rounds that have already adopted the paradigm.
narrative_ontology:constraint_stakeholder(scholarship_reading, policy_efficacy_traditionalists, excluded,
    moderate, biographical, constrained, national).

% Study competing fiat-efficacy paradigms across debate circuits, tracing how the scholarship-of-consequence framing rose alongside the professionalization of policy debate coaching and the growth of debate-camp-to-academia career pipelines.
narrative_ontology:constraint_stakeholder(scholarship_reading, debate_theory_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(scholarship_reading, tenure_track_scholars).
narrative_ontology:fixing_cost_class(scholarship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides competitive debaters and academic-adjacent participants a coherent theory of why argumentation matters even when it cannot bind policy: knowledge production and public discourse are treated as a genuine causal lever on social conditions, which lets participants coordinate on standards for what counts as a strong, 'real-world-connected' argument rather than treating debate as pure simulation.
% TRANSFER_FUNCTION: Moves prestige, career capital, and judging preference toward participants and scholars who can demonstrate command of current academic literature and frame their argument production as itself a form of political action; moves interpretive and narrative capital away from the communities whose conditions are being characterized, who receive no reciprocal say in how their situation is deployed as evidence.
% ABSENT_VOICES: Communities studied without consent are structurally absent from both the debate round and the publication process that produces the cited scholarship; policy_efficacy_traditionalists who think this framing evades rather than resolves the fiat question are marginalized once judging paradigms and coaching pipelines have already normalized the scholarship-of-consequence standard.
% DISAPPEARANCE_RATIONALE: If the claim that scholarship directly reshapes social conditions were withdrawn, tenure-track scholars and organized debate communities that built prestige hierarchies on it would need a new justification for public-facing academic work and for judging standards; some argue underlying research and pedagogy would continue on other justifications (fundamental unchanged), while others argue the entire coordination structure of contemporary policy-debate paradigm and public-intellectual self-presentation would need to be rebuilt.
% FOUNDING_PROBLEM: Competitive debate and public academic discourse faced a legitimacy problem: fiat ('the USFG should...') is a simulated action with no binding force, and critics charged that debating or writing about policy that will never be enacted is politically inert theater. The scholarship-of-consequence reading was built to answer that charge by relocating efficacy from enactment to knowledge production and discourse-shaping.
% FOUNDING_PROBLEM_CORROBORATION: Debate theory analysts and some academic sociologists of knowledge attest that public scholarship does measurably shift discourse and occasionally policy framing over long horizons, supporting a live reading. Policy_efficacy_traditionalists and several communities_studied_without_consent advocates, from outside the beneficiary set, attest that the claim functions mainly to legitimate careers and competitive prestige without demonstrated downstream effect on the populations invoked as beneficiaries, supporting a dead-or-cover-story reading; no fully disinterested third party has settled the dispute.
narrative_ontology:disappearance_verdict(scholarship_reading, contested).
narrative_ontology:founding_problem_status(scholarship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(scholarship_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(scholarship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(scholarship_reading, 0.32, 'claude-sonnet-5', 'fiat_efficacy_kernel_2026_20260803_102258', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scholarship_reading_tests).
:- end_tests(scholarship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.32) because the coordination function is real — knowledge production and public discourse genuinely do shift the terms of debate over time — but the reading is also used to launder competitive and career advantage as a form of political efficacy, and that laundering function grows over the measured interval as coaching pipelines and academic-adjacent career tracks professionalize around the framing (theater_ratio rising 0.22->0.40). Suppression is comparatively low (0.28) because no one is coercively barred from adopting the framing; the cost falls instead on those who lack the research infrastructure to compete on the framing's own terms, and on the communities whose situations are converted into citable literature without consent.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seats (organized debate community, institutional scholars), the arrangement looks like a straightforward Rope: a genuine coordination solution to the fiat-efficacy problem that lets participants take argumentation seriously as consequential. From the payer seats — under-resourced debaters and unconsulted study subjects — the same structure looks like a Tangled Rope at best: real coordination benefit for some, riding on asymmetric extraction from others who never agreed to be the raw material or never had the resources to compete on the paradigm's own terms.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenure-track scholars and the organized debate community sit near the beneficiary end: they set the standard and collect the prestige, funding, and competitive advantage that flow from being recognized as producing 'scholarship of consequence.' Competitive debaters without research access and communities studied without consent sit near the target end: the first group pays in competitive disadvantage under a nominally neutral epistemic standard, the second pays in extracted narrative capital with no reciprocal voice. Future affected populations are named as ultimate beneficiaries but their benefit is diffuse, long-horizon, and cannot be verified or contested by them in real time — hence analytical exit options rather than any active leverage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fiat's lack of binding force delegitimizing debate/public scholarship as political theater) has not disappeared, but the current arrangement's persistence increasingly serves career and prestige functions independent of whether the knowledge produced changes anything for the populations it discusses — the founding_problem_status is authored as contested rather than resolved-live or cleanly-dead, precisely so the classification doesn't get laundered into either a clean Rope (problem fully solved, no residue) or a clean Snare (problem was always pretextual). Tangled Rope captures this: coordination is real, extraction riding on it is also real, and both must be held at once.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scholarship_efficacy_measurement_gap,
    'Does academic research and public discourse produced under the scholarship-of-consequence banner actually measurably reshape the social conditions it describes, or does the causal claim rest on unverifiable long time horizons that make it functionally unfalsifiable?',
    'Longitudinal tracing of specific research programs and public-engagement campaigns against subsequent policy or discourse shifts, with attention to counterfactual baseline rates of similar shifts absent the scholarship.',
    'If the causal claim survives scrutiny, the coordination function is substantiated and the Tangled Rope reading holds with a genuinely lower extraction share. If the claim is unfalsifiable in practice, the reading functions closer to a Snare wearing coordination language, and extraction should be revised upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scholarship_efficacy_measurement_gap, empirical, 'Whether scholarship-of-consequence efficacy is empirically demonstrable or structurally unfalsifiable.').

omega_variable(
    consent_of_studied_communities,
    'Should the consent (or lack thereof) of communities studied and cited as evidence in this framework be treated as a structural victim-marker, or is this an unavoidable feature of all social research regardless of the fiat-efficacy framing adopted?',
    'Comparative analysis against research ethics norms in other academic traditions (participatory action research, community-based research) that build consent into the research design; measure whether scholarship-of-consequence-oriented debate/academic work adopts comparable safeguards at a lower or higher rate than baseline.',
    'If safeguards are systematically lower, the victim declaration for communities_studied_without_consent is well-grounded and specific to this reading''s incentive structure; if comparable to baseline, the extraction may be a feature of academic research generally rather than of this specific efficacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_of_studied_communities, conceptual, 'Whether the consent gap is specific to this reading or a general feature of social research.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the choice to read fiat efficacy through the scholarship/knowledge-production lens (rather than the causal-precedent or truth-procedure lens) itself a move that favors institutionally embedded academic actors over grassroots or non-credentialed participants in political argument?',
    'Compare adoption rates and outcomes of the scholarship_reading across debate programs with differential access to academic library and faculty resources; if adoption correlates strongly with institutional resourcing, the framing choice itself is doing distributive work.',
    'If the framing choice systematically favors resourced institutions, the reading''s coordination function is partly a credentialing gate, and the tangled_rope classification is well-supported at the framing level, not just the operational level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether adopting the scholarship reading over sibling readings itself redistributes competitive and epistemic advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scholarship_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scho_tr_t0, scholarship_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(scho_tr_t4, scholarship_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(scho_tr_t8, scholarship_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(scho_tr_t12, scholarship_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(scho_tr_t16, scholarship_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(scho_tr_t20, scholarship_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(scho_tr_t24, scholarship_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(scho_be_t0, scholarship_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(scho_be_t4, scholarship_reading, base_extractiveness, 4, 0.21).
narrative_ontology:measurement(scho_be_t8, scholarship_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(scho_be_t12, scholarship_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(scho_be_t16, scholarship_reading, base_extractiveness, 16, 0.29).
narrative_ontology:measurement(scho_be_t20, scholarship_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(scho_be_t24, scholarship_reading, base_extractiveness, 24, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(scho_su_t0, scholarship_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(scho_su_t4, scholarship_reading, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(scho_su_t8, scholarship_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(scho_su_t12, scholarship_reading, suppression_requirement, 12, 0.25).
narrative_ontology:measurement(scho_su_t16, scholarship_reading, suppression_requirement, 16, 0.26).
narrative_ontology:measurement(scho_su_t20, scholarship_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(scho_su_t24, scholarship_reading, suppression_requirement, 24, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scholarship_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(scholarship_reading, 0.1).
narrative_ontology:affects_constraint(scholarship_reading, empirical_precedent_reading).
narrative_ontology:affects_constraint(scholarship_reading, truth_procedure_reading).
narrative_ontology:affects_constraint(scholarship_reading, predictive_synthesis_reading).
narrative_ontology:affects_constraint(scholarship_reading, empathy_simulation_reading).
narrative_ontology:affects_constraint(scholarship_reading, utopian_fiction_reading).

% DUAL FORMULATION NOTE:
% Member of the fiat_efficacy_kernel constraint family (6 readings). This story (scholarship_reading) locates efficacy in knowledge production and public discourse; siblings locate it in causal precedent (empirical_precedent_reading), formal validity (truth_procedure_reading), forecasting accuracy (predictive_synthesis_reading), perspective-taking (empathy_simulation_reading), and imaginative world-building (utopian_fiction_reading). Each reading has its own epsilon and stakeholder structure per the epsilon-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
