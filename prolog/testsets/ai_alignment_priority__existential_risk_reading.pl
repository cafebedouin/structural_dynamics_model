% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: AI Alignment via Existential Risk Prevention (Existential Reading)
 *   domain: ai_governance/technology_ethics/existential_risk
 *
 * SUMMARY:
 *   The existential-risk reading of AI alignment frames the constraint as
 *   preventing catastrophic loss of control over advanced AI systems, with
 *   priority allocated to existential safety over near-term harm prevention.
 *   This is one of three contested readings of the same kernel: 'what does
 *   alignment mean and who gets priority?' The existential reading dominates
 *   current institutional resource allocation in AI safety research, but this
 *   dominance is not universal or uncontested. The reading instantiates a
 *   specific structural configuration: speculative future capabilities define
 *   the victim set as 'all humanity' (undifferentiated and not yet present),
 *   the beneficiary is 'long-term future existence' (abstract and temporally
 *   distant), and the research methodology (adversarial red-teaming,
 *   capability-focused safety analysis) flows resources toward institutions
 *   that specialize in catastrophic-risk modeling. This constraint exhibits
 *   all six DR types from different perspectives, revealing deep structural
 *   tensions between how different communities experience the same AI safety
 *   apparatus. The existential reading is not false — the risk of loss of
 *   control over advanced AI systems is real and consequential — but it is a
 *   *constructed prioritization* atop a contested kernel, not a discovered
 *   natural law. The false summit risk is severe: the constraint risks
 *   naturalizing a value choice (existential > near-term) as a logical
 *   necessity.
 *
 * KEY AGENTS:
 *   - Existential Risk Research Institutions: Primary beneficiary (institutional/arbitrage) — receive funding concentration, narrative legitimacy, research priority, and capability-focused methodology. Net benefit.
 *   - Long-Term Future (Abstract): Nominal beneficiary (powerless/trapped at civilizational horizon) — the victim-to-be if existential catastrophe occurs; serves as the normative anchor for existential prioritization; undifferentiated and future-absent.
 *   - Marginalized Populations in Present Deployed Systems: Primary victim (powerless/trapped) — bear present algorithmic harms (discriminatory lending, predictive policing, hiring bias) that are suppressed by existential narrative dominance. Maximum extraction: costs of neglect while justifying narratives focus on abstract risk.
 *   - Near-Term Harm Prevention Community: Secondary victim (moderate/constrained) — face resource scarcity and narrative subordination; benefit from alignment infrastructure but experience extraction as resource concentration.
 *   - Regulatory Compliance Apparatus: Institutional actor (institutional/constrained) — maintains performative oversight while substantive harm-prevention capacity has atrophied; theater ratio indicates performative compliance.
 *   - Integrated Safety Advocates: Organized agents (organized/mobile) — see the existential/near-term binary as a temporary coordination failure with an integrated sunset; have agency and clear exit path.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing prioritization choice as discovered natural law; false summit candidate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.58).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.72).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "AI Alignment via Existential Risk Prevention (Existential Reading)").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "ai_governance/technology_ethics/existential_risk").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, 'db315280-2038-4aff-83e1-3a501f4b954e').
narrative_ontology:cs_kernel_codification('db315280-2038-4aff-83e1-3a501f4b954e', distributed).
narrative_ontology:cs_authority_grounding('db315280-2038-4aff-83e1-3a501f4b954e', extraction).
narrative_ontology:cs_reading_relation('db315280-2038-4aff-83e1-3a501f4b954e', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('db315280-2038-4aff-83e1-3a501f4b954e', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('db315280-2038-4aff-83e1-3a501f4b954e', foundational, catastrophic_loss_of_control_is_existential).
narrative_ontology:cs_axiom_status(catastrophic_loss_of_control_is_existential, holdable).
narrative_ontology:cs_axiom_grounding('db315280-2038-4aff-83e1-3a501f4b954e', catastrophic_loss_of_control_is_existential, empirically_contingent).
narrative_ontology:cs_axiom('db315280-2038-4aff-83e1-3a501f4b954e', foundational, existential_risk_deserves_resource_priority_over_near_term_harms).
narrative_ontology:cs_axiom_status(existential_risk_deserves_resource_priority_over_near_term_harms, holdable).
narrative_ontology:cs_axiom_grounding('db315280-2038-4aff-83e1-3a501f4b954e', existential_risk_deserves_resource_priority_over_near_term_harms, instrumental).
narrative_ontology:cs_reference_frame('db315280-2038-4aff-83e1-3a501f4b954e', advanced_ai_capability_catastrophe_prevention_primacy).
narrative_ontology:cs_drift_state('db315280-2038-4aff-83e1-3a501f4b954e', contemporary_institutional_landscape_2024_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db315280-2038-4aff-83e1-3a501f4b954e', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, long_term_future_existence).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, capability_research_prioritization).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, humanity_undifferentiated).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, near_term_harm_prevention).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED POPULATIONS (SNARE) — Trapped in present algorithmic harms (discriminatory lending, predictive policing, hiring bias) with no exit. The existential priority frame suppresses resources and attention away from demonstrable present injuries toward speculative future scenarios. Maximum extraction: bear costs of neglect while justifying narratives focus on abstract existential risk. Cannot organize response within the existential risk hierarchy.
constraint_indexing:constraint_classification(ai_alignment_priority__existential_risk_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEAR-TERM HARM PREVENTION COMMUNITY (TANGLED ROPE) — Constrained by funding scarcity and narrative dominance of existential risk. Benefits from alignment research infrastructure (computational resources, institutional legitimacy, technical methodologies) but experiences extraction as resource concentration flows toward capability research and away from near-term safety audits. Moderate agency — can conduct research but must operate in subordinate position relative to existential narrative.
constraint_indexing:constraint_classification(ai_alignment_priority__existential_risk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EXISTENTIAL RISK RESEARCH INSTITUTIONS (ROPE) — Primary beneficiaries. Receive funding concentration, narrative legitimacy, and research priority. The constraint functions as coordination: establishing 'alignment = existential prevention' creates coherence within the safety research community, enabling collaborative red-teaming and capability-focused research agendas. Net benefit — extraction runs toward these institutions.
constraint_indexing:constraint_classification(ai_alignment_priority__existential_risk_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEGRATED ALIGNMENT FRAMEWORK ADVOCATES (SCAFFOLD) — Organized agents (regulatory bodies, multi-stakeholder coalitions, integrated safety initiatives) view the binary existential/near-term split as a temporary coordination failure with a sunset. The integrated reading sees both catastrophic-risk prevention and near-term harm reduction as necessary and complementary. Lower effective extraction because the coalition has both agency and sees a clear exit path: formal integration of near-term justice requirements into existential safety protocols.
constraint_indexing:constraint_classification(ai_alignment_priority__existential_risk_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COMPLIANCE THEATER (PITON) — Regulatory frameworks that nominally address AI harms operate largely as performative compliance: audits, bias testing, impact assessments exist but lack enforcement mechanism, resource allocation, or genuine consequence for violation. The regulatory apparatus persists through institutional mandate while substantive capability to reduce actual harms has atrophied. Theater ratio (0.68) reflects this — the apparatus produces documentation of alignment but not structural change.
constraint_indexing:constraint_classification(ai_alignment_priority__existential_risk_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a technical/logical perspective, preventing catastrophic loss of control over advanced AI systems is a non-negotiable requirement of any alignment solution — it appears as an immutable priority, a logical necessity independent of social or ethical framing. The constraint appears as a fundamental principle: 'you cannot have alignment without preventing existential catastrophe.' However, this perspective risks naturalizing a prioritization choice (existential > near-term) as a logical necessity. The engine's false summit detector will identify the beneficiary presence and structural data as evidence of constructed priority framing rather than discovered natural law.
constraint_indexing:constraint_classification(ai_alignment_priority__existential_risk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_alignment_priority__existential_risk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_alignment_priority__existential_risk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, TR),
    TR >= 0.70.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The existential reading redirects resources and narrative authority toward capability-focused research and away from near-term harm audits. The extraction is not maximal because there are genuine coordination benefits: the existential frame provides coherence to safety research agendas and creates institutional incentives for safety-oriented capability analysis. However, the extraction is real: marginalized populations and near-term safety researchers bear demonstrable costs (funding scarcity, research marginalization) while existential institutions accrue benefits (resources, prestige, methodological authority). The trajectory (0.35 → 0.58 over 8 time units) reflects accumulating extraction as the existential frame has consolidated institutional dominance and as near-term harms have accumulated without corresponding resource allocation. Suppression (0.72): High. Significant barriers suppress alternative framings and near-term priorities: (1) Narrative dominance of existential risk in AI governance discourse; (2) Funding concentration toward existential institutions; (3) Publication bias in favor of existential risk research; (4) Institutional barriers (regulatory exemptions for 'safety research,' capability lab self-governance); (5) Temporal framing that treats present harms as relatively minor compared to future risks; (6) Epistemic barriers — existential risk claims are difficult to falsify, and negative results are reframed as 'important capability insights' rather than refutations. Theater ratio (0.68): High and rising. Regulatory compliance mechanisms (bias audits, impact assessments, responsible AI frameworks) operate largely as performative compliance. The apparatus produces documentation without corresponding changes in deployed AI behavior or resource allocation to harm reduction. The theater has increased as regulatory frameworks have become more sophisticated while enforcement capacity has remained minimal.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence from a single base properties set. Marginalized populations see pure extraction (Snare) — abstract future risk justifies neglect of present demonstrable harms. Near-term safety researchers see mixed coordination-and-extraction (Tangled Rope) — they benefit from alignment infrastructure but experience resource deprivation. Existential institutions see pure coordination (Rope) — the constraint enables coherent safety research and collaborative red-teaming. Integrated advocates see a temporary coordination failure with a sunset (Scaffold) — integration of near-term justice into existential protocols is both necessary and technically feasible. Regulatory apparatus sees its own degraded ritual (Piton) — compliance mechanisms persist without corresponding enforcement or harm reduction. The analytical observer risks seeing immutable natural law (Mountain) — 'preventing catastrophic loss of control is non-negotiable' appears as logical necessity, but the structural data reveals this as naturalization of a constructed institutional priority.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from each agent's structural relationship to the existential-priority constraint: their power level, exit options, beneficiary/victim status, and ability to challenge or escape the frame. Marginalized populations (powerless/trapped) experience maximum d (~0.95) — they bear costs with no exit and no institutional voice in safety prioritization. Near-term safety researchers (moderate/constrained) experience high d (~0.65-0.75) — they face resource barriers and narrative subordination but retain some research capacity and growing coalition visibility. Existential institutions (institutional/arbitrage) experience low d (~0.15-0.20) — they are beneficiaries with high exit capacity and can arbitrage across funding sources and research agendas. The analytical observer (analytical/analytical) experiences moderate d (~0.72) — seeing both the existential risk as real and the institutional prioritization as potentially constructed puts the observer in an ambiguous structural position. The sigmoid f(d) maps these d values to experienced extractiveness: high d → high f(d) → high χ for trapped agents; low d → low f(d) → negative χ for beneficiaries. The perspectival gap in classification (Snare for victims, Rope for beneficiaries) emerges from this directionality derivation applied to the base ε (0.58).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speculative_capability_timeline_uncertainty,
    'What is the probability and timeline for transformative AI capabilities that would create existential risks as defined by this reading?',
    'Comparative analysis of capability forecasts (2024-2040): benchmark against historical AI progress rates, analyze methodological disagreements in timeline estimates, track accuracy of prior capability predictions, conduct Delphi-style expert elicitation with explicit confidence intervals',
    'If timeline > 30 years or probability < 10%: existential prioritization becomes contingent rather than categorical, and the Tangled Rope classification shifts toward Rope (pure coordination) or even Piton (theater). If timeline < 5 years and probability > 50%: justifies the existential reading''s prioritization structure as rational risk allocation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(speculative_capability_timeline_uncertainty, empirical, 'Probability and timeline of transformative AI capabilities creating existential risk').

omega_variable(
    present_harm_causation_attribution,
    'What is the causal pathway and magnitude through which deprioritizing near-term harms (by focusing resources on existential scenarios) produces measurable increases in algorithmic discrimination, exclusion, and economic extraction from marginalized populations?',
    'Longitudinal resource allocation analysis: track funding flows from near-term safety to existential research (2016-2026); correlate with incident rates of algorithmic harm in lending, hiring, criminal justice; survey affected communities on actual vs. counterfactual exposure to deployed-AI harms if funding had been allocated differently; measure citation/publication bias patterns favoring existential narratives',
    'If causation is strong and direct: suppression rating increases and snare classification from marginalized-population perspective is reinforced. If causation is weak or mediated: snare classification weakens to constrained, and extraction becomes less clear — the harm may result from competing legitimate priorities rather than intentional suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_harm_causation_attribution, empirical, 'Causal pathway between existential prioritization and near-term algorithmic harms').

omega_variable(
    adversarial_red_teaming_capture_risk,
    'Does capability-focused adversarial red-teaming (the preferred methodology of existential-risk reading) create opportunities for capability companies to justify capability development as ''safety research''?',
    'Analysis of red-teaming publications: distinguish genuine safety-oriented capability analysis from capability-focused research using ''safety'' framing; track which red-teaming findings lead to actual capability constraints vs. which findings are published as ''important capability insights''; audit whether red-teaming methodology is asymmetrically available to capability labs vs. independent safety teams',
    'If capture is high: the beneficiary set expands to include capability companies, and the constraint becomes a mechanism for legitimizing capability research. If capture is low: the constraint remains aligned with stated existential-safety goals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adversarial_red_teaming_capture_risk, empirical, 'Whether adversarial red-teaming methodology is captured by capability development').

omega_variable(
    contested_kernel_frame_structure,
    'Is the choice between existential-risk prioritization and integrated-harm-prevention a resolvable empirical question, a value choice that depends on moral axioms, or a constructed institutional frame that privileges one community''s research agenda?',
    'Philosophical and institutional analysis: examine whether the binary choice is grounded in empirical differences (different claims about AI timelines, harm magnitudes, causal mechanisms) or in irreducible value differences (discounted-future calculus, whose harms count, whose knowledge counts); trace institutional history of how the existential reading achieved narrative dominance; identify what structural changes would be required for the integrated reading to achieve equal priority',
    'If empirical disagreement: the constraint''s type and beneficiary structure depends on which empirical claims are true. If value choice: the constraint is constructed (Tangled Rope minimum) and the false summit risk is severe. If institutional frame: the constraint is most accurately classified as Piton (theater-driven) at the institutional level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_frame_structure, conceptual, 'Whether the existential vs. integrated prioritization is empirical, valuative, or institutional framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_align_ex_theater_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ai_align_ex_theater_t4, ai_alignment_priority__existential_risk_reading, theater_ratio, 4, 0.62).
narrative_ontology:measurement(ai_align_ex_theater_t8, ai_alignment_priority__existential_risk_reading, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(ai_align_ex_extractiveness_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_align_ex_extractiveness_t4, ai_alignment_priority__existential_risk_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(ai_align_ex_extractiveness_t8, ai_alignment_priority__existential_risk_reading, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_align_ex_suppression_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_align_ex_suppression_t4, ai_alignment_priority__existential_risk_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(ai_align_ex_suppression_t8, ai_alignment_priority__existential_risk_reading, suppression_requirement, 8, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_capability_development_research_prioritization).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, algorithmic_discrimination_in_deployed_systems).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_governance_institutional_authority).

% DUAL FORMULATION NOTE:
% The ai_alignment_priority kernel decomposes into three structurally distinct constraints with different ε values, victim sets, and beneficiary structures. Each reading instantiates a different prioritization of the same underlying phenomena: risks from advanced AI systems. The existential reading (this file) has ε=0.58 and treats speculative future capabilities as the primary victim-to-be. The nearterm_harms_reading has ε=0.72 and centers present algorithmic discrimination. The integrated_reading has ε=0.45 and attempts to coordinate both priorities. These are not three measurements of one constraint — they are three structurally distinct constraints grounded in incompatible axioms about what alignment means. Network edges reflect which reading influences which: existential influences integrated and nearterm; integrated influences existential toward integration; nearterm influences existential toward expanded victim recognition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, institutional, 0.18).
constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
