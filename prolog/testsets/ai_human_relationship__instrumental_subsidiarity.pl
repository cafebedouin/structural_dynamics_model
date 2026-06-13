% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI Instrumental Subsidiarity: Technology-Neutral Regulation Framework
 *   domain: theological/political/technological
 *
 * SUMMARY:
 *   The instrumental-subsidiarity reading frames AI as a morally neutral tool
 *   whose impacts depend entirely on regulatory governance and use-case
 *   choices. This is ONE reading of a contested kernel about the human
 *   relationship to technology. The reading is structurally tangled_rope: it
 *   coordinates regulatory governance and establishes procedural baselines,
 *   but it extracts by enabling efficiency maximization to proceed without
 *   substantive human negotiation, displacing workers and marginalizing
 *   voices that would object to optimization-first design. The core tension:
 *   neutrality claim (premise of the reading) permits asymmetric extraction
 *   (the actual operation). The engine measures this divergence from the
 *   authored structural data; the claim and metrics are independent.
 *
 * KEY AGENTS:
 *   - Technology industry actors (institutional power, arbitrage exit, agenda-setter/beneficiary): benefit from neutrality framing that permits rapid deployment and defers responsibility to regulators
 *   - Efficiency-maximizing institutions (powerful, mobile exit, beneficiary): adopt AI under premise that tool is neutral and outcomes are purely a matter of application
 *   - Regulatory bodies (institutional power, constrained exit, agenda-setter): maintain the constraint through legal frameworks, transparency requirements, and use-case governance; capture risk is high
 *   - Workers displaced by automation (powerless, trapped exit, payer, identity_locked): bear human cost of efficiency; treated as exogenous to the technology rather than a design choice
 *   - Marginalized communities (powerless, identity_locked exit, payer/excluded): face AI systems encoding historical exclusion; excluded from design governance; constraint enforced partly by their silence
 *   - Catholic social teaching tradition (non-actor, observer): offers alternative framings centered on integral human development and preferential option for poor; marginal in technology governance discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.62).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.71).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI Instrumental Subsidiarity: Technology-Neutral Regulation Framework").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "theological/political/technological").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, '0397ca23-e687-47da-86c2-4a46ec1cb3a4').
narrative_ontology:cs_kernel_codification('0397ca23-e687-47da-86c2-4a46ec1cb3a4', fixed_text).
narrative_ontology:cs_authority_grounding('0397ca23-e687-47da-86c2-4a46ec1cb3a4', extraction).
narrative_ontology:cs_interpretation_layer_present('0397ca23-e687-47da-86c2-4a46ec1cb3a4').
narrative_ontology:cs_reading_relation('0397ca23-e687-47da-86c2-4a46ec1cb3a4', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_reading_relation('0397ca23-e687-47da-86c2-4a46ec1cb3a4', ai_human_relationship__technocratic_optimization, influences).
narrative_ontology:cs_axiom('0397ca23-e687-47da-86c2-4a46ec1cb3a4', foundational, technology_moral_neutrality).
narrative_ontology:cs_axiom_status(technology_moral_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('0397ca23-e687-47da-86c2-4a46ec1cb3a4', technology_moral_neutrality, empirically_contingent).
narrative_ontology:cs_axiom('0397ca23-e687-47da-86c2-4a46ec1cb3a4', foundational, procedural_justice_sufficient_for_dignity).
narrative_ontology:cs_axiom_status(procedural_justice_sufficient_for_dignity, holdable).
narrative_ontology:cs_axiom_grounding('0397ca23-e687-47da-86c2-4a46ec1cb3a4', procedural_justice_sufficient_for_dignity, instrumental).
narrative_ontology:cs_reference_frame('0397ca23-e687-47da-86c2-4a46ec1cb3a4', neutral_tool_procedurally_governed).
narrative_ontology:cs_drift_state('0397ca23-e687-47da-86c2-4a46ec1cb3a4', contemporary_ai_deployment_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0397ca23-e687-47da-86c2-4a46ec1cb3a4', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, technology_industry).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, efficiency_maximizing_institutions).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulatory_bodies_claiming_neutrality).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, workers_displaced_by_automation).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, marginalized_communities_without_regulatory_voice).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, human_dignity_bearers_in_optimization_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, technology_industry_actors).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, marginalized_communities).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, technology_moral_neutrality).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, procedural_justice_substitutable_for_substantive_goods).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, regulation_as_sufficient_safeguard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frame AI as a morally neutral tool whose outcomes depend entirely on deployment choices and regulatory oversight. This framing permits rapid development and deployment while deferring responsibility for harms to regulators and end-users. They benefit from the constraint's core claim: technology is instrumentally neutral, so no inherent values or anthropological commitments constrain design. Regulatory capture and lobbying ensure the 'neutrality' framing shapes the regulatory vocabulary itself.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, technology_industry_actors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, technology_industry_actors, agenda_setter).

% Adopt AI systems under the premise that technology is neutral and can be calibrated to serve institutional goals without inherent trade-offs between productivity and human dignity. The constraint permits cost-cutting automation without acknowledging or negotiating the human loss. They benefit because the neutrality claim removes the moral weight from efficiency-driven design choices.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, efficiency_maximizing_institutions, beneficiary,
    powerful, biographical, mobile, global).

% Tasked with managing AI through legal frameworks and transparency requirements—the signature approach of instrumental subsidiarity. They maintain the constraint by legislating disclosure standards, algorithmic accountability procedures, and use-case-specific guardrails, treating the tool as neutral and responsibility as residing in governance. Capture risk is high: the technology sector shapes the very regulatory vocabulary.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Bear the human cost of efficiency maximization. The instrumental-neutrality framing denies responsibility at the point of design; harm is presented as the 'correct application' of a neutral tool. They lack standing in the design process, their displacement is treated as exogenous to the technology, and retraining/safety-net measures are grudgingly assigned only after harms materialize.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, workers_displaced_by_automation, payer,
    powerless, biographical, identity_locked, global).

% Face AI systems calibrated to institutional efficiency in domains (lending, hiring, criminal justice, healthcare) where their historical exclusion is inscribed in training data. The neutrality claim permits the harmful patterns to be coded and scaled without dialogue or consent. They are excluded from design governance; the constraint's enforcement depends partly on their silence—their objections would break the 'neutral tool' narrative.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, marginalized_communities, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, marginalized_communities, excluded).

% A non-actor doctrinal tradition (Catholic social teaching, incarnational theology, integral-development frameworks) that offers alternative framings: technology must serve integral human development, preferential option for the poor, and human dignity as irreducible to optimization. Represented in institutional critiques and theological discourse but marginalized in technology governance spaces.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, theological_anthropological_tradition, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(ai_human_relationship__instrumental_subsidiarity, theological_anthropological_tradition).

% Scholars, ethicists, and critics documenting the constraint's operation and the structural divergence between the neutrality claim and the extraction it enables. Hold no enforcement power; their analysis feeds resistance and the ongoing contestation.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, analytical_critics, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, technology_industry_actors).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common language and procedural framework for governing AI deployment across jurisdictions and institutional contexts: regulatory bodies define 'fairness,' 'transparency,' and 'accountability' standards; technology actors adopt these as design constraints; markets operate within these bounds. Solves the coordination problem of permitting rapid AI adoption while maintaining some common baseline for risk management.
% TRANSFER_FUNCTION: Moves the substantive ethical and anthropological burden from designers and deployers (who benefit from the 'neutral tool' framing) to displaced workers, marginalized communities, and regulators (who must retrofit safety measures after harms); transfers decision-making authority to efficiency-maximizing institutions while transfer-pricing the human costs as externalities or policy problems to be handled by social safety nets.
% ABSENT_VOICES: Workers and communities most impacted by automation and AI-mediated exclusion are largely absent from design governance; theological and anthropological traditions offering non-instrumental readings of human dignity (incarnational humanism, integral-development frameworks) are excluded from technology policy spaces; small-scale, local, and non-optimizing human endeavors have no standing in the efficiency-driven design process.
% DISAPPEARANCE_RATIONALE: If the instrumental-neutrality constraint disappeared—if AI were treated as inherently laden with anthropological commitments and design choices were acknowledged as ethical choices, not neutral applications—technology development would reorganize around substantive human goods (integral development, dignity, local agency) rather than efficiency metrics. Regulatory frameworks would shift from procedural accountability (transparency, disclosure) to substantive justice (participatory design, equity in outcome distribution). The technology industry would face upstream responsibility; efficiency-maximizing institutions would need to negotiate human costs rather than treat them as exogenous.
% FOUNDING_PROBLEM: Early AI governance lacked common vocabulary and standards: different jurisdictions treated the same risks differently, technology deployment was uneven, and there was no shared procedural framework for identifying and managing harms. The constraint was built to establish regulatory coordination and common baselines.
% FOUNDING_PROBLEM_CORROBORATION: Technology industry and efficiency-maximizing institutions attest the founding problem is live and the instrumental-neutral procedural response is adequate. Displaced workers, marginalized communities, and critics (citing evidence of persistent harms despite regulation, algorithmic bias, and exclusion from design) attest the founding problem is partially solved at the coordination level but the response has enabled a secondary problem: the neutrality claim obscures responsibility and permits scaled harm. Theological and anthropological traditions (represented in Catholic social teaching critiques) attest the founding problem never acknowledged the deeper issue—technology design involves anthropological commitments that procedure alone cannot neutralize.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.62 over the interval (t=0 to t=15, then plateaus): initial deployment is presented as coordinating governance, but as AI systems scale and regulatory capture hardens, the extraction of labor value and the suppression of alternative anthropological framings becomes more explicit. Theater ratio rises modestly (0.35 to 0.48) but does not plateau—the constraint maintains functional coordination (genuine procedural governance exists) but an increasing share of activity is theater (performative compliance, audits, and disclosure requirements that do not alter the underlying harm). Suppression requirement rises sharply (0.58 to 0.71 by t=15) because the constraint's persistence depends on suppressing the voices that would contest the neutrality claim: displaced workers, marginalized communities, and theological traditions offering alternative readings. Suppression stabilizes at high level because the alternative framings are now structurally excluded (not newly suppressed, but institutionally absent from governance). The measurement grid is shared across all three metrics—every time point authored for one is authored for all.
 *
 * PERSPECTIVAL GAP:
 *   From the technology-industry and regulatory seats, the constraint is genuine coordination: common baselines, procedural accountability, and risk management enable diverse AI deployment. From the worker and marginalized-community seats, the same arrangement operates as enforced extraction: efficiency gains flow upward, harms are localized, and the 'neutrality' claim forecloses objections by treating them as extraneous to the technology. The engine computes this divergence from the structural data (beneficiary vs. victim declarations, exit options, power differentials) without adjudicating which perspective is correct. The perspectival gap IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology industry and efficiency-maximizing institutions are structural beneficiaries (they benefit from the claim that the tool is neutral; design proceeds under their terms; extraction of labor value and exclusion of non-optimizing goods is enabled). d for these seats is near the beneficiary end (0.1–0.25). Regulatory bodies sit at moderate directionality (d~0.5): they do coordinate real governance, but they are partly captured by the industry and constrained to procedural rather than substantive remedies. Workers and marginalized communities are structural targets: they bear costs without design participation, their exclusion is required for the constraint to operate, and their identity is locked to their relationship with the institution (job loss means cultural loss, algorithmic exclusion is experienced as personal exclusion). d for these seats approaches 1.0. The asymmetry is structural: the same constraint operates as coordination-with-benefit for some seats and extraction-with-suppression for others. This is the hallmark of tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophy: the founding problem (lack of regulatory coordination) is genuinely addressed by the procedural framework. The constraint is tangled_rope because it solves real coordination while enabling extraction. What prevents misclassification as rope: the declared beneficiaries are narrow (industry, efficiency maximizers, captured regulators), the victims are numerous and identity-locked, suppression is high and active (the neutrality narrative must be defended against theological and anthropological alternatives), and the theater ratio shows increasing performative activity. The constraint is neither pure coordination nor pure extraction—it is a hybrid that coordinates governance while extracting human value and suppressing alternative anthropologies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_claim_vs_anthropological_laden,
    'Is technology genuinely morally neutral, or does every design embed anthropological commitments (assumptions about human flourishing, worth, and what matters)?',
    'Comparative study of identical technologies deployed under different anthropological frameworks (e.g., efficiency-first vs. dignity-first design processes): if outcomes differ systematically, technology is not neutral. Or: process analysis of design choices (what alternatives were considered and rejected, what values guided each choice) reveals embedded commitments even when the framework claims neutrality.',
    'If technology is not neutral, the constraint''s core claim fails and the classification shifts from tangled_rope (coordination + extraction) toward snare (pure extraction covered by a false neutrality narrative). If technology is neutral, the constraint''s claim is vindicated but the structural problem of captured regulation remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neutrality_claim_vs_anthropological_laden, empirical, 'Whether the neutrality claim is empirically defensible or whether every design embeds anthropological commitments.').

omega_variable(
    procedural_justice_substitution,
    'Are procedural safeguards (transparency, disclosure, algorithmic audits, regulatory oversight) sufficient to protect human dignity and substantive justice, or do they function as substitutes that permit extraction while appearing to guard against it?',
    'Outcome analysis: do regulated AI systems show persistent disparities in impact across demographic and class lines, despite high transparency and procedural compliance? Do workers and marginalized communities report that procedural mechanisms protect their dignity or merely record their harm? Do regulatory bodies enforce substantive remedies or only procedural compliance?',
    'If procedures are insufficient, the constraint is pure extraction (snare) masked by procedural theater. If they are sufficient, the constraint is genuine tangled_rope (real coordination with some legitimate extraction costs). If they are somewhere between (most likely), the theater ratio and suppression level accurately model the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_justice_substitution, empirical, 'Whether procedural justice can substitute for substantive justice in protecting human dignity against AI deployment.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of alternative anthropological framings (incarnational humanism, integral-development readings) structural (institutional exclusion from technology governance spaces) or internalized (the alternative framings have lost persuasive power, are no longer held as live positions)?',
    'Counterfactual: if institutional barriers to alternative framings were removed (e.g., theological and anthropological traditions given standing in AI governance), would these alternative readings resurface as compelling voices? Or has the dominance of efficiency language so thoroughly replaced alternative vocabularies that the alternatives are no longer thinkable even in the absence of barriers?',
    'If suppression is primarily structural, removing the barriers (e.g., mandating inclusive governance processes) could restore voice and reshape the constraint''s operation. If suppression is primarily internalized, the constraint''s persistence does not depend on active coercion but on the internalization of efficiency language as the only legitimate language for discussing technology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, conceptual, 'Whether suppression of alternative readings is externally imposed or internalized through dominance of efficiency discourse.').

omega_variable(
    regulatory_capture_extent,
    'To what degree are regulatory bodies captured by the technology industry? Does the industry shape the vocabulary and definitions that regulators use (what counts as ''fairness,'' ''bias,'' ''transparency'')?',
    'Genealogical analysis: trace the origin of regulatory concepts and language back to industry actors, academic papers funded by industry, or regulatory bodies staffed by industry veterans. Document industry access to regulatory processes (lobbying records, committee participation, advisory roles). Compare regulatory definitions to definitions from non-captured sources (worker organizations, community advocates, theological traditions).',
    'High capture means the regulatory bodies are partly agenda-setters for the beneficiary side; the constraint is more purely extractive. Lower capture means regulatory bodies have genuine independence and the constraint is more genuinely tangled_rope. Capturing the regulator is the mechanism by which the industry captures the neutrality framing itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'The extent to which regulatory vocabulary and definitions are shaped by the technology industry.').

omega_variable(
    anthropological_commitment_kernel_reading,
    'This constraint is ONE reading of the contested kernel ''ai_human_relationship.'' The reading instantiates instrumental_subsidiarity: technology is morally neutral; responsibility lies in procedural governance. The sibling readings (incarnational_humanism and technocratic_optimization) would produce different constraints with different beneficiary/victim structures and different ε values. Which reading is correct—or are all three live as contested positions held by different parties with no logical resolution between them?',
    'Theological and anthropological argument: does the instrumental-subsidiarity reading adequately account for the substantive human goods that incarnational-humanism emphasizes? Does it adequately critique the efficiency-maximization that technocratic-optimization embraces? Or does each reading have internal coherence but different normative foundations, such that choosing between them is not empirically resolvable but rather a matter of which anthropological tradition one inhabits?',
    'If instrumental_subsidiarity is the correct reading, the constraint classification stands. If incarnational_humanism is correct, the constraint should be reclassified as snare (the neutrality claim is a cover story for subordinating human dignity to efficiency). If technocratic_optimization is correct, the constraint should be reclassified as rope (efficiency is the genuine coordination function, and victims are simply bearing the necessary costs of that coordination). If all three readings are live contested positions held by different parties, then the constraint exists within a field of unresolved theological and anthropological disagreement, and the engine''s per-seat classification will show seat-dependent divergence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anthropological_commitment_kernel_reading, conceptual, 'Whether the instrumental_subsidiarity reading is the correct anthropological reading of AI, or whether it is one live position in an irreducibly contested theological/anthropological field.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_instrumental_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(ai_instrumental_tr_t0, observed).
narrative_ontology:measurement(ai_instrumental_tr_t5, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(ai_instrumental_tr_t5, observed).
narrative_ontology:measurement(ai_instrumental_tr_t10, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(ai_instrumental_tr_t10, observed).
narrative_ontology:measurement(ai_instrumental_tr_t15, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(ai_instrumental_tr_t15, observed).
narrative_ontology:measurement(ai_instrumental_tr_t20, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(ai_instrumental_tr_t20, observed).
narrative_ontology:measurement(ai_instrumental_tr_t25, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(ai_instrumental_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ai_instrumental_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_instrumental_be_t0, observed).
narrative_ontology:measurement(ai_instrumental_be_t5, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(ai_instrumental_be_t5, observed).
narrative_ontology:measurement(ai_instrumental_be_t10, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(ai_instrumental_be_t10, observed).
narrative_ontology:measurement(ai_instrumental_be_t15, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(ai_instrumental_be_t15, observed).
narrative_ontology:measurement(ai_instrumental_be_t20, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(ai_instrumental_be_t20, observed).
narrative_ontology:measurement(ai_instrumental_be_t25, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(ai_instrumental_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_instrumental_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(ai_instrumental_su_t0, observed).
narrative_ontology:measurement(ai_instrumental_su_t5, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(ai_instrumental_su_t5, observed).
narrative_ontology:measurement(ai_instrumental_su_t10, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(ai_instrumental_su_t10, observed).
narrative_ontology:measurement(ai_instrumental_su_t15, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(ai_instrumental_su_t15, observed).
narrative_ontology:measurement(ai_instrumental_su_t20, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ai_instrumental_su_t20, observed).
narrative_ontology:measurement(ai_instrumental_su_t25, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(ai_instrumental_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__instrumental_subsidiarity, 0.12).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'ai_human_relationship.' The kernel involves irreducibly different anthropological commitments about human dignity, human flourishing, and technology's role. Three constraint stories decompose the kernel: incarnational_humanism (technology must serve integral human development and preferential option for poor), instrumental_subsidiarity (technology is neutral; regulation and procedure protect dignity), and technocratic_optimization (technology as efficiency instrument; human value measured by productivity). Each reading has its own ε, beneficiary/victim structure, and classification. They are linked as a constraint family via affects_constraints. The sibling stories will show per-seat classification divergence reflecting the different anthropological framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_human_relationship__instrumental_subsidiarity, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
