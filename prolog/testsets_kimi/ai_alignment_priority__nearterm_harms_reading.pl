% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__nearterm_harms_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: Near-Term Harms Priority Reading of AI Alignment
 *   domain: AI governance/ethics
 *
 * SUMMARY:
 *   This constraint story models the 'near-term harms' reading of the
 *   contested AI alignment priority kernel. In this reading, alignment is
 *   defined as preventing present discriminatory and extractive harms from
 *   deployed AI, with justice for marginalized populations as the priority.
 *   The reading operates as an institutional constraint on the AI governance
 *   field, determining funding flows, regulatory attention, and
 *   methodological legitimacy. It is claimed as a coordination mechanism for
 *   justice but exhibits high extractiveness through audit-industry capture,
 *   data extraction from marginalized groups, and the suppression of
 *   alternative alignment frames such as existential risk. The beneficiaries
 *   include the audit institutions and the claimed beneficiary populations;
 *   the victims are the specific marginalized communities (defined by race,
 *   disability, age) who bear the costs of the audit apparatus. The story is
 *   part of a constraint family with the existential_risk_reading and
 *   integrated_reading of the same kernel.
 *
 * KEY AGENTS:
 *   - bias_mitigation_auditors: Primary agenda_setter (institutional/constrained) â sets audit priorities and captures resource flows
 *   - present_vulnerable_populations: Claimed beneficiary (powerless/trapped) â receives intermittent protection
 *   - marginalized_communities: Primary target/payer (powerless/trapped) â bears extractive audit costs and continued system exposure
 *   - x_risk_researchers: Excluded party (organized/constrained) â deprioritized and structurally silenced
 *   - ai_developers: Secondary payer (powerful/constrained) â bears compliance and pipeline alteration costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.78).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.74).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "Near-Term Harms Priority Reading of AI Alignment").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "AI governance/ethics").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '5462d1c7-5790-4ec3-8c34-5989487c39bc').
narrative_ontology:cs_kernel_codification('5462d1c7-5790-4ec3-8c34-5989487c39bc', formalized).
narrative_ontology:cs_authority_grounding('5462d1c7-5790-4ec3-8c34-5989487c39bc', expertise).
narrative_ontology:cs_interpretation_layer_present('5462d1c7-5790-4ec3-8c34-5989487c39bc').
narrative_ontology:cs_reading_relation('5462d1c7-5790-4ec3-8c34-5989487c39bc', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('5462d1c7-5790-4ec3-8c34-5989487c39bc', ai_alignment_priority__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('5462d1c7-5790-4ec3-8c34-5989487c39bc', foundational, justice_for_marginalized_as_priority).
narrative_ontology:cs_axiom_status(justice_for_marginalized_as_priority, holdable).
narrative_ontology:cs_axiom_grounding('5462d1c7-5790-4ec3-8c34-5989487c39bc', justice_for_marginalized_as_priority, deontological).
narrative_ontology:cs_axiom('5462d1c7-5790-4ec3-8c34-5989487c39bc', foundational, alignment_as_present_harm_prevention).
narrative_ontology:cs_axiom_status(alignment_as_present_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('5462d1c7-5790-4ec3-8c34-5989487c39bc', alignment_as_present_harm_prevention, conventional).
narrative_ontology:cs_reference_frame('5462d1c7-5790-4ec3-8c34-5989487c39bc', present_harm_accountability).
narrative_ontology:cs_drift_state('5462d1c7-5790-4ec3-8c34-5989487c39bc', post_generative_ai_deployment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5462d1c7-5790-4ec3-8c34-5989487c39bc', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, bias_mitigation_auditors).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, marginalized_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the research and policy agenda defining AI alignment as the prevention of present discriminatory and extractive harms. Receives funding, publication venues, and institutional legitimacy from this framing. Develops and administers sociotechnical audit methodologies and bias mitigation frameworks. Professional careers and research programs are built around maintaining this priority.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, bias_mitigation_auditors, agenda_setter,
    institutional, generational, constrained, global).

% The claimed beneficiaries of the alignment priority: marginalized communities promised protection from discriminatory AI through audits, bias testing, and regulatory attention. Receive intermittent redress such as model retractions or disclosure reports, but remain embedded in the algorithmic systems being audited.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, national).

% Bear the direct costs of the extractive audit apparatus: uncompensated data and labor extraction for fairness testing, consultation fatigue, and continued exposure to harmful systems that are audited but rarely removed. Includes specific groups defined by race, disability, and age who are disproportionately subjected to algorithmic classification and surveillance in the name of harm prevention.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_communities, payer,
    powerless, immediate, trapped, national).

% Argue for existential safety and loss-of-control prevention as the core of AI alignment. Structurally excluded from funding pools, policy conversations, and mainstream governance fora that have been captured by the near-term harms frame. Their research is deprioritized and their legitimacy contested within the dominant sociotechnical paradigm.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, x_risk_researchers, excluded,
    organized, civilizational, constrained, global).

% Bear compliance costs of mandatory sociotechnical audits, documentation requirements, and bias mitigation interventions. Must alter development pipelines and product designs to satisfy audit criteria determined by the ethics community. Some absorb costs directly; others pass them to users or abandon products in regulated jurisdictions.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_developers, payer,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, bias_mitigation_auditors).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates research funding, regulatory attention, and methodological development toward preventing discriminatory and extractive harms from currently deployed AI systems, creating a centralized accountability mechanism for algorithmic impacts on vulnerable populations.
% TRANSFER_FUNCTION: Moves financial and human resources from long-term AI safety research and unrestricted AI development toward sociotechnical audit infrastructures and bias mitigation teams; moves the labor burden of fairness testing and the epistemic burden of harm definition onto marginalized communities and compliant developers.
% ABSENT_VOICES: Existential risk researchers and future generations are structurally excluded from priority-setting; industry voices emphasizing capabilities and speed are marginalized; affected communities are consulted as data sources but not as governance equals.
% DISAPPEARANCE_RATIONALE: If this priority framing vanished, research funding would reallocate toward existential safety and capabilities, regulatory attention would shift to long-term risk and open-source governance, the sociotechnical audit industry would contract, and marginalized communities would face a different but potentially equally unaccountable set of algorithmic systems.
% FOUNDING_PROBLEM: Deployed AI systems were producing measurable, unaccountable discriminatory outcomes against marginalized populations with no governance framework for redress, audit, or systematic mitigation.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations and affected community advocates outside the AI ethics funding stream attest to ongoing algorithmic discrimination and document specific harms through litigation and journalism. Existential risk researchers and some industry actors contest that this problem warrants exclusive priority over long-term safety.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__nearterm_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__nearterm_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the audit infrastructure captures significant resources while the underlying systems remain largely deployed; the extraction is decoupled from the genuine protection delivered. Suppression (0.74) reflects the active enforcement of this priority frame through funding allocation, peer review gatekeeping, and the marginalization of x-risk research. Theater ratio (0.58) indicates that a substantial portion of audit activity serves legitimization rather than harm reduction â models are audited, modified marginally, and redeployed without structural change. Accessibility collapse (0.60) captures the partial suppression of alternative alignment frames (x-risk, open capabilities) within mainstream governance discourse. Resistance (0.50) reflects ongoing pushback from the x-risk community and some industry actors. The measurement series share one time grid to prevent misaligned temporal substitution.
 *
 * PERSPECTIVAL GAP:
 *   The bias_mitigation_auditors experience this constraint as genuine coordination â they are solving a real accountability problem and receiving legitimate support. The marginalized_communities experience it as extraction â their data and labor fuel an audit industry that leaves them in the same systems. The x_risk_researchers experience it as suppression â their research frame is rendered illegitimate. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The bias_mitigation_auditors are structural beneficiaries (low d): they collect resources, set rules, and have professional mobility. Present_vulnerable_populations are nominal beneficiaries but structurally near-symmetric or slightly target-ward because their benefit is diffuse and intermittent. Marginalized_communities are full targets (high d): they pay with data, labor, and continued exposure. X_risk_researchers are excluded targets (high d) via resource deprivation. AI_developers are constrained payers (moderate-high d) who could theoretically exit markets but face competitive pressure to comply.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â unaccountable algorithmic discrimination â remains live, preventing a clean piton verdict. However, the solution has atrophied into an audit-industry complex that extracts from the communities it claims to serve. The classification as tangled_rope captures that genuine coordination (harm documentation, some redress) and asymmetric extraction (audit capture, community data extraction) coexist in the same structure. A snare classification would miss the real coordination; a rope classification would miss the capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sociotechnical_audit_extraction,
    'Does the sociotechnical audit methodology genuinely reduce harms to marginalized communities, or does it extract data and labor from them while providing cover for continued deployment?',
    'Comparative outcome studies of audited versus unaudited systems; labor accounting of community consultation and dataset construction; community-led review of audit effectiveness.',
    'If extraction dominates, the reading computes as snare or highly extractive tangled rope; if protection dominates, it moves toward rope. Resolves the beneficiary-victim overlap ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sociotechnical_audit_extraction, empirical, 'Whether audits extract from or protect marginalized communities.').

omega_variable(
    priority_zero_sum_resource,
    'Does institutionalizing this reading as the alignment priority structurally deprioritize existential risk research, or can governance resources expand to cover both near-term and long-term safety?',
    'Longitudinal funding flow analysis; citation network and hiring data between FAccT and x-risk research communities; cross-jurisdictional comparison of integrated versus siloed governance.',
    'If zero-sum, the reading''s extraction includes opportunity costs on civilizational safety and the victim set expands to future generations; if not, the tension is coordination rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priority_zero_sum_resource, empirical, 'Whether near-term priority is resource-zero-sum with existential safety.').

omega_variable(
    marginalized_beneficiary_or_victim,
    'Are the present vulnerable populations named as beneficiaries structurally distinct from the marginalized groups counted as victims, or does the constraint simultaneously claim and extract from the same communities?',
    'Demographic mapping of audit beneficiaries versus audit subjects; community-participatory review of who receives protection versus who bears costs.',
    'If the sets overlap substantially, the reading is a tangled rope or snare extracting from its claimed beneficiaries; if distinct, the extraction falls on a different group than the benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginalized_beneficiary_or_victim, conceptual, 'Overlap between claimed beneficiaries and structural victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(ai_a_tr_t18, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 18, 0.52).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(ai_a_be_t18, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(ai_a_su_t18, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 24, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_alignment_priority kernel, which decomposes into three structurally distinct claims: near-term harms (this file), existential risk, and integrated. The epsilon values and beneficiary/victim structures differ across readings. Each story links to its siblings to form a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
