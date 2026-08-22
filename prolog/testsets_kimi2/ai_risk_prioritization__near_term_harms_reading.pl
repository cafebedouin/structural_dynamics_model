% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__near_term_harms_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: Near-Term AI Harms Prioritization Regime
 *   domain: technological/governance
 *
 * SUMMARY:
 *   This constraint instantiates the near_term_harms_reading of the
 *   ai_risk_prioritization kernel, which holds that deployed AI systems'
 *   present, measurable impacts on marginalized populations constitute the
 *   primary locus of risk and governance priority. The sibling
 *   existential_risk_reading holds that misaligned AGI poses extinction-level
 *   threat. This reading structures resource allocation, peer review norms,
 *   and policy agendas to foreground bias, surveillance, and labor
 *   displacement over long-term alignment research. It is actively enforced
 *   through funding criteria, conference organization, and the rhetorical
 *   framing of existential risk as speculative distraction.
 *
 * KEY AGENTS:
 *   - fairness_accountability_researchers: Primary beneficiary (organized/constrained) â receive funding and prestige from the prioritization regime
 *   - x_risk_researchers: Primary target (moderate/constrained) â bear resource diversion and delegitimization
 *   - ai_governance_funders: Agenda setter (institutional/arbitrage) â adjudicate priorities and enforce the framing
 *   - marginalized_communities: Claimed beneficiary (powerless/trapped) â intended beneficiaries but unevenly delivered outcomes
 *   - ai_industry_ethics_teams: Secondary beneficiary (organized/mobile) â channel accountability into manageable audits
 *   - longtermist_advocates: Excluded voice (moderate/constrained) â structurally discounted in policy venues
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.62).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.71).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term AI Harms Prioritization Regime").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "technological/governance").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '536a2d60-39b5-4b4a-8053-823022cad366').
narrative_ontology:cs_kernel_codification('536a2d60-39b5-4b4a-8053-823022cad366', distributed).
narrative_ontology:cs_authority_grounding('536a2d60-39b5-4b4a-8053-823022cad366', distributed).
narrative_ontology:cs_reading_relation('536a2d60-39b5-4b4a-8053-823022cad366', ai_risk_prioritization__existential_risk_reading, influences).
narrative_ontology:cs_axiom('536a2d60-39b5-4b4a-8053-823022cad366', foundational, present_harm_takes_normative_priority).
narrative_ontology:cs_axiom_status(present_harm_takes_normative_priority, holdable).
narrative_ontology:cs_axiom_grounding('536a2d60-39b5-4b4a-8053-823022cad366', present_harm_takes_normative_priority, deontological).
narrative_ontology:cs_axiom('536a2d60-39b5-4b4a-8053-823022cad366', secondary, x_risk_is_speculative_distraction).
narrative_ontology:cs_axiom_status(x_risk_is_speculative_distraction, holdable).
narrative_ontology:cs_axiom_grounding('536a2d60-39b5-4b4a-8053-823022cad366', x_risk_is_speculative_distraction, empirically_contingent).
narrative_ontology:cs_reference_frame('536a2d60-39b5-4b4a-8053-823022cad366', present_accountability_framework).
narrative_ontology:cs_drift_state('536a2d60-39b5-4b4a-8053-823022cad366', contemporary_ai_governance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('536a2d60-39b5-4b4a-8053-823022cad366', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, ai_industry_ethics_teams).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, x_risk_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive funding streams, tenure lines, conference prestige, and policy access from the institutional prioritization of near-term harms. Their research agendas center bias audits, transparency metrics, and accountability mechanisms. Career success is tied to the continued dominance of this framing in AI governance venues.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    organized, biographical, constrained, global).

% Bear the cost of resource diversion and epistemic delegitimization. Their research on long-term safety and alignment is characterized as speculative or disconnected from present reality in major funding calls and policy documents. Exit options include reframing work as near-term safety or leaving the field entirely.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, x_risk_researchers, payer,
    moderate, civilizational, constrained, global).

% Claimed beneficiaries of the prioritization regime. Subject to present AI harms including hiring bias, facial recognition surveillance, and exploitative content moderation labor. The constraint asserts their interests are paramount, but material benefits such as regulatory relief and redress are unevenly delivered; they remain structurally exposed regardless of which risk frame dominates discourse.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, beneficiary,
    powerless, immediate, trapped, national).

% Benefit from the near-term framing by channeling accountability demands into manageable, non-structural reforms such as bias audits and transparency reports. These measures deflect regulatory pressure without altering core business models. Their budgets and headcount expand under the present-harm prioritization discourse.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_industry_ethics_teams, beneficiary,
    organized, biographical, mobile, global).

% Set funding priorities and evaluation criteria that enshrine near-term harms as the primary locus of AI risk. They adjudicate grant competitions and policy mandates, determining which research communities thrive. Their legitimacy is tied to delivering tangible, measurable outcomes within electoral or reporting cycles.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_governance_funders, agenda_setter,
    institutional, generational, arbitrage, national).

% Would argue for existential risk and long-term value considerations but are structurally excluded from near-term prioritization frameworks. Their arguments are preemptively discounted as speculative in policy and funding venues shaped by this constraint.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, longtermist_advocates, excluded,
    moderate, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates policy attention, research funding, and regulatory scrutiny toward present, measurable harms from deployed AI systems including discrimination, surveillance, and labor displacement, rather than toward diffuse future risks.
% TRANSFER_FUNCTION: Moves funding, conference prestige, policy bandwidth, and researcher careers from long-term safety and alignment fields toward fairness, accountability, and transparency research; moves moral legitimacy and rhetorical priority from future populations to present marginalized groups.
% ABSENT_VOICES: Future generations and long-term safety researchers are structurally excluded from near-term prioritization frameworks; their claims are discounted as speculative or hyperbolic. Workers in the Global South whose displacement is less visible in Western audit regimes are also largely absent from the prioritization discourse.
% DISAPPEARANCE_RATIONALE: If the near-term prioritization constraint vanished, research funding would reallocate toward long-term safety, policy agendas would shift to speculative risk governance, and present-harm advocacy organizations would lose their institutional leverage. The AI governance landscape would reorganize around different temporal priorities and beneficiary structures.
% FOUNDING_PROBLEM: AI systems were being deployed without accountability for present harms: biased hiring tools, discriminatory facial recognition, exploitative content moderation labor, and opaque surveillance systems disproportionately affecting marginalized communities.
% FOUNDING_PROBLEM_CORROBORATION: Independent civil liberties organizations, affected community groups, and empirical audit studies conducted outside the direct fairness-research funding network corroborate that present harms are ongoing. Long-term safety researchers contest the framing as primary but do not dispute the underlying harms.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the regime diverts substantial resources and legitimacy from long-term safety research while delivering genuine coordination on present harms. Suppression is high (0.71) because the constraint's persistence depends on actively framing existential risk as speculative and excluding it from mainstream funding and policy channels. Theater is moderate (0.48) because a significant share of industry adoption consists of performative bias audits and transparency reports that do not alter underlying system behavior. Accessibility collapse (0.58) reflects the delegitimization of x-risk alternatives in key governance venues. Resistance (0.52) captures the ongoing pushback from the long-term safety community and some technologists. The measurement series track the regime's consolidation over a five-year interval as generative AI deployment intensified present-harm visibility and policy response.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (x_risk_researchers) experiences the constraint as epistemic capture and resource extraction: their field is defunded and ridiculed. The beneficiary seat (fairness_accountability_researchers) experiences the same structure as legitimate justice coordination correcting a historical neglect of marginalized populations. The agenda setter (ai_governance_funders) experiences a manageable portfolio with measurable near-term outcomes. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Fairness and accountability researchers are structural beneficiaries: they collect funding, citations, and policy access under this regime, placing them toward the low-d end. X-risk researchers are structural targets: they pay through lost funding and epistemic exclusion, placing them toward the high-d end. Marginalized communities are claimed beneficiaries but their structural position is ambiguous because the regime does not guarantee material relief; they sit closer to symmetric but with limited exit. Industry ethics teams are beneficiaries who capture regulatory delay. The funders are agenda setters with arbitrage-grade exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the genuine coordination function â the real present harms of biased hiring, surveillance, and labor exploitation â this constraint would be a pure snare suppressing x-risk research under cover of justice rhetoric. Without the extraction function â the resource capture by the fairness field, the ethics-washing by industry, and the active suppression of alternative risk framings â it would be a rope coordinating society around legitimate accountability. The tangled_rope classification captures the hybrid reality: both the coordination and the asymmetric extraction run through the same institutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performative_vs_material_delivery,
    'Does the near-term prioritization regime deliver material regulatory and distributive benefits to marginalized communities, or primarily performative visibility and institutional cover for industry?',
    'Longitudinal outcome studies comparing harm rates in jurisdictions with strong near-term prioritization frameworks versus those without, controlling for industry pressure and enforcement capacity.',
    'If performative, the constraint''s coordination function is weaker than claimed and its extraction via ethics-washing is higher, pushing the seat computation toward snare. If material, the coordination function is stronger and the classification remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_vs_material_delivery, empirical, 'Uncertainty about whether near-term benefits are material or performative').

omega_variable(
    x_risk_suppression_mechanism,
    'Is the suppression of existential risk discourse a correction of epistemic overreach or an institutional resource competition mechanism?',
    'Historical bibliometric and funding-flow analysis: if x-risk research quality was high and funding collapse correlates with the rise of near-term advocacy rather than with empirical refutation of long-term risk, suppression is institutional capture.',
    'If epistemic correction, extraction is lower and the constraint is more defensible as coordination; if capture, extraction is higher and the coordination story functions partly as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(x_risk_suppression_mechanism, conceptual, 'Ambiguity about whether x-risk suppression is epistemic or extractive').

omega_variable(
    kernel_reading_zero_sum,
    'Can the near-term and existential risk readings be integrated into a unified governance framework, or are they structurally zero-sum in resource allocation?',
    'Policy experiments in dual-track governance that fund both present-harm accountability and long-term safety without measurable degradation in either; observing whether the constraint''s proponents treat such frameworks as legitimate or as threats.',
    'If integratable, the kernel may resolve into a scaffold or rope; if zero-sum, the kernel remains contested with ongoing extraction across readings and the tangled_rope classification persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_zero_sum, preference, 'Whether sibling readings are necessarily in conflict').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_risk_nt_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_risk_nt_tr_t1, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 1, 0.28).
narrative_ontology:measurement(ai_risk_nt_tr_t2, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2, 0.35).
narrative_ontology:measurement(ai_risk_nt_tr_t3, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 3, 0.4).
narrative_ontology:measurement(ai_risk_nt_tr_t4, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 4, 0.45).
narrative_ontology:measurement(ai_risk_nt_tr_t5, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_risk_nt_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ai_risk_nt_be_t1, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 1, 0.4).
narrative_ontology:measurement(ai_risk_nt_be_t2, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(ai_risk_nt_be_t3, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(ai_risk_nt_be_t4, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 4, 0.59).
narrative_ontology:measurement(ai_risk_nt_be_t5, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 5, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_risk_nt_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(ai_risk_nt_su_t1, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 1, 0.5).
narrative_ontology:measurement(ai_risk_nt_su_t2, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(ai_risk_nt_su_t3, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 3, 0.64).
narrative_ontology:measurement(ai_risk_nt_su_t4, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(ai_risk_nt_su_t5, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 5, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% This constraint and ai_risk_prioritization__existential_risk_reading are sibling readings of the ai_risk_prioritization kernel. They share the referent of AI risk but instantiate different epsilon values, beneficiary and victim structures, and temporal scopes. The near-term reading foregrounds present marginalized populations; the existential reading foregrounds future existential catastrophe.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
