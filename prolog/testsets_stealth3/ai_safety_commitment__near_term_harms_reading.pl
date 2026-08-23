% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__near_term_harms_reading, []).

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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: Near-Term Harms Reading of the AI Safety Commitment
 *   domain: technological/governance
 *
 * SUMMARY:
 *   Deployed AI systems produce documented, recurring harms: discriminatory
 *   outcomes in hiring, credit, housing, benefits, and policing; piece-rate
 *   annotation and moderation work performed under conditions the
 *   responsible-AI announcements do not reach; information environments that
 *   amplify false content. The governance arrangement organized under the
 *   near-term-harms definition of AI safety responds principally with
 *   voluntary mechanisms — bias audits, transparency reports, ethics
 *   principles, impact assessments — supplemented by emerging regulatory
 *   frameworks that largely delegate measurement to the assessed parties. The
 *   arrangement has a real coordination core (shared harm documentation makes
 *   failures comparable and actionable) and a real extraction asymmetry (the
 *   harmed populations continue bearing unremediated costs while deploying
 *   firms convert participation into reputational and regulatory shielding).
 *   KEY AGENTS (by structural relationship): - large_ai_deployers:
 *   Agenda-setting beneficiary (institutional/arbitrage) — runs deployments,
 *   funds the assessment apparatus, shapes the governance agenda -
 *   marginalized_algorithmic_subjects: Primary target (powerless/trapped) —
 *   bears discriminatory denials with no exit from digitized essential
 *   services - gig_data_labelers: Target (moderate/constrained) — performs
 *   the annotation and moderation labor under piece-rate terms -
 *   misinformation_bearing_publics: Diffuse target (moderate/constrained) —
 *   bears civic and epistemic costs - algorithmic_audit_industry: Secondary
 *   beneficiary (organized/mobile) — collects recurring assessment revenue -
 *   near_term_safety_research_community: Secondary beneficiary
 *   (organized/identity_locked) — careers constituted by the framing -
 *   affected_community_advocates: Excluded voice (moderate/constrained) —
 *   seeks binding remedies, seated late or never - ai_regulatory_bodies:
 *   Analytical observer (institutional/analytical) — decides whether
 *   commitments harden into obligations
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.58).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "Near-Term Harms Reading of the AI Safety Commitment").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technological/governance").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '8a890c3b-2737-4323-8fec-a8d35ad284e0').
narrative_ontology:cs_kernel_codification('8a890c3b-2737-4323-8fec-a8d35ad284e0', distributed).
narrative_ontology:cs_authority_grounding('8a890c3b-2737-4323-8fec-a8d35ad284e0', expertise).
narrative_ontology:cs_interpretation_layer_present('8a890c3b-2737-4323-8fec-a8d35ad284e0').
narrative_ontology:cs_reading_relation('8a890c3b-2737-4323-8fec-a8d35ad284e0', ai_safety_commitment__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('8a890c3b-2737-4323-8fec-a8d35ad284e0', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('8a890c3b-2737-4323-8fec-a8d35ad284e0', foundational, documented_present_harm_primacy).
narrative_ontology:cs_axiom_status(documented_present_harm_primacy, holdable).
narrative_ontology:cs_axiom_grounding('8a890c3b-2737-4323-8fec-a8d35ad284e0', documented_present_harm_primacy, deontological).
narrative_ontology:cs_axiom('8a890c3b-2737-4323-8fec-a8d35ad284e0', foundational, speculative_outcomes_insufficient_obligation_grounding).
narrative_ontology:cs_axiom_status(speculative_outcomes_insufficient_obligation_grounding, holdable).
narrative_ontology:cs_axiom_grounding('8a890c3b-2737-4323-8fec-a8d35ad284e0', speculative_outcomes_insufficient_obligation_grounding, empirically_contingent).
narrative_ontology:cs_reference_frame('8a890c3b-2737-4323-8fec-a8d35ad284e0', documented_harm_prevention_standard).
narrative_ontology:cs_drift_state('8a890c3b-2737-4323-8fec-a8d35ad284e0', contemporary_post_deployment_scaling, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8a890c3b-2737-4323-8fec-a8d35ad284e0', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, large_ai_deployers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, algorithmic_audit_industry).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, near_term_safety_research_community).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_algorithmic_subjects).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_data_labelers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, misinformation_bearing_publics).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, documented_harm_evidentiary_standard).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, voluntary_transparency_sufficiency_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the platforms and models whose deployments generate the documented harms; fund and staff ethics teams, publish transparency reports and voluntary pledges, and shape policy consultations. Deployment revenue continues while governance remains voluntary; operations, subsidiaries, and product lines can be relocated or restructured across jurisdictions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, large_ai_deployers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, large_ai_deployers, beneficiary).

% Are scored, ranked, and filtered by models in hiring, lending, housing, benefits, and policing; wrongful denials and misclassifications fall on them directly. Essential services have moved behind these systems, so opting out is not available; recourse runs through complaint and audit channels they did not design.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_algorithmic_subjects, payer,
    powerless, biographical, trapped, global).

% Perform annotation and content moderation under piece-rate contracts, often distributed across borders; absorb psychological strain and income instability. Protections announced under responsible-AI programs rarely reach their contracts; leaving means giving up one of the few accessible income sources.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_data_labelers, payer,
    moderate, immediate, constrained, global).

% Live inside information environments tuned for engagement; absorb the civic and epistemic costs of amplified false content. Stepping outside the platforms carries social and economic price; protection depends on moderation choices made elsewhere.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, misinformation_bearing_publics, payer,
    moderate, biographical, constrained, global).

% Sells bias audits, red-team exercises, and impact assessments to deploying firms; revenue recurs only while assessments remain voluntary and repeatable. Client relationships discipline how findings are framed and released.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, algorithmic_audit_industry, beneficiary,
    organized, biographical, mobile, global).

% Works in fairness, accountability, and transparency subfields; careers, venues, and grant lines are organized around measuring deployment harms. The field's definition of its own subject is bound up with practitioners' accumulated expertise and standing; a shift in what the field is defined to study would strand both.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, near_term_safety_research_community, beneficiary,
    organized, biographical, identity_locked, global).

% Organize residents and workers harmed by automated decisions and seek binding remedies; they are seldom seated in the technical standard-setting and corporate consultation processes where audit scope, thresholds, and timelines are decided, arriving after parameters are fixed.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, affected_community_advocates, excluded,
    moderate, generational, constrained, regional).

% Draft deployment rules, commission studies, and hear testimony from firms, auditors, and civil society; their enforcement choices determine whether published commitments become enforceable obligations.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_regulatory_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, large_ai_deployers).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Builds shared instruments for detecting, documenting, and comparing harms from deployed systems — incident databases, audit methodologies, harm taxonomies, transparency formats — so that firms, regulators, journalists, and researchers can identify failures through common procedures.
% TRANSFER_FUNCTION: Moves compliance spending (assessment fees, reporting labor, ethics staffing) from deploying firms to auditors and internal teams; moves grant funding, attention, and career capital toward near-term harm research; leaves the costs of the harms themselves — denied applications, precarious piecework, degraded information environments — with affected populations.
% ABSENT_VOICES: Frontline data workers and residents of affected communities are seldom seated where audit scope, thresholds, and timelines are negotiated; independent labor organizations for annotation and moderation workforces have thin access; the publics bearing misinformation costs have no seat at all. Their characteristic demands — binding remedies, wage floors, liability — enter the process mostly after parameters are fixed.
% DISAPPEARANCE_RATIONALE: Harm-documentation infrastructure, the audit market, ethics-team staffing, and the research and advocacy ecosystems organized around them would dissolve within months; deployments would continue but with sharply less measurement, comparability, or public record. The surrounding arrangements visibly depend on it.
% FOUNDING_PROBLEM: Deployed systems were producing discriminatory and exploitative outcomes with no shared method to detect, compare, or act on them; the arrangement was built to make those harms visible and governable.
% FOUNDING_PROBLEM_CORROBORATION: Independent university audits, investigative journalism on moderation and annotation working conditions, and civil-society incident registries — all outside the benefiting parties — attest that the harms persist. Deploying firms attest progress instead; the gap between those testimonies is itself the signal that corroboration must come from outside the beneficiary set.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68: the harms are documented and continuing at scale while the response is partial and increasingly procedural. Suppression is authored at 0.58 as a RAW STRUCTURAL PROPERTY — it is not scaled by power or scope in the engine's computation; it reflects locked-in exposure to non-exitable algorithmic services, piecework income dependence, and framing discipline over what counts as a safety problem. Theater_ratio at 0.52: ethics-washing and audit performance are substantial, but genuine documentation and some real mitigations exist, so the ratio sits just above even. Accessibility_collapse at 0.48: alternatives (binding legislation, litigation, refusal, collective withdrawal) remain partly open — the framing narrows the option space without closing it. Resistance at 0.62: data-worker organizing, community campaigns, adversarial auditing, and regulatory friction are active and growing. The claimed type (tangled_rope) is stated from structure — a genuine documentation-and-comparison function bound to an asymmetric cost structure requiring active maintenance (voluntary-regime upkeep, framing defense, consultation management) — and the metrics are authored descriptively and independently; the engine computes per-seat classifications from the structural data. The measurement series run on ONE SHARED GRID ({0,2,4,6,8,10}) with every tracked metric authored at every point; all three series rise monotonically over the interval as deployment scale outpaced mitigation capacity and the voluntary apparatus thickened.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seat should compute differently. From the deployer position the arrangement is responsible-governance leadership it designed and funds; from the trapped subject position it is enforced exposure to erroneous decisions with complaint channels as the only recourse; from the auditor position it is a recurring market; from the researcher position it is a vocation. Same structure, four experienced realities — the engine derives this divergence from power, exit, and directionality data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: large_ai_deployers sit near the beneficiary end (d low) — they set the terms and accrue the shielding value; the audit industry and research community derive low-to-moderate d as secondary beneficiaries with mobile and identity-locked exits respectively. The three victim groups derive high d: marginalized_algorithmic_subjects nearest the full-target end (trapped exit amplifies), gig_data_labelers and misinformation_bearing_publics slightly less (constrained but not absent exits). The regulatory observer seat is analytical. No directionality overrides are authored: the structural derivation from declared positions and exit options captures each seat's relationship without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — the harms the arrangement was built to make governable persist and are still being documented — so the mandate has not outlived its function and no mandatrophy resolution is declared. The tangled_rope classification prevents two symmetrical errors: labeling the arrangement a pure snare would erase the genuine harm-documentation coordination that victims, journalists, and regulators themselves rely on; labeling it a rope would erase the extraction asymmetry in which deployers pay compliance-performance prices while harmed populations bear unremediated costs. On the mismatch consumer: founding_problem_status=live crossed with disappearance_verdict=world_rearranges raises no zombie flag — the arrangement's persistence tracks a problem that still exists. Coalition potential among the powerless is real and noted: data-labeler organizing and community-labor alliances are the principal route by which the trapped and constrained payer seats convert diffuse grievance into bargaining power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (near_term_harms_reading) of the ai_safety_commitment kernel; what would change structurally if a sibling reading (existential_risk_reading, dual_priority_reading) governed instead?',
    'Read the linked sibling stories and compare their structural deltas: victim sets, beneficiary sets, and epsilon profiles are authored per reading and joined through the network edges in this file.',
    'Under the existential-risk sibling the victim set shifts from present-day harmed populations to future persons, epsilon redistributes from deployed-system governance to frontier-research governance, and the beneficiary set moves toward alignment-lab funding structures; this file''s classification is invariant within its own reading and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: which reading of the AI-safety kernel this constraint is, and what siblings would change.').

omega_variable(
    disagreement_location_temporal_index,
    'Where exactly do the readings of the ai_safety_commitment kernel disagree — is the dispute located in the temporal index of the safety obligation (documented present harm versus projected catastrophic outcome) or in the evidentiary standard attached to each?',
    'Conceptual analysis of which safety obligations survive under each evidentiary standard, informed by empirical track records of harm documentation versus long-horizon forecasting accuracy.',
    'If the dispute is purely temporal-index, the readings could in principle be satisfied sequentially without conflict; if it is evidentiary-standard, they impose mutually exclusive resource-allocation disciplines and no single governance regime can satisfy both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_temporal_index, conceptual, 'Locates the structural element on which sibling readings diverge.').

omega_variable(
    audit_effectiveness_vs_theater,
    'Do the audit, transparency, and impact-assessment practices organized under this reading reduce realized harm rates, or do they chiefly produce compliance artifacts?',
    'Longitudinal measurement of harm incidence around audit adoption, and comparison across jurisdictions with binding versus voluntary assessment regimes.',
    'If audits show no harm-rate effect, the authored theater_ratio understates the condition and the arrangement drifts toward pure extraction; if audits show real effects, the coordination function is stronger than the theater ratio suggests and the hybrid classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_effectiveness_vs_theater, empirical, 'Whether the arrangement''s assessment machinery functions or performs.').

omega_variable(
    suppression_mechanism_structural_vs_discursive,
    'Is the measured suppression carried by structural barriers (non-exitability of algorithmically mediated essential services, piecework income dependence) or by discursive enforcement (framing control over what counts as a safety problem, chilling of internal dissent)?',
    'Post-exit trajectory analysis of leavers (whether former participants shed the framing after exiting) and natural experiments where structural barriers lift while the discourse apparatus remains.',
    'If the discursive share is high, suppression persists after structural reform and remediation must target the framing infrastructure itself, not merely access rules; the effective suppression exceeds what the structural measure alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_discursive, empirical, 'Structural versus discursive composition of the arrangement''s suppressive force.').

omega_variable(
    framing_dominance_provenance,
    'Is the institutional dominance of the near-term-harms definition of AI safety an organic response to documented harm, or substantially cultivated by deploying firms that prefer soft, voluntary governance?',
    'Funding-flow analysis of ethics and audit institutions, and archival tracing of framing adoption in corporate communications versus academic literature.',
    'If cultivated, attribution strengthens toward the arrangement functioning as cover for continued deployment on favorable terms; if organic, the coordination function is more genuine than the theater ratio implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_dominance_provenance, empirical, 'Provenance of the framing''s dominance: organic or manufactured.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_safety_nt_harms_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_safety_nt_harms_tr_t2, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2, 0.34).
narrative_ontology:measurement(ai_safety_nt_harms_tr_t4, ai_safety_commitment__near_term_harms_reading, theater_ratio, 4, 0.39).
narrative_ontology:measurement(ai_safety_nt_harms_tr_t6, ai_safety_commitment__near_term_harms_reading, theater_ratio, 6, 0.44).
narrative_ontology:measurement(ai_safety_nt_harms_tr_t8, ai_safety_commitment__near_term_harms_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(ai_safety_nt_harms_tr_t10, ai_safety_commitment__near_term_harms_reading, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_safety_nt_harms_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_safety_nt_harms_be_t2, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2, 0.54).
narrative_ontology:measurement(ai_safety_nt_harms_be_t4, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 4, 0.59).
narrative_ontology:measurement(ai_safety_nt_harms_be_t6, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(ai_safety_nt_harms_be_t8, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(ai_safety_nt_harms_be_t10, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_safety_nt_harms_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(ai_safety_nt_harms_su_t2, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2, 0.46).
narrative_ontology:measurement(ai_safety_nt_harms_su_t4, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 4, 0.49).
narrative_ontology:measurement(ai_safety_nt_harms_su_t6, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement(ai_safety_nt_harms_su_t8, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(ai_safety_nt_harms_su_t10, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, information_standard).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI safety' decomposes into structurally distinct commitments over what safety obligates. This file authors the near-term-harms reading only: epsilon refers to the deployed-system governance arrangement as this reading sees it — high extraction from present harmed populations, low salience of burdens on speculative research. Sibling files author the existential-risk and dual-priority readings with their own epsilon values, victim sets, and beneficiaries; family membership is expressed through these network edges, never by hedging a single story across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
