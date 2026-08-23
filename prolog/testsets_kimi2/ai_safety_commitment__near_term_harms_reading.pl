% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: AI Safety as Near-Term Harm Prevention (Near-Term Harms Reading)
 *   domain: AI Safety / Technology Governance / Risk Assessment
 *
 * SUMMARY:
 *   This constraint instantiates the near_term_harms_reading of the contested
 *   ai_safety_commitment kernel. Under this reading, AI safety is defined as
 *   preventing documented present-day harmsâbias, discrimination, labor
 *   exploitation, and misinformationâfrom deployed systems. The framing
 *   coordinates regulators, civil society, and industry around tractable
 *   audit and transparency standards, but also functions as a regulatory
 *   avoidance mechanism for tech incumbents who capture the safety agenda
 *   while externalizing harm costs to marginalized populations and gig
 *   workers. Sibling readings include existential_risk_reading
 *   (extinction-level misalignment) and dual_priority_reading (non-competing
 *   near-term and x-risk priorities).
 *
 * KEY AGENTS:
 *   - ai_industry_incumbents (institutional/arbitrage/global): Primary agenda-setter and beneficiaryâframes AI safety as near-term harm prevention to avoid broader structural regulation.
 *   - marginalized_populations (powerless/trapped/national): Primary targetâbear persistent algorithmic discrimination and exclusion under audit-washing regimes.
 *   - gig_workers (powerless/constrained/national): Primary targetâsubject to algorithmic management and wage suppression under platform-defined safety frameworks.
 *   - ai_ethics_researchers (moderate/constrained/national): Secondary beneficiaryâreceive funding and career opportunity from industry-sponsored audit regimes.
 *   - existential_risk_advocates (organized/constrained/global): Excluded voiceâstructurally marginalized in policy and funding by the near-term harm dominance.
 *   - independent_regulatory_analysts (institutional/analytical/national): Observerâevaluate the gap between transparency activity and harm reduction without enforcement power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.72).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety as Near-Term Harm Prevention (Near-Term Harms Reading)").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "AI Safety / Technology Governance / Risk Assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '2a94fb3a-b255-433a-9ecc-7284adef0948').
narrative_ontology:cs_kernel_codification('2a94fb3a-b255-433a-9ecc-7284adef0948', distributed).
narrative_ontology:cs_authority_grounding('2a94fb3a-b255-433a-9ecc-7284adef0948', distributed).
narrative_ontology:cs_reading_relation('2a94fb3a-b255-433a-9ecc-7284adef0948', ai_safety_commitment__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('2a94fb3a-b255-433a-9ecc-7284adef0948', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('2a94fb3a-b255-433a-9ecc-7284adef0948', foundational, documented_harm_precedence_over_speculative_risk).
narrative_ontology:cs_axiom_status(documented_harm_precedence_over_speculative_risk, holdable).
narrative_ontology:cs_axiom_grounding('2a94fb3a-b255-433a-9ecc-7284adef0948', documented_harm_precedence_over_speculative_risk, empirically_contingent).
narrative_ontology:cs_axiom('2a94fb3a-b255-433a-9ecc-7284adef0948', foundational, algorithmic_accountability_through_audit_and_transparency).
narrative_ontology:cs_axiom_status(algorithmic_accountability_through_audit_and_transparency, holdable).
narrative_ontology:cs_axiom_grounding('2a94fb3a-b255-433a-9ecc-7284adef0948', algorithmic_accountability_through_audit_and_transparency, instrumental).
narrative_ontology:cs_reference_frame('2a94fb3a-b255-433a-9ecc-7284adef0948', present_harm_prevention_norm).
narrative_ontology:cs_drift_state('2a94fb3a-b255-433a-9ecc-7284adef0948', contemporary_policy_arena, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2a94fb3a-b255-433a-9ecc-7284adef0948', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_industry_incumbents).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_ethics_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frames AI safety as the prevention of documented present-day harms through corporate ethics programs, transparency reports, and audit initiatives. This framing deflects broader structural regulationâantitrust, labor law, and existential-risk governanceâwhile preserving public legitimacy and regulatory forebearance. They administer the standards and fund the research agenda that operationalizes this definition.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_industry_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the ongoing costs of algorithmic discrimination in credit, housing, criminal justice, and social services under regimes that document harms but do not eliminate them. They lack meaningful exit from the platforms and systems that shape their economic and civic opportunities.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_populations, payer,
    powerless, biographical, trapped, national).

% Subject to algorithmic management, wage suppression, and disciplinary classification by platforms that claim to optimize fairness and safety. The near-term harm frame acknowledges labor issues but channels solutions into platform-controlled audits rather than worker-determined protections.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_workers, payer,
    powerless, immediate, constrained, national).

% Receive funding, career opportunity, and institutional standing from industry-sponsored AI ethics programs and audit regimes. Their research is bounded by the near-term harm frame; structural critiques that would threaten funding sources are systematically de-emphasized.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_ethics_researchers, beneficiary,
    moderate, biographical, constrained, national).

% Argue for prioritizing catastrophic and extinction risks from advanced AI systems. Their framing is marginalized in policy arenas and funding pools where near-term harms dominate the operational definition of AI safety; they are structurally excluded from the agenda by the commitment frame itself.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_advocates, excluded,
    organized, civilizational, constrained, global).

% Evaluate whether industry-administered audit regimes reduce harm incidence or serve as regulatory avoidance. They compile evidence on the gap between transparency activity and outcome improvement, but lack enforcement power to alter the constraint.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, independent_regulatory_analysts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, ai_industry_incumbents).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates industry, civil society, and regulators around measurable, tractable standards for deployed AI system behaviorâbias metrics, transparency reports, and audit protocolsâcreating a shared vocabulary for accountability that avoids intractable speculative debates.
% TRANSFER_FUNCTION: Moves regulatory attention, public legitimacy, and research funding from structural governance of AI developers to industry-administered audit and disclosure regimes, while the costs of persistent algorithmic discrimination and labor exploitation remain externalized to marginalized populations and gig workers.
% ABSENT_VOICES: Existential risk researchers and structural labor reformers are excluded from the AI safety agenda; affected communities are included in consultations but excluded from standard-setting authority, and their testimony is treated as data inputs rather than governance mandates.
% DISAPPEARANCE_RATIONALE: If the near-term harms framing vanished overnight, industry would lose its primary discursive shield against broader AI regulation; civil society and policymakers would need to choose between existential risk frameworks and direct structural governance of AI deployment; funding and talent would shift away from audit-washing toward either catastrophic risk research or labor and anti-discrimination law enforcement.
% FOUNDING_PROBLEM: Deployed machine learning systems in the 2010s caused repeated, documented harms to marginalized groups and workersâdiscriminatory hiring, biased credit scoring, platform exploitationâwithout any accountability mechanism, creating a demand for governance that industry and regulators could not ignore.
% FOUNDING_PROBLEM_CORROBORATION: Affected communities and labor unions attest that harms persist despite the framing; industry attests the framing is necessary and sufficient; independent academic studies and civil society organizations outside the benefiting parties document that audit regimes have not reduced harm incidence proportionally to investment, suggesting the founding problem is partially addressed but the arrangement persists beyond its functional justification.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because the framing captures governance attention and resources while externalizing harm costs to marginalized communities. Suppression (0.65) reflects active discursive and agenda-control enforcement that marginalizes existential risk and structural reform alternatives. Theater ratio (0.45) captures the growing gap between audit activity and measured harm reduction. Accessibility collapse (0.60) registers that alternative governance framesâstructural regulation, antitrust, x-riskâhave become politically inaccessible under near-term dominance. Resistance (0.55) reflects sustained critique from affected communities and independent researchers. The measurement series share a single time grid (0â8) to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   From the industry seat, the constraint is a rope: it coordinates civil society, regulators, and researchers around tractable standards that reduce real harms. From the affected-community seat, it is a snare: the coordination story is cover for regulatory avoidance, and the suppression of structural alternatives (labor law, antitrust, existential risk governance) is the enforcement mechanism. The engine computes this divergence from the structural dataâthe same arrangement reads as coordination to the agenda-setter and extraction to the payer.
 *
 * DIRECTIONALITY LOGIC:
 *   ai_industry_incumbents are the structural beneficiary (d near the beneficiary end): they collect regulatory forebearance and public legitimacy from the constraint. marginalized_populations and gig_workers are structural targets (d near the target end): they bear the externalized costs of algorithmic harm that the constraint documents but does not eliminate. ai_ethics_researchers sit between beneficiary and symmetric: they gain materially from the audit economy but lack power to alter its terms. existential_risk_advocates are excluded rather than directly targetedâthe constraint suppresses their voice by agenda dominance.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as a rope by requiring both a genuine coordination function (the audit and transparency infrastructure is real and harm-reducing in part) and asymmetric extraction (victims bear costs that beneficiaries do not). Without the victim set, the story would read as scaffold or rope. Without the coordination function, it would read as snare. The tangled_rope classification captures that the audit infrastructure is simultaneously a genuine response to documented harms and a mechanism for extracting regulatory forebearance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the near_term_harms_reading of kernel ai_safety_commitment. How would its classification change if instantiated through the existential_risk_reading or dual_priority_reading instead?',
    'Compare victim sets (present-day marginalized populations vs future humanity), beneficiary sets (tech companies vs x-risk institutions), and extractiveness profiles across the three generated constraint stories in the same family.',
    'An existential_risk_reading would likely shift the victim set toward present-day populations funding speculative research; a dual_priority_reading would show more distributed extraction and benefit patterns, potentially altering the computed directionality for the same agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural delta between sibling readings of the AI safety commitment kernel').

omega_variable(
    industry_capture_vs_genuine_accountability,
    'Does the near-term harms framework represent genuine accountability coordination, or is it primarily a regulatory avoidance mechanism for the tech industry?',
    'Jurisdictional comparison where strong structural AI regulation (antitrust, labor law) coexists with harm-prevention auditing; observe whether industry support for the framework persists when structural regulation is on the table.',
    'If industry support collapses under structural regulation, the constraint is primarily extractive cover; if support persists, the coordination function is more genuine and extraction may be incidental rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_capture_vs_genuine_accountability, empirical, 'Whether the coordination function is separable from industry extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative AI safety framings structural (agenda control, funding capture, institutional exclusion) or internalized (affected communities believing audits are sufficient protection)?',
    'Track whether resistance rises when structural alternatives (labor organizing, x-risk advocacy) gain independent funding and media access outside industry channels.',
    'If suppression is primarily structural, opening alternative funding channels would increase resistance and lower effective suppression; if internalized, resistance would remain low even with opened channels, indicating higher effective extraction than the structural measure suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression of alternative safety framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_s_tr_t2, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(ai_s_tr_t3, ai_safety_commitment__near_term_harms_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__near_term_harms_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__near_term_harms_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__near_term_harms_reading, theater_ratio, 8, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_s_be_t2, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(ai_s_be_t3, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 8, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_s_su_t2, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement(ai_s_su_t3, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 8, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, information_standard).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, dual_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_safety_commitment kernel. The kernel decomposes into three structurally distinct readings: near_term_harms_reading (this file), existential_risk_reading, and dual_priority_reading. Each reading has a distinct victim set, beneficiary set, and extractiveness profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
