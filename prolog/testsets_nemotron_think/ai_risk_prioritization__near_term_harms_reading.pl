% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Near-Term AI Harms Prioritization Framework
 *   domain: technology_governance/ai_safety
 *
 * SUMMARY:
 *   This constraint story represents the 'near-term harms' reading of the
 *   contested AI risk prioritization kernel. The reading asserts that AI risk
 *   is primarily constituted by measurable harms from currently deployed
 *   systems — algorithmic discrimination against racialized populations,
 *   automation-driven displacement of low-wage workers, and surveillance
 *   expansion — and that justice interventions (bias audits, worker
 *   protections, surveillance regulation) are paramount. The sibling reading
 *   (existential_risk_reading) asserts that misaligned AGI poses
 *   extinction-level threat and alignment research is paramount. This story
 *   authors the near-term reading as a clean ε-invariant constraint: its
 *   extraction refers to the standing arrangement (resource/priority
 *   allocation favoring near-term justice work) assessed by this reading's
 *   lights, not the alternative it would prefer.
 *
 * KEY AGENTS:
 *   - marginalized_communities: Primary beneficiary (powerless/constrained) — bears present harms, gains from justice interventions
 *   - fairness_accountability_researchers: Primary beneficiary (organized/mobile) — receives funding, recognition, policy influence
 *   - existential_risk_researchers: Primary payer (organized/constrained) — loses funding, attention, career capital
 *   - ai_companies_facing_regulation: Dual payer/agenda_setter (powerful/mobile) — bears compliance costs, shapes regulatory agenda
 *   - policymakers: Agenda setter (institutional/analytical) — allocates legislative attention and public funding
 *   - x_risk_advocates: Excluded (organized/constrained) — would object but kept out of dominant governance channels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.65).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.55).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term AI Harms Prioritization Framework").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "technology_governance/ai_safety").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '758b4cff-a7db-47cb-b963-4d4deb272fbe').
narrative_ontology:cs_kernel_codification('758b4cff-a7db-47cb-b963-4d4deb272fbe', distributed).
narrative_ontology:cs_authority_grounding('758b4cff-a7db-47cb-b963-4d4deb272fbe', distributed).
narrative_ontology:cs_reading_relation('758b4cff-a7db-47cb-b963-4d4deb272fbe', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('758b4cff-a7db-47cb-b963-4d4deb272fbe', foundational, present_harm_primacy).
narrative_ontology:cs_axiom_status(present_harm_primacy, holdable).
narrative_ontology:cs_axiom_grounding('758b4cff-a7db-47cb-b963-4d4deb272fbe', present_harm_primacy, empirically_contingent).
narrative_ontology:cs_axiom('758b4cff-a7db-47cb-b963-4d4deb272fbe', foundational, justice_intervention_imperative).
narrative_ontology:cs_axiom_status(justice_intervention_imperative, holdable).
narrative_ontology:cs_axiom_grounding('758b4cff-a7db-47cb-b963-4d4deb272fbe', justice_intervention_imperative, deontological).
narrative_ontology:cs_reference_frame('758b4cff-a7db-47cb-b963-4d4deb272fbe', present_harm_justice_framework).
narrative_ontology:cs_drift_state('758b4cff-a7db-47cb-b963-4d4deb272fbe', contemporary_ai_governance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('758b4cff-a7db-47cb-b963-4d4deb272fbe', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, ai_companies_facing_regulation).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__near_term_harms_reading, present_harm_primacy).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__near_term_harms_reading, justice_intervention_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience algorithmic discrimination in hiring, lending, housing, and policing; face displacement from automation without transition support; live under expanded surveillance. Benefit from bias audits, worker protections, and surveillance regulation that this prioritization directs resources toward. Exit from AI-mediated systems is constrained by infrastructure dependence.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, beneficiary,
    powerless, biographical, constrained, global).

% Receive funding, institutional recognition, and policy influence from the near-term harms prioritization. Their research agenda (bias detection, fairness metrics, accountability frameworks) becomes the dominant paradigm. Can move between academia, industry, and civil society; exit options are relatively open.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    organized, biographical, mobile, global).

% See research funding, talent pipeline, and policy attention diverted to near-term work. Their framing (extinction risk from misaligned AGI) is treated as speculative distraction. Career capital is tied to x-risk paradigm; pivoting to near-term work requires substantial retraining and loses community standing.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, existential_risk_researchers, payer,
    organized, biographical, constrained, global).

% Bear compliance costs for bias audits, transparency mandates, and surveillance restrictions. Simultaneously shape the regulatory agenda through lobbying and standard-setting to favor manageable near-term rules over existential risk governance that might constrain core business models. Can relocate jurisdictions and restructure products.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_companies_facing_regulation, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, ai_companies_facing_regulation, agenda_setter).

% Allocate legislative attention, regulatory capacity, and public funding. The near-term harms reading provides concrete, measurable policy targets (discrimination audits, worker protections) with visible constituents. They adopt this prioritization because it yields legible wins within electoral cycles; x-risk governance offers no comparable political payoff.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, policymakers, agenda_setter,
    institutional, generational, analytical, national).

% Would argue that extinction risk warrants absolute priority regardless of present harms, and that near-term work is negligible if civilization ends. Structurally excluded from the dominant governance conversation because their claims are framed as untestable, distant, and diverting resources from justice. Their exclusion is maintained by the near-term coalition's control of funding and policy channels.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, x_risk_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating policy, research funding, and regulatory attention on measurable present harms from deployed AI systems — discrimination in automated decisions, labor displacement without transition, and surveillance expansion — so that justice interventions address harm that is occurring now.
% TRANSFER_FUNCTION: Moves research funding, regulatory attention, legislative priority, and talent pipeline from existential risk research (alignment, interpretability, governance of hypothetical AGI) to near-term justice interventions (bias audits, worker protections, surveillance regulation, fairness accountability).
% ABSENT_VOICES: Long-termist/x-risk researchers who argue extinction risk warrants priority; future generations who cannot advocate for themselves but bear the consequences of prioritization choices; AI safety researchers working on alignment who are not part of the fairness/accountability community.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished overnight, funding and policy attention would shift back toward existential risk research; near-term justice interventions (bias audits, worker protections, surveillance regulation) would lose institutional support and regulatory momentum; the AI governance agenda would reorganize around long-term alignment and hypothetical AGI scenarios.
% FOUNDING_PROBLEM: Early AI governance discourse was dominated by speculative existential scenarios (misaligned AGI, extinction risk) while deployed systems were already causing measurable harm to marginalized populations — algorithmic discrimination in hiring/lending/policing, automation-driven displacement without safety nets, and surveillance expansion.
% FOUNDING_PROBLEM_CORROBORATION: Documented by algorithmic fairness literature (Buolamwini & Gebru on facial recognition bias; Noble on algorithmic oppression; Eubanks on automated inequality), labor displacement studies (Acemoglu, Autor), and surveillance technology critiques (Zuboff, Lyon) — all from researchers and communities outside the x-risk beneficiary set.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) reflects the reading's structural diversion of resources from x-risk work to near-term justice work — a genuine coordination function (addressing present harms) coupled with asymmetric extraction (x-risk researchers lose funding/career capital). Suppression (0.55) is rhetorical and institutional: the 'speculative distraction' framing delegitimizes x-risk claims in funding and policy venues, but does not legally forbid the work. Theater ratio (0.3) captures that some justice interventions are performative (bias audits that change nothing, ethics boards without power) while core coordination (worker protections, surveillance limits) is functional. Accessibility collapse (0.6) reflects that the x-risk alternative is framed as illegitimate within this reading's governance frame. Resistance (0.55) from x-risk community is real but institutionally disadvantaged.
 *
 * PERSPECTIVAL GAP:
 *   From the near-term justice seat, the constraint is a rope (genuine coordination on present harms). From the x-risk researcher seat, it is a snare (extraction via delegitimization). From the AI company seat, it is a tangled rope (manageable compliance cost that deflects existential governance). The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the authoring seat's structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and fairness researchers are structural beneficiaries (d near 0.0) — the constraint subsidizes their interests. X-risk researchers are structural targets (d near 1.0) — the constraint extracts their funding and legitimacy. AI companies are near-symmetric (d ~0.5) — they pay compliance costs but gain regulatory capture of the near-term agenda. Policymakers are agenda-setters with analytical exit (d ~0.3) — they benefit from legible wins. X-risk advocates are excluded (identity_locked exit) — their exclusion is the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (speculative x-risk discourse dominating while present harms were ignored) is live — present harms persist and grow. However, the arrangement shows mandatrophy signs: the 'speculative distraction' framing has become a categorical suppression tool rather than a proportional prioritization argument; theater is rising as bias audits become checkbox exercises; the coordination function (justice interventions) is being displaced by the extraction function (defunding x-risk work). The constraint persists because the near-term coalition controls funding/policy channels, not because the founding problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the ai_risk_prioritization kernel a single commitment with multiple readings, or are these genuinely distinct constraints with different referents?',
    'Trace whether both readings cite the same foundational texts/institutions (e.g., early AI safety workshops, FLI open letter, EU AI Act debates) as authoritative for their opposing claims. If yes, single kernel; if they cite disjoint authorities, distinct constraints.',
    'If single kernel, the readings are structural rivals for the same commitment — classification divergence measures the kernel''s internal contestation. If distinct constraints, each stands alone and the ''kernel'' label is a category error.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the near-term and existential risk readings share a kernel or are separate constraints.').

omega_variable(
    suppression_mechanism,
    'Is the suppression of x-risk work structural (funding gates, policy exclusion) or rhetorical (framing as ''distraction'')?',
    'Track funding flows: if x-risk grants are denied explicitly because reviewers cite ''near-term priority'' language, suppression is structural. If x-risk work continues funded but is rhetorically marginalized in discourse, suppression is rhetorical.',
    'Structural suppression raises the constraint toward snare; rhetorical suppression keeps it in tangled_rope territory. The measurement series shows suppression_requirement rising — if this reflects hardening structural gates, the trajectory is toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism, empirical, 'Whether x-risk marginalization operates through material gates or discourse framing.').

omega_variable(
    resource_zero_sum,
    'Is the research funding/talent pool actually zero-sum between near-term and x-risk work, or does the near-term prioritization expand the total pie?',
    'Compare total AI safety/governance funding before and after near-term prioritization became dominant. If total grew while x-risk share shrank, not zero-sum. If total flat and x-risk absolute funding fell, zero-sum extraction.',
    'If not zero-sum, the extraction claim weakens — the constraint may be rope (coordination expanding the field) rather than tangled_rope. If zero-sum, extraction is real and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_zero_sum, empirical, 'Whether the resource tradeoff between near-term and x-risk work is genuinely zero-sum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2, 0.4).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__near_term_harms_reading, 0.15).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% This constraint and existential_risk_reading form the ai_risk_prioritization constraint family. They share the kernel (AI risk prioritization) but instantiate different ε values: this reading ε=0.65 (substantial extraction from x-risk work), the sibling likely ε≈0.3 (coordination on alignment with minimal extraction from near-term work). The upstream influence flows from this reading to the sibling: the near-term coalition's control of funding/policy channels structurally pressures the x-risk reading's operating environment (influences relation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__near_term_harms_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
