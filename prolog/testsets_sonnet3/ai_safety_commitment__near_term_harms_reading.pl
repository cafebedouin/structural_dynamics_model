% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: AI Safety as Near-Term Documented Harms Prevention
 *   domain: technology governance / labor / civil rights
 *
 * SUMMARY:
 *   This story instantiates the near-term-harms reading of the contested 'AI
 *   safety' kernel: the claim that AI safety work should center documented,
 *   present-day, auditable harms — algorithmic bias, discrimination in hiring
 *   and lending, exploitative data-labeling and content-moderation labor, and
 *   misinformation propagation — rather than speculative future catastrophic
 *   risk. Under this reading, the standing arrangement is the current
 *   institutional AI safety apparatus (corporate trust-and-safety teams,
 *   academic fairness research, voluntary audit frameworks) as it actually
 *   operates: producing substantial public communication and compliance
 *   documentation while the underlying labor exploitation and discriminatory
 *   deployment patterns persist largely unremediated. The coordination
 *   function is real (naming, measuring, and in some cases reducing concrete
 *   harms to identifiable populations); the extraction is that this framing
 *   is disproportionately funded and amplified by developers precisely
 *   because it is easier to perform than the structural change (unionization,
 *   binding regulation, deployment moratoria) that would actually address
 *   root causes, and because emphasizing it can crowd out or substitute for
 *   binding legal accountability.
 *
 * KEY AGENTS:
 *   - frontier_ai_developers: primary beneficiary (institutional/arbitrage) — fund and amplify near-term-harms framing, sit on advisory boards, control what counts as 'addressed'
 *   - gig_content_moderators: primary target (powerless/trapped) — perform the labor that produces the training data and moderation decisions the harms framework studies, bear psychological and economic cost directly
 *   - algorithmically_screened_job_applicants: primary target (powerless/constrained) — bear discriminatory outcomes from deployed systems the framework claims to be addressing
 *   - communities_facing_algorithmic_discrimination: primary target (organized/constrained) — bear disparate-impact outcomes in lending, policing, and housing algorithms
 *   - ai_safety_research_institutions: secondary beneficiary (institutional/mobile) — receive funding, publish, build careers on the near-term-harms research agenda
 *   - existential_risk_researchers: analytical observer from a sibling reading — see the same 'AI safety' label applied to a structurally distinct claim about catastrophic misalignment risk
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
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety as Near-Term Documented Harms Prevention").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technology governance / labor / civil rights").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '20949812-43c3-4b89-9fb4-35b03003a0cb').
narrative_ontology:cs_kernel_codification('20949812-43c3-4b89-9fb4-35b03003a0cb', distributed).
narrative_ontology:cs_authority_grounding('20949812-43c3-4b89-9fb4-35b03003a0cb', distributed).
narrative_ontology:cs_reading_relation('20949812-43c3-4b89-9fb4-35b03003a0cb', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('20949812-43c3-4b89-9fb4-35b03003a0cb', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('20949812-43c3-4b89-9fb4-35b03003a0cb', foundational, documented_harm_has_priority_claim).
narrative_ontology:cs_axiom_status(documented_harm_has_priority_claim, holdable).
narrative_ontology:cs_axiom_grounding('20949812-43c3-4b89-9fb4-35b03003a0cb', documented_harm_has_priority_claim, empirically_contingent).
narrative_ontology:cs_axiom('20949812-43c3-4b89-9fb4-35b03003a0cb', secondary, speculative_risk_cannot_justify_deferring_present_remediation).
narrative_ontology:cs_axiom_status(speculative_risk_cannot_justify_deferring_present_remediation, holdable).
narrative_ontology:cs_axiom_grounding('20949812-43c3-4b89-9fb4-35b03003a0cb', speculative_risk_cannot_justify_deferring_present_remediation, instrumental).
narrative_ontology:cs_reference_frame('20949812-43c3-4b89-9fb4-35b03003a0cb', documented_harm_accountability_standard).
narrative_ontology:cs_drift_state('20949812-43c3-4b89-9fb4-35b03003a0cb', post_generative_ai_commercialization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('20949812-43c3-4b89-9fb4-35b03003a0cb', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, frontier_ai_developers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_safety_research_institutions).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_content_moderators).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, algorithmically_screened_job_applicants).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, data_labeling_workers).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, algorithmic_harm_is_measurable_now).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund and publicize bias audits, trust-and-safety teams, and voluntary harm-reduction commitments; control which audits are commissioned, which findings are disclosed, and what counts as 'addressed.' Can point to this activity when resisting binding external regulation. Face essentially no exit cost from the framework as currently structured — it largely operates on their terms.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, frontier_ai_developers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, frontier_ai_developers, agenda_setter).

% Receive funding, publish research, and build careers studying documented AI harms. Genuinely produce some real remediation (algorithmic audits that changed deployment decisions) but their institutional survival depends on the continued salience of the near-term-harms framing, giving them a stake in its persistence distinct from the underlying harm itself.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_safety_research_institutions, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, ai_safety_research_institutions, observer).

% Perform the psychologically taxing labor of reviewing violent, abusive, or misinformation content that trains and maintains deployed AI systems, often under NDA and via contractor arrangements that exclude standard labor protections. Their working conditions are the subject of studies and reports but they rarely have a binding voice in remediation and often cannot pursue litigation due to arbitration clauses.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_content_moderators, payer,
    powerless, immediate, trapped, global).

% Perform low-wage, often outsourced labeling work that produces the datasets underlying deployed systems; frequently geographically concentrated in lower-income countries with limited local labor recourse. Named as a population the near-term-harms framework studies, with little structural power to set the terms of remediation.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, data_labeling_workers, payer,
    powerless, immediate, trapped, global).

% Are screened, ranked, or rejected by deployed hiring algorithms whose discriminatory patterns are documented in the near-term-harms research this framework produces. Can decline to apply to specific employers but face limited disclosure of how they were scored and limited legal recourse absent binding anti-discrimination enforcement.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, algorithmically_screened_job_applicants, payer,
    powerless, biographical, constrained, national).

% Bear disparate-impact outcomes from algorithmic systems in lending, housing, and policing. Have organized advocacy and litigation capacity greater than individual workers, and have in some cases won specific remediations, but face a well-resourced counter-apparatus that can absorb and slow-walk complaints through voluntary audit processes rather than binding rules.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination, payer,
    organized, generational, constrained, national).

% Argue that centering 'AI safety' resources and public attention on documented present-day harms diverts institutional bandwidth from what they consider the far larger stakes of catastrophic misalignment risk. Not part of this constraint's own coordination structure — their objection is registered elsewhere (the sibling existential_risk_reading), not adjudicated within this reading's arrangement.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_advocates, excluded,
    organized, civilizational, analytical, global).

% Weigh whether voluntary near-term-harms commitments by developers substitute adequately for binding regulation, or whether they function as a delay mechanism. Can compel disclosure and impose remedies, but currently often defer to the industry's self-reported harm-reduction activity.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, policy_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, frontier_ai_developers).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary and measurement apparatus for identifying and, in some cases, remediating concrete algorithmic harms to specific populations — bias audits, labor condition studies, misinformation tracking — where none existed before deployment scaled.
% TRANSFER_FUNCTION: Moves research funding, public attention, and regulatory goodwill toward developers who can point to near-term-harms compliance activity, while the underlying costs of discriminatory deployment and exploitative data/content labor continue to be borne by the populations the activity studies.
% ABSENT_VOICES: Gig content moderators and data labeling workers are the subjects of extensive research but rarely sit on the advisory boards or standard-setting bodies that define what 'addressing' their harm means; existential risk advocates are excluded from this reading's own coordination structure because their concern is definitionally routed to a sibling reading of the same kernel.
% DISAPPEARANCE_RATIONALE: Developers and safety research institutions would say the world rearranges badly — audits stop, harm documentation ends, whatever remediation exists disappears. Worker advocates and community organizers would say the underlying labor and discrimination structures were never actually addressed by this apparatus and its disappearance would mainly remove a compliance shield, pushing the same actors toward whatever binding regulation might otherwise have been avoided. The verdict genuinely depends on which seat is asked.
% FOUNDING_PROBLEM: Deployed AI systems were producing measurable, documented harm — discriminatory hiring and lending outcomes, exploitative content-moderation and data-labeling labor, and misinformation amplification — with no established institutional mechanism to name, measure, or remediate it.
% FOUNDING_PROBLEM_CORROBORATION: Independent journalism (labor investigations into content moderation contractors), academic fairness researchers publishing outside corporate funding structures, and worker organizing campaigns (moderator unionization efforts, data-labeler collective actions) all attest the underlying harms remain substantially unremediated; this corroboration comes from outside the primary beneficiary set (frontier developers and the research institutions they fund).
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, contested).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.68 — substantial but not maximal — because real, measurable harm reduction does occur (bias audits have changed some deployment decisions; labor organizing among moderators has produced some contract improvements) even as the framework as a whole is disproportionately used by developers to demonstrate 'safety' without binding structural change. Suppression at 0.58 reflects the real but partial coercive apparatus: NDAs binding content moderators, arbitration clauses blocking gig worker litigation, and the difficulty of contesting algorithmic decisions that are not disclosed. Theater ratio rising from 0.25 to 0.52 across the interval reflects a Goodhart-style drift: as the near-term-harms framing became the dominant public 'AI safety' vocabulary (roughly 2021-2024), the ratio of published harm-reduction commitments to measured structural change in labor conditions and discriminatory outcomes widened. Accessibility collapse is moderate (0.4) — alternatives (binding regulation, worker organizing, litigation) remain visible and are actively pursued by some actors, unlike a mountain where alternatives have essentially vanished. Resistance is real (0.62): worker organizing, community advocacy litigation, and journalist investigation actively contest the framework's adequacy.
 *
 * PERSPECTIVAL GAP:
 *   From the frontier developer seat, this reading of AI safety looks like responsible, well-resourced coordination: audits are commissioned, bias bounties are paid, trust-and-safety teams are staffed. From the gig content moderator or discriminated-community seat, the same structure looks like a well-funded apparatus that studies their harm in detail without giving them either a seat at the table that defines remediation or binding legal recourse when remediation fails. The engine should compute these as different per-seat classifications from the same structural data — the developer's institutional power, arbitrage-grade exit, and beneficiary role produce a different effective extraction reading than the moderator's powerless, trapped, target position.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier AI developers sit near the beneficiary end: they fund, publicize, and substantially control what 'addressing' near-term harms means, and the framework's existence lets them point to compliance activity in lieu of binding external regulation — low derived d. Gig content moderators and data labelers sit at the target end: they are structurally trapped (economically dependent, often under NDA, frequently classified as contractors without standard labor protections) and bear the framework's studied-but-underremedied costs directly — high derived d. Algorithmically screened applicants and discriminated communities are also targets, though with somewhat more exit (they can decline specific platforms, pursue litigation) — moderate-high d. AI safety research institutions are a secondary beneficiary: their funding and career structures depend on the near-term-harms framing's continued institutional salience, giving them some coordination interest but less direct extraction-capture than the developers themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that deployed AI systems were producing measurable, present-day harm to identifiable populations with no established accountability mechanism — remains substantially live; discriminatory hiring algorithms, exploitative content-moderation labor conditions, and misinformation-driven harms are all still documented and ongoing. This is NOT a case of an obsolete mandate persisting by inertia (that would point toward piton). The mandatrophy risk here is narrower and different in kind: it is that a genuinely live coordination problem has been substantially captured by the very developers whose products cause the harm, who use visible engagement with the near-term-harms framing as a substitute for binding external accountability. The tangled_rope classification is deliberately chosen over snare because the coordination leg is real (some workers, some communities, some researchers genuinely benefit from the harm documentation and occasional remediation this framework produces) — it is not pure extraction dressed as coordination. But it is not a clean rope either, because the beneficiary/victim asymmetry and the active enforcement of who gets to define 'addressed' (via NDAs, arbitration clauses, and gatekept audit access) is structural, not incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Is ''AI safety'' properly a claim about present, documented, auditable harms, or about speculative future catastrophic risk from misaligned systems — and does resourcing one starve the other?',
    'Track budget and staffing allocation across major AI labs'' safety/trust teams over a multi-year window: if near-term harms teams are defunded whenever existential-risk framing gains institutional traction (or vice versa), the readings are in real resource competition, not merely rhetorical tension.',
    'If resourcing is zero-sum, this reading''s claim that near-term harms deserve primary institutional attention is a genuine rival to the existential_risk_reading, not a complementary emphasis; if resourcing is additive, the dual_priority_reading''s non-competing framing is empirically supported and this reading''s implicit rivalry claim is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether near-term-harms and existential-risk framings compete for finite institutional resources or are independently fundable.').

omega_variable(
    regulatory_deflection_intent,
    'Do frontier AI developers who publicly emphasize near-term harms frameworks (bias audits, content moderation labor standards) do so because they believe those are the tractable and important problems, or because near-term-harms framing is easier to satisfy performatively than existential-risk framing would be, and thereby forestalls binding regulation?',
    'Compare internal audit remediation rates and labor condition changes against public disclosure/PR volume on the same topics; a large gap between disclosure activity and measured harm reduction supports the deflection hypothesis.',
    'If deflection, the beneficiary structure (tech companies avoiding regulation) is confirmed and theater_ratio should be read as substantive rather than incidental; if genuine prioritization, the tangled_rope classification may overstate extraction relative to a rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_deflection_intent, empirical, 'Whether near-term-harms framing functions partly as regulatory deflection by incumbent developers.').

omega_variable(
    victim_voice_capture,
    'Are the gig workers, content moderators, and algorithmically screened communities named as this reading''s core constituency actually consulted in setting the near-term-harms agenda, or is the agenda set by researchers and advocacy organizations who speak of them without structural input from them?',
    'Audit governance boards, advisory panels, and standard-setting bodies for near-term AI harms work for direct representation (not just testimony) from affected worker and community groups.',
    'If representation is absent, the coordination-function claim (that this reading coordinates protection FOR these groups) weakens relative to an extraction-with-advocacy-cover reading; if present, the tangled_rope''s coordination leg is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_voice_capture, empirical, 'Whether the populations this reading centers actually set its priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__near_term_harms_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__near_term_harms_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(ai_s_tr_t15, ai_safety_commitment__near_term_harms_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__near_term_harms_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(ai_s_tr_t25, ai_safety_commitment__near_term_harms_reading, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ai_s_be_t15, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(ai_s_be_t25, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(ai_s_su_t15, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(ai_s_su_t25, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__near_term_harms_reading, 0.1).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the ai_safety_commitment kernel. near_term_harms_reading (this file) authors high epsilon for the standing near-term-harms apparatus, with tech developers as primary beneficiary and gig/labeling workers plus discriminated communities as primary victims. existential_risk_reading authors a structurally distinct claim about catastrophic misalignment risk with a different beneficiary/victim structure (alignment researchers as beneficiary; diffuse future populations as notional victim, largely unmeasurable in present ε terms). dual_priority_reading claims the two are non-competing and should be evaluated for whether resourcing is genuinely additive or zero-sum (see omega kernel_reading_disagreement_locus in this file). Each reading carries its own independent epsilon; none is derived from or averaged with the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
