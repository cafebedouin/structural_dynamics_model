% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: Dual-Priority AI Safety Commitment (Non-Competing Priorities Reading)
 *   domain: technological/governance
 *
 * SUMMARY:
 *   The kernel 'AI safety commitment' — what the field takes itself to be
 *   committed to when it says it is doing AI safety — has three live
 *   readings. This story instantiates the dual-priority reading: the
 *   commitment that both existential-risk prevention and present-harm
 *   prevention are required, non-competing priorities. The reading was built
 *   to reconcile the field's two camps, and it now structures how safety
 *   funding, convenings, frameworks, and lab safety cases are organized. Its
 *   genuine coordination function is holding a single safety coalition
 *   together across two communities with different time horizons and methods.
 *   Its extraction lives in the non-competing premise itself: under real
 *   budget constraints the priorities do compete, and a frame that forbids
 *   ranking defers every marginal allocation decision — a deferral whose
 *   rents (agenda control, convening authority, unranked portfolio
 *   discretion, comprehensive-coverage legitimacy) accrue to the institutions
 *   that administer the frame, while the union victim set —
 *   present-harm-affected communities and the future-generation constituency
 *   — receives diluted protection on both fronts. The sibling readings
 *   (existential_risk_reading, near_term_harms_reading) are separate
 *   constraints with their own victim sets and extraction structures; see the
 *   network note.
 *
 * KEY AGENTS:
 *   - ai_safety_bridge_institutions: agenda-setter and primary capture seat (institutional/constrained) — administers the frame, convenes the field, drafts the documents declaring both priorities non-competing; collects the rents of deferred allocation
 *   - frontier_lab_governance_teams: beneficiary with secondary agenda-setting power (powerful/arbitrage) — adopts the frame in safety cases and policy submissions, collecting comprehensive-coverage legitimacy without adjudicating priorities
 *   - safety_funding_intermediaries: beneficiary (institutional/mobile) — allocates across both intervention types without publishing a ranking
 *   - present_harm_affected_communities: primary payer and excluded voice (powerless/trapped) — bear documented present harms while resources committed to their protection are diluted across the dual agenda
 *   - future_generation_constituency: primary payer (powerless/trapped) — the diffuse, unorganized constituency of existential-risk prevention, represented only by proxy
 *   - xrisk_research_community: payer with secondary beneficiary position (organized/mobile) — receives umbrella funding and standing but pays the framing tax
 *   - near_term_harms_research_community: payer with secondary beneficiary position (organized/mobile) — symmetric to the existential-risk community
 *   - legislative_policy_bodies: observer (institutional/analytical) — receive the frame as the field's consensus safety position and must translate it into mandates that force the deferred ranking
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.55).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.5).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "Dual-Priority AI Safety Commitment (Non-Competing Priorities Reading)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technological/governance").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '68b270b2-0c66-412e-8d07-76a02b93564e').
narrative_ontology:cs_kernel_codification('68b270b2-0c66-412e-8d07-76a02b93564e', distributed).
narrative_ontology:cs_authority_grounding('68b270b2-0c66-412e-8d07-76a02b93564e', distributed).
narrative_ontology:cs_reading_relation('68b270b2-0c66-412e-8d07-76a02b93564e', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('68b270b2-0c66-412e-8d07-76a02b93564e', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('68b270b2-0c66-412e-8d07-76a02b93564e', foundational, both_priority_classes_constitutive_of_safety).
narrative_ontology:cs_axiom_status(both_priority_classes_constitutive_of_safety, holdable).
narrative_ontology:cs_axiom_grounding('68b270b2-0c66-412e-8d07-76a02b93564e', both_priority_classes_constitutive_of_safety, deontological).
narrative_ontology:cs_axiom('68b270b2-0c66-412e-8d07-76a02b93564e', foundational, priorities_non_competing_under_allocation).
narrative_ontology:cs_axiom_status(priorities_non_competing_under_allocation, holdable).
narrative_ontology:cs_axiom_grounding('68b270b2-0c66-412e-8d07-76a02b93564e', priorities_non_competing_under_allocation, empirically_contingent).
narrative_ontology:cs_reference_frame('68b270b2-0c66-412e-8d07-76a02b93564e', non_competing_dual_agenda).
narrative_ontology:cs_drift_state('68b270b2-0c66-412e-8d07-76a02b93564e', contemporary_resource_scarcity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('68b270b2-0c66-412e-8d07-76a02b93564e', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_bridge_institutions).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, frontier_lab_governance_teams).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, safety_funding_intermediaries).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, present_harm_affected_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, future_generation_constituency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, xrisk_research_community).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, near_term_harms_research_community).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, xrisk_research_community).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harms_research_community).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, non_competition_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the institutes, convenings, and framework-drafting processes through which the field's safety agenda is articulated. They author the documents declaring both existential-risk prevention and present-harm prevention required commitments, and they control which voices enter the agenda-setting conversation. Their funding, staffing, and standing depend on continuing to hold both research communities inside one process; their day-to-day work is maintaining the appearance and reality of joint coverage. Leaving the bridging role would mean dissolving the institution's reason to exist.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_bridge_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Write the safety cases, model specifications, and policy submissions through which frontier labs present their safety posture. Adopting the dual-priority frame lets them present comprehensive coverage — addressing both catastrophe and present harm — without committing to an internal ranking that would expose which risks they are actually resourcing. They co-draft field-level frameworks and fund portions of both research communities. If the frame lost standing, they could reframe their safety posture around whichever priority regulators and publics weight most.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, frontier_lab_governance_teams, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, frontier_lab_governance_teams, agenda_setter).

% Allocate grant portfolios across both intervention types. The frame's non-competing premise lets them fund both without publishing a ranking that would antagonize one community or the other; portfolio reviews describe synergies rather than tradeoffs. They could rank explicitly at any time; doing so would put them in direct conflict with one camp or the other and expose their allocation reasoning to public contest.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, safety_funding_intermediaries, beneficiary,
    institutional, biographical, mobile, global).

% Live with the documented outputs of deployed systems — discriminatory classifications, exploitative labor arrangements, degraded information environments. The safety agenda nominally committed to their protection spreads its attention and funding across a second priority class; the portion of resources that reaches their harms is mediated by institutions that do not include them. They cannot exit the systems that harm them, and they hold no seat in the convenings where the safety agenda is set.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, present_harm_affected_communities, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, present_harm_affected_communities, excluded).

% The people whose exposure to catastrophic outcomes from advanced systems the safety agenda claims to reduce. They cannot organize, speak, or fund representation; their seat is held entirely by proxy advocates whose standing depends on the umbrella that also resources the present-harm portfolio. If the frame ever ranked the priorities, their protection would depend on winning an argument they are structurally unable to join.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, future_generation_constituency, payer,
    powerless, civilizational, trapped, global).

% Researchers working on alignment, oversight, and catastrophic-risk reduction. The umbrella gives them funding, field legitimacy, and protection from the charge of speculation; in exchange they share the agenda with the present-harm portfolio and sometimes reframe their work in present-harm terms to fit joint funding calls. Their skills and methods travel: a funder or institute adopting the pure existential-risk reading could resource them directly.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, xrisk_research_community, payer,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, xrisk_research_community, beneficiary).

% Researchers documenting and mitigating bias, labor exploitation, and misinformation in deployed systems. The umbrella gives them access to safety funding and standing they would lack as a standalone constituency; in exchange they operate inside an agenda whose center of gravity they do not control, and their findings are sometimes subordinated to coalition maintenance. They could organize around the pure near-term-harms reading, at the cost of the umbrella's resources and legitimacy.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harms_research_community, payer,
    organized, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, near_term_harms_research_community, beneficiary).

% Regulators and legislators who receive the dual-priority frame as the field's consensus account of what AI safety requires. They must translate it into mandates, standards, and budgets — which forces exactly the ranking the frame defers. They observe the coalition dynamics from outside them.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, legislative_policy_bodies, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, ai_safety_bridge_institutions).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single AI safety coalition spanning two research communities with different time horizons, methods, and funding bases; gives funders and institutions one legitimate frame under which both intervention types are resourced; prevents the safety agenda from splitting into two camps that deployers and funders could play against each other.
% TRANSFER_FUNCTION: Moves funding, attention, and legitimacy from safety funders and institutional patrons to both research communities under one umbrella; moves agenda-setting authority to the bridge institutions that administer the frame; defers — rather than resolves — the ranking of the two priorities, leaving every marginal allocation decision unmade.
% ABSENT_VOICES: Present-harm-affected communities are largely absent from safety governance tables — the frame is negotiated by labs, institutes, and funders about harms rather than with those harmed. The future-generation constituency is structurally absent, represented only by proxy advocates. Single-priority dissenters are present in the discourse, but their core objection — that the priorities do compete — is treated as coalition-breaking rather than as a position to adjudicate.
% DISAPPEARANCE_RATIONALE: If the dual-priority frame vanished overnight, the safety field would reorganize around the two sibling readings: funders would be forced to rank or explicitly split portfolios, labs would have to choose which safety case to lead with, and the bridge institutions' convening role would dissolve — the coalition the frame holds together would fragment into the two camps it was built to join.
% FOUNDING_PROBLEM: By the late 2010s the AI safety project was splitting: existential-risk work was dismissed by near-term-harm advocates as speculative distraction from documented discrimination, labor displacement, and misinformation; near-term-harm work was dismissed by existential-risk advocates as treating symptoms while the larger risk grew. Funders faced a forced ranking between the two, and deployers could play each camp against the other. The dual-priority frame was built to keep both communities inside one agenda and to unblock funding for both.
% FOUNDING_PROBLEM_CORROBORATION: Both research communities attest the fragmentation was real — the public disputes and funding fights of the era are documented in field histories and contemporaneous debate — and funders attest it from the allocation seat. No party outside the frame's beneficiaries attests that 'non-competing' is the correct resolution of that problem: the field's sociologists and the continued live status of both sibling readings corroborate the founding problem while leaving the frame's resolution contested.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55): the frame extracts attention, legitimacy, and allocation discretion rather than money directly — its cost lands as diluted protection for both victim populations, since resources nominally committed to each are allocated by a mechanism that never ranks. Suppression is authored as a raw structural property (unscaled; only extractiveness is scaled by directionality and scope in the engine) at 0.50: the frame is held up by real enforcement machinery — funding gatekeeping, convening access, framework-drafting control, and coalition discipline against single-priority commitment — but exit is genuinely available (both sibling readings are live, organized positions), which caps suppression well below snare levels. Theater (0.40) is rising: a growing share of dual-banner activity is framework documents, communiqués, and convenings that declare joint coverage while allocating little to either front. Accessibility collapse is low (0.30): the alternatives are not collapsed at all — the pure existential-risk and pure near-term-harms readings are fully available and actively held, which is precisely why the frame needs enforcement. Resistance (0.55) comes from both single-priority camps, each of which experiences the frame as subordinating its priority. The claim (tangled_rope) is authored from the structure — genuine coordination function plus asymmetric extraction plus active enforcement; the metrics are authored independently from the frame's observed operation. The measurement series share one time grid: every tracked metric is authored at every point, so no end-state value is substituted into earlier periods. The rising suppression_requirement series is authored because the story genuinely tracks enforcement-capacity change: the frame's policing infrastructure (portfolio gatekeeping, program control, editorial norms) matured as the single-priority camps grew.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the bridge-institution seat, the frame is coordination it built and maintains: without it the field fragments, funders face a forced ranking, and both communities lose the umbrella. From the two payer seats the same structure operates as the reason their protection is diluted — resources committed to them are allocated by a mechanism designed to avoid ranking them. The research communities sit between: umbrella funding and legitimacy are real benefits; the framing tax and loss of agenda control are real costs. Frontier lab teams, with arbitrage exit, experience the frame as nearly costless — they can reframe around whichever priority the environment rewards. Legislative bodies experience the frame as an unhelpful consensus that forces them to do the ranking it defers. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridge institutions, frontier lab governance teams, and funding intermediaries are declared beneficiaries: they collect agenda control, legitimacy, and unranked portfolio discretion, so their directionality sits near the beneficiary end — the labs nearest of all, since arbitrage exit means the frame costs them little and legitimates them much. The two victim populations sit near the full-target end: both are trapped (one cannot exit the systems harming them; the other cannot organize at all), and both bear the diluted-protection cost of the deferred ranking — the future-generation constituency sits at the extreme target end because its proxy representation is itself funded through the frame it cannot contest. The two research communities derive intermediate directionality from their dual declarations: they appear in the payer position (framing tax, shared agenda) while holding beneficiary secondary roles (umbrella funding and standing) and mobile exit. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the structural relationships directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The dual-priority frame is claimed by its maintainers as pure coordination — the mechanism that keeps the field from fragmenting. The tangled_rope classification preserves what is real in that claim (the coalition function is genuine: both research communities receive funding and standing under the umbrella that neither would reliably command alone) while registering what the claim hides (the non-competing premise defers the ranking decision, and the deferral rents — agenda control, convening authority, unranked portfolio discretion — accrue to the bridge seat while both victim populations remain under-resourced). It blocks the two symmetrical mislabels: reading the frame as pure coordination would erase the deferral extraction; reading it as pure extraction would erase the real coordination and the real resources that reach both communities. On mandatrophy proper: the founding problem — field fragmentation under a forced ranking — is still live, since both sibling readings persist as organized camps, so the frame has not outlived its mandate; but its function has drifted from holding the coalition toward avoiding the ranking, which is why theater and extraction rise together across the interval. Fixing is prohibitive for the seat that could fix it: a transparent allocation rule is trivially available, but adopting it would fracture the coalition whose maintenance is the bridge institutions' reason to exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the ai_safety_commitment kernel (reading: dual_priority_reading). What structural differences would the sibling readings — existential_risk_reading and near_term_harms_reading — produce if adopted in place of this one?',
    'Generate and compare the sibling stories: the existential-risk reading concentrates the victim set on the future-generation constituency and concentrates the intervention portfolio; the near-term-harms reading concentrates the victim set on present-harm-affected communities. The disagreement between readings is located in the scope of the safety commitment and in the allocation structure that follows from it.',
    'Adopting a sibling would collapse the union victim set to a single class and replace the deferred-allocation structure with a ranked one; this reading''s extraction profile (deferral rents to the bridge seat) would not exist under either sibling — each sibling carries its own extraction structure instead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three live readings of the AI-safety-commitment kernel; siblings are separate constraints, not observables of this one.').

omega_variable(
    non_competition_under_scarcity,
    'Does the non-competing premise survive contact with actual allocation decisions under budget constraints, or do the two priority classes compete for every marginal dollar of safety funding and attention?',
    'Track marginal-dollar decisions in safety portfolios: when a funder or lab must add or cut, does the decision treat the two intervention types as substitutes (competing) or complements (non-competing)? Synergy claims are testable against real reallocation events.',
    'If the priorities compete, the reading''s foundational empirical axiom fails and the arrangement operates as an unranked deferral mechanism whose costs fall on both victim populations — extraction rises and the coherence challenge becomes the constraint''s defining feature. If they genuinely complement, much of the measured extraction is contingent on funder behavior rather than inherent to the frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_competition_under_scarcity, empirical, 'Whether the reading''s load-bearing non-competing claim is empirically sustainable under scarcity.').

omega_variable(
    union_coverage_vs_dilution,
    'Does the union victim set receive better protection under the dual-priority frame than it would under either sibling reading — or does dual coverage dilute protection for both populations below what a ranked single-priority portfolio would deliver for one?',
    'Counterfactual allocation analysis: compare actual dual-portfolio outcomes for each population against the outcomes each sibling reading''s concentrated portfolio would plausibly have delivered with the same total resources, using documented intervention effectiveness on both fronts.',
    'If union coverage beats concentration for both populations, the frame''s coordination function is vindicated and its classification moves toward pure coordination; if dual coverage dilutes both below the single-priority counterfactuals, the union victim set is the extraction surface and the frame''s beneficiaries are collecting at both populations'' expense.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(union_coverage_vs_dilution, empirical, 'Whether the reading''s union victim set is served or diluted by dual coverage.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the enforcement that holds the dual-priority frame in place structural (funding gatekeeping, program control, framework-drafting access) or internalized (professional identity fused with the broad-tent safety project, making single-priority commitment feel like field-fracturing)?',
    'Post-exit trajectory of researchers and funders who leave for single-priority positions: if the framing tax and coalition discipline persist in their new settings through habit and identity rather than material gatekeeping, the internalized component is substantial.',
    'If suppression is largely internalized, exit to the sibling readings is nominally open but psychologically costly — effective suppression is higher than the structural measure suggests and the frame persists even where its enforcement machinery weakens. If largely structural, the frame''s persistence tracks the funding and convening infrastructure directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized enforcement of the broad-tent frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ai_s_tr_t0, observed).
narrative_ontology:measurement(ai_s_tr_t3, ai_safety_commitment__dual_priority_reading, theater_ratio, 3, 0.24).
narrative_ontology:measurement_basis(ai_s_tr_t3, observed).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__dual_priority_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(ai_s_tr_t6, observed).
narrative_ontology:measurement(ai_s_tr_t9, ai_safety_commitment__dual_priority_reading, theater_ratio, 9, 0.31).
narrative_ontology:measurement_basis(ai_s_tr_t9, observed).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__dual_priority_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(ai_s_tr_t12, observed).
narrative_ontology:measurement(ai_s_tr_t15, ai_safety_commitment__dual_priority_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(ai_s_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(ai_s_be_t0, observed).
narrative_ontology:measurement(ai_s_be_t3, ai_safety_commitment__dual_priority_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement_basis(ai_s_be_t3, observed).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__dual_priority_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement_basis(ai_s_be_t6, observed).
narrative_ontology:measurement(ai_s_be_t9, ai_safety_commitment__dual_priority_reading, base_extractiveness, 9, 0.49).
narrative_ontology:measurement_basis(ai_s_be_t9, observed).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__dual_priority_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(ai_s_be_t12, observed).
narrative_ontology:measurement(ai_s_be_t15, ai_safety_commitment__dual_priority_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(ai_s_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(ai_s_su_t0, observed).
narrative_ontology:measurement(ai_s_su_t3, ai_safety_commitment__dual_priority_reading, suppression_requirement, 3, 0.36).
narrative_ontology:measurement_basis(ai_s_su_t3, observed).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__dual_priority_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(ai_s_su_t6, observed).
narrative_ontology:measurement(ai_s_su_t9, ai_safety_commitment__dual_priority_reading, suppression_requirement, 9, 0.43).
narrative_ontology:measurement_basis(ai_s_su_t9, observed).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__dual_priority_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement_basis(ai_s_su_t12, observed).
narrative_ontology:measurement(ai_s_su_t15, ai_safety_commitment__dual_priority_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(ai_s_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI safety' covers a contested kernel with three live readings. This story instantiates the dual-priority reading only: its victim set is the union of both populations and its extraction lives in the deferred ranking. The sibling stories — ai_safety_commitment__existential_risk_reading (single victim class: the future-generation constituency; concentrated intervention portfolio) and ai_safety_commitment__near_term_harms_reading (single victim class: present-harm-affected communities) — carry different ε, different beneficiary structures, and their own classifications; they are separate files linked here, not observables of one constraint. The dual reading sits downstream of both siblings historically (it was built to reconcile them) but now exerts structural pressure back on them: its institutional dominance changes the resource environment in which each pure reading must compete.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
