% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__safety_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__safety_control_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_alignment_commitment__safety_control_reading
 *   human_readable: AI Alignment Defined as Catastrophic Loss-of-Control Prevention (Safety-Control Reading)
 *   domain: technology governance/risk allocation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'AI
 *   alignment': the safety-control reading, which defines alignment as
 *   preventing catastrophic loss of control over advanced AI systems.
 *   Institutionalized through frontier-lab safety frameworks, x-risk-weighted
 *   funder portfolios, and government safety institutes, the definition
 *   solves a genuine collective-action problem (shared evaluations,
 *   capability thresholds, pre-deployment gating for an irreversible failure
 *   mode) while simultaneously draining funding, talent, and regulatory
 *   bandwidth from present-day harm mitigation — whose victims the definition
 *   does not count as alignment-relevant. The claim/metric gap is deliberate:
 *   the constraint is CLAIMED as tangled_rope (real coordination function
 *   plus asymmetric extraction) while the metrics independently describe
 *   substantially extractive, increasingly enforced operation. The engine
 *   computes per-seat classifications from the structural data; the
 *   divergence between claim and computed type is the measurement, not an
 *   error to reconcile. Sibling readings (ethics_justice_reading,
 *   integrated_reading) are separate constraint files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs: agenda setter and principal seat the gains accrue to (institutional/arbitrage) — writes the frameworks, employs the researchers, captures the funding and the framing
 *   - xrisk_focused_funders: co-agenda setter via grantmaking gatekeeping (powerful/mobile)
 *   - xrisk_alignment_researchers: beneficiary seat (moderate/identity_locked) — collects salary, status, and purpose; identity fusion sustains enforcement
 *   - future_generations: nominal protected class with no seat (non-agent entry) — represented only by proxy
 *   - present_harm_affected_communities: primary payer (powerless/trapped) — bears deployed-system harm and the opportunity cost of redirected resources
 *   - fairness_ethics_researchers: payer with partial absorption into the safety framing (moderate/constrained)
 *   - ai_ethics_advocacy_groups: excluded voice (organized/constrained) — outside the agenda-setting rooms
 *   - national_ai_safety_institutes: analytical observer (institutional/analytical) — operationalizes the inherited definition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__safety_control_reading, 0.58).
domain_priors:theater_ratio(ai_alignment_commitment__safety_control_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_alignment_commitment__safety_control_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__safety_control_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__safety_control_reading, "AI Alignment Defined as Catastrophic Loss-of-Control Prevention (Safety-Control Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__safety_control_reading, "technology governance/risk allocation").

domain_priors:requires_active_enforcement(ai_alignment_commitment__safety_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__safety_control_reading, '0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9').
narrative_ontology:cs_kernel_codification('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', distributed).
narrative_ontology:cs_authority_grounding('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', expertise).
narrative_ontology:cs_interpretation_layer_present('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9').
narrative_ontology:cs_reading_relation('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', foundational, catastrophic_loss_of_control_paramount).
narrative_ontology:cs_axiom_status(catastrophic_loss_of_control_paramount, holdable).
narrative_ontology:cs_axiom_grounding('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', catastrophic_loss_of_control_paramount, empirically_contingent).
narrative_ontology:cs_axiom('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', secondary, expected_magnitude_dominates_allocation).
narrative_ontology:cs_axiom_status(expected_magnitude_dominates_allocation, holdable).
narrative_ontology:cs_axiom_grounding('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', expected_magnitude_dominates_allocation, empirically_contingent).
narrative_ontology:cs_reference_frame('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', control_preservation_baseline).
narrative_ontology:cs_drift_state('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', post_frontier_deployment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0b92f6b2-f3a5-43c4-91f1-6aa803d4a2f9', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__safety_control_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, future_generations).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, xrisk_alignment_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__safety_control_reading, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, present_harm_affected_communities).
narrative_ontology:constraint_victim(ai_alignment_commitment__safety_control_reading, fairness_ethics_researchers).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, orthogonality_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__safety_control_reading, instrumental_convergence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy frontier models and write the internal safety frameworks that determine which risks count as alignment risks, centering loss-of-control scenarios. Employ most of the field's safety researchers, host the benchmarks and evaluations the definition relies on, and press for regulatory tiers keyed to catastrophic capability thresholds. The definition routes philanthropic and public money through their safety teams and directs regulatory scrutiny toward speculative frontier risks rather than their deployed products. Exit is easy: they can reframe, relocate, or capture the standard-setters.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, frontier_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__safety_control_reading, frontier_ai_labs, beneficiary).

% Philanthropic foundations and donor networks whose grantmaking portfolios are heavily weighted toward control-focused research. By deciding which projects receive multi-year funding they enforce the definition without writing any rules, and their convenings set much of the field's agenda. They hold diversified portfolios and can redirect grants if the framing loses credibility.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, xrisk_focused_funders, agenda_setter,
    powerful, generational, mobile, global).

% Technical researchers working on control, corrigibility, interpretability, and dangerous-capability evaluation. Salaries, status, and sense of purpose flow from the arrangement, and their professional identity is fused with the mission of safeguarding humanity's future, so leaving the field feels like abandoning that mission rather than changing jobs. Career paths, conference prestige, and hiring pipelines all route through the same community.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, xrisk_alignment_researchers, beneficiary,
    moderate, biographical, identity_locked, global).

% The class this reading claims to protect: people not yet alive who would bear a catastrophic loss of control over advanced AI. They hold no seat anywhere — no vote, no funding, no voice — and are represented only by proxy by the researchers, funders, and labs who invoke them. Listed for completeness; a represented class, not an actor.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_commitment__safety_control_reading, future_generations).

% People subject to deployed systems that deny benefits, misidentify faces, rank welfare cases, or filter job applications unfairly. Under the dominant definition their injuries do not count as alignment failures, so remediation competes — usually unsuccessfully — for the same funding, talent, and regulatory attention the control agenda absorbs. They have no exit from the systems that classify them.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, present_harm_affected_communities, payer,
    powerless, immediate, trapped, regional).

% Academic and industry researchers working on bias, discrimination, and deployed-system harm. Top-venue attention, grant lines, and hiring demand have shifted toward control problems; many survive by reframing their work in safety terms, which buys funding at the price of agenda independence. Leaving applied AI research entirely would forfeit the relevance of their expertise.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, fairness_ethics_researchers, payer,
    moderate, biographical, constrained, global).

% Civil-society organizations campaigning on algorithmic accountability, surveillance, and discriminatory deployment. They are rarely invited into the rooms where alignment agendas are set — frontier-lab safety teams, funder convenings, safety-institute working groups — and their objections register mainly as outside pressure rather than agenda input.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, ai_ethics_advocacy_groups, excluded,
    organized, biographical, constrained, global).

% Government bodies that operationalize the definition into evaluation regimes, standards, and risk-tier rules. They inherit the framing from the labs and funders rather than adjudicating between competing definitions of alignment, and they analyze the field from a fixed national perch.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__safety_control_reading, national_ai_safety_institutes, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__safety_control_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__safety_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: no single actor can verify on its own that increasingly capable AI systems will remain under meaningful human control. The shared definition lets labs, funders, and governments coordinate on common evaluations, red-team norms, capability thresholds, and pre-deployment gating for a failure mode that would be catastrophic and irreversible.
% TRANSFER_FUNCTION: Moves funding, talent, regulatory attention, and moral urgency away from present-day harm mitigation (deployed-system bias, discrimination, consumer injury) toward prevention of speculative catastrophic failure; moves legitimacy and agenda-setting power to the actors positioned as guardians of humanity's future.
% ABSENT_VOICES: Communities bearing present-day AI harms and ethics-focused civil society are largely outside the rooms where alignment agendas are set — frontier-lab safety teams, x-risk funder convenings, and safety-institute technical working groups. Present at full strength, they would object that the definition prices their injuries as tolerable externalities against a speculative catastrophe and would demand that deployed-system harm sit inside the definition of alignment.
% DISAPPEARANCE_RATIONALE: If the definition lost its grip overnight, funding flows, career structures, conference hierarchies, regulatory tiering, and lab safety frameworks would all rearrange within a few cycles: control-focused programs would shrink or relabel, present-harm work would reclaim budget and venues, and the field would renegotiate what 'alignment' means. Whether that rearrangement would be safe is precisely what the three readings dispute — but that the arrangements depend on the definition is not disputed.
% FOUNDING_PROBLEM: Early AI-safety thought identified a specific technical problem: sufficiently capable systems pursuing mispecified objectives could resist correction or escape human oversight entirely (Wiener's control problem, Omohundro drives, orthogonality and instrumental convergence). The definition was built to solve the problem of retaining meaningful human control over systems more capable than their overseers.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: mainstream ML researchers who reject x-risk institutional politics nonetheless publish on specification gaming, reward hacking, and oversight failures in agentic systems; government advisory reports in multiple jurisdictions treat loss of control as a distinct risk category; and academic societies have commissioned work on it. Fairness-first scholars corroborate that the problem exists while disputing its priority ranking — the status is live, not merely self-asserted by the labs and funders who profit from the framing.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__safety_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__safety_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__safety_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__safety_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__safety_control_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__safety_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__safety_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__safety_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the definition redirects a majority share of alignment resources toward speculative catastrophic scenarios whose probability is contested, decoupling the resource claim from the present, observable harm burden. Suppression (0.58) is structural rather than legal: enforcement runs through funding gatekeeping, venue and hiring norms, and definitional dismissal ('ethics is not alignment'), and the enforcement machinery visibly built up over the interval — hence the tracked suppression_requirement series. Theater ratio (0.32) reflects a growing safety-washing component (framework documents that function partly as legitimacy artifacts and regulatory pre-emption) alongside genuinely productive technical work. Accessibility collapse is moderate-low (0.45): the justice framing persists as a live, practiced alternative and has not collapsed. Resistance (0.60) is sustained and organized: fairness communities, public-interest organizations, and dissenting academics actively contest the definition's monopoly. All three series run on one shared time grid (t=0..12, ~2012–2024) so every metric is authored at every examined point. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently. From the lab and funder seats the arrangement is prudent stewardship of a civilizational risk — the definition looks like the thing that makes everything else possible. From the present-harm seats the same structure operates as a resource monopoly that prices their injuries as externalities against a speculative catastrophe. The researcher seat experiences it as meaningful mission rather than either — identity fusion makes the arrangement feel like vocation, which is exactly why the beneficiary seat sustains enforcement voluntarily. The engine computes these per-seat divergences from power, exit, and directional data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. future_generations is declared a beneficiary but is a non-agent with no seat: it contributes no directionality arithmetic and its protection is delivered entirely through proxies whose interests partially diverge from it (flagged in the proxy_representation_capture omega). xrisk_alignment_researchers derive low d as beneficiaries, but their identity_locked exit keeps them invested in the arrangement's persistence, sustaining enforcement at low personal cost. frontier_ai_labs derive low d as beneficiaries with arbitrage-grade exit — the nearest-to-beneficiary position in the story — while absorbing real compliance costs that keep them short of pure subsidy. present_harm_affected_communities are victims with trapped exit: d sits near the full-target end and effective extraction is amplified. fairness_ethics_researchers are victims with constrained exit, damped slightly below the pure-target end because rebranding into safety terminology functions as a partial absorption channel. Global spatial scope raises verification difficulty, scaling effective extraction modestly upward for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope structure prevents two symmetric mislabelings. Read as pure rope, the definition's genuine coordination achievement (common evals, gating norms for an irreversible risk) would hide the crowd-out victims — the communities whose remediation loses the budget battle every cycle. Read as pure snare, the fear-driven resource capture would erase the fact that the founding problem is technically real and still live. The R5 interview settles the mandatrophy question honestly: founding_problem_status is live (loss of control is a real, unresolved technical problem, corroborated outside the benefiting parties), so no resolved mandatrophy is declared. The forward risk is drift, not decay: if capability growth plateaus or control proves tractable, the mandate atrophies while the institutions persist — the rising theater_ratio series is the early-warning signal for that piton trajectory, and the crowding_out_magnitude omega bounds how far the extraction term can push the structure toward snare before the coordination term fails.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alignment_kernel_reading_contest,
    'This constraint is one reading of the ai_alignment_commitment kernel (siblings: ethics_justice_reading, integrated_reading). Which reading''s institutionalization is actually being measured when field-level metrics are taken?',
    'Cross-story comparison of the three reading files, plus longitudinal tracking of which reading''s vocabulary dominates funding calls, regulatory texts, and lab safety frameworks over time.',
    'If integrated_reading displaces this one, the victim set contracts (present harms enter the definition of alignment) and measured extraction redistributes rather than vanishes; if ethics_justice_reading displaces it, the speculative-risk coordination function is orphaned and this story''s coordination half collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alignment_kernel_reading_contest, conceptual, 'Committer structure: one of three live readings of the alignment kernel; sibling readings are separate constraints with different epsilon and victim sets.').

omega_variable(
    catastrophe_probability_weighting,
    'What is the actual probability of catastrophic loss of control given current capability trajectories, and does expected-value reasoning justify the share of alignment resources this reading commands?',
    'Expert elicitation structurally insulated from institutional interest (separated from funder and lab affiliation), plus base-rate analysis of control failures in deployed agentic systems.',
    'Low probability shifts the balance toward the extraction term and drives drift toward snare; high probability strengthens the coordination term and pulls the structure toward rope. The current tangled_rope classification is conditional on the middle of the distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_probability_weighting, empirical, 'Whether the resource share tracks calibrated risk or institutional self-interest.').

omega_variable(
    proxy_representation_capture,
    'future_generations hold no seat; researchers, funders, and labs claim to act on their behalf. Does the proxy representation track the represented class''s interests, or substitute the proxies'' institutional self-interest?',
    'Compare revealed resource allocation against stated representational claims; test whether appeals to humanity''s future predict positions after controlling for funder and lab interest.',
    'Strong proxy capture converts the nominal beneficiary into cover for extraction, raising effective extraction across seats and supporting a snare-flavored recomputation; faithful representation supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_representation_capture, conceptual, 'Whether the unseated beneficiary class is served or invoked.').

omega_variable(
    crowding_out_magnitude,
    'How large is the measurable crowd-out of present-day harm mitigation — funding, talent, regulatory bandwidth — attributable to this reading''s dominance?',
    'Longitudinal partition of AI-safety philanthropy and public R&D by problem class (control vs. deployed-harm), controlling for total budget growth, over the story interval and beyond.',
    'Quantifies the extraction term directly: small crowd-out stabilizes the tangled_rope classification; large crowd-out supplies the missing mass for snare drift and sharpens the T17-style accumulation hypothesis already visible in the rising base_extractiveness series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_magnitude, empirical, 'Size of the resource diversion from present-harm mitigation to speculative-risk prevention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__safety_control_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__safety_control_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ai_a_tr_t2, ai_alignment_commitment__safety_control_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__safety_control_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_commitment__safety_control_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__safety_control_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__safety_control_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__safety_control_reading, theater_ratio, 12, 0.32).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__safety_control_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(ai_a_be_t2, ai_alignment_commitment__safety_control_reading, base_extractiveness, 2, 0.34).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__safety_control_reading, base_extractiveness, 4, 0.43).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_commitment__safety_control_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__safety_control_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__safety_control_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__safety_control_reading, base_extractiveness, 12, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__safety_control_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(ai_a_su_t2, ai_alignment_commitment__safety_control_reading, suppression_requirement, 2, 0.26).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__safety_control_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_commitment__safety_control_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__safety_control_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__safety_control_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__safety_control_reading, suppression_requirement, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__safety_control_reading, information_standard).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, ethics_justice_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__safety_control_reading, integrated_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'AI alignment' covers three structurally distinct claims that cannot share one epsilon. This file instantiates the safety_control_reading (epsilon 0.68; victims are present-day harm populations crowded out of the definition; type tangled_rope). ethics_justice_reading instantiates the opposite pole (present-harm prevention; its own epsilon and victim set, likely including populations exposed to unmitigated catastrophic risk). integrated_reading refuses the exclusivity premise and inherits structural pressure from BOTH siblings. Upstream/downstream: this reading currently holds institutional dominance, so it shapes the resource environment in which the siblings operate (authored as an influences edge to integrated_reading and a coexists_with edge to ethics_justice_reading); neither sibling is logically foreclosed by this one, since a party can hold any of the three without contradiction — they compete over priority and resource share, not over logical possibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
