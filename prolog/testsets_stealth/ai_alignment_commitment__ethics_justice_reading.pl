% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: Alignment-as-Present-Harm-Prevention (Ethics & Justice Reading)
 *   domain: technological/governance
 *
 * SUMMARY:
 *   Within AI governance, 'alignment' is a contested commitment, and this
 *   story authors one reading of it as a standing institutional arrangement:
 *   funders, conference tracks, editorial norms, and lab governance treat
 *   alignment as meaning the prevention of bias reproduction and present-day,
 *   demonstrable harms. The reading has a genuine coordination function — it
 *   gives a fragmented field a shared, measurable target and channels real
 *   remedies to communities with documented injuries. It also carries an
 *   asymmetric cost: grant eligibility, review norms, and career incentives
 *   progressively crowd out long-term control research, and the populations
 *   exposed to tail risks hold no seat in the allocation. The claim/metric
 *   gap is deliberate: the reading is CLAIMED as tangled_rope (coordination
 *   plus asymmetric burden) while the metrics are authored independently from
 *   its observed operation — the engine measures the divergence rather than
 *   the author reconciling it.
 *
 * KEY AGENTS:
 *   - alignment_funding_bodies: agenda setter (institutional/arbitrage) — administers the grant lines and eligibility criteria that operationalize the reading
 *   - communities_harmed_by_biased_systems: primary intended beneficiary (organized/constrained) — receives protection and remedies
 *   - fairness_auditing_sector: secondary beneficiary and receipt seat (moderate/mobile) — collects the contract and grant pipeline the definition generates
 *   - ai_lab_public_affairs_offices: beneficiary (powerful/mobile) — collects reputational legitimacy from demonstrable-harm milestones
 *   - ai_ethics_advocacy_networks: dual-positioned (organized/constrained) — presses the definition while drawing support from it
 *   - long_term_safety_researchers: primary payer (moderate/identity_locked) — bears the crowding-out of their research program
 *   - future_catastrophic_risk_bearers: unrepresented payer (powerless/trapped) — bears the deferred risk with no seat
 *   - product_development_teams: payer (moderate/mobile) — absorbs compliance and audit overhead
 *   - interdisciplinary_governance_scholars: analytical observer — maps the allocation without a stake in its outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.58).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.52).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "Alignment-as-Present-Harm-Prevention (Ethics & Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "technological/governance").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, '02fe1b59-19a5-4662-8eff-49a161f354d7').
narrative_ontology:cs_kernel_codification('02fe1b59-19a5-4662-8eff-49a161f354d7', distributed).
narrative_ontology:cs_authority_grounding('02fe1b59-19a5-4662-8eff-49a161f354d7', distributed).
narrative_ontology:cs_reading_relation('02fe1b59-19a5-4662-8eff-49a161f354d7', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('02fe1b59-19a5-4662-8eff-49a161f354d7', ai_alignment_commitment__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('02fe1b59-19a5-4662-8eff-49a161f354d7', foundational, present_harms_morally_prior).
narrative_ontology:cs_axiom_status(present_harms_morally_prior, holdable).
narrative_ontology:cs_axiom_grounding('02fe1b59-19a5-4662-8eff-49a161f354d7', present_harms_morally_prior, deontological).
narrative_ontology:cs_axiom('02fe1b59-19a5-4662-8eff-49a161f354d7', foundational, demonstrated_harm_evidential_standard).
narrative_ontology:cs_axiom_status(demonstrated_harm_evidential_standard, holdable).
narrative_ontology:cs_axiom_grounding('02fe1b59-19a5-4662-8eff-49a161f354d7', demonstrated_harm_evidential_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('02fe1b59-19a5-4662-8eff-49a161f354d7', justice_centered_present_harm_frame).
narrative_ontology:cs_drift_state('02fe1b59-19a5-4662-8eff-49a161f354d7', contemporary_frontier_labs_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('02fe1b59-19a5-4662-8eff-49a161f354d7', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, communities_harmed_by_biased_systems).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, fairness_auditing_sector).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ai_lab_public_affairs_offices).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ai_ethics_advocacy_networks).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, future_catastrophic_risk_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, product_development_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the major grant lines, fellowships, and research programs that decide what counts as alignment work. Their calls for proposals define eligible topics: bias audits, fairness benchmarks, and community-impact studies qualify readily, while speculative control research struggles to fit published criteria. They can rebalance portfolios between harm categories with an administrative decision, though each rebalance draws organized protest from whichever side loses standing.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, alignment_funding_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% People screened by automated hiring filters, scored by credit and risk models, matched by facial recognition, or triaged by clinical algorithms who received worse outcomes correlated with race, gender, disability, or language. Civil-rights organizations aggregate their complaints into litigation and audit campaigns. They cannot opt out of AI-mediated services, but organized advocacy gives them standing in the allocation that they historically lacked.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, communities_harmed_by_biased_systems, beneficiary,
    organized, biographical, constrained, global).

% Consultancies, academic labs, and nonprofit audit shops that measure discriminatory system behavior and certify mitigations. Every widening of the reading's definition enlarges their contract pipeline: model cards, disparate-impact reviews, red-team engagements. Individual skills transfer readily to adjacent compliance markets, so practitioners stay mobile even as the sector's revenue becomes dependent on the definitional boundary it helps police.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, fairness_auditing_sector, beneficiary,
    moderate, biographical, mobile, global).

% Communications and policy staff at frontier labs who translate internal practices into public commitments. Demonstrable harm-prevention is legible to regulators and journalists in a way that speculative risk work is not, so the reading supplies them with reportable milestones each quarter. Their standing rises with every published fairness benchmark; their exit from the position is a job change, not a change of circumstances.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_lab_public_affairs_offices, beneficiary,
    powerful, immediate, mobile, global).

% Coalitions of academics and campaign organizations that pressed the reading into funding criteria and conference tracks and now defend it. They draw grants, speaking invitations, and institutional appointments from the arrangement they promote, while their credibility rests on continued identification with the justice framing — stepping back from the definitional fight would cost them the constituency that sustains them.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_ethics_advocacy_networks, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, ai_ethics_advocacy_networks, agenda_setter).

% Researchers working on control of advanced systems whose grant applications increasingly fail published eligibility tests, whose papers land outside the tracks reviewers treat as core, and whose students are steered toward fundable topics. Their professional identities formed inside the alignment project; relocating to mainstream machine learning or leaving the field means abandoning the mission that organized their careers. Some have moved to well-funded industry safety teams, but the field-wide signal they receive is that their problem is somebody else's definition of success.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers, payer,
    moderate, civilizational, identity_locked, global).

% Everyone who would bear the consequences of advanced systems escaping human control — populations not yet born and contemporaries with no organized voice. They hold no grants to lose and no seats to fill; they enter the arrangement only as the discounted party in other people's trade-offs. Proxy advocates speak for them and are routinely outmatched by constituencies with present, documentable injuries.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, future_catastrophic_risk_bearers, payer,
    powerless, generational, trapped, global).

% Engineering groups inside AI companies that absorb the compliance load: pre-deployment bias evaluations, documentation requirements, incident review boards. The overhead is real but budgeted, and the skills involved are portable; their grievance is schedule slip, not livelihood.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, product_development_teams, payer,
    moderate, immediate, mobile, global).

% Researchers spanning law, philosophy, and computer science who study how the field allocates moral attention. They take testimony from every seat, publish comparisons of the rival definitions of alignment, and hold no stake in which definition wins.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, interdisciplinary_governance_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__ethics_justice_reading, fairness_auditing_sector).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__ethics_justice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of which harms count when safety attention is scarce: it gives funders, reviewers, and labs a shared, measurable target — documented discriminatory outcomes in deployed systems — and a common toolkit of audits, benchmarks, and disparity metrics for coordinating remediation across organizations that would otherwise each define success privately.
% TRANSFER_FUNCTION: Moves grant funding, conference slots, faculty lines, and public legitimacy from long-term control research toward bias measurement, discrimination red-teaming, and remediation programs; moves reputational protection to deploying labs; moves material remedies toward communities with documented injuries.
% ABSENT_VOICES: Future populations exposed to tail risks have no seat anywhere in the process. Long-term safety researchers attend, but their objections are discounted as speculative. Communities injured by AI systems in jurisdictions without audit infrastructure are named as beneficiaries yet rarely consulted on program design.
% DISAPPEARANCE_RATIONALE: If the reading stopped organizing the field overnight, grant lines would re-tender under rival definitions, fairness-audit pipelines would shrink to statutory compliance work, conference tracks would redistribute, and several thousand careers organized around the justice framing would re-sort; the protections currently flowing to documented-harm communities would lose their dedicated funding channel until a successor arrangement formed.
% FOUNDING_PROBLEM: Deployed systems in hiring, credit, policing, and healthcare were reproducing documented discrimination at scale while the field's attention and funding concentrated on speculative future risks; the reading was built to force present, measurable harms onto the alignment agenda.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: peer-reviewed audit studies of hiring and facial-recognition systems, disparate-impact litigation dockets, and findings from consumer-protection and civil-rights regulators attest that the underlying harms persist. No disinterested party attests that the arrangement as constituted is the right-sized response — long-term safety researchers dispute the allocation, which is contestation about the remedy, not denial of the problem.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: substantial resources, publication space, and talent demonstrably moved away from control research, but the arrangement also delivers real protection, so the burden is meaningful rather than total. Suppression at 0.52 reflects gatekeeping rather than prohibition — nothing is banned, yet eligibility criteria, review norms, and hiring signals raise the cost of rival work enough to shape the field. Theater at 0.30 captures a growing bias-washing layer (reports and benchmarks produced for legibility rather than remediation) sitting on top of a functional core. Accessibility collapse is low (0.35) because rival readings remain publishable, fundable, and hirable somewhere — alternatives persist, degraded but not closed. Resistance is high (0.60): the displaced research community contests the framing openly and continuously. The three temporal series share one grid (t=0..12, step 2) with all metrics authored at every point; trajectories are monotonic consolidation, not cycles — the reading gained definitional territory steadily over the interval, with theater and enforcement intensity rising alongside its resource pull.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the long-term safety researcher's position — identity-fused with the mission the definition rules out of bounds — the arrangement operates as the organized displacement of their life's work. From the harmed community's position it is the first allocation of field-level attention their injuries have ever received. From the funder's position it is ordinary portfolio management under a defensible rubric. From the auditor's position it is a growing market. Same structure, divergent computed classifications per seat; the divergence is the data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place communities_harmed_by_biased_systems near the subsidized end (d near 0) — the arrangement exists to protect them, though delivery depth is an open omega. The fairness_auditing_sector and ai_ethics_advocacy_networks sit slightly higher: they collect directly from the definitional boundary and help maintain it. ai_lab_public_affairs_offices benefit through reputation rather than funds. The payers sit near the target end: long_term_safety_researchers combine victim status with identity_locked exit, pushing their effective burden toward the full-target pole; future_catastrophic_risk_bearers are victims with zero exit and zero voice — maximal exposure, no damping. product_development_teams bear real but budgeted costs with mobile exit, damping their effective burden below their nominal payment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — documented algorithmic discrimination persists and scales with deployment — so no mandate-atrophy declaration is made and none should be inferred from the theater ratio's rise; the theater growth is a symptom of success attracting performers, not of a dead mandate. The classification risk runs in both directions: labeling the arrangement pure coordination would hide the crowding-out of control research behind the justice framing, while labeling it pure extraction would erase the material protection delivered to documented-harm communities. The tangled-rope claim keeps both halves structurally visible. On the R5 mismatch consumer: founding_problem_status is live and disappearance_verdict is world_rearranges — aligned, so no zombie flag is expected; the watch item is remediation_capture_depth, which is the path by which the beneficiary half could hollow out while the extraction half persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the ethics_justice_reading of the ai_alignment_commitment kernel; how would the sibling readings (safety_control_reading, integrated_reading) restructure the victim set and the referent of the measured burden?',
    'Comparative authoring of the sibling stories as separate epsilon-invariant constraints, plus engine computation of foreclosure relations from axiom grounding types and drift states.',
    'Under the safety_control_reading the victim set relocates to populations exposed to tail risks and the present-harm communities become a subsidized constituency; under the integrated_reading the crowding-out component dissolves and the arrangement computes closer to pure coordination. Each sibling is a different constraint with its own epsilon, not a measurement parameter on this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a contested kernel, with sibling readings as separate files.').

omega_variable(
    zero_sum_vs_complementarity,
    'Is the trade-off between present-harm work and long-term control work actually zero-sum, or does bias-measurement tooling complement control research?',
    'Citation-flow and talent-flow analysis between fairness venues and safety venues; natural experiment from labs that fund both portfolios at scale and track cross-pollination.',
    'If strongly complementary, the extraction-from-safety-research component is largely nominal and the arrangement computes nearer to rope; if genuinely zero-sum, the measured burden on long-term safety researchers is real and the tangled-rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_sum_vs_complementarity, empirical, 'Whether the reading''s resource pull imposes net costs on the displaced research program.').

omega_variable(
    remediation_capture_depth,
    'Do remediation resources actually reach the communities named as beneficiaries, or do they concentrate in audit intermediaries and program administration?',
    'Program-level expenditure tracing: share of bias-remediation funding reaching affected individuals and community organizations versus consultancy fees, benchmark construction, and administrative overhead.',
    'If intermediaries capture most of the flow, the effective victim set widens beyond long-term safety researchers and the arrangement drifts toward snare; if delivery is substantial, the coordination half dominates and the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_capture_depth, empirical, 'Depth at which the named beneficiary communities receive material delivery rather than representation.').

omega_variable(
    definitional_enforcement_trajectory,
    'Will the gatekeeping that maintains the reading''s definitional boundary (review norms, eligibility criteria, hiring signals) keep intensifying, or relax as the integrated reading gains institutional ground?',
    'Track acceptance rates of control-focused submissions at justice-oriented venues, grant eligibility language across successive funding cycles, and job-posting taxonomy shifts.',
    'Continued intensification supports a hardening tangled-rope-to-snare trajectory; relaxation would indicate convergence toward the integrated reading and falling suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definitional_enforcement_trajectory, empirical, 'Future path of the enforcement machinery defending the reading''s boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(ai_a_tr_t2, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2, 0.19).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 12, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(ai_a_be_t2, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2, 0.41).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 6, 0.49).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(ai_a_su_t2, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2, 0.41).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI alignment' decomposes into at least three structurally distinct arrangements with different epsilon values, victim sets, and failure modes. This story (ethics_justice_reading) carries the present-harm victim set and the crowding-out burden on long-term safety research; safety_control_reading carries the tail-risk victim set and its own displacement politics; integrated_reading claims both problem classes simultaneously and its feasibility is precisely what the other two stories contest. The upstream/downstream structure runs through funding and legitimacy: whichever reading controls grant eligibility conditions the operating environment of the others. Family members are linked via affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
