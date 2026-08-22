% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: Near-Term Harms Prioritization Norm (Justice-First Reading)
 *   domain: technology governance/AI safety/risk assessment
 *
 * SUMMARY:
 *   This story authors the near-term-harms prioritization norm as an
 *   operative allocation constraint in AI safety and technology governance:
 *   the standing rule that AI risk is primarily discrimination, displacement,
 *   and surveillance affecting identifiable present populations, and that
 *   justice interventions accordingly take precedence in funding, legislative
 *   attention, and research labor. The norm has a genuine coordination
 *   function (verifiable-harm accountability infrastructure) and a real
 *   extraction side (crowding out a rival research program, shifting
 *   compliance costs, discounting unrepresented future interests at a steep
 *   rate), which is why it is claimed as tangled_rope rather than rope or
 *   snare. Per the epsilon-invariance principle, the rival prioritization is
 *   a separate constraint in a separate file; this file contains one reading
 *   with one stable epsilon over one referent: the near-term-prioritizing
 *   allocation arrangement itself, assessed through this reading's own lights
 *   (present harms are real, measurable, and morally weighty) while the
 *   metrics describe the arrangement's full operating costs, including the
 *   ones this reading's adherents discount.
 *
 * KEY AGENTS:
 *   - marginalized_harm_communities: intended primary beneficiary (moderate/trapped) — bears deployed-system harms; receives the norm's remedies through intermediary organizations
 *   - fairness_accountability_researchers: secondary beneficiary (moderate/identity_locked) — collects funding, careers, and standing that attach to the near-term frame
 *   - civil_rights_policy_coalition: agenda setter (organized/constrained) — decides which harms reach the policy agenda; administers the selection rule
 *   - long_horizon_alignment_researchers: primary payer (moderate/identity_locked) — loses grants, venues, and hiring lines; exit would dissolve their research program's premise
 *   - frontier_ai_labs: payer with dual position (powerful/arbitrage) — absorbs compliance costs while capturing moat benefits and agenda displacement
 *   - future_populations: excluded seat (powerless/trapped) — bears discounted deferred costs with no procedural presence
 *   - interdisciplinary_risk_scholars: analytical observer (analytical/analytical) — maps the allocation dispute without holding a stake in either frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.46).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.58).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term Harms Prioritization Norm (Justice-First Reading)").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "technology governance/AI safety/risk assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '0b21f237-3a53-4327-a073-37ed546066ee').
narrative_ontology:cs_kernel_codification('0b21f237-3a53-4327-a073-37ed546066ee', distributed).
narrative_ontology:cs_authority_grounding('0b21f237-3a53-4327-a073-37ed546066ee', distributed).
narrative_ontology:cs_reading_relation('0b21f237-3a53-4327-a073-37ed546066ee', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('0b21f237-3a53-4327-a073-37ed546066ee', foundational, present_harm_moral_priority).
narrative_ontology:cs_axiom_status(present_harm_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('0b21f237-3a53-4327-a073-37ed546066ee', present_harm_moral_priority, deontological).
narrative_ontology:cs_axiom('0b21f237-3a53-4327-a073-37ed546066ee', secondary, allocation_follows_verifiable_harm).
narrative_ontology:cs_axiom_status(allocation_follows_verifiable_harm, holdable).
narrative_ontology:cs_axiom_grounding('0b21f237-3a53-4327-a073-37ed546066ee', allocation_follows_verifiable_harm, instrumental).
narrative_ontology:cs_reference_frame('0b21f237-3a53-4327-a073-37ed546066ee', present_harm_justice_primacy).
narrative_ontology:cs_drift_state('0b21f237-3a53-4327-a073-37ed546066ee', post_frontier_risk_salience, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0b21f237-3a53-4327-a073-37ed546066ee', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_harm_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, long_horizon_alignment_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, future_populations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, frontier_ai_labs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, frontier_ai_labs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Racialized workers, content moderators, gig workers under algorithmic management, and residents of heavily surveilled neighborhoods bear discriminatory lending and hiring models, opaque workplace scoring, and predictive policing. They cannot opt out of being governed by deployed systems. The prioritization norm directs audits, worker protections, and surveillance limits toward them, largely mediated through civil-rights organizations rather than exercised directly.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_harm_communities, beneficiary,
    moderate, immediate, trapped, national).

% The FAccT-community researchers whose methods (disparity measurement, audit studies, incident documentation) define the near-term frame. Grant lines, faculty positions, conference venues, and a growing consultancy market attach to this framing. Their professional identity was built inside the accountability tradition; pivoting to long-horizon work would mean abandoning accumulated standing and methodological capital.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    moderate, biographical, identity_locked, global).

% Coalitions of advocacy organizations, legal clinics, and aligned legislators draft model algorithmic-accountability bills, bias-audit mandates, and surveillance ordinances, and decide which harms reach the policy agenda. Their leverage depends on harms being concrete, documented, and attributable within an electoral cycle, which reinforces the near-term selection rule they administer.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, civil_rights_policy_coalition, agenda_setter,
    organized, biographical, constrained, national).

% Researchers working on misalignment, loss-of-control, and long-horizon hazards lose grants, seminar slots, and hiring lines as funders and departments rebalance toward deliverables on 0-5 year timescales. Their entire research program presupposes that long-horizon stakes are real and weighty; accepting the dominant frame would dissolve the premise of their own work, so exit from the paying position is not practically available to them.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, long_horizon_alignment_researchers, payer,
    moderate, generational, identity_locked, global).

% Deployers absorb audit costs, documentation burdens, and deployment friction under near-term accountability regimes. Simultaneously they benefit: compliance regimes with high fixed costs raise barriers that entrench large incumbents, and a policy agenda occupied by audits and disclosure displaces more structurally disruptive proposals such as strict liability or licensing. They can relocate operations, lobby, or reframe commitments across jurisdictions.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, frontier_ai_labs, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, frontier_ai_labs, beneficiary).

% Whoever will exist under whatever risks the current allocation leaves unmitigated. They hold no seat on funding panels, in legislatures, or at conferences; their interests enter the process only through advocates of the rival framing, whom the operative norm casts as speculative. Whatever deferred costs the discounting produces, they cannot decline, appeal, or exit.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, future_populations, excluded,
    powerless, civilizational, trapped, global).

% Decision theorists, historians of science, and governance scholars who map how the field allocates attention across timescales, compare evidential standards applied to present versus catastrophic harms, and publish analyses owned fully by neither camp.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, interdisciplinary_risk_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates scarce safety, governance, and research capacity toward verifiable present harms where intervention efficacy can be observed and corrected within political timescales, and builds shared accountability infrastructure (audit standards, incident reporting, documentation norms) that dispersed advocates, regulators, and journalists can coordinate around without trusting any single actor's assurances.
% TRANSFER_FUNCTION: Moves funding, prestige, legislative attention, and researcher labor from long-horizon and speculative-risk programs toward near-term justice interventions; moves compliance and audit costs onto deployers; and, on the descriptive account this story authors, moves deferred risk costs onto unrepresented future populations at a steep implicit discount.
% ABSENT_VOICES: Future populations have no seat anywhere in the allocation process. Long-horizon alignment researchers are nominally present in the field but outgunned in the venues that set agendas (legislatures, major funders, mainstream press), where the operative frame labels their concern speculative. In several jurisdictions the communities bearing deployed-system harms lack standing to sue, so even the intended beneficiaries sometimes speak only through intermediaries.
% DISAPPEARANCE_RATIONALE: If the norm vanished overnight, the field's resource allocation would reorganize around whichever competing prioritization captured the vacated agenda space; audit mandates in progress would stall, the FAccT funding and career structure would contract, deployers would face a different (likely lighter near-term, heavier long-horizon) compliance mix, and the advocacy coalitions built around measurable-harm remedies would lose their organizing principle.
% FOUNDING_PROBLEM: In the mid-2010s, deployed systems were measurably harming identifiable people (recidivism-score disparities, facial-recognition demographic differentials, opaque gig-management terminations) while field attention and elite concern flowed elsewhere. The norm was built to force attention and resources onto harm that is happening now, to people who can be identified, at a scale interventions can reach.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains corroborated from outside the benefiting parties: NIST's facial-recognition demographic-differential benchmarks, litigation records and regulatory findings (housing, hiring, credit), investigative journalism, and replication studies by teams outside the FAccT community all continue to document present, measurable disparities. Corroboration for the problem's continued liveness does not rest on the beneficiary set's own testimony.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__near_term_harms_reading_tests).
:- end_tests(ai_risk_prioritization__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.46) rather than high because the norm's primary operation is protective coordination with measurable remedial output; the extraction consists of redirected careers and funding, imposed compliance costs, and steeply discounted deferred risk, not confiscation. Suppression (0.58) is discursive and institutional rather than coercive: grant-panel gatekeeping, venue and hiring preferences, and the 'speculative distraction' framing that delegitimizes the rival program's claim on resources. Theater ratio (0.30) reflects a maturing audit industry in which a growing minority of bias audits function as compliance performance that launders deployment decisions rather than changing systems. Accessibility collapse is low-moderate (0.40): the rival framing persists visibly, so alternatives are suppressed but nowhere near collapsed. Resistance (0.60) is sustained and organized — the rival camp publishes counter-framings, funders maintain parallel portfolios, and the 2023 salience spike for catastrophic-risk concerns forced this frame onto the defensive. All three temporal series run on one shared six-point grid; the suppression_requirement series is authored because enforcement dynamics are central to this story: the frame's dominance had to be actively maintained, and the maintenance burden intensified after the rival frame gained mainstream salience late in the interval. Base-property values equal the series endpoints by construction of the grid, not by tuning.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the beneficiary seats the arrangement computes as coordination: real harms, real remedies, measurable progress. From the long-horizon payer seat the same structure operates as enforced extraction with an added identity-lock mechanism: alignment researchers cannot exit the paying position without abandoning the premise of their own professional identity, mirroring the identity lock on the FAccT side (whose members would forfeit accumulated methodological and institutional capital by defecting). Both camps are identity_locked, which stabilizes the contest and raises the cost of any pluralist settlement. The lab seat experiences the constraint as a manageable, partly profitable cost of doing business — its arbitrage-grade exit keeps its effective extraction low despite nominal payer status. Future populations experience whatever the discounting produces, with no seat from which to register it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: marginalized_harm_communities and fairness_accountability_researchers sit near the subsidized end (low d), with the researchers' identity lock and the communities' trapped exit preventing leakage back toward symmetry. long_horizon_alignment_researchers derive high d (full-target side): they bear the transfer and cannot exit. future_populations derive maximal d — powerless, trapped, and structurally unable to appear. frontier_ai_labs are declared victims but their arbitrage exit and secondary beneficiary position pull their derived d well down toward the middle, which is the honest reading: they pay some costs and recapture others. No directionality overrides are authored because the beneficiary/victim plus exit-option data already yields these relationships; the interesting asymmetries here come from exit options and identity locks, not from misderived positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two opposite mislabels. Reading the norm as a snare would erase its genuine coordination achievement: accountability infrastructure that converts diffuse anecdotal harm into auditable, litigable, regulable facts, with documented remedial wins. Reading it as a pure rope would erase the asymmetric extraction: a rival research program actively crowded out of resources, deployers paying rents in the audit market, and unrepresented future interests discounted to near zero by the frame's own selection rule. On the genealogy interview, the founding problem is live and externally corroborated, so no mandatrophy is declared: the mandate has not outlived its function. The open lifecycle risk runs the other direction — rising theater_ratio and rising suppression_requirement together trace the standard tangled-rope degradation path toward snare, driven by audit-industry capture and the hardening of gatekeeping as the frame became dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the ai_risk_prioritization kernel; what exactly would change if the existential_risk_reading were instantiated instead?',
    'Comparison against the sibling story file: the sibling flips the victim set (future populations primary), the resource-allocation target (alignment research paramount), the operative timescale (decades-plus), and the suppression relation (near-term concerns reframed as distraction from the catastrophic).',
    'Classification is per-reading by design; resolving this omega does not merge the stories but confirms the family decomposition and lets cross-reading contamination analysis run over the pair.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega recording that this story is the near-term reading of a two-reading kernel.').

omega_variable(
    catastrophic_probability_estimate,
    'What is the actual probability of catastrophic outcomes from advanced AI systems within planning-relevant horizons?',
    'Convergent forecasting: structured expert elicitation with calibration scoring, base-rate analysis of transformative-technology transitions, and incremental capability evaluation — none decisive alone, but converging estimates would narrow the band.',
    'A sufficiently high credible probability would make the norm''s implicit near-zero discount a massive unpriced transfer onto future populations, pushing the classification toward snare; a very low estimate would vindicate the near-term frame''s allocation and push toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophic_probability_estimate, empirical, 'The empirical half of the inter-reading dispute: how much risk does the discount actually defer?').

omega_variable(
    intertemporal_moral_weighting,
    'How should allocation weigh identifiable present persons against statistically described future persons, given any fixed probability estimate?',
    'Not resolvable by data: this is the preference-theoretic core of the kernel contest. Public justification processes (citizen assemblies, published impartial-benevolence arguments) can surface and stabilize a weighting, but cannot discover a uniquely correct one.',
    'Whichever weighting prevails determines whether the norm''s discounting is a defensible coordination choice or extraction from the unrepresented; the same measured behavior classifies differently under different weightings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intertemporal_moral_weighting, preference, 'The irreducibly normative half of the discount-rate dispute between the sibling readings.').

omega_variable(
    audit_functionality_drift,
    'What fraction of bias-audit activity changes deployed-system behavior versus functioning as compliance performance that launders deployment decisions?',
    'Longitudinal outcome tracking: follow audited systems past certification to measure remediation rates, repeat-disparity incidence, and whether audit findings correlate with subsequent model changes, controlling for selection effects in what gets audited.',
    'A rising non-functional share would confirm Goodhart drift in the norm''s central instrument, accelerating the tangled_rope-toward-piton/snare degradation path and dating the transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_functionality_drift, empirical, 'Whether the audit industry is doing the coordination work or performing it.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the crowding-out of long-horizon research structural (funder and venue gatekeeping) or internalized (researchers preemptively self-censoring long-horizon framing to survive review)?',
    'Post-liberalization trajectory test: if funding lines or venues opened to long-horizon work and proposal rates stayed depressed, the suppression is substantially internalized; if activity rebounded quickly, it was structural.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — the paying camp carries the suppression with them even where formal barriers drop, deepening the identity-lock asymmetry and slowing any pluralist rebalance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of the frame''s suppression of the rival program.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2, 0.15).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 10, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2, 0.4).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI risk prioritization' covers two structurally distinct allocation constraints that the epsilon-invariance principle requires separating. This file authors the near-term-harms reading: victim class = present marginalized populations, remedy portfolio = bias audits, worker protections, surveillance regulation, timescale 0-5 years, epsilon 0.46 over the near-term-prioritizing arrangement. The sibling file authors the existential-risk reading: victim class = future populations under misalignment, remedy portfolio = alignment research, timescale decades-plus, with its own epsilon over the long-horizon-prioritizing arrangement. The readings share a kernel and a resource pool, so each constrains the other's operating environment; they are linked here and in the sibling's network block. Neither epsilon is recoverable from the other's observables — measuring allocation by present-harm remediation yields one number, by deferred-catastrophe mitigation another — which is precisely why the label had to decompose into two stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
