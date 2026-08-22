% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Expertise Boundary (Peer-Review Validation Reading)
 *   domain: epistemology/science_technology_studies/political_theory
 *
 * SUMMARY:
 *   The arrangement under contest is the modern knowledge-legitimacy
 *   boundary: a claim counts as publicly legitimate knowledge when it issues
 *   from methodologically rigorous inquiry and has been validated by
 *   credentialed peer review. The boundary solves a real problem —
 *   non-experts cannot verify specialized claims, so someone must supply a
 *   delegable reliability signal — while simultaneously concentrating
 *   epistemic authority, collecting publishing and credentialing rents, and
 *   discounting systematically produced experiential knowledge. KEY AGENTS
 *   (by structural relationship): credentialed_academics
 *   (organized/identity_locked) — dual-positioned: collect authority rents,
 *   pay the treadmill tax; academic_publishers (institutional/arbitrage) —
 *   administer the validation machinery and collect its monetary rents;
 *   professional_accreditation_bodies and funding_agencies (institutional) —
 *   set entry criteria and enforce them through degrees and grants;
 *   policy_advisory_elites (powerful/constrained) — translate credentialed
 *   consensus into law; general_public (powerless/constrained) — delegate
 *   judgment, pay diffusely; independent_researchers (powerless/trapped) and
 *   experiential_knowledge_holders (powerless/identity_locked) — bear the
 *   boundary's costs; sts_reform_scholars (moderate/analytical) — observe and
 *   critique from inside. Kernel decomposition note: this file instantiates
 *   the credentialed_expertise_reading only; the experiential_pluralism and
 *   hybrid_coproduction readings are separate stories with their own epsilon
 *   values over the same standing arrangement, linked via
 *   network.affects_constraints. The claim/metric pair is authored
 *   independently: the claimed type states what this reading holds
 *   structurally true; the metrics state what is descriptively true of the
 *   arrangement's operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.45).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.62).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Expertise Boundary (Peer-Review Validation Reading)").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, '958b8b29-d811-4aec-b1d6-b6a6afcca6f1').
narrative_ontology:cs_kernel_codification('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', formalized).
narrative_ontology:cs_authority_grounding('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', expertise).
narrative_ontology:cs_interpretation_layer_present('958b8b29-d811-4aec-b1d6-b6a6afcca6f1').
narrative_ontology:cs_reading_relation('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', legitimate_knowledge_boundary__experiential_pluralism_reading, forecloses).
narrative_ontology:cs_reading_relation('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', foundational, credentialed_validation_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(credentialed_validation_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', credentialed_validation_constitutes_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', secondary, expert_consensus_truth_proxy).
narrative_ontology:cs_axiom_status(expert_consensus_truth_proxy, holdable).
narrative_ontology:cs_axiom_grounding('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', expert_consensus_truth_proxy, empirically_contingent).
narrative_ontology:cs_reference_frame('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', methodologically_certified_consensus).
narrative_ontology:cs_drift_state('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', contemporary_replication_crisis_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('958b8b29-d811-4aec-b1d6-b6a6afcca6f1', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academics).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_publishers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, professional_accreditation_bodies).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_advisory_elites).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, general_public).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, independent_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academics).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, general_public).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, methodological_rigor_sufficiency).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, expert_consensus_truth_proxy).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, meritocratic_credential_neutrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold doctorates and faculty or laboratory positions; produce the research the validation system certifies and staff its panels as editors and referees. Career advancement, grant eligibility, and public standing flow through the credential-and-review channel. They also pay into the same channel: unpaid reviewing labor, article processing charges, subscription costs borne by their libraries, and career risk under hypercompetitive publication metrics. Leaving the channel means forfeiting the standing their training built.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academics, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academics, payer).

% Own and operate the major journals, appoint editors, and run the review workflow that converts submissions into certified publications. Set acceptance standards, negotiate subscription and open-access fees, and collect revenue from institutions and authors. Their catalogs function as the de facto registry of what counts as published science; exit for them means selling or repivoting portfolios, which they can do from strength.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Define degree requirements, license professions, and accredit the programs that confer credentials. Universities and employers treat their marks as prerequisites for practice. They collect fees and deference from the institutions they accredit and shape who can enter the expert class at all.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, professional_accreditation_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, professional_accreditation_bodies, beneficiary).

% Public and private grantmakers that require peer-reviewable proposals, principal investigators with track records, and publication outputs in recognized venues. Their criteria decide which questions become answerable careers and which lines of inquiry starve. They can shift criteria, but doing so invites accusations of politicizing science.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Serve on advisory committees, commissions, and expert panels where credentialed consensus is translated into regulation and law. Their authority in those rooms depends on the line between expert and lay knowledge holding; they draw salaries, influence, and legacy from serving as its translators.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_advisory_elites, beneficiary,
    powerful, biographical, constrained, national).

% Rely on the arrangement as trust infrastructure: they cannot personally verify drug trials, climate models, or engineering safety cases, so they delegate judgment to credentialed consensus. They pay for the system through taxes, tuition, and library subscriptions, and they bear the cost when locally valid knowledge — agricultural practice, patient experience — is dismissed as anecdote.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, general_public, beneficiary,
    powerless, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, general_public, payer).

% Do serious scholarship without institutional affiliation: retired scientists, autodidacts, community researchers. Without credentials they struggle to publish in recognized venues, win grants, or be cited; their work is routinely discounted before evaluation. Entry to the legitimating channel is effectively closed, and stopping the work is not a real option for those defined by it.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, independent_researchers, payer,
    powerless, biographical, trapped, global).

% Patients with rare diseases, Indigenous land managers, frontline nurses, informal caregivers — people whose systematic knowledge comes from sustained lived contact rather than formal training. Their testimony is admissible only after translation into credentialed formats; the untranslated form is treated as anecdote. Their knowing is bound up with identity and community, so abandoning it is not an option.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_holders, payer,
    powerless, generational, identity_locked, global).

% Science-studies scholars, metascience researchers, and open-science advocates who study the validation system itself: its replication record, its incentive distortions, its exclusions. They publish critiques inside the same venues they critique and propose reforms — registered reports, participatory review, plural validation channels — from an analytical vantage point.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, sts_reform_scholars, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academics).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves trust-at-scale: non-experts cannot verify specialized claims directly, so the arrangement provides a delegable reliability signal (credentials plus peer validation), enables division of epistemic labor, and gives institutions a common standard for hiring, funding, regulation, and evidence.
% TRANSFER_FUNCTION: Moves certified epistemic authority toward credentialed insiders and away from uncredentialed knowers; moves money (subscriptions, article processing charges, tuition, licensing fees) from states, universities, and readers toward publishers and accrediting institutions; moves unpaid evaluative labor from reviewers to journals.
% ABSENT_VOICES: Uncredentialed knowers — independent researchers, patient and Indigenous knowledge communities — sit outside editorial boards, review panels, and funding committees. Their standing objection is that the definition of validity is written by those who profit from holding it. They enter the conversation only when translated into credentialed formats or as subjects of study.
% DISAPPEARANCE_RATIONALE: Regulatory approval, court evidence standards, university hiring, journalism sourcing, and funding allocation are all keyed to the credential-and-review mark. Overnight removal would force every institution to rebuild its legitimacy screen around some other marker — or none — rearranging medicine, law, science policy, and public debate within months.
% FOUNDING_PROBLEM: Late nineteenth- and twentieth-century professionalization faced a concrete problem: distinguishing reliable expertise from quackery, press sensationalism, and state propaganda at mass scale, and building a trust infrastructure adequate to industrial-sized science.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science document the professionalization drive against quackery and sensationalism; metascience and misinformation researchers outside the benefiting parties attest the noise-and-charlatan problem remains live and has intensified. STS and epistemic-justice scholars corroborate that the founding problem is real while disputing that the current arrangement is its best solution. No corroborating source outside the benefiting parties claims the problem is dead.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).
:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45: real transfers exist (subscription and APC rents decoupled from marginal service cost, unpaid review labor, foreclosed careers and discounted testimony), but they are bounded by a genuine service the arrangement delivers; from this reading's own lights the arrangement is net-functional with acknowledged excess. Suppression 0.62 is a raw structural property, unscaled by power or scope: rejection cascades, funding gatekeeping, venue exclusivity, and the treatment of untranslated experiential testimony as anecdote are enforced, not emergent. Theater_ratio 0.30: reviewer rituals, impact-factor gaming, and metric-driven formalism are real but the filtering function is substantially performed. Accessibility_collapse 0.50: alternatives (preprints, citizen science, community validation) persist but are marginalized rather than eliminated. Resistance 0.55: open-science movements, STS critique, epistemic-justice scholarship, and replication-crisis reform meet the boundary head-on. Claimed type tangled_rope is asserted independently of these numbers: the arrangement possesses both a genuine coordination function and asymmetric extraction, and it requires active enforcement to hold. The measurement series run on one shared nine-point grid (every tracked metric authored at every examined time point, t=0..80, roughly 1945-2025); trajectories are monotonic rather than cyclical — extraction accumulates with the serials crisis and hypercompetition, theater rises with audit culture, and enforcement capacity hardens through integrity offices and moderation infrastructure, which is why suppression_requirement is tracked rather than left static. Receipt surface: gain_flow names credentialed_academics because the primary good the arrangement distributes — certified epistemic authority — demonstrably accrues to the credentialed class; publishers monetize that authority downstream (receipt-of-gain is distinct from beneficiary-role, and publishers' monetary capture is recorded in their stakeholder situation). fixing_cost is prohibitive: wholesale replacement of the trust infrastructure risks epistemic chaos faster than any replacement could be built, which is why even the arrangement's sharpest critics pursue reform-within rather than abolition.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the publisher and accreditation seats the arrangement is the working machinery of quality control they operate and profit from; from senior credentialed academics it is a meritocracy that ratified their careers; from junior and contingent academics the same structure presents as a treadmill of unpaid labor and metric-chasing; from independent researchers and experiential knowledge holders it is a closed guild that discounts their work before evaluating it; from the public it is invisible trust infrastructure noticed only at moments of failure. Same-level divergence is visible among the institutional agenda-setters themselves: publishers hold arbitrage-grade exit and generational horizons, while funding agencies and accreditation bodies are constrained by statutory missions and political exposure. The engine computes these per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place publishers, accreditation bodies, funding agencies, and advisory elites near the beneficiary pole (low d, damped or inverted effective extraction); the general_public sits mildly beneficiary-side with diffuse indirect costs. Victim declarations place independent_researchers (trapped) and experiential_knowledge_holders (identity_locked) near the full-target pole — identity lock pushes them further toward full-target than mobile targets would sit. One override is declared: the derivation chain reads credentialed_academics' beneficiary declaration alone and would place them near d~0.1, but the same agents fund the system through article processing charges, page charges, unpaid review labor, and hypercompetitive career risk — the treadmill tax runs through the beneficiary class itself. The override sets d=0.32 for the organized power atom (occupied in this story only by credentialed_academics), keeping them net beneficiaries while registering the internal extraction. Scope amplification applies modestly: the arrangement operates globally, making verification of its enforcement harder and scaling effective extraction upward for target-side seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — separating reliable expertise from quackery, sensationalism, and propaganda at scale — remains live, and the misinformation era has intensified it, so no mandatrophy resolution is declared and the arrangement's persistence currently rides on both function and enforcement. The classification discipline prevents two opposite mislabels. A pure-coordination label would ignore the documented extraction: serials-crisis rents, APC inflation, unpaid labor, and the epistemic losses catalogued by science-studies scholarship. A pure-extraction label would ignore that the filter catches real noise, that the crank problem is corroborated from outside the benefiting parties, and that no ready substitute trust infrastructure exists — which is exactly why fixing_cost is prohibitive. The tangled_rope reading holds both facts simultaneously: whoever is coordinated (public, institutions receiving calibrated advice) and whoever pays (uncredentialed knowers, subscribing institutions, treadmill-bound juniors) are coordinated and charged through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the legitimate_knowledge_boundary kernel — would instantiating the experiential_pluralism or hybrid_coproduction reading change the victim set, the epsilon value, and the computed classification?',
    'Generate the sibling reading stories over the same standing arrangement and compare computed classifications; divergence between readings locates the disagreement structurally.',
    'If sibling readings compute materially different types over the same referent, the kernel contest is substantive rather than verbal; if they converge, the arrangement''s structure dominates the reading index.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification over a shared kernel referent.').

omega_variable(
    replication_crisis_axiom_threat,
    'Does the replication-crisis evidence base undermine the foundational premise that credentialed peer review reliably certifies reliable knowledge?',
    'Large-scale replication projects, registered-report uptake rates, and metascience estimates of false-positive rates in credentialed literatures.',
    'Severe confirmation would push the reading''s foundational axiom toward overridden status and accelerate authority erosion; demonstrated resilience would stabilize the reference frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_crisis_axiom_threat, empirical, 'Empirical status of the certification-reliability premise.').

omega_variable(
    gate_quality_separability,
    'Is the credential-and-exclusivity gate separable from the genuine quality-filtering function, or does filtering depend on the gate?',
    'Compare error rates and validity of outputs from gated venues versus ungated channels (preprints, open review, citizen-science replications) matched by field and period.',
    'If separable, a larger share of measured extraction is removable without losing the coordination function; if inseparable, part of the extraction is the price of the trust infrastructure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gate_quality_separability, empirical, 'Whether gatekeeping and quality control are structurally separable.').

omega_variable(
    suppressed_valid_knowledge_share,
    'How much genuinely valid knowledge does the boundary suppress, as opposed to noise it correctly filters?',
    'Audit studies of later-vindicated rejected work, citation trajectories of experiential and Indigenous knowledge after credentialed adoption, and delayed-recognition bibliometrics.',
    'A high vindicated-rejection rate raises effective extraction on uncredentialed knowers and strengthens the victims'' claim; a low rate supports the filtering defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppressed_valid_knowledge_share, empirical, 'Magnitude of epistemic loss imposed by the gate.').

omega_variable(
    enforcement_trajectory_meaning,
    'Is the rising enforcement intensity (research-integrity offices, content moderation, metric audits) a defense of epistemic quality against misinformation, or protection of incumbent rents and gate control?',
    'Trace whether enforcement expansions correlate with demonstrated quality improvements or with revenue and authority retention; compare fields where enforcement rose without rent growth.',
    'A quality-defense reading supports the coordination framing; a rent-protection reading shifts weight toward the extraction side of the ledger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_trajectory_meaning, preference, 'Interpretive question over the enforcement ratchet''s driver.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lkb_cred_exp_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(lkb_cred_exp_tr_t0, observed).
narrative_ontology:measurement(lkb_cred_exp_tr_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(lkb_cred_exp_tr_t10, observed).
narrative_ontology:measurement(lkb_cred_exp_tr_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(lkb_cred_exp_tr_t20, observed).
narrative_ontology:measurement(lkb_cred_exp_tr_t30, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement_basis(lkb_cred_exp_tr_t30, observed).
narrative_ontology:measurement(lkb_cred_exp_tr_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement_basis(lkb_cred_exp_tr_t40, observed).
narrative_ontology:measurement(lkb_cred_exp_tr_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(lkb_cred_exp_tr_t50, observed).
narrative_ontology:measurement(lkb_cred_exp_tr_t60, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement_basis(lkb_cred_exp_tr_t60, observed).
narrative_ontology:measurement(lkb_cred_exp_tr_t70, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 70, 0.27).
narrative_ontology:measurement_basis(lkb_cred_exp_tr_t70, observed).
narrative_ontology:measurement(lkb_cred_exp_tr_t80, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement_basis(lkb_cred_exp_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(lkb_cred_exp_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(lkb_cred_exp_be_t0, observed).
narrative_ontology:measurement(lkb_cred_exp_be_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(lkb_cred_exp_be_t10, observed).
narrative_ontology:measurement(lkb_cred_exp_be_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(lkb_cred_exp_be_t20, observed).
narrative_ontology:measurement(lkb_cred_exp_be_t30, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement_basis(lkb_cred_exp_be_t30, observed).
narrative_ontology:measurement(lkb_cred_exp_be_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement_basis(lkb_cred_exp_be_t40, observed).
narrative_ontology:measurement(lkb_cred_exp_be_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 50, 0.37).
narrative_ontology:measurement_basis(lkb_cred_exp_be_t50, observed).
narrative_ontology:measurement(lkb_cred_exp_be_t60, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement_basis(lkb_cred_exp_be_t60, observed).
narrative_ontology:measurement(lkb_cred_exp_be_t70, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 70, 0.42).
narrative_ontology:measurement_basis(lkb_cred_exp_be_t70, observed).
narrative_ontology:measurement(lkb_cred_exp_be_t80, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 80, 0.45).
narrative_ontology:measurement_basis(lkb_cred_exp_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(lkb_cred_exp_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(lkb_cred_exp_su_t0, observed).
narrative_ontology:measurement(lkb_cred_exp_su_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement_basis(lkb_cred_exp_su_t10, observed).
narrative_ontology:measurement(lkb_cred_exp_su_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(lkb_cred_exp_su_t20, observed).
narrative_ontology:measurement(lkb_cred_exp_su_t30, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement_basis(lkb_cred_exp_su_t30, observed).
narrative_ontology:measurement(lkb_cred_exp_su_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(lkb_cred_exp_su_t40, observed).
narrative_ontology:measurement(lkb_cred_exp_su_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement_basis(lkb_cred_exp_su_t50, observed).
narrative_ontology:measurement(lkb_cred_exp_su_t60, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 60, 0.54).
narrative_ontology:measurement_basis(lkb_cred_exp_su_t60, observed).
narrative_ontology:measurement(lkb_cred_exp_su_t70, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 70, 0.58).
narrative_ontology:measurement_basis(lkb_cred_exp_su_t70, observed).
narrative_ontology:measurement(lkb_cred_exp_su_t80, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement_basis(lkb_cred_exp_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'expertise-based legitimate knowledge' decomposes into three readings of one kernel; this file instantiates the credentialed_expertise_reading. Epsilon differs by reading over the same referent: this reading authors moderate epsilon (net-coordination with acknowledged excess), the experiential_pluralism reading authors high epsilon (the gate suppresses valid knowledge), and the hybrid_coproduction reading authors intermediate epsilon. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
