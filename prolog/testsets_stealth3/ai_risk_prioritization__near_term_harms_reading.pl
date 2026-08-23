% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: Near-Term Harms Primacy Norm in AI Risk Prioritization
 *   domain: technology governance / epistemic / social
 *
 * SUMMARY:
 *   Within AI-safety governance a contested kernel — how the field should
 *   prioritize 'AI risk' — is read two ways. This file instantiates the
 *   near-term-harms reading as a clean, epsilon-invariant constraint: the
 *   operative norm that AI risk means the measurable discrimination, labor
 *   displacement, and surveillance inflicted by deployed systems on present
 *   populations, and that justice interventions (bias audits, worker
 *   protections, surveillance limits) are therefore paramount. Per the
 *   epsilon-invariance principle, the colloquial label 'AI risk' decomposes
 *   into two structurally distinct constraints: this reading's epsilon (0.58)
 *   is authored over the prioritization norm's actual operation — genuine
 *   remediation coordination carrying an enforced tax on a rival research
 *   program — while the sibling file authors its own epsilon over the
 *   existential reading's arrangement. The two are linked by
 *   network.affects_constraints; neither reading's epsilon contaminates the
 *   other's. KEY AGENTS (by structural relationship): -
 *   alignment_xrisk_researchers: Primary target (organized/identity_locked) —
 *   loses grants, venue standing, and policy access under the frame -
 *   marginalized_ai_harm_communities: Primary intended beneficiary
 *   (moderate/trapped) — receives directed audits, protections, and attention
 *   - fairness_accountability_researchers: Primary beneficiary and
 *   method-provider (organized/identity_locked) — careers and standing flow
 *   along the frame - civil_society_justice_orgs: Agenda setter
 *   (organized/constrained) — propagates the frame, collects grants and
 *   contracts - frontier_ai_developers: Payer with dual position
 *   (institutional/arbitrage) — absorbs compliance costs, banks social
 *   license - algorithmic_audit_consultancies: Incidental beneficiary
 *   (moderate/mobile) — sells the mandated assessments -
 *   ai_governance_policymakers: Codifying agenda setter
 *   (institutional/constrained) — decides which reading becomes statute
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.58).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.62).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term Harms Primacy Norm in AI Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "technology governance / epistemic / social").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '6c8bdb1f-763a-439d-8a24-c4d9db2db3ae').
narrative_ontology:cs_kernel_codification('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', distributed).
narrative_ontology:cs_authority_grounding('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', distributed).
narrative_ontology:cs_reading_relation('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', foundational, measurable_present_harms_take_priority).
narrative_ontology:cs_axiom_status(measurable_present_harms_take_priority, holdable).
narrative_ontology:cs_axiom_grounding('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', measurable_present_harms_take_priority, empirically_contingent).
narrative_ontology:cs_axiom('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', foundational, justice_interventions_are_paramount).
narrative_ontology:cs_axiom_status(justice_interventions_are_paramount, holdable).
narrative_ontology:cs_axiom_grounding('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', justice_interventions_are_paramount, deontological).
narrative_ontology:cs_axiom('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', secondary, speculative_long_horizon_risk_is_secondary).
narrative_ontology:cs_axiom_status(speculative_long_horizon_risk_is_secondary, holdable).
narrative_ontology:cs_axiom_grounding('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', speculative_long_horizon_risk_is_secondary, empirically_contingent).
narrative_ontology:cs_reference_frame('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', present_harms_primacy_framework).
narrative_ontology:cs_drift_state('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', contemporary_institutionalization_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6c8bdb1f-763a-439d-8a24-c4d9db2db3ae', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_ai_harm_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, civil_society_justice_orgs).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, alignment_xrisk_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, frontier_ai_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, frontier_ai_developers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, algorithmic_audit_consultancies).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__near_term_harms_reading, measurable_harm_actionability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Racialized workers, low-wage data-labeling and warehouse labor, and heavily policed neighborhoods: they are scored, filtered, monitored, and displaced by deployed systems. The prioritization norm sends audits, worker-protection rules, and surveillance limits toward their documented harms. They cannot opt out of being subjected to the systems; their leverage comes through organizing and testimony, and it is thin relative to the deployers they face.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_ai_harm_communities, beneficiary,
    moderate, biographical, trapped, global).

% University-center and industry-lab researchers who build the bias-audit methods, dataset documentation standards, and displacement studies the norm calls for. Grants, chairs, citations, and conference standing flow along the frame, and their career paths are built inside it. Leaving would mean abandoning the research program their training, networks, and publication records are made of.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    organized, biographical, identity_locked, global).

% Advocacy coalitions that run the campaigns, testify before legislatures, grade company disclosures, and channel community complaints into policy demands. They set much of the frame's agenda and also receive grants and consulting income from the interventions it produces. Their funding and membership are bound to the issue focus; dropping it would dissolve their organizational identity.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, civil_society_justice_orgs, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, civil_society_justice_orgs, beneficiary).

% Researchers at safety labs and mission-driven nonprofits working on alignment and long-horizon catastrophic risk. Under the norm their agenda is recast as speculative distraction: grant reviewers discount it, general-audience venues decline it, and policy attention routes elsewhere. Their commitment is worldview-deep — the work is defined as defending the long-term future — so exiting the frame's verdict would mean exiting the vocation itself.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, alignment_xrisk_researchers, payer,
    organized, civilizational, identity_locked, global).

% Large labs deploying the systems in question. They absorb audit, documentation, and workforce-transition compliance costs, and simultaneously bank the social license and procurement eligibility that visible compliance buys. When costs bite in one jurisdiction they can shift compute, corporate entities, or product launches toward permissive ones.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, frontier_ai_developers, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, frontier_ai_developers, beneficiary).

% Firms selling bias audits, impact assessments, and conformity documentation. Revenue scales with the breadth of mandated assessment rather than with measured harm reduction; they hold the lightest exit of anyone in the story and multiply wherever mandates spread.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, algorithmic_audit_consultancies, beneficiary,
    moderate, immediate, mobile, global).

% Legislators and agencies deciding which reading gets codified into audit mandates, worker protections, and surveillance limits versus frontier-model obligations. They hear both communities' testimony and allocate statutory attention; electoral cycles keep their horizon short and their commitments reversible.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_governance_policymakers, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves an attention-allocation problem: a young field with unlimited candidate worries and finite research, regulatory, and journalistic bandwidth needs a shared rule for what counts as the risk worth governing. The norm coordinates funders, regulators, and researchers on harms that are measurable now and victims who can testify.
% TRANSFER_FUNCTION: Moves research funding, regulatory attention, and moral urgency from long-horizon scenarios toward present-harm remediation; moves compliance spending from AI deployers to audit and assessment providers; moves career security and institutional standing toward justice-oriented researchers and away from alignment-focused ones.
% ABSENT_VOICES: Alignment researchers object from inside the conversation but are structurally discounted — heard, then filed as speculative. The communities the frame serves are invoked more than seated: participatory slots in the conferences and funder panels where the intervention package is designed are thin, so the audits-and-protections package is built largely without its supposed end-beneficiaries in the room. Future people — the sibling reading's constituency — have no seat anywhere in a 0-5 year frame.
% DISAPPEARANCE_RATIONALE: Funding portfolios, conference tracks, legislative agendas, and newsroom beats would re-sort around whichever frame filled the vacuum within a budget cycle or two; pending audit mandates would stall; alignment programs would regain standing without having to win the argument.
% FOUNDING_PROBLEM: In the late 2010s, AI governance attention was dominated by speculative superintelligence scenarios while documented, datable harms — racially biased risk scores, discriminatory ad delivery, exploitative data-labeling labor, pervasive biometric surveillance of already-policed communities — went unremedied. The reading was built to force the field to count those harms first.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties by investigative and empirical work not run by justice advocates: the ProPublica Machine Bias investigation, the Gender Shades audit team's peer-reviewed results, government and press investigations of data-labeling working conditions, and the surveillance-studies literature documenting biometric policing. These attest the founding harms were real and unaddressed before and independently of the frame's beneficiaries. No corroboration exists for the claim that the problem is now solved; on that status the benefiting parties speak alone.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   The claim and the metrics are authored independently. Structurally I read a hybrid: the norm solves a real attention-allocation problem with identifiable net beneficiaries, and the same gatekeeping machinery taxes a rival program — hence claimed_type tangled_rope with requires_active_enforcement true. Descriptively: extractiveness 0.58 because a large share of a fixed attention-and-funding pool is redirected and rival standing stripped, while a substantial fraction of the norm's operation is genuine remediation reaching real victims of deployed systems; suppression 0.62 because persistence depends on active gatekeeping (funder filters, venue norms, speculative-distraction rhetoric) rather than participant preference, though nothing bans the rival work; theater_ratio 0.28 because audits and protections mostly function while an ethics-washing share grows with corporate adoption of the language; accessibility_collapse 0.38 because hybrid framings and parallel funding survive — alternatives narrow but do not close; resistance 0.58 because the taxed program is well-resourced and pushes back continuously. The measurement series shares one grid (t = 0,5,10,15,20,25,30) across all three tracked metrics. The mild oscillation around the rising trend is cyclical, not noise: each publicized deployment harm (peaks at t5, t15, t25) re-hardens the frame and its gatekeeping; each capabilities milestone (dips at t10, t20) briefly lifts existential-risk salience and loosens it. The oscillation itself does extractive work — intermittent reinforcement keeps both constituencies donating attention to the fight. Suppression splits roughly 60 percent structural (funding and venue gatekeeping) and 40 percent internalized (anticipatory self-censorship), carried by omega suppression_mechanism_ambiguity. Base metrics were measured at t30, a post-scandal consolidation phase near a local peak of frame strength.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the alignment researcher's position the norm operates as enforced extraction: identity-locked exit means the speculative-distraction verdict attaches to the person, not just the proposal, and organized power cannot buy back venue standing. From the marginalized-community seat the same structure is overdue subsidy — attention finally routed to harms they cannot exit. Fairness researchers experience it as professional opportunity fused with mission; consultancies as demand; developers as a manageable, arbitrageable overhead that doubles as license. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them. Identity-lock runs on both researcher seats through professional-ideological fusion — the work is constituted as either remediating injustice or defending the long-term future — so were the frame to break, both seats would recompute toward more mobile exits and the constraint's enforcement cost would spike.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: marginalized communities (trapped, so no arbitrage damping, but they pay no costs into the norm — their d sits near the beneficiary pole), fairness researchers and advocacy organizations (collect standing and funding), consultancies (pure fee capture). Victim declarations drive high d: alignment researchers are identity-locked, placing them at the trapped end of the amplification curve — nearer the full-target pole than any other seat; frontier developers pay compliance but recoup license and can arbitrage jurisdictions, damping their d below the researchers'. Policymakers sit near symmetric: they spend attention and gain legislative credit roughly in balance. Global discourse scope raises verification difficulty, which the engine folds into effective extraction — hardest to verify exactly where the trapped targets sit. No directionality overrides were needed: beneficiary/victim data plus exit options derive every seat's d without residue.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid classification is what blocks both mislabels. Reading the norm as pure rope would erase the suppressed rival program — the same gatekeeping that concentrates attention on measurable harm strips alignment research of standing, and that asymmetry is the extraction half. Reading it as pure snare would erase the genuine function: the harms are documented, the victims are real, and the interventions reach them at least partly (omega benefit_delivery_vs_capture prices exactly how partly). Mandatrophy is not resolved: the founding problem — documented present harms outpacing field attention — remains live, corroborated by audit studies and labor investigations from outside the benefiting parties, so no sunset or atrophy claim is authored. Receipt surface: diverted funding and compliance fees land in identifiable seats — fairness_accountability_researchers as the institutional recipient of the diverted stream, with consultancies taking the compliance-fee flow — so gain_flow names a seat rather than asserting diffusion. Fixing is cheap relative to the benefit: rebalancing portfolios toward pluralism is an administrative act available to the same funders and legislatures who built the frame; what makes it rare is positional, not material, cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the ai_risk_prioritization kernel — the near_term_harms_reading. What exactly is at stake between it and the sibling existential_risk_reading?',
    'No dataset resolves a framing dispute; resolution proceeds by locating the disagreement. The sibling swaps the victim set (future generations for present marginalized populations), the allocation target (alignment research for bias audits and worker protections), and the timescale (decades-plus for 0-5 years). The dispute lives in the primacy axiom — whether ''primarily'' forces an exclusive ranking or merely weights.',
    'If ''primarily'' is read as compositional weighting, both readings can be jointly satisfied and the measured suppression of the rival program is political rather than logical; if it is read as exclusive ranking, the readings are mutually exclusive within one framework and the corpus should expect foreclosure dynamics between them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of the ai_risk_prioritization kernel; sibling reading and locus of disagreement recorded here rather than in standard fields.').

omega_variable(
    zero_sum_pool_question,
    'Do justice-intervention and alignment funding actually draw on the same pools, or are they largely complementary budgets?',
    'Longitudinal grant-level data across major AI-safety funders: track substitution elasticity between justice-style and alignment-style awards as earmarks shift.',
    'If the pools are mostly disjoint, the extraction this story measures against alignment researchers is overstated and the constraint sits closer to the rope end of the hybrid; if strongly substitutable, the resource rivalry is the enforcement object and the hybrid reading holds firmly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_sum_pool_question, empirical, 'Whether the two readings compete for one budget or run on parallel ones.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of alignment work structural (funder gatekeeping, venue norms, legislative framing) or internalized (anticipatory self-censorship by researchers who pre-concede the distraction label)?',
    'Post-exit trajectory: track the proposals and career moves of alignment researchers who move into frame-neutral institutions; if discounted-status behavior persists after gatekeeping contact ends, part of the suppression is internalized.',
    'Internalized suppression travels with the target after exit, raising effective suppression above the structural measure and hardening the target-seat classification; purely structural suppression would relax quickly if funders rebalanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized split of the suppression applied to the rival research program.').

omega_variable(
    benefit_delivery_vs_capture,
    'Do the norm''s interventions deliver to marginalized communities, or do the gains accrue to the audit-and-research complex that administers them?',
    'Follow the intervention receipts: outcome evaluations of deployed audits and protection regimes, disaggregated by whether measured harm actually fell for the communities named as beneficiaries.',
    'If delivery dominates, the declared beneficiaries are end-beneficiaries and the coordination half of the hybrid is genuine; if capture dominates, the named beneficiaries are conduits and the constraint slides toward the snare side of the hybrid with the administering complex as capturer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_delivery_vs_capture, empirical, 'Whether the frame''s gains reach its declared beneficiaries or are captured by its administrators.').

omega_variable(
    measurability_gatekeeping_assumption,
    'Is the measurable-harm standard an epistemic necessity for accountable governance, or a selection device that systematically excludes hard-to-measure risks — including the sibling reading''s?',
    'Conceptual analysis plus stress cases: examine whether risks with delayed, diffuse, or catastrophic-but-unmeasured profiles can be governed under any variant of the standard, and whether the standard''s proponents accept that limitation openly.',
    'If the standard is load-bearing exclusion, the reading''s empiricism doubles as its suppression mechanism and the foundational axiom weakens; if it is a genuine accountability requirement, the exclusion is a defensible trade-off and the axiom stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurability_gatekeeping_assumption, conceptual, 'Whether the reading''s measurability criterion is method or gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(near_term_harms_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(near_term_harms_tr_t0, observed).
narrative_ontology:measurement(near_term_harms_tr_t5, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement_basis(near_term_harms_tr_t5, observed).
narrative_ontology:measurement(near_term_harms_tr_t10, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(near_term_harms_tr_t10, observed).
narrative_ontology:measurement(near_term_harms_tr_t15, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(near_term_harms_tr_t15, observed).
narrative_ontology:measurement(near_term_harms_tr_t20, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(near_term_harms_tr_t20, observed).
narrative_ontology:measurement(near_term_harms_tr_t25, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement_basis(near_term_harms_tr_t25, observed).
narrative_ontology:measurement(near_term_harms_tr_t30, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(near_term_harms_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(near_term_harms_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(near_term_harms_be_t0, observed).
narrative_ontology:measurement(near_term_harms_be_t5, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 5, 0.49).
narrative_ontology:measurement_basis(near_term_harms_be_t5, observed).
narrative_ontology:measurement(near_term_harms_be_t10, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(near_term_harms_be_t10, observed).
narrative_ontology:measurement(near_term_harms_be_t15, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(near_term_harms_be_t15, observed).
narrative_ontology:measurement(near_term_harms_be_t20, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement_basis(near_term_harms_be_t20, observed).
narrative_ontology:measurement(near_term_harms_be_t25, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(near_term_harms_be_t25, observed).
narrative_ontology:measurement(near_term_harms_be_t30, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(near_term_harms_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(near_term_harms_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(near_term_harms_su_t0, observed).
narrative_ontology:measurement(near_term_harms_su_t5, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(near_term_harms_su_t5, observed).
narrative_ontology:measurement(near_term_harms_su_t10, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(near_term_harms_su_t10, observed).
narrative_ontology:measurement(near_term_harms_su_t15, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement_basis(near_term_harms_su_t15, observed).
narrative_ontology:measurement(near_term_harms_su_t20, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(near_term_harms_su_t20, observed).
narrative_ontology:measurement(near_term_harms_su_t25, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement_basis(near_term_harms_su_t25, observed).
narrative_ontology:measurement(near_term_harms_su_t30, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(near_term_harms_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI risk' covers two structurally distinct claims (epsilon-invariance decomposition): the near-term-harms prioritization norm (this file, epsilon 0.58, targets = the taxed rival research program and compliance-bearing deployers, beneficiaries = present-harm constituencies) and the existential-risk prioritization norm (sibling file, its own epsilon, by its lights targeting present-harm-framed spending and benefiting alignment research). Neither is cleanly upstream: they compete for the same attention and funding pool, so each structurally influences the other's resource environment. The affects_constraints edge models that rivalry, not citation dependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
