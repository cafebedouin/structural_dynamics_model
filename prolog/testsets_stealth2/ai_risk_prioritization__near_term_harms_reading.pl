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
 *   human_readable: Near-Term Harms Reading of AI Risk Prioritization
 *   domain: technology_governance/ai_safety/social_justice
 *
 * SUMMARY:
 *   This story instantiates the near_term_harms_reading of the
 *   ai_risk_prioritization kernel: the claim that AI risk consists primarily
 *   of measurable present harms — algorithmic discrimination in hiring,
 *   lending, and policing; automation-driven displacement of low-wage work;
 *   pervasive surveillance — and that justice interventions (bias audits,
 *   worker protections, surveillance regulation) are therefore paramount. As
 *   a constraint, the reading operates as a prioritization arrangement
 *   governing a finite pool of AI-safety resources: funding, research talent,
 *   regulatory bandwidth, media attention, and moral urgency. Its
 *   coordination function is real — it concentrates effort on documented
 *   injuries with identifiable victims — and its enforcement is real too: the
 *   rival long-horizon program must be continuously held down as 'speculative
 *   distraction,' a framing maintained through grant committees, editorial
 *   gatekeeping, and discourse norms. Constraint-family note
 *   (epsilon-invariance decomposition): 'AI risk prioritization' is a
 *   colloquial label covering two structurally distinct arrangements. This
 *   story authors epsilon for the near-term prioritization arrangement as
 *   this reading sees it; the sibling story
 *   (ai_risk_prioritization__existential_risk_reading) authors epsilon for
 *   the long-horizon prioritization arrangement by its own lights. Different
 *   victim sets, different timescales, different allocations — two files,
 *   linked via network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - marginalized_communities: Declared beneficiaries
 *   (organized/trapped) — receive audits, protections, and regulation; cannot
 *   exit the systems that harm them - fairness_accountability_researchers:
 *   Beneficiaries (organized/identity_locked) — careers and grant economies
 *   route through the near-term framing -
 *   civil_rights_advocacy_organizations: Beneficiaries
 *   (organized/constrained) — litigate and mobilize on the near-term harm
 *   record - alignment_safety_researchers: Primary payers
 *   (organized/identity_locked) — their program is delegitimized and crowded
 *   out through the same allocation structure - ai_developing_labs:
 *   Dual-positioned payers/beneficiaries (institutional/arbitrage) — bear
 *   compliance costs, gain relief from heavier frontier mandates -
 *   philanthropic_and_public_funders: Agenda-setters (institutional/mobile) —
 *   operationalize the prioritization in portfolios -
 *   global_south_data_workers: Excluded voice (powerless/trapped) — bear
 *   production-side harms the deployment-focused framing rarely reaches -
 *   policy_regulators: Observers (institutional/analytical) — arbitrate
 *   bandwidth between the two programs
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.55).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.6).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term Harms Reading of AI Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "technology_governance/ai_safety/social_justice").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, 'ce4a5043-eb54-4dd9-a794-d814bbe6c966').
narrative_ontology:cs_kernel_codification('ce4a5043-eb54-4dd9-a794-d814bbe6c966', distributed).
narrative_ontology:cs_authority_grounding('ce4a5043-eb54-4dd9-a794-d814bbe6c966', distributed).
narrative_ontology:cs_reading_relation('ce4a5043-eb54-4dd9-a794-d814bbe6c966', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('ce4a5043-eb54-4dd9-a794-d814bbe6c966', foundational, present_measurable_harms_command_paramount_response).
narrative_ontology:cs_axiom_status(present_measurable_harms_command_paramount_response, holdable).
narrative_ontology:cs_axiom_grounding('ce4a5043-eb54-4dd9-a794-d814bbe6c966', present_measurable_harms_command_paramount_response, deontological).
narrative_ontology:cs_axiom('ce4a5043-eb54-4dd9-a794-d814bbe6c966', foundational, unverifiable_long_horizon_scenarios_cannot_anchor_primary_allocation).
narrative_ontology:cs_axiom_status(unverifiable_long_horizon_scenarios_cannot_anchor_primary_allocation, holdable).
narrative_ontology:cs_axiom_grounding('ce4a5043-eb54-4dd9-a794-d814bbe6c966', unverifiable_long_horizon_scenarios_cannot_anchor_primary_allocation, empirically_contingent).
narrative_ontology:cs_reference_frame('ce4a5043-eb54-4dd9-a794-d814bbe6c966', documented_present_harm_primacy).
narrative_ontology:cs_drift_state('ce4a5043-eb54-4dd9-a794-d814bbe6c966', post_frontier_capability_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ce4a5043-eb54-4dd9-a794-d814bbe6c966', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, alignment_safety_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, ai_developing_labs).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, ai_developing_labs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Racialized and low-wage workers, tenants, and policed residents bear hiring-algorithm rejection, automated benefit denials, predictive-policing surveillance, and automation-driven displacement. The near-term framework channels bias audits, worker protections, and surveillance limits toward these injuries. They cannot exit the labor markets, housing markets, or streets where the systems operate, so their recourse runs entirely through the interventions this framework prioritizes.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, beneficiary,
    organized, biographical, trapped, national).

% Run audits, document disparate impact, and build accountability tooling for deployed systems. Grant portfolios, faculty lines, consultancy contracts, and the conference economy route through the near-term framing; their methods presume measurable, present-world injury. Pivoting to long-horizon speculative work would strand their expertise, datasets, networks, and publication records, so their professional position is bound to the framework continuing to define the field's subject matter.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    organized, biographical, identity_locked, global).

% Litigate, lobby, and mobilize members using the documented record of algorithmic discrimination and surveillance. Their funding streams and membership energy attach to concrete, provable cases; the near-term framing keeps justice interventions at the top of the AI-governance agenda, which is the terrain where these organizations win. A shift of the agenda to long-horizon scenarios would leave their case inventory and donor base mismatched to the new priorities.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, constrained, national).

% Work on long-horizon misalignment and catastrophic capability risk in advanced systems. Under the near-term framing their research program is classified as speculative: grant committees deprioritize it, editorial venues treat it as science fiction, hiring pipelines steer students toward measurable-harm work, and public commentators dismiss it as distraction. Some individuals pivot to adjacent empirical work, but the core of the community is professionally and ideologically committed to the long-horizon problem and cannot relocate without abandoning the premise of their careers.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, alignment_safety_researchers, payer,
    organized, generational, identity_locked, global).

% Bear the framework's compliance costs: bias audits, documentation duties, worker-transition obligations, surveillance-use restrictions. Simultaneously they benefit from the same framing, because it forecloses the heavier frontier-capability mandates the rival reading would impose, and funded ethics teams supply reputational cover. They can shift operations across jurisdictions, shape the framing through sponsored research and ethics initiatives, and time compliance investments to regulatory cycles.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_developing_labs, payer,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, ai_developing_labs, beneficiary).

% Set the grant portfolios and public research programs that operationalize the prioritization: weighting bias audits, worker-transition programs, and surveillance regulation on one side against long-horizon safety research on the other. They can rebalance at will, but face organized advocacy pressure from both camps, legislative scrutiny of public funds, and the reputational risk of backing work that either side calls wasted.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, philanthropic_and_public_funders, agenda_setter,
    institutional, generational, mobile, global).

% Annotate training data, moderate content, and perform the invisible labor upstream of deployed systems, under piece-rate precarity and exposure to traumatic material. The near-term framing scopes harm to deployment in wealthy markets — biased outputs, displaced jobs, surveilled users — while their production-side injuries fall outside the audit categories and jurisdictional reach of the framework's interventions. They would object that 'present harm' is being defined to exclude them, but they hold no seat in the venues where the framing is maintained.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, global_south_data_workers, excluded,
    powerless, biographical, trapped, global).

% Allocate inspection capacity, rulemaking agendas, and enforcement bandwidth between bias-audit regimes and frontier-model oversight. They take testimony from all camps, commission economic and technical analysis, and their bandwidth decisions are the zero-sum surface on which the two readings compete. They hold no stake in either framing's victory but determine how much of each program becomes enforceable practice.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, policy_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: without a shared prioritization, AI-governance attention scatters across speculative and concrete concerns alike, and scarce regulatory bandwidth, funding, and advocacy capacity fail to reach documented injuries. The framework concentrates effort on measurable present harms with identifiable victims — bias audits, worker protections, surveillance limits — where evidence, remedy, and accountability are all tractable.
% TRANSFER_FUNCTION: Moves funding, research talent, policy attention, and moral urgency from long-horizon speculative-risk mitigation toward present-harm remediation; moves compliance costs onto deployers of discriminatory and surveillant systems; and moves the institutional rents of the safety portfolio (grants, chairs, consultancies, convening power) toward the fairness-and-accountability research and advocacy complex.
% ABSENT_VOICES: Alignment and long-horizon safety researchers are present in the discourse but structurally discounted — their objections arrive pre-classified as 'speculative distraction,' so their participation does not function as representation. Future populations who would bear deferred catastrophic risk have no seat at all and no proxy with standing in the framework's evidential standards. Global South data workers bear production-side harms the deployment-scoped framing never reaches. Each absence is load-bearing: the unanimity that present harms are THE risk is easier to maintain when the seats that would price the omitted risks are outside the room.
% DISAPPEARANCE_RATIONALE: If the near-term prioritization vanished overnight, the justice coalition would lose its organizing claim: bias-audit mandates would lose their justification, worker-protection and surveillance-regulation bills would lose their framing, and advocacy organizations would face a mismatched agenda. The vacated resource pool would not sit idle — the rival long-horizon program would expand to fill it, shifting the entire AI-governance apparatus toward capability-risk management. Deployed-system harms would continue but lose their dedicated remediation machinery. The arrangement's disappearance rearranges who is protected, on what timescale, at whose expense.
% FOUNDING_PROBLEM: The reading was built to force institutions to count present, measurable harms. In the mid-2010s, deployed systems were rejecting qualified renters and job applicants by race, automating benefit cuts, and enabling mass biometric surveillance, while public imagination and elite attention fixated on distant science-fiction scenarios; affected people had no accountability machinery, and 'AI safety' as a funding category did not yet recognize their injuries as safety issues at all.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated extensively from outside the benefiting parties: peer-reviewed audit studies of hiring and facial-recognition systems (including independent NIST testing showing demographic error disparities), labor-economics research on automation-driven displacement, litigation records and regulatory findings on algorithmic tenant screening and benefit automation, and investigative journalism documenting surveillance deployments. None of these sources depends on the fairness-research or advocacy establishments for its findings; the founding problem's liveness is attested by courts, standards bodies, and affected workers' own testimony.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.55: the arrangement transfers a substantial share of the safety resource pool away from the rival research program — grant deprioritization, editorial dismissal, student-flow diversion — while its primary flow remains genuine remediation, so extraction is significant but not dominant. Suppression 0.60: persistence depends on actively maintaining the 'speculative distraction' classification of the rival program; suppression here is a raw structural property of the discursive regime and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater ratio 0.34 and rising: bias audits increasingly serve reputational and compliance-signaling functions alongside real remediation (ethics-washing is documented across the industry), but a majority of intervention activity remains functional. Accessibility collapse 0.30: the alternative framing remains fully live — long-horizon work retains lab funding, prominent public advocates, and state-level uptake — so alternatives are far from collapsed. Resistance 0.60: the rival reading actively contests, producing the observable framing war the suppression series tracks. The measurement series run on one shared time grid (2016-2024, six points, all three metrics authored at every point) so no metric's end-state value is silently substituted into earlier rows. Receipt surface: gain_flow names fairness_accountability_researchers because the institutional rents of the diverted resources — grant portfolios, chairs, consultancies, the conference economy — demonstrably concentrate in that seat, while marginalized communities receive service delivery without capturing the rents; this is an affirmative checked claim, not a default. fixing_cost is 'cheap': funders and laboratories could operate both programs in parallel at material cost small relative to the stakes; the binding obstacle is framing legitimacy, not money — which is why the arrangement persists despite affordable alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the alignment_safety_researchers seat, the arrangement operates as enforced delegitimation: their problem is ruled out of bounds, their grants dry up, their students are advised away — a high-extraction experience with identity-locked exit. From the marginalized_communities and advocacy seats, the same arrangement is overdue coordination on injuries they live with daily — low extraction, genuine benefit. From the ai_developing_labs seat, it is a favorable bargain: compliance costs are real but cheaper than the frontier-capability mandates the rival reading would impose, and funded ethics teams buy legitimacy. From the funders' seat, it is portfolio management under advocacy pressure from both directions. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. marginalized_communities, fairness_accountability_researchers, and civil_rights_advocacy_organizations sit at the beneficiary end (low d): the arrangement subsidizes them, and their trapped or identity-locked positions deepen the subsidy's reliability. alignment_safety_researchers sit near the full-target end (high d): they bear the transfer through the same structure, and their identity lock removes arbitrage-grade exit. ai_developing_labs occupy a genuinely dual position — payer on compliance costs, beneficiary on deflected frontier regulation — placing them nearer the middle; the derivation handles this from the declared secondary role, so no directionality override is needed. No overrides are authored: the beneficiary/victim plus exit data produce the correct directionalities, and an override keyed to the institutional power atom would wrongly flatten the funders and regulators who share that atom but not the labs' dual position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: discrimination, displacement, and surveillance by deployed systems are ongoing and measurable, so the arrangement has not outlived its function and no mandatrophy is declared. The classification work here cuts both ways. Reading the arrangement as pure rope would erase the real cost it imposes on the rival program and on the unrepresented future; reading it as pure snare would erase the documented injuries of present victims and hand the resource pool to a program whose harms are, by this reading's lights, speculative. Tangled rope holds both facts: genuine coordination for identifiable victims, asymmetric extraction from a program outside the beneficiary frame, sustained by active enforcement. The mismatch consumer should note that founding_problem_status is live and disappearance_verdict is world_rearranges — no zombie flag fires — but the rising suppression_requirement series is the early-warning signature: if the founding problem were ever substantially remediated while the framing war continued to escalate, the arrangement would be drifting toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the ai_risk_prioritization kernel — the near_term_harms_reading. What exactly would the sibling reading (existential_risk_reading) change structurally, and where is the disagreement located?',
    'Comparative analysis of the two reading-stories: the sibling instantiates a different victim set (future humanity rather than present marginalized populations), a different timescale (decades-plus rather than 0-5 years), and a different resource allocation (alignment research and compute governance rather than bias audits, worker protections, surveillance regulation). The disagreement is located in the primacy ordering and in the evidential standards for counting future, unverifiable risks against present, measurable ones.',
    'If the primacy claim is read as lexical (only present harms count) rather than weighted (both count, present first), this reading forecloses the sibling within a strict evidentialist framework and the classification hardens toward snare dynamics against the rival program; if weighted, the two readings coexist and the contest is a resource-share dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling reading changes victim set, timescale, and allocation.').

omega_variable(
    crowding_out_vs_triage,
    'Is the diversion of funding, talent, and attention away from long-horizon safety research a genuine extraction operating through the prioritization structure, or the ordinary opportunity cost of any defensible triage between competing claims?',
    'Counterfactual portfolio analysis: compare total AI-safety funding levels and composition before and after the near-term framework consolidated, controlling for overall field growth; test whether long-horizon work declined beyond what merit-neutral allocation would predict, using grant-committee records and career-flow data.',
    'If composition merely shifted while the total pie grew, the arrangement trends toward rope and the measured extraction is largely benign reallocation; if long-horizon work was actively crowded out below its counterfactual level, the tangled_rope reading strengthens and snare dynamics at the margin become plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_vs_triage, empirical, 'Whether the rival program''s losses are extraction through the structure or legitimate triage.').

omega_variable(
    audit_theater_share,
    'What share of bias-audit and ethics-review activity is compliance theater (checkbox audits, ethics-washing, reputational shielding) versus functional remediation that reduces measured disparities?',
    'Longitudinal outcome tracking: measure disparity reduction in audited hiring, lending, and policing systems after audit completion, against the volume of audit reports, ethics-board memberships, and consultancy activity.',
    'A rising theater share within the near-term portfolio would indicate Goodhart drift inside the reading''s own intervention set, pushing piton symptoms upward and weakening the coordination half of the tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_theater_share, empirical, 'Functional versus performative share of the reading''s flagship interventions.').

omega_variable(
    suppression_structural_or_internalized,
    'Is the discounting of long-horizon safety work structural (grant gatekeeping, editorial lines, hiring filters) or internalized (researchers self-censor, students avoid the field believing it disreputable, labs quietly defund it without stated reasons)?',
    'Post-barrier trajectory: if long-horizon research output and career entry recover when explicit gatekeeping is removed (e.g., dedicated funding streams appear), the residual gap measures the internalized component.',
    'If substantially internalized, the effective suppression exceeds the structural measure — the rival program carries the discount with it even after formal neutrality is restored, and removal of visible barriers would overstate the fix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural versus internalized mechanism of the rival program''s suppression.').

omega_variable(
    beneficiary_capture_ambiguity,
    'Do marginalized communities receive substantive remediation from the near-term intervention portfolio, or do institutional intermediaries (research groups, consultancies, advocacy organizations) capture the gains while underlying disparities persist?',
    'Compare longitudinal disparity trajectories in audited domains against growth in intermediary-sector employment, grant volume, and conference economies serving the fairness and accountability field.',
    'If capture dominates, the gain_flow seat shifts toward the intermediary complex, the coordination function weakens relative to its cover-story value, and the arrangement''s classification moves toward snare flavor despite its justice framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_ambiguity, empirical, 'Whether declared beneficiaries receive the goods or intermediaries capture them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 2016, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(near_term_harms_tr_t2016, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement_basis(near_term_harms_tr_t2016, observed).
narrative_ontology:measurement(near_term_harms_tr_t2018, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement_basis(near_term_harms_tr_t2018, observed).
narrative_ontology:measurement(near_term_harms_tr_t2020, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement_basis(near_term_harms_tr_t2020, observed).
narrative_ontology:measurement(near_term_harms_tr_t2022, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2022, 0.26).
narrative_ontology:measurement_basis(near_term_harms_tr_t2022, observed).
narrative_ontology:measurement(near_term_harms_tr_t2023, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2023, 0.3).
narrative_ontology:measurement_basis(near_term_harms_tr_t2023, observed).
narrative_ontology:measurement(near_term_harms_tr_t2024, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2024, 0.34).
narrative_ontology:measurement_basis(near_term_harms_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(near_term_harms_be_t2016, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2016, 0.3).
narrative_ontology:measurement_basis(near_term_harms_be_t2016, observed).
narrative_ontology:measurement(near_term_harms_be_t2018, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2018, 0.36).
narrative_ontology:measurement_basis(near_term_harms_be_t2018, observed).
narrative_ontology:measurement(near_term_harms_be_t2020, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement_basis(near_term_harms_be_t2020, observed).
narrative_ontology:measurement(near_term_harms_be_t2022, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2022, 0.48).
narrative_ontology:measurement_basis(near_term_harms_be_t2022, observed).
narrative_ontology:measurement(near_term_harms_be_t2023, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2023, 0.52).
narrative_ontology:measurement_basis(near_term_harms_be_t2023, observed).
narrative_ontology:measurement(near_term_harms_be_t2024, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2024, 0.55).
narrative_ontology:measurement_basis(near_term_harms_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(near_term_harms_su_t2016, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2016, 0.32).
narrative_ontology:measurement_basis(near_term_harms_su_t2016, observed).
narrative_ontology:measurement(near_term_harms_su_t2018, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2018, 0.36).
narrative_ontology:measurement_basis(near_term_harms_su_t2018, observed).
narrative_ontology:measurement(near_term_harms_su_t2020, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement_basis(near_term_harms_su_t2020, observed).
narrative_ontology:measurement(near_term_harms_su_t2022, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2022, 0.5).
narrative_ontology:measurement_basis(near_term_harms_su_t2022, observed).
narrative_ontology:measurement(near_term_harms_su_t2023, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2023, 0.56).
narrative_ontology:measurement_basis(near_term_harms_su_t2023, observed).
narrative_ontology:measurement(near_term_harms_su_t2024, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement_basis(near_term_harms_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% 'AI risk prioritization' decomposes under the epsilon-invariance principle into two structurally distinct constraint stories: this near-term-harms arrangement (victims: present marginalized populations; timescale 0-5 years; allocation to bias audits, worker protections, surveillance regulation) and the sibling existential-risk arrangement (victims: future populations; timescale decades-plus; allocation to alignment research and compute governance). Each file authors its own epsilon for its own arrangement by its own reading's lights; neither hedges across readings. The upstream/downstream coupling runs in both directions rhetorically — each side cites the other's blind spots — but the resource-pool dependency is the structural edge recorded here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
