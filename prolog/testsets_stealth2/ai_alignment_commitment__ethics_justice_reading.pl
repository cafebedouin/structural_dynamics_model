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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: Ethics-Justice Reading of the AI Alignment Commitment: Present-Harm Prevention Standard
 *   domain: technology governance/ethics
 *
 * SUMMARY:
 *   A governing standard has consolidated across AI development under which
 *   the commitment that systems must be 'aligned' is operationalized as the
 *   prevention of reproduced social bias and demonstrable present-day harm.
 *   Fairness metrics, bias audits, disparate-impact documentation, model
 *   cards, and deployment gates define what responsible practice means;
 *   funding panels, hiring plans, procurement requirements, and reputational
 *   enforcement flow through that apparatus. The standard protects a real
 *   constituency — communities scored, ranked, and filtered by algorithmic
 *   systems in lending, hiring, housing, benefits, and policing — and
 *   sustains the audit industry, fairness research programs, and advocacy
 *   organizations that administer and enforce it. Its costs concentrate on a
 *   different seat: research programs aimed at long-term control and
 *   catastrophic-risk reduction, whose funding share, hiring priority, and
 *   definitional authority have contracted as the present-harm standard
 *   matured from voluntary audits into statutory conformity regimes. This
 *   story authors one reading of a decomposed kernel and one ε for that
 *   reading's arrangement; the family structure is documented in
 *   kernel_context and the network note. KEY AGENTS (by structural
 *   relationship): - long_term_safety_researchers: primary target
 *   (moderate/identity_locked) — bears the agenda, funding, and definitional
 *   costs of the present-harm standard - marginalized_decision_subjects:
 *   primary protected constituency (powerless/trapped) — beneficiary seat
 *   with indirect cost exposure through deployment gates -
 *   algorithmic_fairness_research_community: agenda-setter and professional
 *   rent collector (organized/mobile) — administers the metrics, audits, and
 *   benchmarks - civil_rights_advocacy_organizations: enforcement driver and
 *   beneficiary (organized/mobile) — converts audit findings into legal and
 *   procurement pressure - ai_development_labs: dual-positioned institutional
 *   actor (institutional/arbitrage) — pays compliance, collects legitimacy
 *   and a tractable responsibility narrative - ai_regulatory_agencies:
 *   analytical observer (institutional/analytical) — investigates, compels
 *   disclosure, can alter enforcement - future_generations: structurally
 *   absent party (powerless/trapped) — bears stored risks, holds no seat
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.55).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.58).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "Ethics-Justice Reading of the AI Alignment Commitment: Present-Harm Prevention Standard").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "technology governance/ethics").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, '9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c').
narrative_ontology:cs_kernel_codification('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', distributed).
narrative_ontology:cs_authority_grounding('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', expertise).
narrative_ontology:cs_interpretation_layer_present('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c').
narrative_ontology:cs_reading_relation('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', ai_alignment_commitment__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', foundational, demonstrated_harm_moral_priority).
narrative_ontology:cs_axiom_status(demonstrated_harm_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', demonstrated_harm_moral_priority, empirically_contingent).
narrative_ontology:cs_axiom('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', secondary, fairness_metrics_operationalize_justice).
narrative_ontology:cs_axiom_status(fairness_metrics_operationalize_justice, holdable).
narrative_ontology:cs_axiom_grounding('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', fairness_metrics_operationalize_justice, conventional).
narrative_ontology:cs_reference_frame('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', demonstrated_harm_prevention_baseline).
narrative_ontology:cs_drift_state('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', contemporary_capability_scaling_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9cd7c59c-5aa3-4d9c-be93-ae92f1fdcb4c', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_decision_subjects).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, algorithmic_fairness_research_community).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ai_development_labs).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, marginalized_decision_subjects).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_development_labs).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, algorithmic_systems_reproduce_structural_bias).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, disparate_impact_doctrine).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, demonstrated_harm_prioritization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are scored, ranked, and filtered by algorithmic systems in lending, hiring, housing, benefits, and policing. The present-harm standard gives them a recognized seat: audits, disparate-impact findings, and deployment gates respond to harms they document. They cannot opt out of algorithmically mediated decisions, and where fairness gates delay or refuse services — credit tools, benefits systems, or medical triage models held back pending bias review — they bear those delays and refusals too.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_decision_subjects, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, marginalized_decision_subjects, payer).

% Work on control, corrigibility, and catastrophic-risk reduction in AI systems. Under the present-harm standard their agenda is framed as speculative relative to demonstrated harms: funding panels weight fairness audits and bias mitigation, hiring plans prioritize fairness-ML skills, and the term 'alignment' in policy and press increasingly denotes bias-prevention. Their work persists in dedicated institutes and a smaller grant ecosystem. Leaving the agenda would mean abandoning the research program their careers and professional identities are built around; staying means competing for a shrinking share of alignment-labeled resources.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers, payer,
    moderate, civilizational, identity_locked, global).

% Define the fairness metrics, audit methodologies, and benchmarks through which the standard operates. They run audits, publish disparate-impact findings, staff ethics boards and standards committees, and train the workforce the audit industry employs. Grants, citations, consultancies, and professional standing flow through the apparatus they administer, and they move readily between academia, corporate responsibility teams, and regulatory advisory roles.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, algorithmic_fairness_research_community, agenda_setter,
    organized, biographical, mobile, global).

% Litigate, campaign, and shape procurement and statutory requirements around documented algorithmic discrimination. The standard gives them a concrete, measurable object for accountability claims and a steady stream of findings to act on; membership, funding, and negotiating leverage track their audit-driven wins. They convert audit findings into complaints, litigation, and legislative pressure.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, civil_rights_advocacy_organizations, agenda_setter).

% Build and deploy the systems the standard governs. They fund responsible-AI teams, publish fairness evaluations, and submit to audits; in exchange they collect reputational capital, procurement eligibility, and a tractable public definition of responsibility that their communications can demonstrate. They bear compliance costs — evaluation overhead, deployment delays, redesign — and can relocate operations, rebrand programs, or shape the standards themselves through the same influence that makes them powerful.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_development_labs, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, ai_development_labs, payer).

% Investigate algorithmic discrimination, run conformity-assessment regimes, and take testimony from the other seats. They can impose remedies — audit mandates, documentation requirements, deployment suspensions — that alter how the standard binds developers, and they commission the economic and technical analysis the disputes turn on.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_regulatory_agencies, observer,
    institutional, generational, analytical, continental).

% Bear whatever risks present-day deployment choices store up and hold no seat in any standards body, funding panel, or audit regime. Within this reading's institutions their interests enter only as arguments made on their behalf, which the standard weights as speculative against demonstrated present harms.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__ethics_justice_reading, algorithmic_fairness_research_community).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__ethics_justice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of holding AI development accountable for discriminatory outcomes: without a shared standard of alignment-relevant harm, each deployment's fairness would need renegotiation from first principles. The reading supplies common fairness metrics, audit methodologies, documentation requirements, and deployment gates through which developers, auditors, regulators, and affected communities coordinate, and through which advocacy and enforcement attach to concrete findings.
% TRANSFER_FUNCTION: Moves funding, hiring slots, institutional attention, and definitional authority toward present-harm measurement and mitigation — audit firms, fairness research programs, advocacy organizations, and corporate responsibility teams — and away from long-term control research. It also moves accountability costs (evaluation overhead, deployment delays, redesign, reputational exposure) onto AI developers, and delivers protection plus a recognized claims-making seat to communities subject to algorithmic decisions.
% ABSENT_VOICES: Future generations, who bear whatever risks present deployment choices store up, have no seat in any standards body, funding panel, or audit regime; their interests enter only as arguments made on their behalf, which the standard weights as speculative. The control-focused wing of the safety research community holds few seats on fairness-oriented panels and benchmark committees; within this reading's institutions the definition of alignment-relevant harm was set largely without them. Both absences are structural: the standard's evidentiary bar — demonstrated, quantifiable, present — is one these constituencies cannot meet by construction.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, fairness audits, bias benchmarks, and procurement gates would lose their shared warrant; the audit industry and fairness research programs would contract sharply; advocacy organizations would lose their primary measurable accountability lever; developers would face unstructured, ad hoc accountability demands; and long-term safety research would regain agenda-space, funding share, and definitional authority. The field's responsibility architecture would reorganize around whichever framing filled the vacuum — every named party's arrangements depend on its specific shape.
% FOUNDING_PROBLEM: Algorithmic decision systems were demonstrably reproducing social bias at scale — discriminatory lending, hiring, housing, benefits, and policing outcomes — with no shared standard, methodology, or accountability mechanism for detecting, documenting, and correcting the harm.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: independent audit studies and academic replications continue to find disparate impact across lending, hiring, and facial-recognition deployments; regulatory enforcement actions and investigative journalism document ongoing algorithmic discrimination. The long-term safety research community — outside this reading's beneficiary set — also attests the founding problem is live while contesting the priority claim built on it, arguing that demonstrated present harms are real but that the reading's resource allocation over-weights them relative to catastrophic risk. No party with standing disputes that the founding problem exists; the contest is over what follows from it.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate-to-substantial (0.55 at interval end): the standard's protective function is real, but its operation diverts a growing share of alignment-labeled funding, hiring, and definitional authority from long-term control research to the present-harm apparatus, and imposes compliance costs on developers. Suppression (0.58) is structural, not violent: it operates through funding criteria, publication and review norms, procurement gates, and reputational enforcement against actors who deprioritize fairness — the alternative framing persists but pays institutional rent to do so. Theater (0.40) reflects documented ethics-washing: principles documents, advisory boards without authority, and audits that change nothing, alongside audits that genuinely alter deployments. Accessibility collapse (0.45): alternatives do not fully collapse — the safety-control research program remains live and publishable — but within the institutional meaning of 'alignment' the fairness definition is entrenched. Resistance (0.55): the control-research community actively contests the definitional claim and lobbies funders; some developers resist compliance scope. The three measurement series run on one shared time grid (t = 0,4,8,12,16,20, mapping to roughly 2016–2026) so every metric is authored at every examined time point. A suppression_requirement series is authored because this story specifically traces enforcement-capacity change: voluntary audits matured into procurement requirements and then statutory conformity assessments — a genuine enforcement build-out, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats compute differently from the same structure. From the long-term safety researcher's position the standard operates as agenda foreclosure: the label 'alignment' — and the resources attached to it — is defined around someone else's harm profile, and their exit is identity-fused, since leaving means abandoning the research program that constitutes their professional self. From the protected communities' position the same structure is the first accountability mechanism with real teeth in algorithmic decision-making. The fairness research community and advocacy organizations sit inside the apparatus they administer and drive, so the arrangement reads as their professional and moral home. The labs, with arbitrage-grade exit, experience it as manageable compliance that doubles as reputational cover. Same nominal field, different constraint: fairness researchers and safety researchers hold similar standing in AI research, but the standard gives one seat the metrics, the panels, and the benchmark authority, and the other a defensive grant category — constraint-specific factors, not global power, differentiate their exits.
 *
 * DIRECTIONALITY LOGIC:
 *   The structural declarations map cleanly to directionality. Protected communities are declared beneficiaries (d near the beneficiary end) with an indirect cost channel captured by their secondary payer role — fairness gates can delay services those very communities need. The fairness research community and advocacy organizations benefit through the apparatus they run and drive; the labs are net beneficiaries with real compliance costs (secondary payer, arbitrage exit keeps d low). Long-term safety researchers are the declared victims with identity-locked exit, which pins their directionality near the full-target end and amplifies effective extraction for that seat — this is where the reading's extractiveness concentrates, per its structural delta. No directionality overrides were needed: beneficiary/victim declarations plus exit options produce the right d values. Future generations are authored as excluded — an authored absence must not drive classification, so they feed the consensus-provenance question, not the arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — algorithmic systems reproducing social discrimination at scale without accountability — is still live: audits continue to find disparate impact across sectors, so the mandate has not outlived its function and no mandatrophy is declared. The tangled-rope structure is what prevents mislabeling in both directions: reading the arrangement as pure extraction would erase the real protection the standard delivers to its intended beneficiaries; reading it as pure coordination would erase the asymmetric, enforcement-dependent diversion of resources from the long-term-safety seat. The founding_problem_status (live) paired with disappearance_verdict (world_rearranges) is a matched pair — no capture/zombie mismatch fires. The temporal series shows the drift to watch: theater_ratio rising toward 0.4 as the apparatus matures is the early signature of the coordination function decaying into compliance performance, and base_extractiveness climbing monotonically tracks the enforcement ratchet documented in the suppression_requirement series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story is one reading (ethics_justice_reading) of the kernel ai_alignment_commitment — how would instantiating a sibling reading change the structural data?',
    'Not resolvable within this story: the sibling readings (safety_control_reading, integrated_reading) are separate constraints with their own ε, beneficiary/victim sets, and stakeholders. Resolution happens at the framework level by comparing the family''s stories; the disagreement is located in the definition of ''alignment'' itself — what the commitment''s content is.',
    'Classification is per-reading and must not be merged: averaging this reading''s ε with the safety_control_reading''s would produce a number that is the ε of no actual arrangement. The victim set flips across the family — this reading extracts from long-term safety research; the safety reading extracts from present-harm mitigation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; siblings are separate constraints.').

omega_variable(
    certainty_asymmetry_stability,
    'Is the empirical certainty asymmetry this reading rests on — demonstrated present harms versus speculative catastrophic risk — stable, or is it eroding as capability evidence accumulates?',
    'Track capability-evidence trajectories, near-miss documentation, and expert-forecast accuracy over time, and compare against the reading''s foundational axiom that speculative risks cannot ground alignment obligations.',
    'Erosion of the asymmetry undermines the foundational axiom, shifting resources and legitimacy back toward control research and moving this reading''s arrangement toward the safety_control_reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certainty_asymmetry_stability, empirical, 'Stability of the demonstrated-versus-speculative certainty asymmetry underpinning the reading.').

omega_variable(
    crowding_out_attribution,
    'How much of the long-term safety research resource deficit is caused by this reading''s institutional dominance, versus independent factors such as talent scarcity, lab commercial incentives, and funder risk-aversion?',
    'Funding-flow counterfactuals: grant-panel composition over time, alignment-labeled budget shares by subfield, and hiring-plan analysis before and after fairness-mandate regimes.',
    'If crowding-out is mostly attributable to this standard, extraction from the long-term-safety seat is high and the tangled character is confirmed; if mostly independent, this constraint''s extractiveness drops toward coordination-cost levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_attribution, empirical, 'Attribution of the safety-research resource deficit to this reading''s dominance.').

omega_variable(
    fairness_gate_incidence,
    'Do fairness deployment gates impose net costs on the protected communities themselves — delayed access, over-refusal, conservative defaults — that partially offset the bias-harm reduction they purchase?',
    'Deployment-delay audits and post-gate error-rate analysis disaggregated by protected class; compare service access before and after gate regimes in matched deployments.',
    'If net-negative for protected communities, the primary beneficiary seat''s position shifts toward bearing costs, weakening the coordination claim and raising measured extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fairness_gate_incidence, empirical, 'Whether the standard''s costs partially land on its intended beneficiaries.').

omega_variable(
    audit_functionality_share,
    'What share of fairness-audit and responsible-AI activity is functionally protective versus performative compliance (ethics washing)?',
    'Track whether audits change deployments: remediation rates, post-audit deployment modifications, and repeat-findings rates across audited systems.',
    'A high performative share raises theater_ratio over the interval and pushes the arrangement toward inertial maintenance; a low share confirms the protective function is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_functionality_share, empirical, 'Functional versus performative share of the audit apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% The colloquial term 'AI alignment' covers multiple structurally distinct governing commitments and is decomposed into a constraint family under kernel ai_alignment_commitment (ε-invariance principle): ethics_justice_reading (this file — present-harm prevention standard; extraction from long-term safety research), safety_control_reading (loss-of-control prevention standard; extraction from present-harm mitigation), and integrated_reading (simultaneous non-exclusive attention). Each member authors its own ε, beneficiaries, and victims; this file links both siblings via affects_constraints. The upstream/downstream structure: each definitional reading cites the field's shared harm evidence but institutionalizes a different victim set, and this reading's institutional dominance changes the resource availability and legitimacy conditions under which the safety_control_reading operates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
