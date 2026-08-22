% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: Near-Term AI Harms Governance Priority (Reading)
 *   domain: technology/governance/ethics
 *
 * SUMMARY:
 *   AI risk governance is contested across three incompatible readings of
 *   what constitutes urgent regulatory priority. The near-term-harms reading
 *   asserts that demonstrable present harms — algorithmic discrimination,
 *   labor displacement, surveillance, misinformation — affecting powerless
 *   and marginalized populations warrant immediate governance action and
 *   resource allocation. This reading competes with an existential-risk
 *   reading that prioritizes superintelligence prevention and a bridge
 *   reading that treats both as entangled. This JSON instantiates ONLY the
 *   near-term-harms reading as a clean ε-invariant constraint: its
 *   beneficiaries are AI corporations and some regulators; its victims are
 *   Global South populations and marginalized workers; its effectiveness
 *   depends on active enforcement of bias testing and accountability. The
 *   claim/metric gap is intentional: this reading is CLAIMED as tangled_rope
 *   (genuine coordination function of present-harm auditing plus asymmetric
 *   extraction favoring corporations) while metrics show substantial
 *   suppression and theatrical compliance — the engine will compute how
 *   differently each seat perceives this constraint.
 *
 * KEY AGENTS:
 *   - Global South populations: powerless, trapped, bearing present algorithmic discrimination — primary victims
 *   - Marginalized workers: moderate power, constrained exit, experiencing labor displacement — primary victims
 *   - AI corporations: institutional power, arbitrage exit, benefit from existential-risk framing that diverts regulatory attention — primary beneficiaries
 *   - Existential-risk advocates: organized power, arbitrage exit, benefit from governance prioritization of superintelligence over present harms — secondary beneficiaries
 *   - Regulators adopting near-term-harms frameworks: institutional power, constrained by global coordination problems — bifurcated: beneficiary where they adopt NTF, excluded where they don't
 *   - AI safety researchers: organized power, excluded from governance conversation by the near-term-harms framing — excluded seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.71).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.68).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "Near-Term AI Harms Governance Priority (Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "technology/governance/ethics").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, 'dfca9fe5-99ba-4794-a65c-472947ac3feb').
narrative_ontology:cs_kernel_codification('dfca9fe5-99ba-4794-a65c-472947ac3feb', distributed).
narrative_ontology:cs_authority_grounding('dfca9fe5-99ba-4794-a65c-472947ac3feb', distributed).
narrative_ontology:cs_reading_relation('dfca9fe5-99ba-4794-a65c-472947ac3feb', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('dfca9fe5-99ba-4794-a65c-472947ac3feb', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('dfca9fe5-99ba-4794-a65c-472947ac3feb', foundational, present_algorithmic_harms_are_identifiable_and_urgent).
narrative_ontology:cs_axiom_status(present_algorithmic_harms_are_identifiable_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('dfca9fe5-99ba-4794-a65c-472947ac3feb', present_algorithmic_harms_are_identifiable_and_urgent, empirically_contingent).
narrative_ontology:cs_axiom('dfca9fe5-99ba-4794-a65c-472947ac3feb', foundational, marginalized_populations_deserve_immediate_accountability).
narrative_ontology:cs_axiom_status(marginalized_populations_deserve_immediate_accountability, holdable).
narrative_ontology:cs_axiom_grounding('dfca9fe5-99ba-4794-a65c-472947ac3feb', marginalized_populations_deserve_immediate_accountability, deontological).
narrative_ontology:cs_reference_frame('dfca9fe5-99ba-4794-a65c-472947ac3feb', pre_algorithmic_scale_governance).
narrative_ontology:cs_drift_state('dfca9fe5-99ba-4794-a65c-472947ac3feb', contemporary_2025_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dfca9fe5-99ba-4794-a65c-472947ac3feb', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_corporations).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, regulators_adopting_ntf).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_workers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, algorithmic_discrimination_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, existential_risk_advocates).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, global_north_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face algorithmic discrimination in credit scoring, hiring, content moderation, and social services — often implemented by foreign companies with minimal local accountability. Bear measurable harms (loan denials, job loss, surveillance) in the present. Exit options are nearly nonexistent: the systems that affect them are not optional, are enforced by local state actors, and the populations have no recourse in the governance of the corporations that build them.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, biographical, trapped, global).

% Experience immediate labor displacement through automation, wage suppression from algorithmic management systems, and workplace surveillance. They organize labor action and seek regulatory protection, but the regulatory focus on existential risk diverts resources from present-harm mitigation. Their exit option is retraining or relocation, both costly and time-limited.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_workers, payer,
    moderate, biographical, constrained, global).

% Include ethnic minorities, women, LGBTQ+ populations, people with disabilities experiencing algorithmic bias in content recommendation, hiring, criminal justice, healthcare allocation. They are 'locked' by identity to the discrimination surface — they cannot opt out of their identity classification. They organize advocacy and civil rights campaigns but lack regulatory leverage over platform governance.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, algorithmic_discrimination_targets, payer,
    organized, biographical, identity_locked, global).

% Benefit from a governance regime where existential-risk framing diverts regulatory attention from present-harm auditing and mitigation. The near-term-harms reading, if adopted as binding, would require frequent audits, bias testing, impact assessments in marginalized populations, and exposure to product liability. They can exit a jurisdiction but not the existential-risk discourse itself; they can fund x-risk research to legitimize the governance frame. They have resources to shape regulatory debate and can point to speculative risks as proportionally more important than documented harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_corporations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, ai_corporations, agenda_setter).

% Adopt near-term-harms governance frameworks in some jurisdictions (EU AI Act bias provisions, EEOC algorithmic discrimination guidance). They benefit from a clear, measurable enforcement agenda: bias testing, documentation, local accountability. They face institutional constraint from other regulators who prioritize x-risk framing and from corporations that can relocate to lower-standards jurisdictions. Their capacity to enforce present-harm requirements is limited by the global supply chain and by lack of coordination with non-adopting regulators.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, regulators_adopting_ntf, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, regulators_adopting_ntf, agenda_setter).

% Benefit from a governance regime where present-harm accountability is treated as lower-priority than superintelligence prevention. They argue that diverting resources to present-harm mitigation delays existential-risk research and reduces the probability of solving alignment before dangerous capability emergence. They have significant funding, academic positioning, and rhetorical influence. They can exit the present-harms debate by reframing it as a subset of existential risk; they arbitrage between academic credibility in AI safety and policy influence.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, existential_risk_advocates, beneficiary,
    organized, civilizational, arbitrage, global).

% In wealthy jurisdictions, advocacy organizations for algorithmic accountability push near-term-harms governance. They benefit from regulatory adoption (funding, seats on advisory boards, legitimacy). They have mobile options: can shift focus to x-risk if funding flows that direction, can move to other jurisdictions with stronger governance. Their power is concentrated in wealthy democracies; their ability to enforce change in Global South governance is limited.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_north_civil_society, beneficiary,
    organized, biographical, mobile, national).

% Researchers focused on alignment, control, interpretability, and superintelligence risk are structurally excluded from the near-term-harms governance conversation in many forums. They would argue that present-harm mitigation is important but insufficient, and that governance frameworks must address speculative catastrophic risk. The near-term-harms reading frames them as the adversarial seat; they would testify to regulatory bodies that prioritizing present harms is a misdirection from the larger existential problem.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_safety_researchers, excluded,
    organized, civilizational, arbitrage, global).

% Academic researchers outside corporate AI labs investigate algorithmic bias, labor impact, and surveillance effects. They provide empirical evidence for the near-term-harms reading but face funding constraints and career pressure from the existential-risk field. They observe the governance contest without formal power but produce data that constituencies rely on.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, independent_researchers, observer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, ai_corporations).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes a focused regulatory agenda for AI deployment: algorithmic impact assessments, bias testing standards, accountability frameworks for demonstrable harms in present systems. Solves the coordination problem of fragmentary, jurisdiction-specific regulation by establishing common measurement baselines for discrimination and labor impact.
% TRANSFER_FUNCTION: Moves regulatory scrutiny and enforcement resources from speculative superintelligence scenarios toward present-harm auditing and mitigation. The transfer is of: enforcement capacity (auditors, lawyers), corporate compliance cost (testing, remediation, documentation), and governance legitimacy (whose harms count as urgent).
% ABSENT_VOICES: AI safety researchers and existential-risk advocates are structurally excluded from the near-term-harms governance frame — they argue for a different risk priority calculus and are kept out by the reading's definition of urgency. Workers in AI-producing countries (wealthy democracies) whose displacement is slower or cushioned by retraining programs have less voice than Global South workers with no retraining infrastructure. Technology companies' own impact assessment divisions are partially present but in a subordinate compliance posture, not as stakeholders shaping the governance problem definition.
% DISAPPEARANCE_RATIONALE: If the near-term-harms governance priority and its enforcement vanished, regulators in EU and some national jurisdictions would lose their primary accountability mechanism for algorithmic discrimination; companies would reduce bias auditing and impact assessment; Global South populations and marginalized workers would lose the sole formal channel through which their harms could generate regulatory remedies. Regulatory focus would shift entirely to existential risk framing; present-harm documentation would be deprioritized.
% FOUNDING_PROBLEM: Algorithmic systems deployed at scale to populations without meaningful consent, oversight, or recourse are causing measurable harms: employment discrimination, credit denial, content suppression, surveillance, erosion of electoral integrity. These harms are present, documented, and concentrated on the least powerful populations. A governance framework that does not address them in real time prioritizes speculative harms and abandons those suffering documented injuries.
% FOUNDING_PROBLEM_CORROBORATION: Independent researchers (Buolamwini, Timnit Gebru, Kate Crawford, Safiya Noble, Virginia Dignum) document algorithmic bias and labor displacement; civil rights organizations (CAIR, National Hispanic Media Coalition, Amnesty International, Data for Black Lives) testify to ongoing harms; regulatory bodies in the EU cite algorithmic discrimination as a present enforcement priority. Technology companies acknowledge some bias issues but dispute the characterization of harms as the primary governance urgency relative to existential risks. Existential-risk researchers argue that even severe present harms are dwarfed by superintelligence risks and should not redirect governance resources.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.71) and rising because the constraint's persistence depends on corporate interests in diverting governance resources from present-harm accountability toward existential-risk narratives. The measurement series shows extractiveness growing from 0.48 at interval start to 0.71: this captures the dynamic where existential-risk framing accumulates institutional legitimacy while present-harm remediation stalls. Suppression (0.68) is substantial because enforcement depends on excluding existential-risk frames from governance conversations, on marginalizing Global South concerns in favor of Global North regulatory agendas, and on preventing workers' labor-impact concerns from reaching corporate governance. Theater (0.42) reflects that companies perform bias auditing and produce ethics statements while resisting structural changes that would reduce algorithmic discrimination in consequential systems. Accessibility collapse (0.48) is moderate because alternatives exist — existential-risk framing is a live competitor, bridge framings are emerging — but once the near-term-harms governance regime is understood, alternatives appear costly to switch to. Resistance (0.72) is high because marginalized populations, workers, and independent researchers actively resist the existential-risk prioritization and push for present-harm accountability; existential-risk advocates resist the near-term-harms frame as misdirected. This is a structurally contested arrangement with real resistance from multiple seats.
 *
 * PERSPECTIVAL GAP:
 *   From the global_south_populations and marginalized_workers seats, this constraint is largely extractive with cosmetic coordination: governance talks about algorithmic bias while enforcement capacity remains minimal, companies perform audits without changing deployment decisions, and the existential-risk discourse legitimizes delay. From the ai_corporations seat, this is genuine coordination of present-harm mitigation (real audits, documented impact, remediation) — the claim. From the existential_risk_advocates seat, even rigorous near-term-harm governance is misdirected from the larger catastrophic risk. From the regulators_adopting_ntf seat (EU), it is tangled rope: coordination function (shared standards, accountability) plus asymmetric cost (corporations bear compliance; non-adopting jurisdictions get competitive advantage). The engine will compute these divergences from the structural data; the strategy is not to reconcile them but to expose them as per-seat measurements.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims (Global South, marginalized workers) have high directionality (d near 1.0) — trapped or identity-locked, powerless, no arbitrage exit. They are the targets of suppression (their concerns are deprioritized) and extraction (governance resources flow away from remedies for their harms). Beneficiaries (AI corporations) have low directionality (d near 0.0) — institutional power, arbitrage exit (can relocate), organize the governance frame — they benefit from the constraint's suppression of accountability. Regulators adopting NTF are bifurcated: in jurisdictions that enforce it, they are low-d beneficiaries (gain legitimacy and enforcement agenda); in non-adopting jurisdictions, they would be medium-d payers or excluded. Existential-risk advocates have low-d beneficiary positioning in a regime where superintelligence risk dominates governance. The engine will compute divergence per seat: from the victim seats, this is high-extraction snare; from the corporation seats, genuine coordination for present-harm auditing (which is real, but partial). The gap reveals the constraint's asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: algorithmic discrimination and labor displacement are present, documented, and ongoing. But there is substantial risk of mandatrophy if the existential-risk framing continues to dominate: as governance cycles pass without measurable improvement in present harms, the near-term-harms frame could lose credibility, displaced populations could cease organizing for accountability they never receive, and the constraint could persist as pure theater — high suppression, high theater ratio, low actual impact on harm reduction. The divergence between claimed_type (tangled_rope: coordination + extraction) and measured suppression (0.68) and theater (0.42, rising) indicates the constraint is being pulled toward snare (pure extraction, where the coordination narrative becomes cover). The measurement series shows theater rising while extractiveness plateaus, which is the signature of a constraint sliding from tangled_rope toward piton: the coordination function persists (audits happen), but enforcement capacity stalls and the constraint becomes theatrical performance on behalf of corporate legitimacy rather than harm reduction. To prevent mandatrophy, the constraint would need to shift from governance talk to enforcement: actual remediation of documented harms, not just documentation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the near-term-harms reading a distinct constraint or a sub-framing of a unified AI governance commitment?',
    'Examine whether the three readings (near-term, existential, bridge) compete for the SAME governance resources and authority, or whether they can coexist with separate resource streams. If they compete (zero-sum allocation), they are distinct constraints; if they coexist (both funded, both have seats at the governance table), the kernel is genuinely tripartite.',
    'If distinct constraints, this reading''s ε and classification are independent of siblings; if sub-framings, the constraint''s ε should be lower (it is one legitimate reading among three, not a full-form governance commitment). The distinction affects how the engine treats ''forecloses'' relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the reading is a constraint-level instantiation or a framing-level variant of a single commitment.').

omega_variable(
    suppression_mechanism,
    'Is the measured suppression (0.68) structural (existential-risk discourse is genuinely more scientifically credible, or regulatory capacity is genuinely limited) or manufactured (deliberate sidelining of present-harm concerns by corporate influence)?',
    'Examine funding flows to x-risk vs. fairness research, personnel movement between corporate AI and x-risk institutes, media coverage volume by risk category, and regulatory budget allocation. If x-risk funding and personnel flow exceed near-term fairness by a structural ratio, suppression is manufactured; if limited regulatory budgets genuinely cannot cover both, suppression is structural.',
    'If manufactured, the constraint is closer to snare (suppression serves extraction). If structural, the constraint is closer to tangled rope (suppression is an externality of genuine coordination scarcity). This affects whether fixing the constraint requires defeating the existential-risk reading or merely better-resourcing the near-term-harms regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism, empirical, 'Whether suppression is deliberate or incidental to resource constraints.').

omega_variable(
    theater_accumulation,
    'Why is theater_ratio rising from 0.28 to 0.42 while base_extractiveness plateaus from 0.66 to 0.71?',
    'Audit the measurement series against concrete corporate bias remediation outcomes: number of flagged systems fixed, populations affected, timeline to remediation. If fixes are declining or slowing while audits and statements increase, theater is accumulating. If fixes track audits, theater is stable performative overhead.',
    'Rising theater with plateau extractiveness is a piton signature — the constraint is becoming more theatrical (governance talk) without increasing actual harm reduction. This indicates mandatrophy risk and suggests the constraint needs enforcement strengthening, not coordination refinement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_accumulation, empirical, 'Whether the constraint is sliding from tangled rope toward piton due to performative accumulation without harm reduction.').

omega_variable(
    identity_lock_mechanism,
    'For algorithmic_discrimination_targets with identity_locked exit, does the lock operate at the level of social identity (they cannot change race/gender/disability status) or at the level of algorithmic classification (systems classify them even when they change their presented identity)?',
    'Test whether identity_locked individuals can evade algorithmic discrimination through presentation changes (de-identification, false identity claims, information asymmetry). If evasion works, the lock is identity-classification (structural to the algorithm). If evasion fails, the lock is social identity (structural to society). This distinction affects whether harm mitigation targets personal presentation or algorithmic design.',
    'If classification-based lock, remediation requires algorithm auditing and bias testing. If social-identity lock, remediation requires anti-discrimination law. The distinction reshapes the governance problem from technical to legal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether identity lock in algorithmic discrimination operates at classification or social-identity level.').

omega_variable(
    extraction_beneficiary_ambiguity,
    'Do AI corporations benefit from the near-term-harms governance regime, or does the regime constrain them and only existential-risk advocates benefit from its suppression?',
    'Measure corporate compliance cost (auditing, testing, product delays) against governance benefit (market access, legitimacy, competitive moats against smaller AI entrants). If cost > benefit and corporations would defect if exit were available, corporations are payers, not beneficiaries, and the beneficiary set is only existential_risk_advocates.',
    'If corporations are payers, the constraint is snare + exclusion (victims + corporate payers, existential advocates as beneficiaries). If corporations are beneficiaries, the constraint is tangled rope with bifurcated payer/beneficiary positions. The classification hinges on this empirical question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_ambiguity, empirical, 'Whether corporations benefit from or are constrained by near-term-harms governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(ai_r_tr_t35, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(ai_r_be_t35, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 35, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(ai_r_su_t35, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 35, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, algorithmic_discrimination_labor_displacement).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, global_south_ai_governance_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_risk_governance_priority kernel. The kernel is contested across three readings with incompatible beneficiary/victim structures and ε values. (1) near_term_harms_reading: high ε on present deployment harms, low on superintelligence; victims are Global South and marginalized populations; beneficiaries are corporations and x-risk advocates; governance resources flow to bias auditing. (2) existential_risk_reading: low ε on present harms, high on superintelligence scenarios; victims are humanity-at-large and future people; beneficiaries are AI safety researchers and alignment funders; resources flow to x-risk research. (3) bridge_reading: mid-range ε on both, structures them as entangled; victims are both present and future; coordination goal is unified frameworks. Each reading has distinct beneficiary/victim sets, distinct resource flows, and distinct suppression mechanisms. The three readings are linked via network.affects_constraints because they compete for governance authority and regulatory resources within the same institutional domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
