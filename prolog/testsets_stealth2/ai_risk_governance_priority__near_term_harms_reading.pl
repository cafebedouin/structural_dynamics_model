% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance Priority — Near-Term Harms Reading
 *   domain: technology governance / AI ethics / risk assessment
 *
 * SUMMARY:
 *   This story instantiates the near-term-harms reading of the contested
 *   kernel 'what must AI risk governance treat as first priority.' The
 *   standing arrangement under contest is the actual allocation of
 *   AI-governance attention, rulemaking capacity, and research funding as it
 *   operated from roughly 2014 through 2026: governance forums, safety
 *   institutes, and funder networks progressively concentrated on speculative
 *   catastrophic-risk scenarios while documented present harms —
 *   discriminatory screening in hiring, lending, benefits, and policing;
 *   surveillance deployments trialed on overpoliced communities; traumatic
 *   and underpaid data-labeling labor in the Global South; automation
 *   displacement of mid-career workers — accumulated faster than they were
 *   remedied. Read by this reading's own lights, the arrangement carries a
 *   genuine coordination function (shared risk taxonomies, incident
 *   disclosure, evaluation standards solve real collective-action problems)
 *   while systematically externalizing its costs onto the populations with
 *   the least seat access, which is why the reading assesses it as a hybrid
 *   rather than as pure coordination or pure extraction. The reading's
 *   endorsed alternative — fairness audits, bias-mitigation mandates,
 *   enforceable present-harm rules — is deliberately NOT the object measured
 *   here; per the epsilon-referent rule, epsilon describes the standing
 *   arrangement, not the reading's program. KEY AGENTS (by structural
 *   relationship): - frontier_ai_developers: Primary beneficiary
 *   (institutional/arbitrage) — gains deferred present-harm liability and
 *   continued deployment freedom under the current allocation -
 *   xrisk_governance_institutes: Secondary beneficiary with informal agenda
 *   power (organized/identity_locked) — captures governance attention,
 *   evaluation mandates, and safety funding - ai_regulatory_bodies: Agenda
 *   setter (institutional/constrained) — allocates finite rulemaking and
 *   enforcement capacity between framings - global_south_data_workers:
 *   Primary payer (powerless/constrained) — bears the labor harms that make
 *   frontier systems possible -
 *   algorithmically_screened_minority_communities: Primary payer
 *   (powerless/trapped) — bears discriminatory-error costs with no exit from
 *   the gating systems - automation_displaced_workers: Payer
 *   (moderate/constrained) — absorbs task substitution with lagging
 *   transition support - surveillance_targeted_communities: Payer
 *   (powerless/trapped) — host first-deployments of monitoring systems -
 *   fairness_auditing_and_civil_society: Excluded challenger
 *   (organized/constrained) — produces the harm evidence but holds few agenda
 *   seats
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.78).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance Priority — Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "technology governance / AI ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9').
narrative_ontology:cs_kernel_codification('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', distributed).
narrative_ontology:cs_authority_grounding('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', distributed).
narrative_ontology:cs_reading_relation('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', ai_risk_governance_priority__existential_risk_reading, forecloses).
narrative_ontology:cs_reading_relation('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', foundational, present_persons_take_lexical_priority_over_speculative_future_persons).
narrative_ontology:cs_axiom_status(present_persons_take_lexical_priority_over_speculative_future_persons, holdable).
narrative_ontology:cs_axiom_grounding('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', present_persons_take_lexical_priority_over_speculative_future_persons, deontological).
narrative_ontology:cs_axiom('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', foundational, governance_resources_follow_demonstrated_harm_evidence).
narrative_ontology:cs_axiom_status(governance_resources_follow_demonstrated_harm_evidence, holdable).
narrative_ontology:cs_axiom_grounding('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', governance_resources_follow_demonstrated_harm_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', demonstrated_harm_first_allocation).
narrative_ontology:cs_drift_state('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', post_xrisk_agenda_ascendance, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('27de8c1e-a2e7-49e3-8056-f7b2a15fb4e9', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, frontier_ai_developers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, xrisk_governance_institutes).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_data_workers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, algorithmically_screened_minority_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, automation_displaced_workers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, surveillance_targeted_communities).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, demonstrated_harm_evidential_standard).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, present_persons_moral_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy large-scale AI systems across jurisdictions. Under the current allocation of governance attention, documented harms from deployed systems — discriminatory outputs, surveillance integrations, labor substitution — draw limited binding remediation while rulemaking capacity concentrates on long-run catastrophic scenarios. When any jurisdiction tightens present-harm rules, they can shift launch schedules, registration entities, and lobbying effort across borders, and they fund a large share of the safety research ecosystem that advises regulators.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, frontier_ai_developers, beneficiary,
    institutional, generational, arbitrage, global).

% Safety institutes, evaluation organizations, and funder networks whose programs, staff careers, and donor relationships are organized around catastrophic-risk scenarios. They hold invited seats in governance forums, supply much of the expert assessment regulators rely on, and capture a large share of AI-safety funding. Their professional identities are constituted by the framing they advance, so a reorientation of governance toward present harms would unsettle not only their budgets but their sense of purpose.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, xrisk_governance_institutes, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, xrisk_governance_institutes, agenda_setter).

% Agencies and intergovernmental bodies that write AI risk frameworks and set enforcement agendas. They must divide finite rulemaking, hiring, and inspection capacity between present-harm enforcement and forward-looking safety mandates, under legislative direction, industry lobbying, and expert advice supplied predominantly by well-funded safety institutes.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_regulatory_bodies, agenda_setter,
    institutional, generational, constrained, continental).

% Perform the annotation, content moderation, and preference-labeling labor on which frontier systems are trained, largely in Kenya, the Philippines, Venezuela, and India, often through layered subcontract chains. They absorb traumatic content exposure and poverty-level piece rates. Local labor markets offer few comparable employers, and subcontracting obscures who is responsible for working conditions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_data_workers, payer,
    powerless, immediate, constrained, global).

% Encounter automated decision systems in hiring, lending, tenant screening, public benefits, and policing. Errors concentrate along racial and class lines, and the systems gate goods people cannot obtain elsewhere: jobs, credit, housing, freedom of movement. Individual appeals are slow, costly, and rarely succeed; there is no opting out of the pipeline.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, algorithmically_screened_minority_communities, payer,
    powerless, biographical, trapped, national).

% Work in customer support, translation, transcription, driving, illustration, and writing — occupations whose task volume is shrinking as deployed models absorb it. Displacement concentrates in particular regions and firms; retraining programs exist but lag the pace of substitution, and many affected workers are mid-career with mortgages and dependents.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, automation_displaced_workers, payer,
    moderate, biographical, constrained, regional).

% Live under facial-recognition dragnets, predictive-policing deployments, and biometric ID systems that are routinely trialed first in their neighborhoods — overpoliced districts, border zones, protest movements. Exiting means leaving home or country; for undocumented members, formal complaint channels carry deportation risk.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, surveillance_targeted_communities, payer,
    powerless, biographical, trapped, regional).

% Audit firms, university fairness labs, and NGO coalitions produce most of the documented evidence of present harms. They hold technical findings and testimony but few voting seats in the forums where governance priority is set; access depends on relationships with the same laboratories and institutes they examine, and their funding is small next to catastrophic-risk philanthropy.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, fairness_auditing_and_civil_society, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, frontier_ai_developers).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: AI risk governance coordinates risk-assessment standards, incident disclosure, and evaluation practice across competing developers and jurisdictions — solving genuine collective-action problems (race-to-the-bottom dynamics, information asymmetry about system behavior) that no single actor solves alone.
% TRANSFER_FUNCTION: Moves governance attention, rulemaking capacity, and research funding — currently disproportionately toward speculative catastrophic-risk scenarios and away from remediation of documented present harms borne by marginalized populations; where present-harm rules do bind, it moves compliance costs onto deployers.
% ABSENT_VOICES: The people bearing the harms — Global South data workers, people denied by screening algorithms, displaced workers, surveilled communities — rarely hold seats in the forums where priority is set; their testimony arrives secondhand through NGOs and audit literature. Fairness-focused researchers outside elite-lab networks are similarly under-seated. Their absence is load-bearing: unanimity in governance forums partly reflects who was never invited.
% DISAPPEARANCE_RATIONALE: If the current allocation vanished overnight, governance forums would immediately renegotiate priority: safety institutes would lose their agenda positions and funding advantage, developers would face renegotiated liability exposure, and the organized challengers already producing harm evidence would move into the vacated agenda space. Resource flows, forum composition, and the litigation environment would all rearrange around the new allocation — the arrangement's beneficiaries are organized around it and would fight to reconstruct it.
% FOUNDING_PROBLEM: AI governance emerged to manage systems whose harms span two registers: documented present injuries (the COMPAS-era discrimination findings, content-moderator trauma, biased recognition systems) and speculative catastrophic outcomes as capabilities grew. The priority question crystallized when capability scaling after 2022 made catastrophic-risk framing institutionally ascendant, forcing a finite-bandwidth allocation between remediating demonstrated harms and preparing for speculative ones.
% FOUNDING_PROBLEM_CORROBORATION: The dispute's liveness is attested from outside any single beneficiary set: civil-society and worker-advocacy organizations document unremediated present harms; peer-reviewed audit literatures establish the harm record; and the catastrophic-risk institutes themselves publicly argue the opposite priority — both camps acknowledge the contest is unresolved. No party to the dispute claims the priority question is settled, which is itself cross-partisan corroboration that the founding problem remains live.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.78 at interval end) because the standing arrangement leaves documented harms unremediated at scale while governance capacity flows elsewhere; the series rises monotonically as deployment scaled faster than remediation across 2014-2026. Suppression (0.65) is a raw structural property, unscaled by power or scope: affected populations cannot opt out of the algorithmic pipelines that gate employment, credit, housing, and movement, and dissenting framings face funding and venue gates. Theater ratio (0.46) reflects the proliferation of ethics principles, advisory boards, and voluntary commitments that bind nothing — performative activity grew steadily as a share of governance output. Accessibility collapse is LOW (0.30): alternatives to the standing allocation remain genuinely live — the bridge reading, reform proposals, and this reading itself are all on the table — which is precisely why the kernel is contested rather than settled. Resistance is high (0.68): content-moderator litigation, algorithmic-justice campaigning, audit literatures, and worker organizing actively contest the allocation. The three measurement series share one time grid (annual units, t=0 maps to 2014, t=12 to 2026) with every tracked metric authored at every point, per the alignment rule; the suppression_requirement series is authored because the story specifically tracks enforcement-capacity change — holding the catastrophic-risk-weighted allocation required intensifying agenda control (invitation gates, funding concentration, framing battles) after 2022, not merely static background coercion. Coalition note: the four payer groups are individually powerless but latently coalition-capable — moderator lawsuits and transnational worker campaigns show sparks — yet geographic dispersion, subcontract opacity, and divergent harm types have so far suppressed durable coalition formation; the engine should read the powerless payer seats with that latent coalition power in mind rather than as isolated individuals.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the frontier-developer seat, the arrangement looks like prudent stewardship: sober attention to worst-case scenarios, voluntary commitments, manageable compliance. From the four payer seats, the same arrangement looks like abandonment-with-harm: their injuries are documented, measurable, and unaddressed, while the forums that could remedy them deliberate about scenarios none of them will live to test. The regulator seat experiences neither — it experiences impossible triage under finite bandwidth, lobbied from both directions. The institute seat experiences the arrangement as its life's work under siege. None of these perceptions is authored as a classification; the engine derives each seat's type from power, exit, and directionality, and the divergence between the beneficiary seats' computed experience and the payer seats' computed experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: frontier_ai_developers and xrisk_governance_institutes sit near the beneficiary end (low d, damped or inverted effective extraction) — developers through arbitrage-grade exit (jurisdictional mobility keeps them near the subsidy end even where rules bind), institutes through identity-lock that binds them INTO the beneficiary position (an unusual direction: identity fusion normally amplifies a target's extraction; here it fortifies a beneficiary's defense of the arrangement, raising the arrangement's persistence rather than the institute's burden). The four victim groups sit near the full-target end (high d, amplified effective extraction): trapped exit for screened and surveilled communities (no exit from gating systems), constrained exit for data workers and displaced workers (labor-market alternatives exist but are thinner than the harm). The regulator sits near symmetric as agenda setter — it administers the allocation without collecting its gains. Scope amplification applies modestly: the arrangement operates globally, making verification of present-harm remediation harder and tilting effective extraction further toward the trapped payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing AI systems whose harm spectrum runs from documented present discrimination to speculative extinction-scale risk under finite institutional bandwidth — remains live, so no mandatrophy verdict is declared and the R5 mismatch consumer should find status=live paired with verdict=world_rearranges: no zombie flag. The classification discipline this story enforces cuts both ways. Calling the standing arrangement a rope would erase the asymmetric extraction this reading documents — the coordination function is real, but its costs land on seats with no exit while its attention-economy gains accrue to seats with maximum exit. Calling it a snare would erase the genuine coordination: shared incident disclosure, evaluation standards, and risk taxonomies solve problems no single actor solves alone, and the arrangement does not exist solely to suppress exits. Tangled rope preserves both facts and locates the dispute where this reading locates it: not whether to govern AI, but who the governing is FOR. The rising theater ratio is the leading indicator to watch — if performative ethics continues substituting for binding remediation, the arrangement drifts toward piton (function atrophied, performance maintained); if the beneficiary seats begin actively enforcing exclusion of present-harm advocates rather than merely out-competing them for bandwidth, it drifts toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the ai_risk_governance_priority kernel. Which structural element do the sibling readings (existential_risk_reading, bridge_reading) actually contest — the priority ordering itself, the evidential threshold for resource claims, or merely the division of the governance budget?',
    'Comparative analysis of the three readings'' victim sets, resource-flow claims, and adoption patterns in governance forums; trace which premise each sibling''s advocacy actually attacks.',
    'If the contest reduces to budget shares, the readings are rival distributive claims and compromise arithmetic applies; if it reduces to evidential thresholds, they are rival epistemologies and no split-the-difference remedy stabilizes the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Locates where the kernel contest structurally bites for this reading.').

omega_variable(
    zero_sum_bandwidth_question,
    'Is AI-governance bandwidth (rulemaking capacity, expert attention, philanthropic and public funding) actually fixed, such that speculative-scenario work diverts resources from present-harm remediation?',
    'Budget and staffing histories of regulatory bodies and safety organizations across the 2022-2025 surge in catastrophic-risk framing: did present-harm enforcement decline, stagnate, or grow alongside scenario work?',
    'If bandwidth is expandable, the diversion harm central to this reading weakens and the bridge reading gains force; if fixed, the priority contest is genuinely distributive and this reading''s victim attribution stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_sum_bandwidth_question, empirical, 'Whether the governance-allocation tradeoff this reading asserts is real.').

omega_variable(
    developer_net_benefit_direction,
    'Do frontier developers net-benefit from the catastrophic-risk-weighted allocation, or does regulatory uncertainty and reputational exposure cost them more than deferred present-harm liability saves?',
    'Compare lobbying expenditure, compliance costs under binding present-harm rules (bias audits, deployment reviews) against participation costs in scenario-planning regimes; examine disclosed risk factors and jurisdiction-shopping behavior.',
    'If developers net-lose, the beneficiary declaration partially inverts and the arrangement reads as coordination with incidental capture; if they net-gain, the capture reading stands and gain_flow attribution is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developer_net_benefit_direction, empirical, 'Direction of the developer seat''s net position under the standing allocation.').

omega_variable(
    demonstrated_vs_speculative_boundary,
    'Can ''demonstrated present harms'' be stably distinguished from speculative risks as capabilities grow, given that yesterday''s speculation repeatedly becomes today''s documented harm?',
    'Retrospective scoring of past speculative claims that matured into documented harms within five-year windows; measure the migration rate from the speculative to the demonstrated category.',
    'A high migration rate erodes the evidential-threshold axiom''s stability and pulls this reading toward the bridge position; a low rate stabilizes this reading''s boundary and its priority claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demonstrated_vs_speculative_boundary, conceptual, 'Stability of the demonstrated/speculative distinction this reading''s axiom depends on.').

omega_variable(
    voice_exclusion_mechanism,
    'Is the marginalization of present-harm-bearing voices in governance forums structural (seat design, funding gates, venue selection) or internalized (learned disengagement after repeated exclusion)?',
    'Post-offer participation trajectory of affected-community representatives: if engagement persists when seats and stipends are actually offered, exclusion was structural; if offered seats go unused, internalized disengagement compounds the structural barrier.',
    'If internalized, remedying formal seat access alone under-corrects and the arrangement''s effective suppression exceeds the structural measure; the omega feeds the suppression-mechanism ambiguity directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voice_exclusion_mechanism, empirical, 'Structural versus internalized mechanism behind absent affected-community voices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(ai_r_tr_t0, observed).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2, 0.27).
narrative_ontology:measurement_basis(ai_r_tr_t2, observed).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement_basis(ai_r_tr_t4, observed).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement_basis(ai_r_tr_t6, observed).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 8, 0.39).
narrative_ontology:measurement_basis(ai_r_tr_t8, observed).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 10, 0.43).
narrative_ontology:measurement_basis(ai_r_tr_t10, observed).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement_basis(ai_r_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(ai_r_be_t0, observed).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2, 0.6).
narrative_ontology:measurement_basis(ai_r_be_t2, observed).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 4, 0.64).
narrative_ontology:measurement_basis(ai_r_be_t4, observed).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 6, 0.69).
narrative_ontology:measurement_basis(ai_r_be_t6, observed).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement_basis(ai_r_be_t8, observed).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement_basis(ai_r_be_t10, observed).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 12, 0.78).
narrative_ontology:measurement_basis(ai_r_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(ai_r_su_t0, observed).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement_basis(ai_r_su_t2, observed).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 4, 0.49).
narrative_ontology:measurement_basis(ai_r_su_t4, observed).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement_basis(ai_r_su_t6, observed).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement_basis(ai_r_su_t8, observed).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(ai_r_su_t10, observed).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement_basis(ai_r_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI risk governance' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle — each reading instantiates a different constraint with its own epsilon, victim set, and classification, so no single story may average across them. This story (near_term_harms_reading) authors epsilon ~0.78 for the standing allocation as seen from present-harm-bearing seats; the existential_risk_reading authors epsilon for the same bandwidth-allocation arrangement as seen from civilization-scale seats (different victim set: future persons, different failure mode: irreversible catastrophe); the bridge_reading authors epsilon for the fragmentation itself (its claimed harm is the unmanaged seam between the other two). Upstream/downstream: the existential reading currently supplies the legitimacy conditions this reading chafes under (its agenda dominance is the diversion this story measures), and this reading supplies the present-harm half that any bridge unification must absorb — hence the affects_constraints edges run between all three family members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
