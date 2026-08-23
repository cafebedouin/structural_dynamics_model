% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI as Neutral Tool Governed by Subsidiarity-Based Regulation
 *   domain: theological/technological/political
 *
 * SUMMARY:
 *   This constraint story instantiates the instrumental_subsidiarity reading
 *   of the contested ai_human_relationship kernel. The reading holds that AI
 *   is a morally neutral tool whose ethical valence derives entirely from
 *   human use-cases and regulatory frameworks. Subsidiarity — the principle
 *   that decisions should be made at the most local competent level —
 *   operates as a procedural safeguard against centralized technocratic
 *   control. Human dignity is protected through legal frameworks (EU AI Act,
 *   UN conventions), transparency requirements (algorithmic audit rights,
 *   explainability mandates), and regulatory oversight. The reading presents
 *   itself as a transitional governance scaffold: adaptive regulation that
 *   evolves with technological capability, with built-in sunset/review
 *   clauses. Structurally, however, the constraint exhibits rising
 *   extractiveness (compliance costs concentrating on smaller actors),
 *   increasing theater (ethics washing, performative compliance), and growing
 *   suppression (marginalization of alternative anthropological frameworks in
 *   standard-setting bodies). The claimed scaffold type (transitional
 *   coordination) diverges from the metric trajectory toward tangled_rope or
 *   piton.
 *
 * KEY AGENTS:
 *   - regulatory_authorities: Primary agenda_setters (institutional/generational/arbitrage/global) — set and enforce AI governance frameworks, derive authority from CST lineage and state legitimacy
 *   - large_technology_companies: Dual beneficiary/payer (powerful/biographical/constrained/global) — capture regulatory process through lobbying/compliance capacity while bearing compliance costs
 *   - small_ai_developers: Primary payers (moderate/biographical/constrained/global) — face disproportionate compliance burden, lack lobbying resources, exit options constrained by platform dependence
 *   - global_south_technologists: Payers/excluded (powerless/biographical/trapped/regional) — bear costs of standards set elsewhere, excluded from standard-setting bodies
 *   - marginalized_communities_subject_to_ai: Payers/excluded (powerless/biographical/trapped/local) — bear deployed AI harms (bias, surveillance, automation) without voice in governance
 *   - civil_society_organizations: Beneficiaries (organized/biographical/mobile/global) — gain regulatory hooks, transparency rights, audit mechanisms
 *   - end_users: Beneficiaries/payers (organized/biographical/constrained/global) — gain protections but bear indirect costs (reduced innovation, higher prices)
 *   - cst_magisterium: Observers (institutional/civilizational/analytical/universal) — authoritative interpreter of kernel, tracks drift from reference_frame
 *   - secular_governance_bodies: Observers/agenda_setters (institutional/generational/arbitrage/global) — adopt procedural vocabulary (subsidiarity, transparency) without anthropological foundation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.42).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.38).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, scaffold).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Tool Governed by Subsidiarity-Based Regulation").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "theological/technological/political").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).
narrative_ontology:has_sunset_clause(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, '3a3d20ee-06eb-4826-854c-21a319736132').
narrative_ontology:cs_kernel_codification('3a3d20ee-06eb-4826-854c-21a319736132', formalized).
narrative_ontology:cs_authority_grounding('3a3d20ee-06eb-4826-854c-21a319736132', lineage).
narrative_ontology:cs_interpretation_layer_present('3a3d20ee-06eb-4826-854c-21a319736132').
narrative_ontology:cs_reading_relation('3a3d20ee-06eb-4826-854c-21a319736132', ai_human_relationship__technocratic_optimization, influences).
narrative_ontology:cs_reading_relation('3a3d20ee-06eb-4826-854c-21a319736132', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('3a3d20ee-06eb-4826-854c-21a319736132', foundational, technology_morally_neutral).
narrative_ontology:cs_axiom_status(technology_morally_neutral, holdable).
narrative_ontology:cs_axiom_grounding('3a3d20ee-06eb-4826-854c-21a319736132', technology_morally_neutral, conventional).
narrative_ontology:cs_axiom('3a3d20ee-06eb-4826-854c-21a319736132', foundational, subsidiarity_procedural_safeguard).
narrative_ontology:cs_axiom_status(subsidiarity_procedural_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('3a3d20ee-06eb-4826-854c-21a319736132', subsidiarity_procedural_safeguard, conventional).
narrative_ontology:cs_axiom('3a3d20ee-06eb-4826-854c-21a319736132', secondary, human_dignity_via_legal_transparency).
narrative_ontology:cs_axiom_status(human_dignity_via_legal_transparency, holdable).
narrative_ontology:cs_axiom_grounding('3a3d20ee-06eb-4826-854c-21a319736132', human_dignity_via_legal_transparency, instrumental).
narrative_ontology:cs_reference_frame('3a3d20ee-06eb-4826-854c-21a319736132', cst_social_doctrine_rerum_novarum_to_laudato_si).
narrative_ontology:cs_drift_state('3a3d20ee-06eb-4826-854c-21a319736132', contemporary_ai_governance_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3a3d20ee-06eb-4826-854c-21a319736132', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulatory_authorities).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, large_technology_companies).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, end_users).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, small_ai_developers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, global_south_technologists).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, marginalized_communities_subject_to_ai).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, large_technology_companies).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, end_users).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, subsidiarity_as_procedural_safeguard).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, technology_moral_neutrality).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, human_dignity_via_legal_frameworks).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, transparency_as_protection_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce AI governance frameworks (EU AI Act, national laws, UN processes). Derive authority from state legitimacy and CST social doctrine lineage. Control regulatory agenda, define risk categories, set compliance requirements. Can exit by deregulating but face political/legitimacy costs. Collect regulatory authority and legitimacy rents.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulatory_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Shape regulation through lobbying, standards bodies, compliance infrastructure investment. Gain regulatory moats (compliance as barrier to entry), legitimacy (ethics washing), and market certainty. Bear substantial compliance costs (legal, engineering, audit). Exit constrained by jurisdictional reach — cannot easily leave EU/US markets. Net position: likely net beneficiary at current extractiveness level.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, large_technology_companies, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, large_technology_companies, payer).

% Face disproportionate compliance burden relative to revenue: same regulatory requirements as large companies without compliance teams, legal resources, or lobbying access. Depend on large platforms for distribution (app stores, cloud, model APIs), making exit costly. No meaningful voice in standard-setting. Bear extraction without capturing coordination benefits.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, small_ai_developers, payer,
    moderate, biographical, constrained, global).

% Subject to standards (EU AI Act, OECD principles) set in Global North forums where they have minimal representation. Compliance costs for market access to Northern markets; no capacity to shape rules. Local AI development constrained by imported regulatory templates that may not fit context. Exit trapped: cannot access global markets without compliance, cannot influence compliance requirements.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, global_south_technologists, payer,
    powerless, biographical, trapped, regional).

% Bear deployed AI harms (algorithmic bias in hiring/lending/policing, surveillance, automation displacement) without meaningful participation in governance. Transparency rights (Art. 13 EU AI Act) exist on paper but require technical/legal capacity to exercise. Subsidiarity fails them: local bodies lack expertise to govern AI systems deployed by distant corporations. Exit trapped: cannot opt out of systems that govern their life chances.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, marginalized_communities_subject_to_ai, payer,
    powerless, biographical, trapped, local).

% Gain regulatory hooks: right to participate in standard-setting, algorithmic audit mandates, transparency obligations, complaint mechanisms. Use these to advocate for affected communities. Mobile exit: can shift focus across jurisdictions, issues, forums. Do not bear compliance costs; collect governance influence.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, civil_society_organizations, beneficiary,
    organized, biographical, mobile, global).

% Gain protections: transparency notices, right to explanation, prohibition of certain high-risk uses, human oversight requirements. Bear indirect costs: reduced service innovation, higher prices passed through from compliance, potential service withdrawal in over-regulated jurisdictions. Exit constrained by digital infrastructure dependence — cannot easily leave AI-mediated services.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, end_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, end_users, payer).

% Authoritative interpreter of the ai_human_relationship kernel in CST tradition. Issues guidance (e.g., Vatican AI ethics documents, Pontifical Academy for Life statements) that tracks drift from reference_frame (CST social doctrine). Does not enforce; authority is moral/epistemic. Analytical exit: can observe without being subject to constraint's enforcement.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, cst_magisterium, observer,
    institutional, civilizational, analytical, universal).

% Adopt procedural vocabulary of this reading (subsidiarity, transparency, accountability, risk-based approach) without CST anthropological foundation (imago Dei, common good, integral human development). EU AI Act, UN Global Digital Compact, OECD AI Principles instantiate the procedural shell. Arbitrage exit: can adopt/reject vocabulary without CST authority constraints. Function as secondary agenda_setters for global implementation.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, secular_governance_bodies, observer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, secular_governance_bodies, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, large_technology_companies).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, legitimate framework for governing AI deployment across jurisdictions: risk classification, transparency requirements, human oversight mandates, and accountability mechanisms — solving the coordination problem of fragmented, absent, or contradictory national regulations.
% TRANSFER_FUNCTION: Moves compliance costs (legal, engineering, audit, organizational) from regulated entities (primarily AI developers/deployers) to the regulatory state and civil society (who gain governance capacity), while transferring regulatory authority and legitimacy to state/institutional actors. Large tech captures net subsidy via regulatory moats; small developers and Global South actors pay net transfer.
% ABSENT_VOICES: Future generations (bear long-term AI trajectory risks without vote), non-human creation (CST: creation has intrinsic value, not merely instrumental — excluded from anthropocentric governance), Global South epistemic communities (indigenous knowledge systems, alternative development models excluded from 'neutral' standard-setting), workers in AI supply chains (data labelers, content moderators, hardware miners — structurally invisible in risk-based frameworks focused on deployment not production).
% DISAPPEARANCE_RATIONALE: If the instrumental_subsidiarity regulatory framework vanished overnight, AI deployment would revert to pure market/state control without transparency, risk classification, or accountability baselines. Large tech would face less compliance cost but more regulatory uncertainty; small developers would lose even the procedural protections that exist; marginalized communities would lose transparency/audit rights. The secular governance bodies (EU, UN, OECD) would lose their primary AI governance vocabulary. A new coordination mechanism would need to emerge — likely either technocratic_optimization (industry self-governance) or incarnational_humanism (rights-based/ban-based approaches).
% FOUNDING_PROBLEM: Early AI governance vacuum (2016-2020): no agreed risk taxonomy, no transparency mandates, no accountability mechanisms, deployment at scale without oversight. CST intervention (Rome Call for AI Ethics 2020, Vatican AI documents) provided anthropological grounding for regulation that secular bodies could adopt procedurally.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory authorities and large tech attest the problem persists (new capabilities = new risks). Incarnational_humanism scholars (e.g., Catholic theologians of technology, UNESCO COMEST) attest the founding problem is misdiagnosed: the vacuum was not 'lack of regulation' but 'lack of anthropological foundation' — the procedural solution embeds the technocratic framing it claims to govern. Independent analysis (AI Now Institute, Ada Lovelace Institute, Global South AI governance networks) corroborates that baseline governance exists but extraction/distribution problems are worsening.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).
:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects real but moderate extraction: compliance costs are genuine but not predatory; the coordination function (unified AI governance framework) delivers value. Suppression (0.38) is moderate: alternative framings (incarnational_humanism, technocratic_optimization) are not banned but are structurally marginalized in regulatory discourse — the 'neutral tool' framing sets the terms of debate. Theater_ratio (0.45) is rising: ethics washing, performative compliance, and procedural mimicry (impact assessments without power to stop deployment) increase over time. Accessibility_collapse (0.48) reflects partial closure: alternative governance imaginaries exist but lack institutional uptake. Resistance (0.55) is significant: from tech lobby (against regulation), from CST scholars (against neutrality claim), from Global South (against standard imposition), from accelerationists (against precaution). The scaffold claim requires has_sunset_clause=true — review clauses exist in EU AI Act (Art. 84) and UN processes, but whether they function as genuine sunset or ritual renewal is the omega question.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is pronounced: regulatory_authorities (agenda_setter, d~0.15) experience the constraint as genuine coordination — they built it, they administer it, they legitimize through it. Large_tech (dual beneficiary/payer, d~0.35) experiences both subsidy (regulatory moat, legitimacy) and extraction (compliance costs) — net position ambiguous. Small_developers and Global_South (payers, d~0.75-0.85) experience enforced extraction with minimal coordination benefit — the constraint coordinates for others, not for them. Marginalized_communities (excluded/payers, d~0.9) experience pure suppression without voice. The engine computes this divergence from structural data; the claimed scaffold type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: regulatory_authorities (authority/legitimacy), large_tech (regulatory capture/moat), civil_society (governance hooks), end_users (protections). Victims declared: small_developers (disproportionate compliance burden), global_south_technologists (standard-taking without standard-making), marginalized_communities (harm bearing without governance voice). Directionality derives from this structure: agenda_setters and captured beneficiaries sit at low d; payers with constrained/trapped exit sit at high d. The dual-role large_tech (beneficiary AND payer) creates the tangled_rope dynamic — genuine coordination function coexists with asymmetric extraction. No directionality_overrides needed; the derivation chain captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early AI governance vacuum: no standards, no accountability, deployment without oversight) is substantially solved — baseline transparency, risk classification, and oversight mechanisms now exist in major jurisdictions. Yet the constraint persists and expands (mandatrophy_resolved=false). The scaffold's transitional justification weakens as the 'temporary' regulatory architecture becomes permanent infrastructure. The constraint prevents mislabeling: it is not pure coordination (rope) because extraction is asymmetric and rising; not pure extraction (snare) because coordination function is real and beneficiaries include non-capturing actors (civil society, users). The mandatrophy tension — solved problem, persisting structure — is the scaffold's defining crisis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the instrumental_subsidiarity reading represent a genuine coordination mechanism for AI governance, or does it function as regulatory capture legitimizing existing power structures?',
    'Longitudinal analysis of regulatory outcomes: whether compliance costs distribute proportionally, whether small developers survive regulatory burden, whether Global South voices gain substantive influence in standard-setting bodies.',
    'If regulatory capture, the constraint reclassifies from scaffold to tangled_rope (coordination + extraction) or snare (extraction with coordination cover). The claimed transitional nature (scaffold) would be falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether the reading''s coordination function is genuine or a cover for asymmetric extraction.').

omega_variable(
    subsidiarity_operationalization_gap,
    'Does procedural subsidiarity (decisions at lowest competent level) actually protect human dignity in AI governance, or does it devolve responsibility without capacity, leaving vulnerable populations exposed?',
    'Case study analysis of AI regulatory implementation across jurisdictions: track whether local/regional bodies have resources and expertise to govern AI systems affecting their communities, or whether subsidiarity becomes abandonment.',
    'If subsidiarity functions as abandonment, the constraint''s protective claim fails; extraction shifts to those least able to resist (marginalized communities). The vindicated proposition ''subsidiarity_as_procedural_safeguard'' would be empirically undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_operationalization_gap, empirical, 'Whether subsidiarity as implemented protects or exposes vulnerable populations.').

omega_variable(
    neutrality_claim_embedded_assumptions,
    'Does the claim ''technology is morally neutral'' embed technocratic_optimization assumptions (efficiency, quantifiability, optimization) that pre-structure regulatory outcomes toward the sibling reading?',
    'Genealogical analysis of the neutrality claim in CST documents vs. secular AI governance frameworks: trace whether ''neutral tool'' language carries implicit teleology toward optimization/efficiency.',
    'If neutrality claim is conceptually contaminated, the reading does not genuinely coexist_with technocratic_optimization but influences it (structural downstream pressure). The reading_relations would need revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neutrality_claim_embedded_assumptions, conceptual, 'Whether the moral neutrality claim is conceptually pure or carries technocratic baggage.').

omega_variable(
    cs_authority_erosion_in_secular_governance,
    'As AI governance migrates to secular international bodies (EU, UN, OECD), does the CST-grounded authority of this reading erode, leaving only the procedural shell (subsidiarity, transparency) without the anthropological foundation?',
    'Discourse analysis of major AI regulatory frameworks (EU AI Act, UN Global Digital Compact, OECD AI Principles): measure presence/absence of CST anthropological claims (imago Dei, integral human development, common good) vs. procedural terms (transparency, accountability, risk management).',
    'If authority erodes, the constraint drifts toward technocratic_optimization structurally while retaining CST vocabulary — a piton trajectory. The reference_frame (CST social doctrine) would show substantial authority_erosion with acknowledged=false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_authority_erosion_in_secular_governance, empirical, 'Whether CST authority persists in secular AI governance or only procedural vocabulary remains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_tr_t0, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_tr_t5, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_tr_t5, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_tr_t10, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_tr_t10, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_tr_t15, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_tr_t15, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_tr_t20, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_be_t0, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_be_t5, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_be_t5, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_be_t10, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_be_t10, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_be_t15, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_be_t15, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_be_t20, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_su_t0, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_su_t5, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 5, 0.25).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_su_t5, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_su_t10, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_su_t10, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_su_t15, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_su_t15, observed).
narrative_ontology:measurement(ai_human_relationship__instrumental_subsidiarity_su_t20, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(ai_human_relationship__instrumental_subsidiarity_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__instrumental_subsidiarity, 0.12).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, eu_ai_act_implementation).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, un_global_digital_compact).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, oecd_ai_principles_governance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ai_human_relationship kernel. The instrumental_subsidiarity reading (this story) claims technology is neutral, governed by subsidiarity-based regulation. The technocratic_optimization reading claims AI maximizes efficiency; human value is productivity. The incarnational_humanism reading claims AI must serve integral human development; person as imago Dei. All three readings share the kernel but instantiate different constraints with different ε, beneficiaries/victims, and types. This reading's ε (0.42) is lower than technocratic_optimization (expected >0.7) but higher than incarnational_humanism (expected <0.2). The network edges reflect structural influence: this reading's procedural vocabulary (subsidiarity, transparency) is adopted by secular governance bodies (EU AI Act, UN GDC, OECD), creating downstream pressure on sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
