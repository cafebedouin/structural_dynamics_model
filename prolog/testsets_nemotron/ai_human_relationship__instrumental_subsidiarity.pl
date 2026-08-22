% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: AI as Neutral Tool Governed by Subsidiarity and Law
 *   domain: technology_ethics/political_theology/catholic_social_teaching
 *
 * SUMMARY:
 *   The instrumental-subsidiarity reading of the ai_human_relationship kernel
 *   frames AI as a morally neutral instrument whose ethical valence depends
 *   entirely on human use-cases and regulatory frameworks. Subsidiarity
 *   operates as a procedural safeguard: decisions about AI should be made at
 *   the lowest competent level, with higher levels (international bodies,
 *   national governments) providing frameworks for transparency,
 *   accountability, and human dignity. This reading dominates current
 *   Catholic magisterial discourse (Rome Call for AI Ethics, Vatican AI
 *   ethics conferences) and secular governance (EU AI Act, UNESCO
 *   Recommendation). It coordinates genuine governance infrastructure but
 *   extracts by deflecting structural critique of the political economy of AI
 *   onto 'bad use-cases' and 'implementation gaps.'
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.22).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.18).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.22).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Tool Governed by Subsidiarity and Law").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "technology_ethics/political_theology/catholic_social_teaching").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, 'b1b9f234-f30c-46e4-b739-85327c29dbf6').
narrative_ontology:cs_kernel_codification('b1b9f234-f30c-46e4-b739-85327c29dbf6', formalized).
narrative_ontology:cs_authority_grounding('b1b9f234-f30c-46e4-b739-85327c29dbf6', lineage).
narrative_ontology:cs_interpretation_layer_present('b1b9f234-f30c-46e4-b739-85327c29dbf6').
narrative_ontology:cs_reading_relation('b1b9f234-f30c-46e4-b739-85327c29dbf6', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_reading_relation('b1b9f234-f30c-46e4-b739-85327c29dbf6', ai_human_relationship__technocratic_optimization, influences).
narrative_ontology:cs_axiom('b1b9f234-f30c-46e4-b739-85327c29dbf6', foundational, technology_morally_neutral).
narrative_ontology:cs_axiom_status(technology_morally_neutral, holdable).
narrative_ontology:cs_axiom_grounding('b1b9f234-f30c-46e4-b739-85327c29dbf6', technology_morally_neutral, conventional).
narrative_ontology:cs_axiom('b1b9f234-f30c-46e4-b739-85327c29dbf6', foundational, subsidiarity_as_procedural_safeguard).
narrative_ontology:cs_axiom_status(subsidiarity_as_procedural_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('b1b9f234-f30c-46e4-b739-85327c29dbf6', subsidiarity_as_procedural_safeguard, conventional).
narrative_ontology:cs_axiom('b1b9f234-f30c-46e4-b739-85327c29dbf6', secondary, human_dignity_protected_through_legal_frameworks).
narrative_ontology:cs_axiom_status(human_dignity_protected_through_legal_frameworks, holdable).
narrative_ontology:cs_axiom_grounding('b1b9f234-f30c-46e4-b739-85327c29dbf6', human_dignity_protected_through_legal_frameworks, deontological).
narrative_ontology:cs_reference_frame('b1b9f234-f30c-46e4-b739-85327c29dbf6', cst_ai_governance_consensus_2018).
narrative_ontology:cs_drift_state('b1b9f234-f30c-46e4-b739-85327c29dbf6', post_generative_ai_deployment_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b1b9f234-f30c-46e4-b739-85327c29dbf6', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulatory_authorities).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, technology_companies).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, legal_profession).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, marginalized_communities).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, workers_displaced_by_automation).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, global_south_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, technology_companies).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, subsidiarity_as_procedural_safeguard).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, technology_moral_neutrality).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, human_dignity_through_legal_frameworks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues magisterial guidance framing AI as a tool requiring ethical governance; convenes expert commissions; sets the interpretive frame for Catholic institutions worldwide. Does not directly enforce regulation but shapes the moral vocabulary regulators and Catholic actors adopt.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, vatican_dicastery_for_culture_education, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, vatican_dicastery_for_culture_education, observer).

% Draft and enforce AI governance frameworks (EU AI Act, US executive orders, national strategies). Gain institutional legitimacy and expanded mandate from the 'neutral tool' framing, which justifies procedural regulation over substantive bans. Capture regulatory rents through compliance oversight.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulatory_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit from the 'neutral tool' narrative: it deflects structural critique of business models (surveillance capitalism, algorithmic management, predictive policing) onto 'misuse' by bad actors. Pay compliance costs (legal teams, audits, transparency reports) but treat them as the price of market access. Use regulatory sandboxes and lobbying to shape rules in their favor.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, technology_companies, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, technology_companies, payer).

% Gains a growing field of AI compliance, liability, and ethics advisory work. The procedural-subsidiarity frame (impact assessments, transparency mandates, human-in-the-loop requirements) creates billable expertise. Not directly harmed by the constraint; exits easily to other practice areas.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Bear disproportionate harms from 'neutrally' deployed systems: predictive policing targeting minority neighborhoods, algorithmic denial of benefits, hiring filters that replicate historic bias. The neutrality frame treats these as implementation bugs, not structural features. No meaningful exit from systems that govern housing, credit, policing, employment. Excluded from standard-setting bodies where 'human ends' are defined.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, marginalized_communities, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, marginalized_communities, excluded).

% Face job loss and deskilling framed as 'inevitable technological progress' rather than policy choice. Subsidiarity rhetoric pushes responsibility to local actors (retraining programs, social safety nets) while the capital-intensive automation decisions are made globally. Exit options limited to precarious gig work or geographic mobility they cannot afford.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, workers_displaced_by_automation, payer,
    moderate, biographical, constrained, national).

% Subject to data extraction, algorithmic governance, and AI systems trained on their data but deployed for northern markets. The 'neutral tool' frame legitimizes digital colonialism: infrastructure built on their labor and resources, governed by laws they did not write. No exit from platform dependency; excluded from the 'human ends' the constraint claims to serve.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, global_south_populations, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, global_south_populations, excluded).

% Analyze the constraint from within the tradition: some endorse the instrumental-subsidiarity reading as faithful to CST; others (incarnational humanists) argue it domesticates the Gospel's radical claim on technology. Their work shapes the internal discourse but does not control enforcement.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, catholic_social_ethicists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedural framework for governing AI development and deployment across jurisdictions: risk categorization, transparency requirements, human oversight mandates, and liability allocation. Solves the coordination problem of fragmented national approaches to a global technology.
% TRANSFER_FUNCTION: Transfers regulatory legitimacy and market access to technology companies in exchange for compliance costs; transfers harms (bias, displacement, surveillance) to marginalized populations who lack power to contest the 'neutral' framing; transfers governance authority to regulatory bodies and legal professions.
% ABSENT_VOICES: The people most affected by algorithmic systems — welfare recipients subject to automated fraud detection, gig workers managed by opaque algorithms, communities targeted by predictive policing, Global South data subjects — are structurally excluded from the standard-setting bodies (ISO, IEEE, OECD, EU expert groups) where 'human ends' and 'risk categories' are defined. Their absence is not accidental: the procedural-subsidiarity frame treats them as objects of protection, not subjects of governance.
% DISAPPEARANCE_RATIONALE: If the instrumental-subsidiarity frame vanished, the 'neutral tool' justification for procedural-only regulation would collapse. Two divergent rearrangements are likely: (1) a shift toward incarnational humanism — substantive bans on certain AI applications (autonomous weapons, social scoring, emotion recognition in schools/workplaces) grounded in human dignity as non-negotiable; (2) a shift toward technocratic optimization — deregulation framed as innovation acceleration, with harms externalized as 'transition costs.' The world does not stay the same; the constraint's disappearance forces a choice between competing readings of the kernel.
% FOUNDING_PROBLEM: The rapid deployment of AI systems in high-stakes domains (healthcare, criminal justice, employment, warfare) without any shared ethical or legal framework created a governance vacuum. The instrumental-subsidiarity reading emerged to fill that vacuum with a minimally controversial procedural consensus: technology is neutral, regulate its uses, protect dignity through transparency and human oversight.
% FOUNDING_PROBLEM_CORROBORATION: The Vatican's Dicastery for Culture and Education and the Pontifical Academy for Life attest the problem is live (ongoing deployment outpaces governance). Technology companies and regulatory authorities attest the founding problem is substantially addressed by existing frameworks (EU AI Act, US AI Executive Order, UNESCO Recommendation) and further regulation risks innovation. Independent civil society groups (Algorithmic Justice League, Access Now, Catholic Worker communities) attest the problem has shifted: the procedural frame itself now obstructs substantive justice by treating structural harms as compliance gaps.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.22) is moderate but rising: the constraint enables technology companies to capture the value of AI deployment while socializing harms onto populations with no exit. Suppression (0.18) is low currently but increasing as regulatory frameworks harden into compliance regimes that entrench incumbent advantages. Theater ratio (0.35) is significant: transparency reports, ethics boards, and impact assessments perform governance while the core business models (surveillance, prediction, behavioral modification) continue unchanged. Accessibility collapse (0.3) is moderate — alternatives (substantive bans, public ownership of AI infrastructure, data commons) remain thinkable but are marginalized by the 'neutral tool' frame. Resistance (0.45) is substantial from civil society, Global South governments, and incarnational-humanist theologians.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory/industry seat, the constraint is a rope: genuine coordination solving a real governance vacuum. From the marginalized-community seat, it is a snare: the 'neutral tool' narrative extracts their data and labor while returning only performative transparency. From the incarnational-humanist seat, it is a false summit: a mountain claim (this is what Catholic teaching requires) that conceals a constructed constraint benefiting power. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authorities and technology companies sit near the beneficiary end (d ~0.15-0.25): they gain legitimacy, market access, and regulatory capture from the frame. The legal profession benefits from new compliance fields (d ~0.3). Marginalized communities, displaced workers, and Global South populations sit near the target end (d ~0.8-0.9): they bear harms with no exit, and the constraint's 'protection' is procedural, not substantive. Catholic social ethicists are analytical observers (d ~0.5). The Vatican dicastery is an agenda-setter with analytical exit (d ~0.2) — it sets the frame but does not bear enforcement costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governance vacuum) was real in 2018. By 2024, the procedural infrastructure exists but the substantive harms have deepened. The constraint now persists not because it solves the problem but because it benefits the institutions that administer it (regulators, corporations, legal profession) and the theological frame that legitimizes it. This is mandatrophy: the mandate (protect human dignity through subsidiarity) has been hollowed out by the very proceduralism it authorized. The constraint is a tangled rope — real coordination function, real asymmetric extraction — not a pure snare, because the coordination infrastructure (risk tiers, transparency mandates, liability frameworks) would be needed under any reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutral_tool_framing_as_extraction_cover,
    'Does the ''morally neutral tool'' framing genuinely describe AI''s ontological status, or does it function as a cover story that extracts by preventing structural critique of the political economy of AI?',
    'Trace the genealogy of the neutrality claim in CST documents vs. industry lobbying documents; measure whether regulatory outcomes correlate more with industry preference or with substantive harm reduction for marginalized groups.',
    'If the framing is a cover story, the constraint''s effective extractiveness is significantly higher than its procedural coordination function warrants — it is a tangled rope masquerading as a rope. If genuine, the extraction is the unavoidable cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutral_tool_framing_as_extraction_cover, conceptual, 'Whether the neutrality claim is ontological or strategic.').

omega_variable(
    subsidiarity_as_procedural_capture,
    'Does subsidiarity-as-procedural-safeguard genuinely empower local actors, or does it capture the principle to deflect responsibility from the global capital-intensive decisions that drive AI deployment?',
    'Compare outcomes: when subsidiarity is invoked in AI governance, does decision-making authority and resources actually devolve to affected communities, or does it remain with national/international bodies while local actors get unfunded mandates?',
    'If procedural capture, the constraint''s coordination function is largely theatrical — the theater_ratio understates the gap between subsidiarity''s rhetorical invocation and its operational reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidiarity_as_procedural_capture, empirical, 'Whether subsidiarity functions as empowerment or deflection.').

omega_variable(
    reading_relations_foreclosure_vs_coexistence,
    'Does the instrumental-subsidiarity reading logically foreclose the incarnational-humanist reading within a single Catholic framework, or do they coexist as live positions held by different factions?',
    'Examine magisterial texts: does any authoritative document treat the incarnational-humanist claims (e.g., substantive bans on certain AI applications as intrinsically contrary to human dignity) as compatible with the instrumental-subsidiarity frame, or does it present them as mutually exclusive?',
    'If forecloses, the kernel has a structural fault line: the Church cannot hold both readings simultaneously. If coexists_with, the kernel is a site of ongoing contestation without logical resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_foreclosure_vs_coexistence, conceptual, 'Structural relationship between this reading and its incarnational-humanist sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 2018, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t2018, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(ai_h_tr_t2020, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(ai_h_tr_t2022, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2022, 0.28).
narrative_ontology:measurement(ai_h_tr_t2024, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2024, 0.35).
narrative_ontology:measurement(ai_h_tr_t2026, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2026, 0.4).
narrative_ontology:measurement(ai_h_tr_t2028, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2028, 0.45).
narrative_ontology:measurement(ai_h_tr_t2030, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2030, 0.5).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t2018, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2018, 0.12).
narrative_ontology:measurement(ai_h_be_t2020, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(ai_h_be_t2022, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2022, 0.19).
narrative_ontology:measurement(ai_h_be_t2024, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2024, 0.22).
narrative_ontology:measurement(ai_h_be_t2026, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2026, 0.24).
narrative_ontology:measurement(ai_h_be_t2028, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2028, 0.26).
narrative_ontology:measurement(ai_h_be_t2030, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2030, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t2018, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2018, 0.05).
narrative_ontology:measurement(ai_h_su_t2020, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2020, 0.08).
narrative_ontology:measurement(ai_h_su_t2022, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2022, 0.12).
narrative_ontology:measurement(ai_h_su_t2024, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2024, 0.18).
narrative_ontology:measurement(ai_h_su_t2026, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement(ai_h_su_t2028, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2028, 0.28).
narrative_ontology:measurement(ai_h_su_t2030, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2030, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__instrumental_subsidiarity, 0.12).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, eu_ai_act_governance).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, unesco_ai_ethics_recommendation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the ai_human_relationship kernel family. The instrumental_subsidiarity reading (this story) dominates institutional governance; the incarnational_humanism reading constitutes the primary critical alternative from within CST; the technocratic_optimization reading constitutes the market-aligned alternative. All three share the kernel's core ambiguity: whether AI's relationship to the human is instrumental, incarnational, or optimizing. Their ε values differ substantially: instrumental_subsidiarity ~0.22 (moderate extraction, real coordination); incarnational_humanism ~0.08 (low extraction, high coordination); technocratic_optimization ~0.65 (high extraction, low coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_human_relationship__instrumental_subsidiarity, institutional, 0.2).
constraint_indexing:directionality_override(ai_human_relationship__instrumental_subsidiarity, powerful, 0.25).
constraint_indexing:directionality_override(ai_human_relationship__instrumental_subsidiarity, powerless, 0.85).
constraint_indexing:directionality_override(ai_human_relationship__instrumental_subsidiarity, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
