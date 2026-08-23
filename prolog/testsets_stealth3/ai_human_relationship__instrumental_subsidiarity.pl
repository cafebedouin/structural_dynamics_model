% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: Instrumental-Subsidiarity Framework for Governing Artificial Intelligence
 *   domain: political theology / technology ethics / economic
 *
 * SUMMARY:
 *   A governance framework — built from Catholic social teaching's
 *   subsidiarity principle and generalized through multilateral soft law into
 *   hard regulation (risk-tiered classification, conformity assessment,
 *   transparency duties) — treats artificial intelligence as a morally
 *   neutral instrument whose moral character is fixed entirely at the point
 *   of use, with human dignity protected through legal process rather than
 *   through design-time ordering of the technology itself. The framework
 *   solves a real coordination problem (no actor can verify AI
 *   trustworthiness personally; shared rules create trust infrastructure)
 *   while simultaneously transferring the moral burden of system behavior
 *   away from design choices and onto adjudicated use-cases, concentrating
 *   legitimacy on deployers large enough to shape the rules they submit to.
 *   KEY AGENTS (by structural relationship): public_regulators
 *   (agenda-setting seat, institutional/constrained) draft and enforce
 *   use-category rules; large_ai_developers (primary beneficiary,
 *   institutional/arbitrage) collect legitimacy and design-time exemption
 *   while funding and staffing the advisory machinery;
 *   ai_compliance_and_audit_industry (secondary beneficiary,
 *   organized/mobile) collects the fee income the documentation duties
 *   create; general_public_as_ai_users (coordinated public, secondarily
 *   cost-bearing) receive disclosures and complaint channels while absorbing
 *   diffuse in-permitted-use harms; small_ai_deployment_firms (cost-bearing
 *   deployer, moderate/mobile) pay formality-proportional compliance;
 *   algorithmically_managed_workers and marginalized_data_subjects (target
 *   seats, powerless/trapped) bear unreviewed design-time harms between
 *   review cycles; faith_based_civil_society_ethics_bodies (excluded voice,
 *   moderate/identity_locked) contest the neutrality premise from outside the
 *   regulatory perimeter; cst_political_theologians (analytical observer) see
 *   the whole structure against the social-doctrine tradition. This file is
 *   ONE reading of the ai_human_relationship kernel; sibling readings are
 *   separate constraint files linked in network.affects_constraints. Claim
 *   and metrics are independent authored facts: the tangled_rope claim states
 *   what I judge structurally true; the metric values state what I judge
 *   descriptively true of the framework's operation as this reading itself
 *   assesses it — neither was tuned toward the other or toward a predicted
 *   engine verdict.
 *
 * KEY AGENTS:
 *   - public_regulators: agenda-setting seat (institutional/constrained) — drafts and enforces use-category rules, absorbs residual blame when harms slip between categories
 *   - large_ai_developers: primary beneficiary seat (institutional/arbitrage) — collects legitimacy and design-time exemption, funds and staffs the advisory machinery it nominally submits to
 *   - ai_compliance_and_audit_industry: secondary beneficiary (organized/mobile) — collects fee income created by the documentation and audit duties
 *   - general_public_as_ai_users: coordinated public, secondarily cost-bearing (organized/constrained)
 *   - small_ai_deployment_firms: cost-bearing deployer (moderate/mobile) — pays compliance costs proportional to formality, not scale
 *   - algorithmically_managed_workers: primary target seat (powerless/trapped) — bears design-time harms that were never adjudicated as uses
 *   - marginalized_data_subjects: primary target seat (powerless/trapped) — bears bias and surveillance in the lag between deployment and review
 *   - faith_based_civil_society_ethics_bodies: excluded voice (moderate/identity_locked) — contests the neutrality premise from outside the perimeter
 *   - cst_political_theologians: analytical observer — evaluates the framework against the tradition's own sources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.46).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.38).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.46).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "Instrumental-Subsidiarity Framework for Governing Artificial Intelligence").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "political theology / technology ethics / economic").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, 'e765c342-a6fc-491e-b1d9-2efcbca462a1').
narrative_ontology:cs_kernel_codification('e765c342-a6fc-491e-b1d9-2efcbca462a1', fixed_text).
narrative_ontology:cs_authority_grounding('e765c342-a6fc-491e-b1d9-2efcbca462a1', lineage).
narrative_ontology:cs_interpretation_layer_present('e765c342-a6fc-491e-b1d9-2efcbca462a1').
narrative_ontology:cs_reading_relation('e765c342-a6fc-491e-b1d9-2efcbca462a1', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_reading_relation('e765c342-a6fc-491e-b1d9-2efcbca462a1', ai_human_relationship__technocratic_optimization, influences).
narrative_ontology:cs_axiom('e765c342-a6fc-491e-b1d9-2efcbca462a1', foundational, technological_instrument_neutrality).
narrative_ontology:cs_axiom_status(technological_instrument_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('e765c342-a6fc-491e-b1d9-2efcbca462a1', technological_instrument_neutrality, empirically_contingent).
narrative_ontology:cs_axiom('e765c342-a6fc-491e-b1d9-2efcbca462a1', foundational, subsidiarity_level_appropriate_authority).
narrative_ontology:cs_axiom_status(subsidiarity_level_appropriate_authority, holdable).
narrative_ontology:cs_axiom_grounding('e765c342-a6fc-491e-b1d9-2efcbca462a1', subsidiarity_level_appropriate_authority, conventional).
narrative_ontology:cs_reference_frame('e765c342-a6fc-491e-b1d9-2efcbca462a1', neutral_instrument_stewardship).
narrative_ontology:cs_drift_state('e765c342-a6fc-491e-b1d9-2efcbca462a1', contemporary_ai_act_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e765c342-a6fc-491e-b1d9-2efcbca462a1', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, large_ai_developers).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ai_compliance_and_audit_industry).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, public_regulators).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, general_public_as_ai_users).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, small_ai_deployment_firms).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, algorithmically_managed_workers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, marginalized_data_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, small_ai_deployment_firms).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, general_public_as_ai_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce the use-category rules: classify AI systems by risk tier, require conformity assessment before deployment, run transparency and incident-reporting regimes, convene advisory and standards bodies. Mandates come from legislatures and treaty processes; budgets and expertise lag deployment pace. When harm slips between reviewed use-categories, the blame lands here first.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, public_regulators, agenda_setter,
    institutional, generational, constrained, continental).

% Build and operate frontier models deployed worldwide. Under this arrangement the scrutiny of a system begins at its use-cases, so design choices — training data, objectives, choice architecture — face no standing review; passing use-case conformity buys legitimacy and market access. Fund and staff the advisory bodies that draft the rules they then implement; keep legal and operational presence in multiple jurisdictions so an adverse rule in one can be answered by shifting emphasis, restructuring products, or relocating compute.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, large_ai_developers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, large_ai_developers, agenda_setter).

% Sell what the framework mandates: conformity assessment, algorithmic audits, documentation pipelines, ethics-board staffing. Revenue scales with the volume and stringency of regulation; clients span deployers and public buyers. Skills and staff move freely across clients and jurisdictions.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_compliance_and_audit_industry, beneficiary,
    organized, biographical, mobile, global).

% Receive the framework's public-facing goods: disclosures about where AI operates, channels to complain, assurance that flagged misuse categories are restricted. Carry the diffuse side: personal data feeding permitted systems, persuasion architectures operating inside approved uses, protection arriving per-use-case only after a category is defined and enforced.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, general_public_as_ai_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, general_public_as_ai_users, payer).

% Meet the same registration, documentation, and audit thresholds written for actors many times their size. The fixed cost of conformity consumes a large share of engineering capacity; the trust signal and procurement access the framework grants are real but cannot be amortized at scale. Can pivot products or enter less stringent markets, though client lock-in and thin capital slow the move.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, small_ai_deployment_firms, payer,
    moderate, immediate, mobile, regional).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, small_ai_deployment_firms, beneficiary).

% Are scheduled, rated, priced, and dispatched by systems whose design decisions were never presented for review as a 'use'. Remedy arrives through labor-law channels after harm accumulates into grievable form. Individual exit is job loss in labor markets where algorithmically managed work is often the available work.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, algorithmically_managed_workers, payer,
    powerless, biographical, trapped, global).

% Live inside datafied infrastructure — credit, housing, benefits, policing — where model error lands hardest on those least able to contest records. Recourse runs through complaint mechanisms keyed to documented use-cases, which typically trail deployment by years. Opting out of the infrastructure is not a live option.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, marginalized_data_subjects, payer,
    powerless, generational, trapped, global).

% Theological and community-rooted ethicists who hold that a technology's moral character is settled partly in its design, not only its uses. Consulted late in drafting, heard in hearings, rarely holding veto or standing membership in standards bodies. Their critique is constitutive of their vocation; withdrawing it would dissolve the witness they exist to offer.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, faith_based_civil_society_ethics_bodies, excluded,
    moderate, generational, identity_locked, global).

% Scholars working from Catholic social doctrine trace the framework against its own sources: subsidiarity paired with solidarity, the dignity of labor, the person as imago Dei. Neither collecting nor paying inside the arrangement; their seat exists to see whether the procedural safeguard honors the doctrine it cites.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, cst_political_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, large_ai_developers).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared legal and ethical operating order for a general-purpose technology no single actor can oversee alone: permissible and prohibited use categories, conformity assessment before deployment, transparency and documentation duties, and a division of review labor by competence level — handled locally where local capacity suffices, escalated where harms exceed local reach.
% TRANSFER_FUNCTION: Moves rule-making authority upward to transnational and national regulators and standards bodies; moves compliance cost onto deployers in proportion to formality rather than scale, falling hardest on small firms; moves legitimacy and market access to deployers who pass review; moves harm-bearance to workers and data subjects in the gap between deployment and regulatory catch-up; relocates moral responsibility for system behavior from design-time choices to adjudicated use-cases.
% ABSENT_VOICES: Faith-based and community ethicists who dispute the neutrality premise hold no veto — they enter as consultees after drafting. Populations in testing and deployment regions outside the drafting jurisdictions are absent from the fora where use-categories are defined. Future generations affected by infrastructure path-dependence have no seat anywhere in the process.
% DISAPPEARANCE_RATIONALE: Compliance industries, regulator mandates, corporate AI-governance offices, and cross-border deployment agreements are organized around this framework; overnight removal would strand conformity regimes, reopen jurisdictional fragmentation, and throw the moral-status question of AI back into raw contest among the rival readings of the human-AI relationship.
% FOUNDING_PROBLEM: Mid-twentieth-century social teaching confronted techniques and economic powers outstripping any single actor's oversight: how to let beneficial technique flourish while guaranteeing it answers to human dignity, without either prohibition or surrender to unchecked expert management. Applied to artificial intelligence (Rome Call for AI Ethics, 2020, and subsequent dicasterial and episcopal documents), the problem was stated as: govern the instrument through layered lawful authority, deciding at the most local competent level.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the framework's benefiting parties: the UNESCO Recommendation on the Ethics of AI (2021) and the OECD AI Principles (2019) record the same governance problem as live; independent science-and-technology-studies and legal scholarship — including critics who characterize the framework's proceduralism as ethics-washing — corroborates that the problem is real while disputing this reading's answer. No attesting source is a deployer or a compliance vendor alone.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.46, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (0.46 at interval end) is moderate and rising: the framework began as aspirational soft law with little to collect (2016: 0.30) and accumulated collection surfaces as it formalized — conformity regimes, documentation economies, agenda-shaped standards — with the decisive gain (design-time exemption from scrutiny) accruing to the largest deployers. Suppression (0.38) is authored as a raw structural property of the framework's enforcement apparatus — penalty regimes, market-access gating, conformity requirements — and is deliberately left unscaled here; the engine alone scales extraction by directionality and scope. Theater (0.43) reflects the documented growth of performative compliance: ethics boards that advise without authority, audits that attest without replicating, transparency reports that disclose selectively. Accessibility collapse (0.38) is moderate: rival readings circulate openly, deployers can forum-shop jurisdictions, and communities can organize — alternatives narrow but do not seal shut. Resistance (0.55) is substantial and bidirectional: industry lobbying against stringency from one side, theological and civil-society insistence on design-time responsibility from the other, plus worker organizing around algorithmic management. The temporal series runs on one shared six-point grid (2016–2026, biennial) with all three metrics authored at every point; the rising suppression_requirement series is included because the story genuinely traces enforcement-capacity change (soft law maturing into penalty-backed conformity regimes), not mere extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat (public_regulators), the framework is its proper work: a protective order it built and administers, experienced as coordination with a duty attached. From the target seats (algorithmically_managed_workers, marginalized_data_subjects), the identical structure operates as delayed protection: remedy keyed to use-categories that trail deployment by years, with no practical exit from the infrastructures involved. From the primary beneficiary seat (large_ai_developers), it is license plus legitimacy: conformity purchased once, design discretion preserved, agenda access maintained. The engine computes this divergence per seat from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: large_ai_developers (arbitrage-grade exit pushes them toward the beneficiary pole despite their own compliance outlays — relocation and product-restructuring options dampen whatever costs land on them), ai_compliance_and_audit_industry (pure collector), public_regulators (mandate, budget, and jurisdiction flow in), general_public_as_ai_users (trust goods received). Victims derive high directionality: the two trapped seats (workers, data subjects) sit nearest the full-target end — their exits are job loss and infrastructural exile respectively — while small_ai_deployment_firms sit targetward but short of trapped, since mobility partially offsets their cost burden. I authored no directionality_overrides deliberately: the override mechanism keys on the power atom alone, and the two institutional seats here diverge in opposite directions (regulators slightly targetward of their derived beneficiary position due to blame absorption and capacity strain; large developers correctly near the beneficiary pole) — a single power-atom-wide correction would move both the wrong way, so the per-seat derivation from roles plus exit options is more faithful than any override available at this keying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing a general-purpose technology without prohibition or technocratic surrender — is still live, so no mandatrophy resolution is declared and none is due. The tangled_rope claim guards against two opposite misreadings. Reading the framework as its self-description (pure coordination: rules creating trust so a powerful technique can serve human ends) would hide the structural transfer this reading performs — moving moral responsibility from design-time to use-time, and moving compliance cost onto actors by formality rather than capacity. Reading it as its critics' description (pure extraction: regulation as ethics-washing in service of deployers) would erase the genuine coordination goods — shared risk taxonomy, conformity infrastructure, complaint channels — that would genuinely rearrange if the framework vanished. Keeping both faces visible is precisely the hybrid's analytic work. The Goodhart watch runs through the temporal series: theater_ratio climbing past 0.5 would signal the protective function being displaced by its performance, the characteristic first symptom on the atrophy trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_disagreement,
    'This constraint is the instrumental_subsidiarity reading of the ai_human_relationship kernel; is use-time adjudication the correct locus of moral responsibility for AI systems, against the sibling readings'' design-time ordering (incarnational_humanism) or output-metric accounting (technocratic_optimization)?',
    'Cross-reading comparison within the kernel family: compile the sibling stories, compare victim sets, epsilon values, and enforcement logics against observed harm incidence; the reading whose victim set captures harms the others miss is the one whose responsibility-locus matches the evidence.',
    'If design-time ordering proves the true harm locus, this reading''s epsilon understates the arrangement''s take and its victim set is incomplete, trending classification toward enforced extraction; if output metrics suffice, this reading over-governs and part of its coordination cost is deadweight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_locus_disagreement, conceptual, 'Kernel-membership ambiguity: which responsibility-locus reading of the human-AI relationship does the evidence support.').

omega_variable(
    design_time_agency_evidence,
    'Do deployed AI systems exercise effective agency at design time — engagement optimization, baked-in bias, choice architecture — such that ''neutral tool awaiting use'' misdescribes them?',
    'Harm-attribution studies separating injuries caused by model and design choices from injuries caused by operator context; audits comparing pre-deployment model behavior against post-deployment use patterns.',
    'Strong design-time agency defeats the neutrality premise empirically: the foundational axiom shifts toward overridden, epsilon rises, and the framework''s protection gap becomes formal rather than incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_time_agency_evidence, empirical, 'Whether artifact-level moral agency falsifies the neutrality axiom this reading stands on.').

omega_variable(
    subsidiarity_capacity_abandonment,
    'Does delegating review to the most local competent level protect dignity, or abandon diffuse harms wherever local capacity is absent — subsidiarity without its traditional pairing with solidarity?',
    'Comparative outcome study across jurisdictions matched for deployment intensity but differing in local review capacity: measure harm-detection latency and remedy completion rates.',
    'Where abandonment dominates, the victim set broadens to all under-resourced localities, effective extraction rises with scope, and the procedural safeguard reads as cost-shifting downward rather than protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_capacity_abandonment, empirical, 'Whether the subsidiarity safeguard delivers protection or exports governance burdens to the least capacitated level.').

omega_variable(
    advisory_capture_depth,
    'How deeply do large deployers shape the use-categories, standards, and advisory composition through which the framework adjudicates?',
    'Disclosure analysis: advisory-body composition, comment-letter provenance in rule dockets, revolving-door employment records between agencies and deployers.',
    'Deep capture consolidates the gains further on large_ai_developers and trends the arrangement from hybrid toward enforced extraction; shallow capture supports the coordination-first reading of the same structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(advisory_capture_depth, empirical, 'Depth of deployer influence over the framework''s adjudicative content.').

omega_variable(
    ethics_washing_ratio,
    'What fraction of the framework''s transparency, audit, and ethics-board activity verifies anything, as opposed to performing compliance?',
    'Independent audit-replication studies: re-run published algorithmic audits, compare disclosed practices against observed ones, track board-recommendation adoption rates.',
    'A performative share above half would indicate proxy goals displacing the protective function (Goodhart drift) and place the arrangement on the atrophy trajectory the temporal series is watching.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ethics_washing_ratio, empirical, 'Performative share of the framework''s protective activity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 2016, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t2016, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2016, 0.25).
narrative_ontology:measurement(ai_h_tr_t2018, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(ai_h_tr_t2020, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2020, 0.32).
narrative_ontology:measurement(ai_h_tr_t2022, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2022, 0.36).
narrative_ontology:measurement(ai_h_tr_t2024, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2024, 0.4).
narrative_ontology:measurement(ai_h_tr_t2026, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2026, 0.43).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t2016, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2016, 0.3).
narrative_ontology:measurement(ai_h_be_t2018, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2018, 0.33).
narrative_ontology:measurement(ai_h_be_t2020, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2020, 0.36).
narrative_ontology:measurement(ai_h_be_t2022, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2022, 0.39).
narrative_ontology:measurement(ai_h_be_t2024, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2024, 0.43).
narrative_ontology:measurement(ai_h_be_t2026, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2026, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t2016, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2016, 0.2).
narrative_ontology:measurement(ai_h_su_t2018, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2018, 0.24).
narrative_ontology:measurement(ai_h_su_t2020, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2020, 0.29).
narrative_ontology:measurement(ai_h_su_t2022, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2022, 0.33).
narrative_ontology:measurement(ai_h_su_t2024, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2024, 0.36).
narrative_ontology:measurement(ai_h_su_t2026, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).

% DUAL FORMULATION NOTE:
% The colloquial label 'how should humanity relate to AI' decomposes, per the epsilon-invariance principle, into three structurally distinct arrangements sharing one label. This story authors the instrumental_subsidiarity arrangement: neutrality premise, use-time adjudication, layered lawful authority — epsilon authored from that reading's own lights over that referent. Siblings: ai_human_relationship__incarnational_humanism (teleological ordering of technology to integral human development; design-time moral weight) and ai_human_relationship__technocratic_optimization (instrumentality without the dignity overlay; efficiency as the governing metric). Upstream/downstream: the incarnational reading supplies the normative vocabulary this reading invokes while declining its design-time commitments; this reading's regulatory apparatus sets the legitimacy conditions under which the technocratic reading may operate. Each file carries its own epsilon, victim set, and enforcement logic; the edges express citation-and-constraint coupling, not equivalence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
