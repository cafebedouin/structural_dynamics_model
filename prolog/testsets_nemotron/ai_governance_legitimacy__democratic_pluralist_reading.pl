% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__democratic_pluralist_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__democratic_pluralist_reading
 *   human_readable: Democratic Pluralist AI Governance Legitimacy
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint story captures the democratic pluralist reading of AI
 *   governance legitimacy: legitimacy derives from inclusive democratic
 *   deliberation and consent, not from any single religious, technocratic, or
 *   market tradition. The encyclical (e.g., Vatican AI ethics documents)
 *   contributes one voice among many in the public square. The constraint is
 *   a scaffold — it builds participatory infrastructure (citizen assemblies,
 *   multi-stakeholder forums, judicial review mechanisms, civil liberties
 *   protections) meant to transition toward self-sustaining democratic
 *   governance of AI. The claimed type is scaffold; the authored metrics
 *   reflect moderate extraction (deliberative cost, compliance burden) and
 *   moderate suppression (exclusion of anti-democratic actors, enforcement of
 *   procedural norms). The kernel_id is 'ai_governance_legitimacy'; this
 *   reading_id is 'democratic_pluralist_reading'. Sibling readings:
 *   magisterial_subsidiarity_reading, technocratic_optimization_reading,
 *   market_libertarian_reading.
 *
 * KEY AGENTS:
 *   - civil_society_organizations: Primary beneficiary (organized/mobile) — gains deliberative infrastructure and institutional voice
 *   - democratic_institutions: Primary beneficiary (institutional/arbitrage) — gains legitimacy framework and accountability mechanisms
 *   - minority_rights_holders: Primary beneficiary (powerless/constrained) — gains protection against majoritarian AI harms
 *   - deliberatively_excluded_populations: Primary victim (powerless/trapped) — bears costs of AI systems without deliberative voice
 *   - populations_under_authoritarian_regimes: Primary victim (powerless/trapped) — subject to AI governance without consent
 *   - magisterial_authority: Excluded (institutional/identity_locked) — would claim unique interpretive authority but is denied monopoly
 *   - technocratic_elites: Secondary victim/payer (powerful/constrained) — loses unrestricted optimization authority
 *   - market_innovators: Secondary victim/payer (powerful/mobile) — loses unencumbered innovation space
 *   - democratic_citizens: Secondary beneficiary (organized/mobile) — gains participatory rights and transparency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.35).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "Democratic Pluralist AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, 'e2d473d1-daeb-4a89-ad1f-619e42115d4b').
narrative_ontology:cs_kernel_codification('e2d473d1-daeb-4a89-ad1f-619e42115d4b', distributed).
narrative_ontology:cs_authority_grounding('e2d473d1-daeb-4a89-ad1f-619e42115d4b', practice).
narrative_ontology:cs_interpretation_layer_present('e2d473d1-daeb-4a89-ad1f-619e42115d4b').
narrative_ontology:cs_reading_relation('e2d473d1-daeb-4a89-ad1f-619e42115d4b', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2d473d1-daeb-4a89-ad1f-619e42115d4b', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2d473d1-daeb-4a89-ad1f-619e42115d4b', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('e2d473d1-daeb-4a89-ad1f-619e42115d4b', foundational, legitimacy_requires_democratic_consent).
narrative_ontology:cs_axiom_status(legitimacy_requires_democratic_consent, holdable).
narrative_ontology:cs_axiom_grounding('e2d473d1-daeb-4a89-ad1f-619e42115d4b', legitimacy_requires_democratic_consent, deontological).
narrative_ontology:cs_axiom('e2d473d1-daeb-4a89-ad1f-619e42115d4b', foundational, no_single_tradition_holds_interpretive_monopoly).
narrative_ontology:cs_axiom_status(no_single_tradition_holds_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('e2d473d1-daeb-4a89-ad1f-619e42115d4b', no_single_tradition_holds_interpretive_monopoly, deontological).
narrative_ontology:cs_axiom('e2d473d1-daeb-4a89-ad1f-619e42115d4b', secondary, principles_emerge_from_inclusive_public_reason).
narrative_ontology:cs_axiom_status(principles_emerge_from_inclusive_public_reason, holdable).
narrative_ontology:cs_axiom_grounding('e2d473d1-daeb-4a89-ad1f-619e42115d4b', principles_emerge_from_inclusive_public_reason, conventional).
narrative_ontology:cs_reference_frame('e2d473d1-daeb-4a89-ad1f-619e42115d4b', pluralist_deliberative_legitimacy).
narrative_ontology:cs_drift_state('e2d473d1-daeb-4a89-ad1f-619e42115d4b', contemporary_ai_governance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e2d473d1-daeb-4a89-ad1f-619e42115d4b', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, deliberatively_excluded_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_citizens).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_elites).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, market_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain institutional voice and deliberative infrastructure for AI governance. They operate the citizen assemblies, multi-stakeholder forums, and advocacy channels that the scaffold builds. Their exit is mobile — they can shift to other issue domains or governance arenas if this constraint becomes extractive.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, global).

% Gain a legitimacy framework and accountability mechanisms for AI governance (judicial review, legislative oversight, regulatory mandates). They set the procedural agenda for deliberation. Their exit is arbitrage-grade — they can adopt alternative legitimacy frameworks (technocratic, market-based) if the democratic scaffold proves costly.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter).

% Gain protection against majoritarian AI harms (algorithmic bias, surveillance, exclusion) through rights frameworks and participatory guarantees. Their exit is constrained — they cannot easily leave the jurisdiction or the technological systems that govern them, but the scaffold's deliberative mechanisms are their primary voice.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    powerless, biographical, constrained, global).

% Bear the costs of AI systems (automated decision-making, surveillance, resource allocation) without access to the deliberative processes that legitimize them. Includes undocumented migrants, stateless persons, populations in conflict zones, and digitally excluded communities. Their exit is trapped — no effective voice, no alternative governance access.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, deliberatively_excluded_populations, payer,
    powerless, biographical, trapped, global).

% Subject to AI governance (social credit, predictive policing, censorship) without consent or deliberative recourse. The democratic pluralist constraint claims universal legitimacy but its enforcement mechanisms (electoral accountability, judicial review, civil liberties) are absent in their context. Their exit is trapped — the constraint's own beneficiaries (democratic institutions) do not exist for them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, populations_under_authoritarian_regimes, payer,
    powerless, biographical, trapped, global).

% Would claim unique interpretive authority over the encyclical's dignity principles and their application to AI. Under this reading, the Magisterium is one voice among many — it participates in public reason but holds no monopoly. Its exit is identity_locked: the Magisterium's self-understanding is bound to its interpretive role; it cannot 'exit' the claim without ceasing to be what it is.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_authority, excluded,
    institutional, civilizational, identity_locked, global).

% Lose unrestricted optimization authority — AI systems must now pass through deliberative review, rights impact assessments, and public justification. They pay compliance costs and accept procedural delays. Their exit is constrained — they can relocate R&D but cannot escape the governance frameworks of major markets.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_elites, payer,
    powerful, biographical, constrained, global).

% Lose unencumbered innovation space — must navigate regulatory sandboxes, ethics review boards, and public consultation requirements. They pay compliance costs and accept slower deployment. Their exit is mobile — they can choose jurisdictions with lighter deliberative burdens, though major markets converge.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, market_innovators, payer,
    powerful, biographical, mobile, global).

% Gain participatory rights (citizen assemblies, consultation processes, transparency mandates) and protection from unaccountable AI systems. They bear the deliberative cost (time, attention, cognitive load) but receive the coordination surplus. Their exit is mobile — they can disengage from participation, though the constraint's protections persist.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_citizens, beneficiary,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__democratic_pluralist_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__democratic_pluralist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of legitimate AI governance in pluralistic societies: how to govern transformative technology when no single moral, religious, or technical tradition commands universal assent. Builds deliberative infrastructure (citizen assemblies, multi-stakeholder forums, judicial review, rights frameworks) that translates diverse values into binding collective decisions.
% TRANSFER_FUNCTION: Moves deliberative costs (time, expertise, institutional overhead) from all participants to produce legitimate governance outputs. Moves protection from unaccountable AI systems to minority rights holders and excluded populations. Moves accountability burdens onto technocratic elites and market innovators. Moves interpretive authority away from the Magisterium and technocratic elites toward inclusive public reason.
% ABSENT_VOICES: Future generations (cannot deliberate on AI systems that will shape their world), non-human animals and ecosystems (affected by AI-driven resource extraction and environmental decisions but excluded from human deliberation), populations in failed states or conflict zones (no democratic institutions to participate in), AI systems themselves (if they develop moral status, they would be governed without representation). These voices are structurally excluded from the deliberative infrastructure this scaffold builds.
% DISAPPEARANCE_RATIONALE: If the democratic pluralist constraint vanished overnight, AI governance would revert to de facto technocratic optimization in democratic states, magisterial authority in Catholic-influenced jurisdictions, market libertarianism in deregulated zones, and authoritarian control elsewhere. The deliberative infrastructure (citizen assemblies, rights frameworks, judicial review of AI) would collapse. Minority rights holders would lose their primary protection against majoritarian AI harms. The world would rearrange around the sibling readings' legitimacy claims.
% FOUNDING_PROBLEM: The legitimacy vacuum of early AI governance: systems deployed at scale with no democratic authorization, no accountability to affected populations, and no mechanism to balance diverse values. Technocratic elites optimized for efficiency; corporations optimized for engagement; states optimized for control. No public consent, no rights floor, no pluralistic deliberation.
% FOUNDING_PROBLEM_CORROBORATION: Democratic theorists (Habermas, Rawls, Benhabib) attest the founding problem is live: pluralistic legitimacy requires continuous deliberative renewal. Technocratic proponents (Crawford, Marcus) attest the problem is substantially solved by expert governance and the deliberative scaffold is obstructive. Magisterial authorities attest the problem is misdiagnosed: the vacuum is filled by Catholic Social Doctrine, not public reason. Market libertarians (Andreessen, Thiel) attest the problem is a feature: permissionless innovation solves it. No single corroboration settles the dispute.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.4, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).
:- end_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.4) reflects the real cost of deliberative participation: time, cognitive effort, institutional overhead, compliance burden distributed across all seats. Suppression (0.35) reflects the constraint's exclusion of anti-democratic governance modes (authoritarian, theocratic, technocratic-autocratic) — not extraction from the excluded, but the cost of maintaining the deliberative space. Theater (0.25) captures the gap between formal inclusion and effective power: citizen assemblies often advisory, consultations ritualized, minority voices tokenized. The scaffold classification fits: the constraint builds infrastructure (deliberative institutions, rights frameworks, accountability mechanisms) justified by the transition to mature democratic AI governance, with a sunset clause (has_sunset_clause: true) tied to the consolidation of participatory capacity. The claimed type and metrics are independent: the reading claims scaffold; the metrics describe a constraint that extracts moderately through deliberative cost and suppresses moderately by excluding anti-democratic alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (civil_society_organizations, democratic_institutions, minority_rights_holders) receive the coordination surplus: deliberative infrastructure, accountability mechanisms, rights protections. Their directionality is toward the beneficiary end (d ~ 0.2–0.3) because the constraint subsidizes their participation. Victims (deliberatively_excluded_populations, populations_under_authoritarian_regimes) bear extraction without voice: they are subject to AI systems governed by processes they cannot access. Their directionality is toward the target end (d ~ 0.8–0.9). The magisterial_authority is excluded — it would be a beneficiary under the magisterial_subsidiarity_reading but here is denied interpretive monopoly; its directionality is ambiguous (d ~ 0.5) because it gains some legitimacy from participation but loses unique authority. Technocratic_elites and market_innovators are payers: they lose unrestricted optimization/innovation space but gain stable governance frameworks.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling this constraint as pure extraction (snare) or pure coordination (rope). It is neither: the deliberative infrastructure is genuine coordination (solves the problem of legitimate AI governance in pluralistic societies) but extracts real costs (participation burden, compliance overhead) and suppresses alternatives (anti-democratic governance). The mandate is transitional: the infrastructure is meant to become self-sustaining civic capacity. If the sunset clause is performative and the infrastructure becomes permanent extraction without portable capacity, mandatrophy resolves to piton. The omega 'sunset_credibility' tracks this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the contested kernel ''ai_governance_legitimacy'' — the democratic_pluralist_reading. What would the sibling readings change structurally?',
    'Comparative constraint authoring: each sibling reading instantiates its own constraint story with its own ε, beneficiary/victim structure, and classification. The kernel_id links them for cross-reading analysis.',
    'Sibling readings produce different ε values (magisterial_subsidiarity: lower ε from Magisterium seat, higher from excluded traditions; market_libertarian: lower ε for innovators, higher for regulated populations; technocratic_optimization: lower ε for technical elites, higher for populations subject to optimization). The democratic_pluralist reading''s ε is moderate (0.4) because it distributes extraction across all seats via deliberative cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This reading''s structural identity within the kernel family').

omega_variable(
    deliberative_inclusivity_gap,
    'How completely does this reading''s deliberative infrastructure actually include the populations it claims to benefit — especially minority rights holders and globally marginalized groups?',
    'Empirical audit of participation mechanisms: who has effective voice in AI governance forums, whose interests are represented in policy outcomes, what structural barriers persist despite formal inclusion.',
    'If the gap is large, the reading''s claimed scaffold function is partially fictive — extraction from excluded populations continues under a coordination cover story. If small, the scaffold is genuinely building participatory capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_inclusivity_gap, empirical, 'Gap between formal inclusivity and effective deliberative power').

omega_variable(
    sunset_credibility,
    'Does the scaffold''s sunset clause represent a genuine transitional commitment, or has the deliberative infrastructure become a permanent feature that extracts compliance without delivering transferable capacity?',
    'Longitudinal tracking: does the constraint''s theater_ratio rise over time while extractiveness holds? Do the deliberative mechanisms produce portable civic capacity or only ritualized consultation?',
    'If theater rises and portable capacity does not accumulate, the scaffold has degraded into a piton — the sunset clause is performative, the infrastructure is extractive maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_credibility, empirical, 'Whether the transitional justification remains operative').

omega_variable(
    encyclical_authority_boundary,
    'Where exactly does this reading draw the line between accepting the encyclical''s dignity claims and rejecting the Magisterium''s interpretive monopoly — and is that boundary stable?',
    'Doctrinal analysis: track whether the reading''s use of encyclical language creates a de facto interpretive dependency that the Magisterium can leverage, or whether the reading maintains genuine independence.',
    'If the boundary is porous, the reading may function as a Trojan horse for magisterial authority under pluralist cover. If sharp, the reading genuinely pluralizes the authority structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encyclical_authority_boundary, conceptual, 'Stability of the encyclical-acceptance/interpretive-rejection boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 25, 0.25).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 25, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(ai_g_su_t25, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 25, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__democratic_pluralist_reading, 0.1).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This is the democratic_pluralist_reading of the ai_governance_legitimacy kernel. It decomposes the kernel's single label into a constraint with moderate ε (0.4) distributed via deliberative cost across all seats, beneficiaries = civil society/democratic institutions/minority rights holders, victims = deliberatively excluded populations, type = scaffold (transitional participatory infrastructure). The magisterial_subsidiarity_reading would have lower ε from the Magisterium seat but higher from excluded traditions; technocratic_optimization_reading would have lower ε for technical elites; market_libertarian_reading would have lower ε for innovators. They are distinct constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__democratic_pluralist_reading, institutional, 0.25).
constraint_indexing:directionality_override(ai_governance_legitimacy__democratic_pluralist_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
