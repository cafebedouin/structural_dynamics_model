% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Negotiated Commons Governance
 *   domain: political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   Software control is conventionally framed as a binary: either developers
 *   have absolute property rights (proprietary/commercial reading) or users
 *   have absolute freedom rights (copyleft/freedom reading). The commons
 *   reading rejects both absolutisms and treats software governance as a
 *   legitimately negotiated domain. What gets extracted: participation rights
 *   and legitimacy are moved from absolutist activist-or-corporate voices
 *   toward institutionalized governance structures. What gets coordinated:
 *   mixed-investment ecosystems that require both proprietary and open
 *   components to function. Both absolutist positions are positioned as
 *   victims of the commons frame because their claims to universal legitimacy
 *   are reinterpreted as one stakeholder position among others. The
 *   constraint is claimed as tangled_rope because it delivers real
 *   coordination (shared infrastructure governance) alongside asymmetric
 *   extraction (absolutists lose universal moral authority; proprietarian
 *   firms lose unilateral control rights; freedom advocates lose the frame of
 *   liberation). Extractiveness increases over the first 30 years as commons
 *   governance institutions solidify, then plateaus or slightly decreases as
 *   stakeholder-accommodation mechanisms mature.
 *
 * KEY AGENTS:
 *   - Stakeholder communities (organized, generational, constrained exit): benefit from participatory governance
 *   - Development ecosystems (organized, generational, mobile): benefit from legitimized mixed-model sustainability
 *   - Absolutist freedom advocates (moderate, biographical, identity-locked): lose universal moral framing; repositioned as stakeholders
 *   - Proprietarian developers (powerful, biographical, arbitrage): lose unilateral control authority; must negotiate in governance
 *   - Infrastructure stewards (institutional, generational, constrained): set and enforce commons governance rules
 *   - Pragmatic hybrid actors (powerful, biographical, mobile): benefit from legitimacy of dual positioning
 *   - Excluded governance participants (moderate, biographical, constrained): nominally included but structurally inaccessible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.58).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.42).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Negotiated Commons Governance").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, 'f282b4a2-7898-470b-81b6-adf01161397b').
narrative_ontology:cs_kernel_codification('f282b4a2-7898-470b-81b6-adf01161397b', distributed).
narrative_ontology:cs_authority_grounding('f282b4a2-7898-470b-81b6-adf01161397b', distributed).
narrative_ontology:cs_reading_relation('f282b4a2-7898-470b-81b6-adf01161397b', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('f282b4a2-7898-470b-81b6-adf01161397b', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('f282b4a2-7898-470b-81b6-adf01161397b', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_axiom('f282b4a2-7898-470b-81b6-adf01161397b', foundational, software_governance_legitimately_negotiated).
narrative_ontology:cs_axiom_status(software_governance_legitimately_negotiated, holdable).
narrative_ontology:cs_axiom_grounding('f282b4a2-7898-470b-81b6-adf01161397b', software_governance_legitimately_negotiated, conventional).
narrative_ontology:cs_axiom('f282b4a2-7898-470b-81b6-adf01161397b', foundational, stakeholder_participation_required_for_legitimacy).
narrative_ontology:cs_axiom_status(stakeholder_participation_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f282b4a2-7898-470b-81b6-adf01161397b', stakeholder_participation_required_for_legitimacy, deontological).
narrative_ontology:cs_axiom('f282b4a2-7898-470b-81b6-adf01161397b', secondary, absolutism_illegitimate_cover_for_power_consolidation).
narrative_ontology:cs_axiom_status(absolutism_illegitimate_cover_for_power_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('f282b4a2-7898-470b-81b6-adf01161397b', absolutism_illegitimate_cover_for_power_consolidation, empirically_contingent).
narrative_ontology:cs_reference_frame('f282b4a2-7898-470b-81b6-adf01161397b', negotiated_collective_governance).
narrative_ontology:cs_drift_state('f282b4a2-7898-470b-81b6-adf01161397b', contemporary_2020s_accessibility_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f282b4a2-7898-470b-81b6-adf01161397b', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, development_ecosystems).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolutist_freedom_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, proprietarian_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, excluded_governance_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, pragmatic_hybrid_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users, developers, and institutions that participate in commons-based governance mechanisms for shared digital infrastructure. They gain from negotiated rules that balance access, modification rights, and sustainability. They contribute to governance structures (RFC processes, foundation boards, license committees) and benefit from codified participation mechanisms that prevent unilateral seizure by either property-absolute or freedom-absolute factions.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, beneficiary,
    organized, generational, constrained, global).

% Open-source projects, shared libraries, and collaborative infrastructure whose sustainability depends on explicit governance arrangements. They benefit from legitimized commons structures that enable mixed funding models (permissive licenses + commercial support, dual-licensing with governance protections, foundation-managed stewardship). Their function persists when software control is treated as a governance question rather than a binary property/freedom choice.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, development_ecosystems, beneficiary,
    organized, generational, mobile, global).

% Activists and developers whose worldview centers software as a human right and proprietary control as inherently oppressive. The commons reading denies their absolutist framing by treating software governance as a legitimate domain for negotiation rather than as a binary choice of freedom vs. oppression. They bear the cost of being repositioned as one stakeholder among others rather than as the voice of universal justice.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolutist_freedom_advocates, payer,
    moderate, biographical, identity_locked, global).

% Commercial software firms and developers whose business model rests on proprietary control as a legitimate property right. The commons reading denies their absolutist framing by treating property claims as negotiable within governance structures rather than as fundamental. They bear the cost of legitimacy pressure to participate in governance rather than simply enforcing property boundaries unilaterally.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, proprietarian_developers, payer,
    powerful, biographical, arbitrage, global).

% Developers and users in the Global South, small-scale implementers, and non-English-speaking communities whose participation in governance structures is often structurally blocked by language, resource, or institutional barriers. The commons framing claims to include them but enforcement mechanisms often exclude them. They pay the cost of nominally participatory governance that remains inaccessible.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, excluded_governance_participants, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, excluded_governance_participants, observer).

% Foundations, governance boards, and maintainers that administer commons frameworks (Linux Foundation, Apache Software Foundation, Python Software Foundation, etc.). They set the terms of participation, enforce license compliance, manage decision-making processes, and mediate between absolutist claims and pragmatic sustainability. They collect legitimacy from the consensus that commons governance is the appropriate frame.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, infrastructure_stewards, agenda_setter,
    institutional, generational, constrained, global).

% Large technology firms (Google, Meta, Apple, Microsoft) that participate in open-source commons while maintaining proprietary products. They benefit from the commons reading because it legitimates their dual positioning: they can contribute to shared infrastructure (deriving benefit and legitimacy) while retaining proprietary control of downstream products. The commons frame enables their operational strategy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, pragmatic_hybrid_actors, beneficiary,
    powerful, biographical, mobile, global).

% Universities and research institutions that study software governance, intellectual property, and free/open-source phenomena. They analyze the commons reading as an analytical frame but remain somewhat removed from the enforcement mechanisms and directly negotiated outcomes. They contribute knowledge but do not vote in foundation boards or license-adoption decisions.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, academic_institutions, observer,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__commons_reading, infrastructure_stewards).
narrative_ontology:fixing_cost_class(software_control_legitimacy__commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of how to manage shared digital infrastructure when no single actor has authority to impose unilateral rules. The commons reading enables mixed-investment models, distributed maintenance, and participatory governance where multiple stakeholders can contribute code, set standards, and share benefits without requiring either absolute freedom (rejecting all property concepts) or absolute property (enforcing unilateral developer control). Enables sustainability of projects with decentralized contributor bases.
% TRANSFER_FUNCTION: Moves legitimacy, governance participation rights, and resource allocation authority from absolutist frames (property-exclusive or freedom-exclusive) to negotiated commons structures. Transfers enforcement mechanisms from purely legal (proprietary contracts) or purely ideological (freedom mandates) to hybrid (license terms + governance participation + community standards). Moves decision-making power from individual developers or firms to foundation boards and RFC processes.
% ABSENT_VOICES: Indigenous software practices and governance models outside Western property/freedom framings; voices from communities unable to access governance participation due to language or resource barriers; subsistence-level developers and users for whom both absolutist positions (property-enforcement legal costs, freedom-mandate compliance costs) are economically inaccessible; voices of non-human actors (ecosystems, future generations) that software infrastructure decisions affect.
% DISAPPEARANCE_RATIONALE: If the commons governance reading were abandoned, software control would rapidly reconcentrate around either pure property claims (firms enforce proprietary control unilaterally) or pure freedom claims (activists successfully delegitimize all restrictions). Mixed-investment ecosystems (Linux, Python, Kubernetes) that depend on legitimized commons governance would fragment or require reorganization. The current state of decentralized, participatory software infrastructure would not persist without the commons reading as a stabilizing legitimacy frame.
% FOUNDING_PROBLEM: Early software distribution was controlled by hardware vendors; independent developers had no viable path to share work at scale. Simultaneously, the emerging internet and personal computing made software a commons-like resource (copying costs near zero) for which traditional property enforcement was either impossible or oppressive. The problem was: how to enable decentralized software collaboration and distribution while ensuring sustainability and preventing unilateral seizure of shared infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Infrastructure stewards, development ecosystems, and large hybrid-model firms attest the founding problem remains live and the commons frame is essential to sustainability. Absolutist freedom advocates and proprietarian developers attest the founding problem has been superseded — freedom advocates claim proprietary software is now the problem, not the solution; proprietarians claim market mechanisms and investment incentives have solved sustainability. Independent software historians and economists cite empirical evidence from both camps: Linux, Apache, and Python sustainability supports the commons narrative; commercial software success supports the proprietarian narrative. No single corroborating source outside benefiting parties exists; the status is inherently contested along the axis of which problem is the primary one.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: moderate-to-high (0.58) because the commons reading extracts legitimacy from absolutist frames and concentrates governance authority in institutional stewards and boards. Neither complete suppression (some absolutists thrive outside commons structures) nor minimal extraction (billions in commercial software value are subjected to governance negotiation). Suppression: moderate (0.42) because the constraint relies on license legitimacy and community acceptance rather than aggressive enforcement. Resistance: high (0.72) from both absolutist flanks—freedom advocates resist any acknowledgment of property legitimacy; proprietarians resist governance participation requirements. Theater: modest (0.28) because commons governance has real function (RFC processes, license enforcement, foundation administration) even though the percentage of governance work that is purely theater (symbolic consensus, performative inclusivity) grows over time. The temporal trajectory shows extractiveness rising sharply 0-20 as commons institutions solidify globally, then plateauing 20-30 as stakeholder accommodation mechanisms mature, then potentially declining 30-40 as excluded-participant pressure forces more authentic accessibility. Theater_ratio rises more slowly and levels off earlier because the core governance function is real; extractiveness from legitimacy capture accumulates faster than theatrical performance. Suppression_requirement stays low relative to tangled_rope baselines because the commons frame's own legitimacy is its enforcement mechanism—unlike coercive snares, the commons reading persists through consent rather than suppression, which means active resistance actually helps stabilize the constraint by proving its legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the steward and stakeholder-community seat, this is genuine coordination: the commons frame enables sustainability of projects that proprietary models would not (Linux ecosystem), creates legitimacy for mixed-investment strategies (dual-licensing), and distributes governance authority. From the absolutist-freedom seat, the commons frame is co-optation: proprietary software still dominates; the compromise legitimates property-like arrangements and extracts the moral urgency from liberation narratives. From the proprietarian seat, the commons frame is a loss of authority: unilateral control becomes negotiable; resistance to governance participation incurs legitimacy cost. From the pragmatic-hybrid seat, the commons frame is ideal: they collect legitimacy from commons participation while retaining proprietary control downstream. The engine computes these divergences directly from the stakeholder structure and directionality; the authored metrics describe what the commons reading treats as the baseline scenario, not what any single seat would claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Stakeholder communities and development ecosystems are beneficiaries (directionality near 0.0, beneficiary end): they gain the ability to coordinate shared infrastructure and legitimate mixed-model sustainability. Infrastructure stewards are agenda-setters (directionality ~0.3-0.4): they set governance rules and benefit from legitimacy/authority but also bear costs of managing absolutist conflict and accessibility barriers. Absolutist freedom advocates and proprietarian developers are payers (directionality near 1.0, target end): both lose their claim to universal legitimacy and are repositioned as stakeholders with bounded authority. Excluded governance participants bear especially high costs (directionality >0.8) because they are nominally included but structurally inaccessible—they pay participation costs (following governance processes, learning governance language) without collecting proportional benefits. Pragmatic hybrid actors have low directionality (~0.2) because they benefit from both sides: proprietary control (proprietarian) + legitimacy from commons participation (freedom-advocate framing). Directionality overrides: the proprietarian_developers atom should override slightly downward from strict target-end (proprietary firms do participate in commons governance and derive some legitimacy benefit even if coerced) — override to 0.65 rather than full 1.0.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing shared infrastructure when no single actor has authority) was real in the 1980s-1990s and remains live in specific domains (Linux kernel, Python, web standards). However, a secondary mandate—that the commons reading is the only legitimate frame for software governance—is approaching death. The measurement series shows extractiveness rising then plateauing: the constraint's extraction capacity (reposition absolutists, negotiate proprietarians) has maxed out. Further enforcement would require suppressing the absolutist freedom narrative entirely or coercing proprietarian participation, neither of which is sustainable. The potential for mandatrophy is real: if excluded-participant pressure succeeds in forcing authentic accessibility, or if an absolutist faction successfully delegitimizes governance-as-compromise by demonstrating its exclusion of peripheral voices, the commons reading's authority as THE frame could erode. The constraint persists because it solves the real founding problem, but the additional mandate (treat commons governance as the only legitimate frame) is vulnerable. No mandatrophy has yet resolved; this is early-stage mandatrophy risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_accessibility_structural_vs_tokenism,
    'Are the current commons governance structures genuinely accessible to excluded participants (Global South developers, small-scale implementers, non-English speakers) or do they constitute token inclusion masking continued marginalization?',
    'Longitudinal study of governance participation rates by region, language, and economic status; analysis of decision outcomes that benefit vs. exclude peripheral voices; implementation and assessment of accessibility interventions (translation, asynchronous participation, distributed governance).',
    'If accessibility is largely tokenism, the constraint''s extraction from excluded participants (nominal inclusion without proportional benefit) is higher than authored, and suppression_requirement is understated (the performance of inclusion requires active work to maintain the illusion). If genuine accessibility is achieved, extraction drops and the commons reading''s legitimacy strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_accessibility_structural_vs_tokenism, empirical, 'Whether commons governance accessibility is structural or performative.').

omega_variable(
    mandate_expiration_vs_live_founding_problem,
    'Is the founding problem (how to manage shared infrastructure without unilateral authority) still live, or has it been solved by the commons governance mechanisms themselves?',
    'Assessment of whether new shared-infrastructure projects (post-2020) require commons governance to achieve sustainability, or whether proprietary and freedom-model alternatives have proven viable for comparable technical domains.',
    'If the founding problem is substantially solved, the commons reading transitions from tangled_rope (coordination + extraction) to piton (extraction without coordination function). If the problem remains live in critical infrastructure (AI model governance, cloud infrastructure, semiconductor design), the constraint persists as tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_expiration_vs_live_founding_problem, empirical, 'Whether the constraint''s primary coordination function remains necessary.').

omega_variable(
    absolutist_foreclosure_vs_coexistence,
    'Do the freedom-imperative and property-rights readings logically foreclose the commons reading, or do all three coexist as live positions held by different parties?',
    'Analysis of whether any single party or framework attempts to hold all three readings simultaneously (functional coexistence) or whether they genuinely partition the stakeholder space into incompatible factions.',
    'If foreclosure is real (one reading logically rules out another), the kernel is not genuinely contested—it is a settled dispute being relitigated. If coexistence is stable, the commons reading''s authority is contingent on it continuing to accommodate both flanks; loss of accommodation capacity would erode its legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_foreclosure_vs_coexistence, conceptual, 'Whether the kernel''s sibling readings are logically incompatible or empirically coexistent.').

omega_variable(
    proprietarian_capture_of_commons_discourse,
    'Is the commons reading being captured by proprietarian actors (large hybrid firms like Google, Meta) who use commons legitimacy to justify proprietary downstream products?',
    'Tracking of which stakeholders collect rents vs. which contribute to shared infrastructure; analysis of whether commons governance decisions increasingly advantage large hybrid actors; measurement of wealth concentration and control consolidation over time.',
    'If proprietarian capture is occurring, the constraint''s extraction from independent developers and freedom advocates increases (they lose both property legitimacy and freedom frame while proprietarians gain both). The constraint would transition toward snare (capture without genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietarian_capture_of_commons_discourse, empirical, 'Whether the commons reading is being used as cover for proprietary rent-seeking.').

omega_variable(
    kernel_reading_vs_non_kernel_constraints,
    'Is this constraint validly a reading of the software_control_legitimacy kernel, or does its framing as ''negotiated governance'' introduce structural elements (multi-stakeholder participation, institutional stewardship) that are not present in the other readings and thus constitute a different constraint altogether?',
    'Comparative analysis of whether all four readings share a common commitment (what is the kernel they''re all reading?) and whether the differences are interpretations of the same kernel or structurally distinct constraints. Examination of whether a ''pure'' commons reading exists that is independent of institutional stewardship.',
    'If the commons reading introduces genuinely new structural elements (institutional stewards as agenda-setters, participatory governance as a structural feature) that the other readings do not address, it may constitute a different constraint entirely (e.g., software_governance_institutionalization) that happens to overlap with software_control_legitimacy but is not a reading of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vs_non_kernel_constraints, conceptual, 'Whether the commons reading is a valid kernel reading or a structurally distinct constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__commons_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__commons_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__commons_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__commons_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(soft_tr_t25, observed).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__commons_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(soft_tr_t30, observed).
narrative_ontology:measurement(soft_tr_t35, software_control_legitimacy__commons_reading, theater_ratio, 35, 0.31).
narrative_ontology:measurement_basis(soft_tr_t35, projected).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__commons_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(soft_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__commons_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__commons_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__commons_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__commons_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(soft_be_t25, observed).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__commons_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(soft_be_t30, observed).
narrative_ontology:measurement(soft_be_t35, software_control_legitimacy__commons_reading, base_extractiveness, 35, 0.63).
narrative_ontology:measurement_basis(soft_be_t35, projected).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__commons_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(soft_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__commons_reading, suppression_requirement, 5, 0.31).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__commons_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__commons_reading, suppression_requirement, 15, 0.37).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__commons_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(soft_su_t20, observed).
narrative_ontology:measurement(soft_su_t25, software_control_legitimacy__commons_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(soft_su_t25, observed).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__commons_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement_basis(soft_su_t30, observed).
narrative_ontology:measurement(soft_su_t35, software_control_legitimacy__commons_reading, suppression_requirement, 35, 0.44).
narrative_ontology:measurement_basis(soft_su_t35, projected).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__commons_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(soft_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__commons_reading, 0.18).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, open_source_sustainability).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_licensing_enforcement).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, governance_legitimacy_crisis).

% DUAL FORMULATION NOTE:
% Part of the software_control_legitimacy kernel constraint family. This file represents the commons_reading; sibling readings (freedom_imperative, property_rights, pragmatic_openness) are separate constraint stories with their own ε values, stakeholder structures, and extracted measurements. All four readings share a kernel (what is the legitimate basis for software control authority) but diverge on its answer. The commons reading treats both property and freedom as negotiable within governance structures; the freedom-imperative reading treats property claims as inherently illegitimate; the property-rights reading treats freedom claims as overreach; the pragmatic-openness reading treats both as methodologically viable. Do not merge these readings into one story—they have structurally different victim sets, beneficiary structures, and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__commons_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
