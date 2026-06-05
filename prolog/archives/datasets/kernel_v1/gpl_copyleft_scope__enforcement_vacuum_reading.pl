% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope — Enforcement Vacuum Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL copyleft scope constraint operates through absence rather than
 *   through explicit enforcement. The GPL v2/v3 licenses claim that
 *   'derivative works' trigger copyleft obligations (requiring source code
 *   disclosure), but the copyleft scope ambiguity — what counts as a
 *   derivative work in a networked, modular software ecosystem — has never
 *   been definitively resolved by U.S. courts. This absence of judicial
 *   precedent creates a structural gap: the GPL's normative authority
 *   (asserted by the FSF and GNU community) coexists with industry-aligned
 *   interpretations (claiming narrow scope) that are equally
 *   license-consistent. The constraint manifests as licensed plurality: both
 *   readings are technically defensible, and enforcement capacity depends
 *   entirely on whether the dispute arises in an FSF-aligned project
 *   ecosystem (where strong-copyleft reading dominates) or an
 *   industry-dominated ecosystem (where narrow-scope reading dominates).
 *   Adopters navigate this ambiguity through risk assessment and community
 *   alignment rather than through legal clarity. The enforcement vacuum is
 *   not accidental — it is structurally sustained because litigation to
 *   clarify scope would be costly and uncertain, and both interpretive
 *   communities have incentives to preserve ambiguity that favors their
 *   position.
 *
 * KEY AGENTS:
 *   - Clarity-Seeking Adopters: Primary victims (powerless/trapped) — face elevated legal review costs and deployment delays; cannot exit ambiguity
 *   - Pragmatic Adopters: Secondary victims (moderate/constrained) — benefit from flexibility but bear risk of retroactive enforcement misalignment
 *   - FSF-Aligned Interpretive Community: Primary beneficiary (institutional/arbitrage) — exercises de facto enforcement capacity in free software ecosystems; benefits from ambiguity that lets them assert strong-copyleft authority
 *   - Industry-Aligned Interpretive Community: Secondary beneficiary (institutional/arbitrage) — exercises de facto enforcement capacity in corporate ecosystems; benefits from ambiguity that lets them assert narrow-scope flexibility
 *   - Open Source Governance Coalition: Mixed actor (organized/constrained) — benefits from transaction costs generated by ambiguity (demand for legal expertise) but bears costs of ecosystem fragmentation and risk consolidation
 *   - Judicial System: Observer with latent capacity (analytical/analytical) — has not exercised precedent-setting power; absence is structural to litigation economics and settlement preferences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.35).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.48).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope — Enforcement Vacuum Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '7477a81b-f60a-44a1-9bfa-4362142bd390').
narrative_ontology:cs_kernel_codification('7477a81b-f60a-44a1-9bfa-4362142bd390', fixed_text).
narrative_ontology:cs_authority_grounding('7477a81b-f60a-44a1-9bfa-4362142bd390', extraction).
narrative_ontology:cs_interpretation_layer_present('7477a81b-f60a-44a1-9bfa-4362142bd390').
narrative_ontology:cs_reading_relation('7477a81b-f60a-44a1-9bfa-4362142bd390', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('7477a81b-f60a-44a1-9bfa-4362142bd390', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('7477a81b-f60a-44a1-9bfa-4362142bd390', foundational, precedent_absence_is_structural).
narrative_ontology:cs_axiom_status(precedent_absence_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('7477a81b-f60a-44a1-9bfa-4362142bd390', precedent_absence_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('7477a81b-f60a-44a1-9bfa-4362142bd390', foundational, licensed_plurality_as_constraint).
narrative_ontology:cs_axiom_status(licensed_plurality_as_constraint, holdable).
narrative_ontology:cs_axiom_grounding('7477a81b-f60a-44a1-9bfa-4362142bd390', licensed_plurality_as_constraint, instrumental).
narrative_ontology:cs_reference_frame('7477a81b-f60a-44a1-9bfa-4362142bd390', gpl_as_distributed_norm_bearer).
narrative_ontology:cs_drift_state('7477a81b-f60a-44a1-9bfa-4362142bd390', contemporary_cloud_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7477a81b-f60a-44a1-9bfa-4362142bd390', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, ambiguity_exploiting_ecosystems).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, license_compliance_certainty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLARITY-SEEKING ADOPTER (SNARE) — Small projects or risk-averse organizations seeking definitive interpretation face maximum extraction: elevated legal review costs, transaction delays, and deployment risk. No authoritative interpretation available; trapped in ambiguity.
constraint_indexing:constraint_classification(gpl_copyleft_scope__enforcement_vacuum_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRAGMATIC ADOPTER (TANGLED ROPE) — Mid-sized projects navigate the ambiguity through risk assessment and community alignment. Benefits from interpretive flexibility (can choose reading that suits their business model); bears costs of ongoing legal uncertainty and potential retroactive enforcement.
constraint_indexing:constraint_classification(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERPRETIVE COMMUNITY — FSF-ALIGNED (ROPE) — FSF and its legal allies have de facto enforcement capacity and can declare strong-copyleft readings. They experience the constraint as pure coordination: ambiguity enables them to maintain normative authority while deferring costly legal clarification. Net beneficiary.
constraint_indexing:constraint_classification(gpl_copyleft_scope__enforcement_vacuum_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERPRETIVE COMMUNITY — INDUSTRY-ALIGNED (ROPE) — Industry actors and permissive-license advocates have enforcement capacity in corporate ecosystems and can declare narrow-scope readings. They experience the constraint as pure coordination: ambiguity enables them to maintain business-friendly norms while evading legal clarification. Net beneficiary from enforcement vacuum.
constraint_indexing:constraint_classification(gpl_copyleft_scope__enforcement_vacuum_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SOURCE GOVERNANCE COALITION (TANGLED ROPE) — Organizations like Software Freedom Law Center (SFLC), Apache, Linux Foundation, and Conservancy experience the ambiguity as both coordination failure and structural resource. They benefit from the transaction costs it generates (demand for their expertise); they bear costs of ecosystem fragmentation and risk consolidation.
constraint_indexing:constraint_classification(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GPL LEGAL CONSENSUS — DEGRADED (PITON) — The canonical GPL interpretation (strong copyleft, derivative works trigger obligation) persists through institutional inertia (FSF authority, GNU copyright, Red Hat case precedent) but lacks definitive judicial validation. The consensus is substantially performative: enforced by social/institutional authority rather than settled law. Theater ratio high because the 'consensus' is repeatedly asserted and defended but never definitively proven in court.
constraint_indexing:constraint_classification(gpl_copyleft_scope__enforcement_vacuum_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the absence of judicial precedent is an immutable constraint: the U.S. legal system has not tested the GPL's most expansive claims in court, and the probability of foundational judicial precedent is low (GPL-licensed software does not reach litigation endpoints at high frequency; settlements occur out of court). This absence is a structural feature of the legal system, not a contingent institutional gap.
constraint_indexing:constraint_classification(gpl_copyleft_scope__enforcement_vacuum_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gpl_copyleft_scope__enforcement_vacuum_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gpl_copyleft_scope__enforcement_vacuum_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, TR),
    TR >= 0.70.

:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The GPL enforcement vacuum generates extraction through transaction costs (legal review, risk assessment, delayed deployment) and through the ability of both interpretive communities to assert readings that benefit them. However, extraction is limited by the fact that ambiguity also benefits many adopters (who exploit it for flexibility) and the GPL's normative legitimacy ensures that naked violation is socially costly. The extractiveness reflects the transaction costs and the asymmetry between those who can navigate ambiguity and those who cannot. Suppression (0.48): Moderate-high. Significant barriers to exit the ambiguity include license consistency (both readings fit the text), institutional entrenchment (FSF authority legitimizes strong reading; corporate practice legitimizes narrow reading), and the difficulty of changing deployed code's license. But suppression is not total — adopters can switch to permissive licenses, and new projects can choose their licensing base. Measured adoption of GPL shows moderate suppression, not entrapment. Theater ratio (0.65): Moderate-high. The GPL legal consensus is substantially performative: the strong-copyleft interpretation is repeatedly asserted by FSF and asserted as settled law, but lacks definitive judicial validation. The performative content is the assertion itself — the ongoing rehearsal of 'this is what the GPL means' — rather than the content of the assertion. As the measurement interval progresses (0.50 → 0.65), theater increases because the ambiguity persists despite accumulating discussion; the performative rehearsal intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The clarity-seeking adopter sees the GPL as a snare: ambiguity cannot be escaped because the license is ubiquitous and reinterpreted constantly. The pragmatic adopter sees it as tangled rope: the ambiguity is exploitable and provides genuine flexibility alongside risk. The FSF-aligned community sees pure coordination (rope): the ambiguity lets them maintain normative authority while deferring costly clarification. The industry-aligned community also sees rope: the ambiguity lets them maintain business-friendly norms. The governance coalition sees tangled rope: the constraint generates demand for their services (extraction), but also ecosystem fragmentation (cost). The degraded consensus perspective sees piton: the GPL is a ritual repeatedly performed and reasserted but never definitively validated. The civilizational observer risks seeing mountain: the absence of precedent is an immutable feature of litigation economics. The range from snare to mountain across a single structural phenomenon is the diagnostic signal: perspectives differ not because the constraint is ambiguous-about-its-type but because the ambiguity itself is a structural feature that creates different experienced constraints for different positioned agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's position relative to the interpretation flow. Clarity-seeking adopters are targets: they cannot navigate ambiguity and face maximum extraction through delay and legal cost. Pragmatic adopters are mixed: they benefit from flexibility but bear enforcement risk. FSF-aligned and industry-aligned interpretive communities are beneficiaries: they have the authority and resources to assert their preferred reading and can extract through setting ecosystem norms. The governance coalition is mixed: it benefits from the demand for expertise but bears costs of ecosystem tension. The derivation reflects that this constraint operates through interpretive authority and resource asymmetry, not through coercive exclusion. Agents with interpretive standing (FSF, industry counsel) extract through normative assertion; agents without standing (clarity-seeking adopters) extract through transaction costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretability_contention,
    'Is the GPL copyleft scope ambiguity a feature of the written license (the kernel is genuinely under-specified) or a feature of enforcement (the kernel is clear but lacks institutional capacity to enforce one reading)?',
    'Textual exegesis of GPL v2/v3 language paired with forensic analysis of FSF''s contemporaneous intent documents vs. actual enforcement practices (cease-and-desist letters, litigation positions, advisory opinions). If intent is clear but enforcement is selective, the ambiguity is institutional, not textual.',
    'If textual: readings should converge toward one interpretation through better drafting (GPL v4 could resolve it). If institutional: ambiguity is structural to multi-stakeholder governance and cannot be resolved by textual clarification alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretability_contention, empirical, 'Whether GPL ambiguity is textual or institutional in origin').

omega_variable(
    enforcement_vacuum_stability,
    'Is the enforcement vacuum stable (absence of precedent will persist indefinitely) or unstable (accumulating test cases will eventually produce precedent)?',
    'Longitudinal analysis of GPL-related litigation frequency; tracking of settlement terms that avoid precedent-setting; identification of structural disincentives to high-stakes litigation (cost, reputational risk, settlement preferences). Compare GPL enforcement patterns to patent/copyright litigation base rates.',
    'If stable: the constraint is indefinitely a tangled_rope with ambiguity as structural feature. If unstable: the constraint approaches a temporal transition point (to strong_copyleft or narrow_scope depending on precedent outcome); adopters should expect clarification within decadal timescale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vacuum_stability, empirical, 'Whether GPL enforcement vacuum is stable or will erode').

omega_variable(
    interpretive_community_decoupling,
    'Can FSF-aligned and industry-aligned interpretive communities genuinely coexist without one eventually foreclosing the other through market dominance, licensing fragmentation, or institutional consolidation?',
    'Historical trend analysis of permissive-vs-copyleft license adoption in high-value sectors (cloud, AI, mobile); assessment of whether permissive dominance in any sector reduces adoption of GPL (indicating foreclosure) or whether dual licensing and copyleft persistence indicate coexistence. Examine whether license choice correlates with sector rather than convergence.',
    'If coexistence is durable: the reading''s modeling of licensed plurality is accurate and the constraint remains tangled_rope long-term. If one reading forecloses the other: the constraint''s classification collapses toward either snare (strong copyleft enforcement) or rope (permissive dominance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_community_decoupling, empirical, 'Whether FSF and industry interpretive communities can coexist indefinitely').

omega_variable(
    judicial_precedent_counterfactual,
    'If a high-stakes GPL copyleft scope litigation were resolved in court, which interpretation would the judiciary most likely adopt: strong copyleft (GPL obligations cascade through the supply chain) or narrow scope (copyleft applies only to direct modifications)?',
    'Comparative analysis of judicial reasoning in analogous copyright/derivative-work cases (Andy Warhol Foundation v. Goldsmith, et al.); assessment of judicial conservatism vs expansionism in copyright doctrine; expert predictions from IP scholars and practicing software lawyers.',
    'Strong-copyleft precedent: FSF reading gains judicial legitimacy, constraint transitions toward snare for non-complying adopters. Narrow-scope precedent: industry reading gains legitimacy, constraint transitions toward rope (pure coordination). Ambiguity persists: constraint remains tangled_rope. Precedent is unstable/contradictory: constraint degrades toward piton (futile legal posturing).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_precedent_counterfactual, conceptual, 'Hypothetical judicial outcome if GPL scope were litigated').

omega_variable(
    mountain_false_summit_risk,
    'Is the absence of precedent a genuine natural law of the U.S. legal system (low probability of high-stakes GPL litigation reaching judgment due to structural settlement incentives) or a contingent institutional feature (ambiguity is maintained because it serves the interests of actors who benefit from it)?',
    'Comparative analysis: (a) settlement frequency and terms in GPL disputes vs other IP categories (indicating whether GPL disputes have structural settlement pressure); (b) actor interviews with FSF, industry counsel, and litigation practitioners about litigation-avoidance incentives; (c) historical tracking of whether clear-precedent-setting opportunities have been deliberately avoided or have arisen only rarely.',
    'If genuine natural law: the mountain perspective is valid, and the constraint is less extractive than modeling suggests. If contingent institutional feature: the mountain is a false summit, revealing that the enforcement vacuum benefits those with interpretive authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_false_summit_risk, empirical, 'Whether absence of precedent is natural law or maintained institutional feature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_enf_vac_tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(gpl_enf_vac_tr_t10, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(gpl_enf_vac_tr_t20, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(gpl_enf_vac_be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpl_enf_vac_be_t10, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(gpl_enf_vac_be_t20, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__enforcement_vacuum_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, free_software_movement_identity_lock).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, open_source_enterprise_capture).

% DUAL FORMULATION NOTE:
% The gpl_copyleft_scope kernel decomposes into three structurally distinct constraints with different epsilon values: strong_copyleft_reading (high epsilon, strong institutional enforcement), narrow_scope_reading (low epsilon, permissive institutional dominance), and enforcement_vacuum_reading (moderate epsilon, this story). All three are linked as readings of the same kernel. The enforcement_vacuum_reading is upstream of the identity-lock constraint (FSF adherents become identity-locked through commitment to strong-copyleft interpretation) and downstream of the enterprise-capture constraint (corporate ecosystem interprets narrowly to enable proprietary derivatives).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
