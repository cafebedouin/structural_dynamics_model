% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Federalist Marriage Authority Fragmentation (Anti-Tyranny Consociational Mechanism)
 *   domain: legal/constitutional/family law
 *
 * SUMMARY:
 *   This constraint is ONE reading of the contested marriage-authority kernel
 *   — specifically, the federalist/millet reading, which frames deliberate
 *   fragmentation of marriage law authority across religious and community
 *   codes as a consociational anti-tyranny mechanism. The kernel itself is a
 *   stabilized commitment: a constitutional structure that grounds multiple
 *   parties' legitimacy claims in different interpretations of what the
 *   fragmentation IS FOR. This reading treats fragmentation as INTENTIONAL
 *   design to prevent majoritarian domination. Sibling readings interpret the
 *   same fragmentation differently: as communal autonomy grounded in
 *   tradition (communal_autonomy_reading), as transitional anomaly awaiting
 *   Uniform Civil Code reform (secularist_reading), or as a site of
 *   gender-equality contestation (gender_rights_reading). The ε-invariance
 *   principle requires separate stories for each reading with separate ε
 *   values. This story's referent is the standing arrangement — fragmented
 *   marriage authority — assessed by THIS reading's own lights as an
 *   anti-tyranny mechanism with low extraction and genuine coordination
 *   function.
 *
 * KEY AGENTS:
 *   - Religious minority communities — holders of parallel personal law authority; beneficiaries of protection against majoritarian override
 *   - Subnational legislatures — authors of personal law codes; distributed law-making power
 *   - National legislature — excluded from unilateral authority; politically constrained by the consociational structure
 *   - Individuals at jurisdictional boundaries — bear friction costs; voice for centralized reform is structurally constrained
 *   - Constitutional court — external monitor ensuring individual rights are not entirely subordinated to community authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.32).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.18).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Federalist Marriage Authority Fragmentation (Anti-Tyranny Consociational Mechanism)").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/constitutional/family law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '8c25b116-2e70-455f-9fc1-41cdcd0e4e3c').
narrative_ontology:cs_kernel_codification('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', fixed_text).
narrative_ontology:cs_authority_grounding('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', extraction).
narrative_ontology:cs_interpretation_layer_present('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c').
narrative_ontology:cs_reading_relation('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_reading_relation('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_axiom('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', foundational, majoritarian_domination_risk_justifies_fragmentation).
narrative_ontology:cs_axiom_status(majoritarian_domination_risk_justifies_fragmentation, holdable).
narrative_ontology:cs_axiom_grounding('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', majoritarian_domination_risk_justifies_fragmentation, deontological).
narrative_ontology:cs_axiom('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', foundational, community_consent_to_pluralism_requires_autonomy_guarantee).
narrative_ontology:cs_axiom_status(community_consent_to_pluralism_requires_autonomy_guarantee, holdable).
narrative_ontology:cs_axiom_grounding('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', community_consent_to_pluralism_requires_autonomy_guarantee, conventional).
narrative_ontology:cs_reference_frame('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', consociational_anti_tyranny_compact).
narrative_ontology:cs_drift_state('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', contemporary_judicial_rights_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8c25b116-2e70-455f-9fc1-41cdcd0e4e3c', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, religious_minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, subnational_legislatures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, majority_religious_community).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, individuals_crossing_jurisdictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain authority over marriage law and dissolution within community borders under personal law frameworks grounded in religious tradition. This reading frames fragmentation as protection: without it, a majoritarian national legislature would unilaterally impose a single family code aligned with dominant religious or secular values, overriding minority conscience. The constraint enables minority communities to maintain marital norms and dispute resolution aligned with their tradition while participating in broader governance.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, religious_minority_communities, beneficiary,
    organized, generational, constrained, national).

% Also retains authority over its own personal law framework and is NOT subordinated to minority rules — the fragmentation treats all communities symmetrically as holders of parallel authority. This reading prevents the majoritarian community from consolidating unilateral control by imposing a single national code. Both majority and minority are constrained by the architecture to bargain rather than dominate.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majority_religious_community, beneficiary,
    powerful, generational, mobile, national).

% Author personal law codes for their communities (often codified along religious/traditional boundaries). They set rules for marriage, divorce, inheritance, guardianship within jurisdiction. The fragmented authority arrangement requires these legislatures to maintain and periodically revise personal law codes; it also insulates them from centralized override, distributing law-making power downward.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, subnational_legislatures, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, subnational_legislatures, beneficiary).

% Face legal friction when marrying across personal law boundaries, seeking divorce in a jurisdiction different from the one where marriage was solemnized, or claiming inheritance rights that differ by code. The fragmentation creates conflicts-of-law problems and forum shopping risks. Women and religious minorities crossing into hostile jurisdictions particularly bear the costs (e.g., a woman divorced under one code seeking recognition in another; an interfaith couple unable to marry under either code).
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, individuals_crossing_jurisdictions, payer,
    powerless, biographical, constrained, national).

% Formally lacks authority to impose a Uniform Civil Code on personal law matters without amending constitutional structures or dismantling the consociational compact. This exclusion is the core feature of the anti-tyranny mechanism — the legislature cannot unilaterally consolidate family law authority. Attempts to author a UCC are politically paralyzed because minority communities and subnational legislatures can veto via constitutional channels (amendment, intergovernmental bargaining, or constitutional court review).
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, national_legislature, excluded,
    institutional, generational, constrained, national).

% Reviews challenges to personal law codes on grounds of constitutional rights (gender equality, freedom of religion, due process). This reading treats the court as an external monitor ensuring individual rights are not entirely submerged to community authority, but without replacing the fragmented structure with a centralized code. The court's role is boundary-checking, not substitution.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates marriage regulation across a religiously/traditionally plural society such that no single majoritarian faction can unilaterally impose family law values on minorities. Solves the tyranny-of-the-majority problem by locking authority into community-specific frameworks that require multi-party negotiation to change.
% TRANSFER_FUNCTION: Transfers law-making authority from a potential central monopoly (majoritarian national legislature) to dispersed subnational and community-based legislatures. Minorities receive protection from majoritarian override; the majority receives assurance that minorities cannot dominate a single national code either. The arrangement moves the locus of family law from one winner-take-all arena to multiple parallel arenas.
% ABSENT_VOICES: Individuals trapped at the intersections of personal law boundaries (interfaith couples, women seeking divorce under codes hostile to them, persons whose community of identity does not align with the community of legal jurisdiction) cannot easily advocate for a unified legal field because the consociational structure gives each community a veto. These voices are structurally excluded by design — their advocacy would require dismantling the anti-tyranny mechanism itself.
% DISAPPEARANCE_RATIONALE: If fragmented marriage authority disappeared overnight and a single Uniform Civil Code consolidated jurisdiction, the political bargain that held the consociation together would collapse. Religious communities would lose the autonomy guarantee that anchored their consent to pluralism. Subnational legislatures would lose law-making power. The absence of the constraint would fundamentally alter the constitutional settlement and the balance of power among communities.
% FOUNDING_PROBLEM: Prevention of majoritarian religious domination over family law and minority conscience in a plural society. The founding problem is not inefficiency or coordination per se — it is the prevention of tyranny: without fragmentation, a majority of voters could impose a single family code that violated minority religious norms, forcing conversions, dissolution of traditional marriages, or loss of inheritance rights.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and comparative law analysts (outside any single benefiting community) attest that majoritarian imposition of family law is a live risk in plural societies; historical examples abound (religious minorities' experience under imposed civil codes in post-Ottoman and post-colonial contexts). Minority communities and subnational legislatures obviously attest the problem is live. Secular reformers dispute whether the problem justifies the solution, but not whether the problem exists.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.32, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.32) because the reading emphasizes genuine coordination: fragmentation prevents a tyranny outcome that would harm minorities; both majority and minority benefit from the guarantee that neither can dominate. Suppression is minimal (0.18) because the mechanism operates through constitutional structure, not coercion — communities CONSENT to pluralism in exchange for immunity from majoritarian override. Theater is modest (0.22) because some rhetorical work is required to maintain the consociational bargain (periodic reaffirmation that no single code is being imposed), but the core function is real. Accessibility_collapse is moderate (0.41) because alternatives DO exist (UCC movements, judicial harmonization, gradual secularization), but the constitutional structure makes them politically difficult to implement without community consent. Resistance is moderate-high (0.58) because communities actively resist UCC movements and secularization pressure to maintain their autonomy. The measurement trajectory shows STABILITY rather than drift: the constraint's metrics remain roughly constant over the interval because the consociational bargain, once struck, persists without requiring constant enforcement or theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Agenda-setter seats (subnational legislatures) experience low effective extraction because they HOLD authority; payer seats (boundary-crossing individuals) experience higher extraction because they BEAR costs. This divergence is structural and should emerge from the engine's per-seat computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious minority communities and subnational legislatures are structural beneficiaries (d near 0.2–0.3): they collect autonomy and protected law-making authority. The majority religious community is also a beneficiary (d near 0.2–0.3): it is equally constrained by pluralism and cannot dominate either. Individuals at jurisdictional boundaries are structural targets (d near 0.7–0.8): they bear friction costs without direct benefit, their exit options are constrained (cannot easily relocate to resolve legal conflicts), and they face identity-locking (religious identity determines which code governs them). The national legislature is EXCLUDED, not payer — it is cut out of the decision-making structure by design, which is the anti-tyranny mechanism's core feature. No directionality override is needed; the derived d values should reflect these positions from power + exit + beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The consociational reading is vulnerable to mandatrophy in a specific way: the founding problem (majoritarian tyranny) could become DEAD if political culture shifts toward inter-community trust and willingness to accept unified law. If communities ceased to fear majoritarian domination, the fragmentation would persist as institutional inertia — the mechanism would be performing the function no longer (theater would rise, extraction unmasked). This story's author asserts the founding problem is LIVE, but future measurement could show it shifting. The mismatch between founding_problem_status=live and potential disappearance_verdict=world_unchanged (if the constraint is actually theater) would flag a mandatrophy candidate. Currently the reading asserts the problem is LIVE and the constraint would cause world_rearrangement if removed (the consociational bargain would collapse), so mandatrophy is not resolved yet. An omega variable addresses the uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_liveness,
    'Is majoritarian domination of family law a live risk, or has political culture shifted to inter-community trust and willingness to accept unified law without fear of majoritarian imposition?',
    'Longitudinal survey of minority community trust in central government; legislative votes on UCC proposals measuring actual coalition patterns (do minorities veto? do majorities override?); historical comparison of attitudes toward unified codes.',
    'If majorities and minorities no longer fear each other''s domination, the founding problem is dead. The constraint would persist as institutional inertia (mandatrophy candidate: theater_ratio would rise, extraction unmasked). This would flip the reading from rope to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Whether the anti-tyranny problem the consociational structure was built to solve still motivates political actors.').

omega_variable(
    consociational_design_intent,
    'Was the fragmentation of marriage authority deliberately DESIGNED as anti-tyranny mechanism, or did it EMERGE from historical particularism (different communities happened to retain different codes) and later a protective rationale was constructed around it?',
    'Constitutional-history scholarship examining founding documents, legislative debates, and framers'' intent. Distinction between ''design by choice'' vs. ''outcome-rationalization of historical path-dependence.''',
    'If fragmentation emerged historically and the anti-tyranny rationale is POST-HOC, the reading''s legitimacy claim is weaker. The constraint might be better characterized under communal_autonomy_reading (passively holding tradition) rather than federalist_millet (active anti-tyranny bargain). ε might rise if the rationalization is seen as cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consociational_design_intent, conceptual, 'Whether federalist anti-tyranny design is the true origin or a retroactive reading onto historical particularism.').

omega_variable(
    suppression_of_unification_movements,
    'Is the political difficulty of enacting a Uniform Civil Code a STRUCTURAL result of the consociational bargain (minorities having constitutional veto), or a BEHAVIORAL result of institutional inertia and lobbying pressure (minorities blocking reform through political action, not constitutional constraint)?',
    'Comparative constitutional analysis: jurisdictions with formal consociational structure (veto power in constitution) vs. jurisdictions with historical plural codes but no formal constitutional veto. Do the latter attempt unification more readily? Do they succeed?',
    'If unification is structurally constrained (constitutional level), the reading''s anti-tyranny mechanism is real and its suppression metric (0.18) is accurate — the mechanism prevents majoritarian override without active coercion. If unification is only behaviorally difficult (political lobbying), suppression might be higher (minorities actively resist) and the mechanism might be weaker (could be overridden by sufficiently determined majority). This affects whether the reading qualifies as genuine coordination (rope) or extractive (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_unification_movements, empirical, 'Whether legislative paralysis on UCC is structural or behavioral.').

omega_variable(
    boundary_crossing_friction_purpose,
    'Is the friction borne by boundary-crossing individuals (conflicts of law, forum shopping, marital recognition problems) a NECESSARY cost of the anti-tyranny mechanism, or an AVOIDABLE byproduct of inadequate conflicts-of-law harmonization?',
    'Comparative analysis of consociational systems with strong conflicts-of-law infrastructure (e.g., recognition of all codes'' valid marriages across boundaries) vs. those with weak infrastructure. Does harmonizing conflicts reduce friction without dismantling community authority?',
    'If friction is necessary (communities demand non-recognition of rival codes), then it is a structural cost of the mechanism and part of the extraction metric. If avoidable through conflict harmonization, the constraint could retain its anti-tyranny benefit while reducing the payer burden on boundary-crossers, lowering effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_crossing_friction_purpose, empirical, 'Whether conflict-of-law friction is integral to the anti-tyranny mechanism or a correctable externality.').

omega_variable(
    reading_vs_communal_autonomy_distinction,
    'This federalist_millet_reading and the communal_autonomy_reading both result in fragmented authority and benefit minority communities. What is the structurally meaningful difference?',
    'Framing analysis: federalist reading emphasizes ANTI-TYRANNY BARGAIN (majority and minority constrain each other); communal reading emphasizes TRADITION-HOLDING (communities hold authority because they always did, not because of bargain). These imply different vulnerability patterns: federalist reading fails if the anti-tyranny problem disappears; communal reading persists if tradition holds. Different omega variables apply; different measurement trajectories would reveal divergence.',
    'If the distinction is purely rhetorical (same outcome, different rationale), the readings are not structurally separable and should be a single constraint. If the distinction is structural (different persistence mechanisms, different vulnerability to drift), the readings are correctly separated and might diverge in future periods.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_communal_autonomy_distinction, conceptual, 'Whether federalist anti-tyranny framing and communal autonomy framing are structurally distinct readings or the same outcome with different narratives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(marr_tr_t5, marriage_authority__federalist_millet_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__federalist_millet_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__federalist_millet_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__federalist_millet_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__federalist_millet_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(marr_tr_t35, marriage_authority__federalist_millet_reading, theater_ratio, 35, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(marr_be_t5, marriage_authority__federalist_millet_reading, base_extractiveness, 5, 0.29).
narrative_ontology:measurement(marr_be_t10, marriage_authority__federalist_millet_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(marr_be_t15, marriage_authority__federalist_millet_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(marr_be_t20, marriage_authority__federalist_millet_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(marr_be_t25, marriage_authority__federalist_millet_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(marr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(marr_be_t35, marriage_authority__federalist_millet_reading, base_extractiveness, 35, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority__federalist_millet_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__federalist_millet_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority kernel is instantiated by five separate constraint stories, one per reading. Each reading interprets the SAME fragmented legal structure (the standing arrangement) from a different normative and structural perspective. This federalist_millet_reading treats fragmentation as anti-tyranny mechanism with low extraction. The communal_autonomy_reading treats the same fragmentation as grounded in tradition and community authority without emphasizing the anti-tyranny bargain. The gender_rights_reading contests fragmentation from within — demanding intra-community gender equality. The judicial_harmonization_reading describes fragmentation as gradually eroding via constitutional court review. The secularist_reading treats fragmentation as transitional anomaly awaiting UCC. Each has distinct ε, beneficiary structure, and type classification. The kernel network links all five readings; ε-invariance requires separate stories (OQ-26 principle applied to kernel readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
