% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Universal Rights Reading of Geneva Protective Scope (Common Article 3 + IHRL Floor)
 *   domain: international_humanitarian_law/legal_theory
 *
 * SUMMARY:
 *   This story authors the universal_rights_reading of the Geneva
 *   protective-scope kernel: the position, grounded in Common Article 3 and
 *   customary/treaty human rights law, that humane-treatment protections
 *   apply to every person affected by armed conflict irrespective of
 *   combatant classification. This reading is deliberately generated as its
 *   own constraint, distinct from the sibling state_centric_reading (which
 *   gates protection on Article 4 combatant status) and the
 *   hybrid_proportionality_reading (which scales protection by conflict-type
 *   classification). Under this reading's own lights, the standing
 *   arrangement it is about is the CURRENT contested practice of state
 *   militaries and intelligence services asserting operational latitude
 *   against persons of ambiguous status — an arrangement this reading regards
 *   as substantially extractive of protection from those persons. ε is
 *   authored high (0.62) because, from inside this reading, the state
 *   practice it contests routinely strips protection from unprivileged
 *   belligerents and civilians through status-gatekeeping; that gatekeeping
 *   is what the universal floor exists to foreclose.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.55).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Universal Rights Reading of Geneva Protective Scope (Common Article 3 + IHRL Floor)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '32b30aa7-5a3c-497f-b7cd-6b3c48f50339').
narrative_ontology:cs_kernel_codification('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', fixed_text).
narrative_ontology:cs_authority_grounding('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', lineage).
narrative_ontology:cs_interpretation_layer_present('32b30aa7-5a3c-497f-b7cd-6b3c48f50339').
narrative_ontology:cs_reading_relation('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', foundational, protection_attaches_to_personhood_not_status).
narrative_ontology:cs_axiom_status(protection_attaches_to_personhood_not_status, holdable).
narrative_ontology:cs_axiom_grounding('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', protection_attaches_to_personhood_not_status, deontological).
narrative_ontology:cs_axiom('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', secondary, common_article_3_as_non_derogable_customary_floor).
narrative_ontology:cs_axiom_status(common_article_3_as_non_derogable_customary_floor, holdable).
narrative_ontology:cs_axiom_grounding('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', common_article_3_as_non_derogable_customary_floor, conventional).
narrative_ontology:cs_reference_frame('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', common_article_3_original_1949_floor).
narrative_ontology:cs_drift_state('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', post_9_11_counterterrorism_detention_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('32b30aa7-5a3c-497f-b7cd-6b3c48f50339', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_group_members).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detained_unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_commanders).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, detaining_state_intelligence_services).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, human_dignity_as_universal_legal_floor).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, martens_clause_customary_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live inside active conflict zones with no combatant status and no way to leave the area of hostilities. Under this reading, they are owed protection under Common Article 3 and human rights law regardless of how belligerents classify them, which constrains targeting and detention practices around them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, regional).

% Fight without uniforms or responsible command structures that would satisfy Article 4 combatant criteria. Under the universal reading they still receive baseline humane-treatment protections when captured or wounded, rather than being treated as unprivileged and outside treaty coverage.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_group_members, beneficiary,
    moderate, biographical, constrained, regional).

% Held by a detaining state after capture, often outside formal POW status. This reading extends interrogation and detention-condition limits to them on the same floor as any other person affected by conflict, foreclosing arguments that they fall outside treaty scope entirely.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detained_unprivileged_belligerents, beneficiary,
    powerless, biographical, trapped, national).

% Plan and execute targeting, detention, and interrogation operations against mixed populations of combatants and non-combatants. This reading narrows the operational latitude they would otherwise claim against unprivileged belligerents, requiring them to apply the same humane-treatment floor across a broader population and slowing or constraining tactical decisions.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_commanders, payer,
    institutional, immediate, constrained, national).

% Conduct interrogation of captured persons for operational intelligence. Under the universal reading, coercive interrogation techniques that might have been justified against 'unprivileged belligerents' outside treaty scope are prohibited, because the same protective floor applies to everyone affected by the conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detaining_state_intelligence_services, payer,
    institutional, immediate, constrained, national).

% Develop and enforce the interpretive position that Common Article 3 plus international human rights law create a non-derogable floor applicable to everyone affected by armed conflict. They issue advisory opinions, general comments, and jurisprudence that operationalize and administer this reading against state objections.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, human_rights_treaty_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Prosecute violations of humane-treatment obligations and, through case law (e.g., Tadic-line jurisprudence), extend and enforce the customary-law basis for universal protective scope, shaping which state conduct becomes criminally actionable.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_tribunals, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_tribunals, observer).

% Bear the operational and political cost of complying with an expanded protective floor but have limited voice in the interpretive bodies that expand it; their operational commanders argue the reading was not negotiated into the treaty text they ratified, but this objection largely surfaces in state practice and diplomatic protest rather than in the interpretive fora themselves.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, troop_contributing_states, excluded,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__universal_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, universal humane-treatment floor that applies regardless of how a person is classified in an armed conflict, so that no combatant-status determination can be used to strip a person of all protection — solving the coordination problem of protection gaps at the edges of formal combatant categories.
% TRANSFER_FUNCTION: Moves operational latitude and interrogation/detention discretion away from state militaries and intelligence services and toward captured or affected persons in the form of enforceable procedural and substantive treatment guarantees.
% ABSENT_VOICES: Field commanders and intelligence officers who must operationalize targeting and interrogation decisions in real time are rarely party to the treaty-body and tribunal proceedings that expand the doctrine; their operational-necessity arguments surface mainly as after-the-fact state objections or dissenting opinions, not as participants in the interpretive process itself.
% DISAPPEARANCE_RATIONALE: If the universal floor disappeared, states would revert to Article-4-style combatant screening as the gate to any protection, unprivileged belligerents and irregular fighters would lose the humane-treatment guarantees they currently claim under Common Article 3 and IHRL, and interrogation/detention practice against 'unlawful combatants' would expand into the space currently foreclosed by this reading.
% FOUNDING_PROBLEM: Post-WWII and subsequent conflicts revealed that combatant-status gatekeeping (Article 4 criteria) left irregular fighters, civilians in non-international conflicts, and captured persons of ambiguous status with no clear legal floor, enabling treatment outside any humane-treatment regime.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's own commentaries and ICTY/ICTR jurisprudence (outside any single state's benefiting position) attest that gaps in combatant-status coverage were real and produced documented mistreatment; UN human rights bodies independent of the states being constrained corroborate that the protection gap persists in ongoing non-international armed conflicts and counterterrorism operations.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.25 in 1949 to 0.62 in 2024) reflecting the growing gap this reading identifies between the original Common Article 3 floor and the expanding scope of irregular and non-international conflict where states have historically claimed exemption. Suppression (0.55) and suppression_requirement growth track the increasing enforcement infrastructure (tribunal jurisprudence, treaty-body general comments, customary law claims) required to make the universal floor stick against resistant state practice. Theater ratio stays comparatively low (0.28) because the doctrine's coordination function — preventing total protection gaps — remains substantively active, not merely performative, though some compliance reporting by states is largely declaratory.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations, non-state armed group members, and detained unprivileged belligerents are the structural beneficiaries: the reading extends enforceable protections to them that a state-centric reading would deny. State military commanders and detaining-state intelligence services are the structural targets: the same doctrine narrows their operational and interrogation latitude, which is precisely the mechanism by which the protection is delivered. Human rights treaty bodies and international tribunals are agenda-setters who administer and expand the doctrine through interpretive authority, not beneficiaries collecting rents themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protection gaps at the edges of combatant-status classification — remains live: ongoing non-international armed conflicts and counterterrorism detention regimes continue to produce persons whose status is contested precisely to strip them of protection. This is not mandatrophy: the mandate has not outlived its function, and corroboration comes from ICRC commentary and tribunal jurisprudence independent of any state benefiting from either expansion or restriction of the floor. Tangled Rope is the correct claim (not Rope) because the same structure that coordinates a universal floor also imposes asymmetric costs on state military operations, and persistence depends on active enforcement (treaty-body pressure, tribunal prosecution) against resistant state practice — it is not simply mutual benefit with low overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_status_of_universal_floor,
    'Has the universal humane-treatment floor claimed under Common Article 3 plus IHRL achieved genuine customary international law status binding on all states, or does it remain a contested interpretive extension resisted by persistent-objector states?',
    'Systematic survey of state practice and opinio juris across non-signatory and objecting states; ICJ or arbitral rulings squarely addressing the customary status question outside the ICRC''s own commentary.',
    'If genuinely customary, the doctrine''s enforceability and legitimacy are far stronger than a contested treaty-interpretation claim; if contested, state resistance is not mere non-compliance but a live disagreement about what the law actually requires, which would lower the confidence that this reading''s high ε is uncontestedly correct even by its own lights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_status_of_universal_floor, conceptual, 'Whether the universal floor is settled customary law or a contested interpretive extension.').

omega_variable(
    reading_selection_and_kernel_indeterminacy,
    'Is the choice among universal_rights_reading, state_centric_reading, and hybrid_proportionality_reading itself resolved by the treaty text plus subsequent practice, or does the underlying kernel (Geneva protective scope) remain genuinely indeterminate such that the ''correct'' reading is a matter of institutional power rather than legal fact?',
    'Comparative doctrinal history tracing which reading has prevailed in which fora (ICTY/ICTR vs. US military commissions vs. ICRC commentary) and whether convergence is occurring or the split is stable.',
    'If the kernel is genuinely indeterminate, all three readings persist as live, mutually irreducible constraints rather than one being an error correction of the others — supporting the coexists_with relations declared below rather than any reading foreclosing its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_and_kernel_indeterminacy, conceptual, 'Whether the three sibling readings reflect a genuinely indeterminate kernel or a resolvable interpretive dispute.').

omega_variable(
    operational_necessity_vs_protection_floor,
    'Does the universal floor''s restriction on interrogation and detention practices measurably degrade state military operational effectiveness (intelligence yield, force protection), or is the operational-cost claim itself largely a justificatory narrative?',
    'Declassified after-action analysis and comparative studies of detention/interrogation outcomes under strict Common Article 3 compliance versus expanded-latitude regimes.',
    'If operational costs are real and substantial, the payer-seat extraction this reading imposes on state militaries is a genuine trade-off, not merely disciplinary theater; if the claimed operational cost is mostly rhetorical, the resistance metric authored here may overstate the doctrine''s true friction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_necessity_vs_protection_floor, empirical, 'Whether restricted operational latitude imposes real costs or is primarily a justificatory claim by resistant state actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1977, 0.12).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(gene_tr_t2006, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2006, 0.24).
narrative_ontology:measurement(gene_tr_t2015, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.25).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1977, 0.35).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(gene_be_t2006, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2006, 0.58).
narrative_ontology:measurement(gene_be_t2015, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1977, 0.3).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(gene_su_t2006, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2006, 0.48).
narrative_ontology:measurement(gene_su_t2015, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% Part of a three-member constraint family reading the geneva_conventions_protective_scope kernel: state_centric_reading (Article 4 combatant-status gate), hybrid_proportionality_reading (conflict-type-scaled protection), and this universal_rights_reading (Common Article 3 + IHRL universal floor). Each carries its own ε, beneficiary/victim structure, and claimed_type per the ε-invariance principle; they are linked here rather than merged into one observable-parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
