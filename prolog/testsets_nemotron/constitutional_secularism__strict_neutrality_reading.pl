% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Strict Neutrality: Equal State Distance from All Religions
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the strict_neutrality_reading of the
 *   constitutional_secularism kernel: the state maintains equal distance from
 *   all religions, offering no preferential treatment and no interference. It
 *   is one of three live readings of the kernel — the others being
 *   principled_intervention_reading (state may intervene for social reform)
 *   and reformist_reading (state has affirmative duty to eliminate oppressive
 *   religious practices). The strict neutrality reading presents itself as a
 *   coordination mechanism (rope): it solves the collective-action problem of
 *   religious pluralism by committing the state to a single, uniform rule
 *   that no community can capture. Its beneficiaries are minority religious
 *   communities and non-religious citizens who gain protection from
 *   majoritarian capture; its victims are reformist state actors who lose
 *   capacity for targeted intervention, and vulnerable members within
 *   religious communities who may lose state protection against internal
 *   oppression. The constraint requires active judicial enforcement to
 *   maintain the equal-distance rule against political pressure for
 *   accommodation or intervention.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.18).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.12).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Strict Neutrality: Equal State Distance from All Religions").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '018ccbec-748d-4373-8326-0dc1d7a20289').
narrative_ontology:cs_kernel_codification('018ccbec-748d-4373-8326-0dc1d7a20289', formalized).
narrative_ontology:cs_authority_grounding('018ccbec-748d-4373-8326-0dc1d7a20289', lineage).
narrative_ontology:cs_interpretation_layer_present('018ccbec-748d-4373-8326-0dc1d7a20289').
narrative_ontology:cs_reading_relation('018ccbec-748d-4373-8326-0dc1d7a20289', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('018ccbec-748d-4373-8326-0dc1d7a20289', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('018ccbec-748d-4373-8326-0dc1d7a20289', foundational, state_religious_equidistance).
narrative_ontology:cs_axiom_status(state_religious_equidistance, holdable).
narrative_ontology:cs_axiom_grounding('018ccbec-748d-4373-8326-0dc1d7a20289', state_religious_equidistance, deontological).
narrative_ontology:cs_axiom('018ccbec-748d-4373-8326-0dc1d7a20289', foundational, non_interference_as_neutrality).
narrative_ontology:cs_axiom_status(non_interference_as_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('018ccbec-748d-4373-8326-0dc1d7a20289', non_interference_as_neutrality, deontological).
narrative_ontology:cs_reference_frame('018ccbec-748d-4373-8326-0dc1d7a20289', founding_constitutional_equidistance).
narrative_ontology:cs_drift_state('018ccbec-748d-4373-8326-0dc1d7a20289', contemporary_pluralism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('018ccbec-748d-4373-8326-0dc1d7a20289', '2026-08-03T14:22:10Z').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, non_religious_citizens).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, interfaith_civic_organizations).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, reformist_state_actors).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, vulnerable_members_within_religious_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, majority_religious_community_leadership).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, state_religious_neutrality_doctrine).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, equal_liberty_of_conscience).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, non_establishment_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain constitutional protection against state interference and majoritarian accommodation. Their internal autonomy is guaranteed, but they cannot demand state resources or recognition beyond equal treatment. Exit from the constraint would mean seeking special accommodation, which the constraint forbids.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, minority_religious_communities, beneficiary,
    organized, generational, constrained, national).

% Gain protection from religious establishment and state-sponsored religious expression. The constraint ensures civic space remains religiously neutral. They bear no direct cost and have high exit options (secular citizenship is the default).
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, non_religious_citizens, beneficiary,
    organized, biographical, mobile, national).

% Operate in a neutral civic space where no religion has state privilege. They benefit from the constraint's coordination of pluralism but do not control it. Their exit options are high — they can advocate for other models without personal cost.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, interfaith_civic_organizations, beneficiary,
    moderate, biographical, mobile, national).

% Legislators, executives, and judges who would use state capacity to reform religious practices (e.g., gender equality in personal law, child protection in religious education). The constraint blocks their agenda; they bear the opportunity cost of forgone reform. Their exit is constrained — they cannot easily change the constitutional rule, only interpret it narrowly or seek amendment.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, reformist_state_actors, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, reformist_state_actors, agenda_setter).

% Women, LGBTQ+ members, children, and dissidents within religious communities who face internal oppression (forced marriage, denial of education, exclusion from leadership, honor violence). The constraint denies them state intervention by treating the community as an autonomous sphere. Their exit is identity-locked: leaving the community means losing family, identity, and social world; staying means enduring practices the state will not touch.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, vulnerable_members_within_religious_communities, payer,
    powerless, biographical, identity_locked, local).

% Leadership of the demographic majority religion. They benefit from strict neutrality because it prevents state reform of their practices while the state's general secular posture prevents rival religions from gaining establishment. They have arbitrage-grade exit: they can mobilize politically for accommodation when convenient, and invoke neutrality when reform is threatened.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, majority_religious_community_leadership, beneficiary,
    powerful, generational, arbitrage, national).

% Courts that interpret and enforce the equal-distance rule. They are the active enforcement mechanism. They do not collect extraction; they administer the constraint. Their role is to police the boundary between permissible accommodation and forbidden preference — a line that is inherently contestable and generates the constraint's theater and resistance.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Academic and civil society actors who analyze, critique, and advocate across the three readings. They see the full structure: the kernel, the three readings, the seat divergence. They do not bear costs or collect benefits from the constraint's operation directly.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, secularism_scholars_and_activists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of religious pluralism: how can a state govern a religiously diverse population without capturing or being captured by any religion? The strict neutrality rule provides a single, uniform, pre-committed constraint that no community can bend to its advantage.
% TRANSFER_FUNCTION: Transfers state intervention capacity from reformist state actors (who lose the power to intervene in religious affairs) to religious community leadership (who gain autonomy). Vulnerable intra-community members bear the cost of lost state protection; majority community leadership gains the most autonomy per unit of state restraint.
% ABSENT_VOICES: Vulnerable members within religious communities (especially women and children in conservative communities) are structurally excluded from the constitutional conversation — they are not parties to the neutrality bargain, and their interests are represented only indirectly through the reformist_state_actor seat, which the constraint constrains. Diaspora and transnational religious actors who would influence state policy are also excluded by the equal-distance rule.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished overnight, states would immediately face pressure for both majoritarian accommodation (establishment, preferential funding) and targeted reform (gender equality in personal law, child protection). The religious governance landscape would reorganize around either principled_intervention or reformist models, or fragment into ad hoc accommodations. Minority communities would lose their autonomy guarantee; vulnerable intra-community members might gain state protection but at the cost of community self-governance.
% FOUNDING_PROBLEM: Post-colonial constitutional founding: how to govern a deeply religiously plural society without reproducing colonial divide-and-rule or majoritarian domination. The strict neutrality reading was built to solve the problem of state capture by any single religious community, ensuring the new state would not become an instrument of religious majoritarianism.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing majoritarian state capture) is attested as still live by minority religious communities, non-religious citizens, and constitutional scholars outside the benefiting parties. However, reformist_state_actors and vulnerable_intra_community_member advocates attest that a second problem (intra-community oppression) has emerged that the founding arrangement does not address and may exacerbate — this is contested, not dead.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).
:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint primarily coordinates by forbidding state capture rather than transferring resources; the cost is opportunity cost for reformist actors, not direct extraction. Suppression is low (0.12) because the constraint operates through judicial review, not coercive enforcement against citizens. Theater ratio is very low (0.08) — the neutrality principle is genuinely operational in constitutional jurisprudence, not performative. Accessibility collapse is low (0.25) because alternative arrangements (principled intervention, reformist) remain live and advocated. Resistance is moderate (0.35) from reformist state actors and civil society groups who see neutrality as complicity in intra-community oppression. The constraint is claimed as rope — a genuine coordination mechanism — and the metrics support this: low extraction, low suppression, active enforcement, clear beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different seat types: from the minority_community seat, the constraint is rope (coordination benefit, low cost); from the vulnerable_intra_community_member seat, it may compute as snare or tangled_rope (they bear the cost of non-intervention while the community leadership benefits); from the reformist_state_actor seat, it is a constraint on their agenda (payer role); from the non_religious_citizen seat, it is rope (protection from establishment). The structural asymmetry is that the constraint's coordination function (preventing state capture) and its extraction function (denying state protection to vulnerable intra-community members) fall on different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: minority_religious_communities (protected from majoritarian state action), non_religious_citizens (protected from establishment), interfaith_civic_organizations (gain neutral civic space). Victims: reformist_state_actors (lose intervention capacity — agenda_setter role constrained), vulnerable_members_within_religious_communities (lose potential state protection against internal oppression — payer role). The state itself is the agenda_setter constrained by its own rule. Directionality derivation: beneficiaries have low d (constraint subsidizes them), reformist_state_actors have moderate d (constraint blocks their agenda), vulnerable_intra_community_members have high d (constraint denies them protection they would otherwise have).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing religious pluralism without state capture) remains live — religious diversity has increased, not decreased. However, a new problem has emerged: intra-community vulnerability under autonomy. The constraint does not suffer mandatrophy in the classic sense (function atrophied but form persists) — its original function is still needed. But it has developed an extraction externality: the autonomy it guarantees to communities becomes a shield for internal power structures. This is not mandatrophy but a structural side effect that the sibling readings (principled_intervention, reformist) were designed to address.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the constitutional_secularism kernel, or does the ''strict neutrality'' label mask a substantively different arrangement?',
    'Comparative doctrinal analysis: trace whether judicial and legislative practice under this reading consistently applies equal-distance logic across all religious communities, or whether it selectively protects majority community autonomy while constraining minority reform.',
    'If the reading systematically exempts majority community practices from equal-distance application, the constraint is not a true strict_neutrality_reading but a majoritarian_autonomy_reading — a different constraint with different beneficiaries and extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the declared reading matches the operational constraint').

omega_variable(
    minority_vulnerability_ambiguity,
    'Does strict neutrality''s preservation of minority autonomy protect vulnerable members within minority communities, or does it entrench internal power structures that harm them?',
    'Longitudinal study of intra-community reform outcomes in jurisdictions with strict neutrality vs. principled intervention: track whether vulnerable members (women, LGBTQ+, dissidents) gain or lose exit options and legal protections under each regime.',
    'If strict neutrality systematically correlates with worse outcomes for vulnerable intra-community members, the constraint extracts from them (they bear costs of non-intervention) — reclassifying them from incidental beneficiaries to victims, and shifting the constraint toward tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_vulnerability_ambiguity, empirical, 'Whether minority autonomy under strict neutrality shields or harms vulnerable intra-community members').

omega_variable(
    enforcement_asymmetry,
    'Is the constraint''s enforcement (judicial review of state religious action) applied symmetrically across communities, or does it disproportionately constrain state action toward minority communities while permitting majoritarian religious accommodation?',
    'Case law audit: code all state-religion cases over a 20-year period for (a) community affected, (b) direction of state action (accommodation vs. restriction), (c) judicial outcome. Test for community-type × outcome interaction.',
    'Asymmetric enforcement would mean the constraint operates as a ratchet: it prevents state reform of minority practices but permits state accommodation of majority practices — a structural extraction from minority communities toward majoritarian norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Whether equal-distance enforcement is symmetric across religious communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_tr_t10, constitutional_secularism__strict_neutrality_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_tr_t20, constitutional_secularism__strict_neutrality_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_tr_t30, constitutional_secularism__strict_neutrality_reading, theater_ratio, 30, 0.08).

% Extraction over time
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_be_t10, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_be_t20, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_be_t30, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 30, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_su_t10, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_su_t20, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(constitutional_secularism__strict_neutrality_reading_su_t30, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 30, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__strict_neutrality_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% The constitutional_secularism kernel decomposes into three constraint stories: strict_neutrality_reading (this file, rope), principled_intervention_reading (tangled_rope — coordination + asymmetric extraction from majority community), reformist_reading (snare — high extraction from religious autonomy for protection of vulnerable members). All three share the kernel_id constitutional_secularism. The strict neutrality reading is the baseline; the interventionist readings are structured as departures from it that must carry their own justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__strict_neutrality_reading, institutional, 0.35).
constraint_indexing:directionality_override(constitutional_secularism__strict_neutrality_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
