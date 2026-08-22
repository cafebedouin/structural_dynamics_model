% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Freedom of Movement as Primary Right Against Border Closure
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint instantiates the reading that freedom of movement is a
 *   fundamental human right that territorial sovereignty cannot override. The
 *   standing arrangement under contest is the global border control regime
 *   that restricts movement based on nationality. This reading assesses that
 *   arrangement as having low extractiveness (0.15) because the constraint
 *   itself — the right to move — is coordinative and non-extractive; the
 *   extraction comes from the border regime that violates it, not from the
 *   right. The constraint coordinates human mobility across borders without
 *   extracting from those who move. Beneficiaries are displaced persons,
 *   migrant workers, refugees, asylum seekers, and stateless persons who gain
 *   access to territory and rights. No victims are declared because the
 *   constraint itself imposes no costs; the costs are imposed by the border
 *   regime that this reading rejects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.15).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.05).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.15).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, rope).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Freedom of Movement as Primary Right Against Border Closure").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political_philosophy/international_law/migration_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '661753b9-95fe-4d30-ba9a-718aefb7f6b7').
narrative_ontology:cs_kernel_codification('661753b9-95fe-4d30-ba9a-718aefb7f6b7', formalized).
narrative_ontology:cs_authority_grounding('661753b9-95fe-4d30-ba9a-718aefb7f6b7', lineage).
narrative_ontology:cs_interpretation_layer_present('661753b9-95fe-4d30-ba9a-718aefb7f6b7').
narrative_ontology:cs_reading_relation('661753b9-95fe-4d30-ba9a-718aefb7f6b7', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('661753b9-95fe-4d30-ba9a-718aefb7f6b7', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('661753b9-95fe-4d30-ba9a-718aefb7f6b7', foundational, freedom_of_movement_prepolitical_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_prepolitical_right, holdable).
narrative_ontology:cs_axiom_grounding('661753b9-95fe-4d30-ba9a-718aefb7f6b7', freedom_of_movement_prepolitical_right, deontological).
narrative_ontology:cs_axiom('661753b9-95fe-4d30-ba9a-718aefb7f6b7', foundational, border_closure_not_constitutive_of_sovereignty).
narrative_ontology:cs_axiom_status(border_closure_not_constitutive_of_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('661753b9-95fe-4d30-ba9a-718aefb7f6b7', border_closure_not_constitutive_of_sovereignty, deontological).
narrative_ontology:cs_axiom('661753b9-95fe-4d30-ba9a-718aefb7f6b7', secondary, state_authority_limited_to_jurisdictional_regulation).
narrative_ontology:cs_axiom_status(state_authority_limited_to_jurisdictional_regulation, holdable).
narrative_ontology:cs_axiom_grounding('661753b9-95fe-4d30-ba9a-718aefb7f6b7', state_authority_limited_to_jurisdictional_regulation, conventional).
narrative_ontology:cs_reference_frame('661753b9-95fe-4d30-ba9a-718aefb7f6b7', universal_declaration_article_13_framework).
narrative_ontology:cs_drift_state('661753b9-95fe-4d30-ba9a-718aefb7f6b7', contemporary_border_regime_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('661753b9-95fe-4d30-ba9a-718aefb7f6b7', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, displaced_persons).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, migrant_workers).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, refugees).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, stateless_persons).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, freedom_of_movement_fundamental_right).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, border_control_not_constitutive_of_sovereignty).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, human_rights_primacy_over_state_exclusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People forced from their homes by conflict, persecution, climate change, or economic collapse. The right to move enables them to seek safety and livelihood. Their exit options are constrained by the border regime this reading opposes — they cannot simply choose to move; they must navigate walls, visas, and enforcement. When the right is respected, they gain full access to territory and protection.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_persons, beneficiary,
    powerless, biographical, constrained, global).

% Workers who move across borders for employment. The right to move coordinates their labor with demand. They have more exit options than displaced persons (skills, contracts, recruitment channels) but still face border barriers. The constraint enables their economic agency; they benefit directly from open movement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, migrant_workers, beneficiary,
    moderate, biographical, mobile, global).

% Persons fleeing persecution with a well-founded fear of return. The right to move is existential for them — without it, they face refoulement. Their exit options are trapped without the right; the constraint is the difference between survival and persecution. They are the most intense beneficiaries.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, refugees, beneficiary,
    powerless, immediate, trapped, global).

% Persons seeking international protection whose claims are pending. The right to move includes the right to seek asylum at borders. They are constrained by the border regime's deterrence policies (pushbacks, detention, externalization). The constraint enables their claim to be heard.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers, beneficiary,
    powerless, immediate, constrained, global).

% Persons not recognized as nationals by any state. The right to move is their only path to rights, since no state claims them. They are trapped in legal limbo without the right; the constraint is their sole structural access to territory and protection.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, stateless_persons, beneficiary,
    powerless, generational, trapped, global).

% States that receive migrants and refugees. This reading treats them as having jurisdictional authority to regulate rights and obligations within territory (labor rights, integration, social services) but not exclusion authority. They observe the constraint's operation and may resist or accommodate it. Their analytical seat reflects the reading's denial of their exclusion authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, receiving_states, observer,
    institutional, generational, analytical, national).

% States from which people flee or migrate. This reading treats them as having no right to prevent exit (freedom of movement includes exit rights) but jurisdictional authority over their territory. They observe the constraint's operation on their populations.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, sending_states, observer,
    institutional, generational, analytical, national).

% UN treaty bodies, regional courts, and human rights mechanisms that interpret and promote the right to freedom of movement. They set the agenda for this reading by issuing rulings, general comments, and country reviews that treat border closure as a human rights violation. They administer the constraint's normative framework.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Agencies that implement border closure (CBP, Frontex, national border guards). This reading treats their function as illegitimate extraction — they are excluded from the constraint's beneficiary structure because the constraint denies the legitimacy of their exclusion authority. They would object to the reading but are structurally excluded from its coordination function.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_agencies, excluded,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__freedom_of_movement_primary, diffuse).
narrative_ontology:fixing_cost_class(border_control_legitimacy__freedom_of_movement_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human mobility across borders for survival, labor, family unity, and safety — enabling people to move to where their needs can be met without requiring permission from the destination state.
% TRANSFER_FUNCTION: Moves the power to exclude from states to no one — the constraint transfers exclusion authority from states to a null set, replacing it with a coordination mechanism where movement is governed by individual need and right rather than state permission. No actor receives the extraction because there is no extraction; the transfer is the removal of extraction (border enforcement) itself.
% ABSENT_VOICES: States that claim absolute exclusion authority (sovereignty_primary reading) are structurally excluded from this reading's beneficiary set. They would argue that border control is constitutive of statehood and that unlimited movement collapses the state's capacity to provide public goods. They are absent because this reading denies their exclusion authority legitimacy. Populations in receiving states who fear labor market competition or cultural change are also absent — their concerns are addressed through jurisdictional regulation (labor rights, integration policy) not exclusion.
% DISAPPEARANCE_RATIONALE: If the right to freedom of movement disappeared overnight, the global border regime would revert to unchallenged sovereignty_primary logic: states would exercise absolute exclusion discretion, displaced persons would have no legal path to safety, migrant workers would be entirely at employer/state mercy, and the international protection system would collapse. The world would rearrange around absolute state exclusion authority.
% FOUNDING_PROBLEM: The founding problem was the totalitarian control of movement by states — the ability of states to trap populations (exit bans), deny entry to refugees (refoulement), and treat human mobility as a privilege granted by sovereignty rather than a right inherent to personhood. The UDHR Article 13 and the 1951 Refugee Convention were built to solve this.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the drafting history of UDHR Article 13 (the Soviet bloc pushed for exit rights; Western states resisted entry rights), the 1951 Refugee Convention's non-refoulement obligation as a direct response to WWII refoulement, and contemporary documentation of exit bans (North Korea, Eritrea, Soviet-era) and entry denials (pushbacks, visa regimes). No single party corroborates the full reading — displaced persons and human rights bodies attest the problem is live; sovereignty_primary states attest it is dead (sovereignty solved it); jurisdictional_sovereignty states attest it is contested (protection obligations exist but are balanced).
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).
:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is rope because this reading presents freedom of movement as a genuine coordination mechanism — people move to where their labor, safety, or family ties are — with minimal coercive overhead. Extractiveness is low (0.15) because the constraint is the right itself, not the border enforcement that violates it. Suppression is near-zero (0.05) because the right does not suppress alternatives; it expands them. Theater ratio is low (0.1) because there is little performative maintenance of the right as coordination — the right either functions or it is violated by states. Accessibility collapse is low (0.25) because alternatives (irregular migration, smuggling networks) persist even when the right is formally recognized but not enforced. Resistance is high (0.7) because the constraint meets active resistance from states that maintain border closure authority — the resistance is against the constraint, not from its subjects.
 *
 * PERSPECTIVAL GAP:
 *   From the displaced person's seat, the constraint is rope — genuine coordination enabling survival and flourishing. From the state's seat (which this reading treats as having no legitimate exclusion authority), the constraint would appear as a limitation on sovereign discretion, but this reading denies that seat's legitimacy to exclude. The engine will compute per-seat classifications from the structural data: displaced persons have low directionality (beneficiaries), while states would have high directionality if they were treated as constrained parties — but this reading excludes states from the victim/beneficiary structure because it denies their authority to exclude.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (displaced_persons, migrant_workers, refugees, asylum_seekers, stateless_persons) are structurally positioned as the primary subjects of the right — they are the ones whose movement the constraint coordinates. They have high exit options (mobile to arbitrage depending on documentation status) and gain the full benefit of movement. No victims are declared because the constraint itself — the right to move — extracts from no one. The extraction in the system comes from the border regime, which is a separate constraint (the sovereignty_primary reading). This reading's ε = 0.15 reflects the residual coordination costs (documentation, verification, integration) not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling human mobility for survival, labor, and family — remains live and contested. The constraint has not suffered mandatrophy because the coordination problem it solves (people need to move across borders) persists and has intensified with climate displacement, conflict, and economic inequality. The reading does not become piton because its coordination function remains essential; it becomes more rope-like as displacement pressures grow. The theater ratio stays low because the right's operation is not performative — it is either respected or violated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the border_control_legitimacy kernel a single commitment with multiple readings, or are these fundamentally different kernels with different referents?',
    'Trace whether all three readings adjudicate the same historical commitment (e.g., the Westphalian sovereignty framework + UDHR Article 13) or whether they invoke different foundational texts and authority structures. If different referents, they are different kernels, not sibling readings.',
    'If different kernels, the network.affects_constraints links should be removed and each should stand alone. If one kernel, the reading_relations and axioms structure here is valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the three declared readings share a single kernel or constitute separate kernels.').

omega_variable(
    state_exclusion_vs_regulation_boundary,
    'Does this reading''s denial of border closure authority leave states with any legitimate exclusion power, or does it collapse all exclusion into jurisdictional regulation?',
    'Examine whether the reading permits any exclusion (e.g., individual security screening, infectious disease quarantine) or whether all exclusion is reclassified as regulation of rights/obligations post-entry. The boundary determines whether the constraint is absolute (rope) or has enforcement exceptions (tangled_rope).',
    'If some exclusion is permitted, the constraint has enforcement machinery and requires active enforcement, shifting toward tangled_rope. If absolute, it remains rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_exclusion_vs_regulation_boundary, conceptual, 'Whether the reading permits any legitimate exclusion or treats all exclusion as regulation.').

omega_variable(
    extraction_referent_disambiguation,
    'Is the ε=0.15 correctly referencing the standing arrangement under contest (the border regime) assessed by this reading''s lights, or does it reference the right itself?',
    'Apply the ε-invariance principle: if measuring the border regime''s extraction from displaced persons gives high ε, and measuring the right''s coordination function gives low ε, these are two constraints. This reading must author ε for the right''s coordination function, not the border regime''s extraction.',
    'Misattribution would conflate the reading''s constraint with the sibling''s constraint. The ε must be invariant to the observable — if it changes when measuring border regime vs. right, there are two constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_referent_disambiguation, conceptual, 'Whether ε correctly references the reading''s constraint (the right) not the contested arrangement (the border regime).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(bord_tr_t1966, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1966, 0.07).
narrative_ontology:measurement(bord_tr_t1990, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(bord_tr_t2000, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(bord_tr_t2015, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(bord_tr_t2025, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1948, 0.1).
narrative_ontology:measurement(bord_be_t1966, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1966, 0.12).
narrative_ontology:measurement(bord_be_t1990, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1990, 0.14).
narrative_ontology:measurement(bord_be_t2000, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(bord_be_t2015, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(bord_be_t2025, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1948, 0.03).
narrative_ontology:measurement(bord_su_t1966, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1966, 0.04).
narrative_ontology:measurement(bord_su_t1990, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(bord_su_t2000, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(bord_su_t2015, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2015, 0.05).
narrative_ontology:measurement(bord_su_t2025, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2025, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__freedom_of_movement_primary, 0.08).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This reading and its siblings decompose the border_control_legitimacy kernel into three constraints with different ε and beneficiary/victim structures. sovereignty_primary has high extraction (border regime as snare/tangled_rope) with states as beneficiaries and displaced persons as victims. jurisdictional_sovereignty has moderate extraction with mixed beneficiary/victim structure. This reading (freedom_of_movement_primary) has low extraction (rope) with displaced persons as beneficiaries and no declared victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
