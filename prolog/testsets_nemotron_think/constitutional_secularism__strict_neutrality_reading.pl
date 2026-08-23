% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Strict Neutrality Reading of Constitutional Secularism
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   The strict neutrality reading of Indian constitutional secularism holds
 *   that the state must maintain equal distance from all religions — no
 *   preferential treatment, no interference in religious affairs. This
 *   reading dominated early Supreme Court jurisprudence (1950s-1970s) and
 *   remains a live position. It coordinates pluralistic coexistence by
 *   foreclosing state theological judgment. However, it extracts autonomy
 *   from minority communities by denying them state partnership for internal
 *   reform, and it abandons intra-community marginalized groups (women,
 *   LGBTQ+, lower castes) to community gatekeepers. The constraint is a
 *   tangled rope: genuine coordination (preventing majoritarian capture)
 *   combined with asymmetric extraction (minorities and their vulnerable
 *   members pay the cost of 'neutrality').
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.48).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.35).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Strict Neutrality Reading of Constitutional Secularism").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, 'c18d8daf-26ff-4c68-acf1-4d68a67c9bad').
narrative_ontology:cs_kernel_codification('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', formalized).
narrative_ontology:cs_authority_grounding('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', lineage).
narrative_ontology:cs_interpretation_layer_present('c18d8daf-26ff-4c68-acf1-4d68a67c9bad').
narrative_ontology:cs_reading_relation('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', foundational, state_religious_equidistance).
narrative_ontology:cs_axiom_status(state_religious_equidistance, holdable).
narrative_ontology:cs_axiom_grounding('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', state_religious_equidistance, conventional).
narrative_ontology:cs_axiom('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', secondary, minority_autonomy_as_noninterference).
narrative_ontology:cs_axiom_status(minority_autonomy_as_noninterference, holdable).
narrative_ontology:cs_axiom_grounding('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', minority_autonomy_as_noninterference, conventional).
narrative_ontology:cs_reference_frame('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', constitutional_secularism_equidistance).
narrative_ontology:cs_drift_state('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', contemporary_majoritarian_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c18d8daf-26ff-4c68-acf1-4d68a67c9bad', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_majority_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, secular_civil_society_organizations).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, religious_minority_communities).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, marginalized_intra_community_groups).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, state_religious_equidistance_principle).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, formal_equality_before_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and administers laws under the constitutional mandate of secularism. The strict neutrality reading constrains the state from targeted interventions in minority religious practices even when such interventions are sought by reformers within those communities. The state bears the political cost of being seen as indifferent to intra-community oppression.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_legislature_executive, agenda_setter,
    institutional, generational, constrained, national).

% Interprets and applies the strict neutrality principle in adjudicating religious freedom cases. Early jurisprudence embraced equidistance; later benches introduced 'essential practices' test allowing limited intervention. The court's interpretive authority makes it both administrator and observer of the constraint's drift.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, supreme_court_judiciary, observer).

% Under strict neutrality, majority religious norms face no state scrutiny or reform pressure. Their personal laws and institutional structures operate with de facto autonomy. The constraint's formal equality masks substantive advantage: majority practices become the baseline against which 'neutrality' is measured.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_majority_communities, beneficiary,
    powerful, generational, mobile, national).

% Minority communities lose the capacity to seek state intervention for internal reform (e.g., gender-just personal law reform). The constraint treats their religious autonomy as absolute, foreclosing protective legislation. Exit from the constraint is identity-locked: communal identity is fused with religious law, making reform feel like existential betrayal.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minority_communities, payer,
    moderate, generational, identity_locked, national).

% Women, LGBTQ+ persons, lower-caste members, and dissenters within religious communities bear the cost of non-intervention. They cannot access state protection against discriminatory personal laws or community sanctions. Their exit options are structurally trapped: leaving the community means social death; staying means subordination.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, marginalized_intra_community_groups, payer,
    powerless, biographical, trapped, national).

% Rights groups, feminist organizations, and liberal legal advocates benefit from the formal equality framework which provides a vocabulary for challenging state favoritism. However, they are split: some defend strict neutrality as protecting minorities from majoritarian intervention; others argue it abandons vulnerable intra-community members.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, secular_civil_society_organizations, beneficiary,
    organized, biographical, mobile, national).

% Internal reformers within religious communities (e.g., Muslim women's groups seeking codified personal law, anti-caste Hindu reformers) are structurally excluded from the state's protective capacity. The constraint treats their reform demands as violations of community autonomy. They would argue for state partnership but are denied standing.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, reformist_religious_actors, excluded,
    moderate, biographical, constrained, national).

% UN treaty bodies and special rapporteurs monitor compliance with CEDAW, ICCPR, and other instruments. They consistently flag that strict neutrality enables violations of women's rights and minority rights within communities. Their observations carry moral weight but no enforcement power domestically.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, international_human_rights_bodies, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the state from becoming an instrument of any single religious tradition; provides a shared procedural framework for a pluralistic society to coexist without theological civil war.
% TRANSFER_FUNCTION: Transfers the capacity for religious reform from the state (which could legislate progressive reforms) to religious communities (which control their own internal norms). The cost falls on intra-community marginalized groups who lose state protection; the benefit accrues to community gatekeepers and majority communities whose norms face no challenge.
% ABSENT_VOICES: Intra-community marginalized groups (women, LGBTQ+, lower-caste members) are structurally excluded from the constitutional conversation about secularism. Their interests are represented neither by the state (which refuses intervention) nor by community leadership (which claims to speak for the whole). International human rights bodies observe but cannot participate in domestic adjudication.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished overnight, the state would immediately face demands for intervention in minority personal laws (from reformists) and demands for protection of majority norms (from majoritarian forces). The constitutional settlement would collapse into contested legislative battles over religious reform, fundamentally rearranging the religion-state relationship.
% FOUNDING_PROBLEM: Post-colonial constitutional framers sought to prevent the new state from replicating colonial 'divide and rule' policies that instrumentalized religious difference. The founding problem was: how to assure minorities that the Hindu-majority state would not interfere in their religious affairs, while preventing any religious community from capturing state power.
% FOUNDING_PROBLEM_CORROBORATION: The Constituent Assembly debates record the founding problem as minority assurance against majoritarian interference (corroborated by Granville Austin, 'The Indian Constitution: Cornerstone of a Nation'). Minority representatives explicitly demanded non-interference guarantees. However, feminist and Dalit scholars (e.g., Flavia Agnes, Marc Galanter) attest that the founding problem ignored intra-community hierarchy, making the 'solution' a new form of entrapment for the most vulnerable.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects the transfer of reform capacity from state to communities, with costs concentrated on intra-community marginalized groups. Suppression (0.35) is moderate: the constraint operates through judicial doctrine foreclosing legislative intervention, not direct coercion, but the 'essential practices' test creates a suppression mechanism by letting courts define religion's boundaries. Theater ratio (0.22) is low-moderate: the principle is genuinely operationalized in case law, but the rhetoric of 'protecting minorities' increasingly masks abandonment of intra-community vulnerable members. Accessibility collapse (0.58) reflects that principled intervention alternatives exist but are legally foreclosed. Resistance (0.52) captures ongoing contestation from reformist, feminist, and Dalit perspectives.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, strict neutrality is a coordination achievement preventing religious civil war. From minority community leadership's seat, it is autonomy protection against majoritarian reform. From intra-community marginalized groups' seat, it is abandonment — the state refuses to protect them from their own community's oppression. From the majority community's seat, it is a favorable settlement locking in their normative dominance. The engine computes these divergent seat classifications from the authored power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The state (agenda_setter) sits near symmetric: it both administers the constraint and bears political costs of non-intervention. Religious majorities are beneficiaries (d ~0.2): their norms become the unmarked baseline. Minority communities are payers (d ~0.7): they lose protective state capacity, and exit is identity-locked (communal identity fused with religious law). Intra-community marginalized groups are full targets (d ~0.9): trapped, powerless, bearing concentrated costs. Secular civil society splits: formal equality advocates near symmetric; feminist/Dalit advocates experience the constraint as extraction. The engine computes this divergence from structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (minority assurance against majoritarian interference) remains live but has mutated: majoritarian forces now weaponize 'neutrality' to block reform while imposing majoritarian norms through other channels (cow protection, anti-conversion laws). The constraint persists because no coalition can agree on a replacement — minorities fear intervention, majorities fear loss of privilege, reformists lack power. This is not pure mandatrophy (the coordination function is real) but a tangled rope where the extraction component has grown as majoritarian norms consolidate under the cover of neutrality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_vs_abandonment_ambiguity,
    'Is the strict neutrality principle a genuine coordination mechanism preventing majoritarian capture, or a constructed constraint that abandons intra-community vulnerable groups while locking in majority normative dominance?',
    'Counterfactual analysis: if the state adopted principled intervention, would minority communities experience net gain or loss in autonomy and rights? Comparative study of jurisdictions with interventionist vs. neutral secularism models (e.g., France vs. India vs. USA) on outcomes for intra-community marginalized groups.',
    'If abandonment, the constraint reclassifies toward snare for intra-community marginalized seats; if genuine coordination with manageable costs, it remains tangled_rope. The FSM candidate check applies: a mountain claim of ''constitutional necessity'' with identifiable beneficiaries (majorities) and victims (intra-community marginalized) triggers false_summit_mountain evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_vs_abandonment_ambiguity, conceptual, 'Whether strict neutrality is a natural constitutional necessity or a constructed arrangement benefiting majority communities.').

omega_variable(
    suppression_mechanism_judicial_vs_legislative,
    'Is the constraint''s suppression primarily judicial (courts defining ''essential practices'' to foreclose legislation) or legislative (parliament refusing to enact reform), and does the mechanism shift over time?',
    'Doctrinal timeline analysis: track Supreme Court ''essential religious practices'' test evolution (Shirur Mutt 1954 → Sabarimala 2018) alongside legislative inaction on personal law reform. Identify whether suppression migrates from judicial door-closing to legislative refusal.',
    'If judicial suppression dominates early and legislative later, the constraint''s suppression profile changes character — judicial suppression is more rigid (harder to reverse); legislative suppression is more contingent (reversible by electoral change). Affects temporal drift modeling and mandatrophy assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_judicial_vs_legislative, empirical, 'Whether suppression operates through judicial doctrine or legislative inaction, and whether the mechanism shifts.').

omega_variable(
    majoritarian_capture_under_neutrality,
    'Does the strict neutrality framework structurally enable majoritarian norms to become the default ''secular'' baseline, effectively extracting from minorities while claiming equidistance?',
    'Empirical audit of personal law reform trajectories: compare state intervention in Hindu law (codified 1955-56) vs. Muslim/Christian/Parsi law (unreformed). Assess whether ''neutrality'' froze minority laws at colonial-era forms while majority law was modernized.',
    'If majoritarian capture is structural, the constraint''s extraction is asymmetric by design — the coordination function serves majority interests. This would push classification toward snare for minority seats and tangled_rope overall, with the ''coordination'' story exposed as cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(majoritarian_capture_under_neutrality, empirical, 'Whether neutrality''s uniform application produces asymmetric outcomes favoring the majority community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_sec_neutral_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(const_sec_neutral_tr_t15, constitutional_secularism__strict_neutrality_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(const_sec_neutral_tr_t30, constitutional_secularism__strict_neutrality_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(const_sec_neutral_tr_t45, constitutional_secularism__strict_neutrality_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement(const_sec_neutral_tr_t60, constitutional_secularism__strict_neutrality_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(const_sec_neutral_tr_t74, constitutional_secularism__strict_neutrality_reading, theater_ratio, 74, 0.22).

% Extraction over time
narrative_ontology:measurement(const_sec_neutral_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(const_sec_neutral_be_t15, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(const_sec_neutral_be_t30, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(const_sec_neutral_be_t45, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 45, 0.42).
narrative_ontology:measurement(const_sec_neutral_be_t60, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(const_sec_neutral_be_t74, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 74, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(const_sec_neutral_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(const_sec_neutral_su_t15, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 15, 0.25).
narrative_ontology:measurement(const_sec_neutral_su_t30, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(const_sec_neutral_su_t45, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 45, 0.32).
narrative_ontology:measurement(const_sec_neutral_su_t60, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement(const_sec_neutral_su_t74, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 74, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__strict_neutrality_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, personal_law_pluralism).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, uniform_civil_code_debate).

% DUAL FORMULATION NOTE:
% This constraint (strict_neutrality_reading) and its siblings (principled_intervention_reading, reformist_reading) form a constraint family decomposing the 'constitutional secularism' label. Each reading instantiates a different constraint with distinct ε, beneficiary/victim structures, and temporal dynamics. The strict neutrality reading has lower base extractiveness but higher accessibility_collapse for reform alternatives; the principled intervention reading has higher extractiveness (state intervention capacity) but lower suppression of reform; the reformist reading has highest extractiveness (affirmative duty) but lowest theater ratio (explicit normative commitment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__strict_neutrality_reading, moderate, 0.75).
constraint_indexing:directionality_override(constitutional_secularism__strict_neutrality_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
