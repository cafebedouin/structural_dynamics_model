% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention as Sovereignty Floor (Restrictive Reading)
 *   domain: international_law/migration_governance
 *
 * SUMMARY:
 *   The Refugee Convention (1951) is a foundational text for international
 *   protection law. This constraint captures ONE READING of that text: the
 *   restrictive sovereignty reading, which interprets the Convention as
 *   setting a MINIMUM floor of protection while preserving maximum state
 *   discretion above it. Under this reading, 'well-founded fear' requires
 *   individualized government persecution (not non-state violence),
 *   'particular social group' is limited to immutable, state-visible
 *   characteristics (not gender or sexual orientation), and states may screen
 *   heavily, process offshore, and reject the vast majority of displacement
 *   applicants while remaining technically compliant with the Convention. The
 *   reading benefits wealthy destination states and origin states alike; it
 *   extracts from applicants fleeing non-state violence, generalized
 *   conflict, and gender-based persecution. The claim/metric gap is
 *   deliberate: this constraint is CLAIMED as tangled_rope (coordination +
 *   asymmetric extraction) and the authored metrics reflect high extraction
 *   and suppression, because the reading's operation extracts heavily from a
 *   narrow victim set while maintaining the legitimacy of a humanitarian
 *   framework.
 *
 * KEY AGENTS:
 *   - Nation-states (agenda-setters): interpret the Convention narrowly to maximize discretion; control definition of persecution and social group; screen at borders and offshore
 *   - Wealthy destination states (beneficiaries): maintain low asylum admission numbers while claiming Convention compliance; avoid domestic resource costs
 *   - Applicants fleeing non-state violence (payers): lack legal pathway; trapped in displacement or dangerous origin; excluded by the reading's narrow definition
 *   - Applicants fleeing generalized violence (payers): fail individualized persecution standard; ineligible for resettlement despite mortal danger
 *   - Applicants claiming gender-based or LGBTQ+ persecution (payers): excluded by narrow 'particular social group' definition; high evidentiary burden
 *   - Humanitarian organizations (excluded): document protection gaps but lack formal determination authority; treated as advocates rather than evidence sources
 *   - International human rights bodies (observers): issue advisory opinions diverging from the restrictive reading; limited enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.68).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.72).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention as Sovereignty Floor (Restrictive Reading)").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, 'bf984bfa-123c-4a7b-aaa0-920f0e8a943b').
narrative_ontology:cs_kernel_codification('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', fixed_text).
narrative_ontology:cs_authority_grounding('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', extraction).
narrative_ontology:cs_interpretation_layer_present('bf984bfa-123c-4a7b-aaa0-920f0e8a943b').
narrative_ontology:cs_reading_relation('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', refugee_convention_text__expansive_humanitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', foundational, convention_as_minimum_floor).
narrative_ontology:cs_axiom_status(convention_as_minimum_floor, holdable).
narrative_ontology:cs_axiom_grounding('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', convention_as_minimum_floor, deontological).
narrative_ontology:cs_axiom('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', foundational, sovereign_discretion_primacy).
narrative_ontology:cs_axiom_status(sovereign_discretion_primacy, holdable).
narrative_ontology:cs_axiom_grounding('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', sovereign_discretion_primacy, deontological).
narrative_ontology:cs_reference_frame('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', state_centric_protection_framework).
narrative_ontology:cs_drift_state('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', contemporary_climate_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bf984bfa-123c-4a7b-aaa0-920f0e8a943b', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, nation_states_exercising_discretion).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, wealthy_countries_controlling_admission).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, applicants_fleeing_non_state_persecution).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, applicants_fleeing_generalized_violence).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, applicants_claiming_social_group_persecution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, countries_of_origin).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, sovereign_state_border_control_doctrine).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, individual_culpability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States interpret the Convention as setting a MINIMUM protective floor, leaving them maximum discretion above it. They screen applicants for 'well-founded fear' defined as individualized government persecution; reject those fleeing non-state violence, gang activity, or generalized instability; and demand proof of state awareness of the persecution and inability/unwillingness to protect. They process asylum claims onshore and offshore, screen at borders, and deny entry to those failing the narrow criteria. Control the definition of 'particular social group,' limiting it to immutable characteristics (ethnicity, nationality) rather than gender, sexual orientation, or clan membership, provided state persecution of that group is demonstrable.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, nation_states_exercising_discretion, agenda_setter,
    institutional, generational, arbitrage, national).

% Wealthy OECD and regional powers (US, EU member states, Australia, Canada) use the restrictive reading to maintain low asylum admission numbers while claiming Convention compliance. The reading permits offshore processing, border pushback, and high evidentiary bars that exclude the bulk of global displacement (estimated 80%+ of applicants fleeing non-state violence). They capture the coordination benefit of a stable international order while minimizing domestic intake and resource costs.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, wealthy_countries_controlling_admission, beneficiary,
    institutional, generational, arbitrage, global).

% Persons fleeing gang violence, drug-cartel recruitment threats, domestic abuse, or kidnapping by armed non-state actors. The restrictive reading excludes them because the persecution is not government action and the fear is not 'well-founded' by individualized government culpability standards. They remain in danger zones or refugee camps, often for years, ineligible for resettlement in wealthy countries even when that is the only safe option. No legal pathway. No administrative remedy that changes the reading of the Convention.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, applicants_fleeing_non_state_persecution, payer,
    powerless, immediate, trapped, global).

% Persons in zones of active conflict, gang warfare, or state collapse where violence is indiscriminate and generalized. The restrictive reading requires individualized persecution (targeted because of a protected characteristic), not mere presence in a war zone or crime-ridden area. They are rejected at asylum screening because their fear cannot be proven to be of the state specifically or to target a protected group. Trapped in displacement without legal admission pathways.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, applicants_fleeing_generalized_violence, payer,
    powerless, immediate, trapped, global).

% Women fleeing gender-based violence; LGBTQ+ persons fleeing sexual orientation or gender identity persecution; persons from persecuted clans or castes. The restrictive reading narrows 'particular social group' to immutable, state-visible characteristics—ethnicity, nationality, religion—with documented state persecution. Gender, sexual orientation, clan membership, and caste are treated as mutable, private, or insufficiently state-recognized. These applicants are rejected or face extremely high evidentiary burdens. Their persecution may be severe and well-founded but falls outside the reading's definition of 'particular social group.'
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, applicants_claiming_social_group_persecution, payer,
    powerless, immediate, trapped, global).

% Sending states benefit from the restrictive reading by avoiding international criticism for persecuting refugees—their citizens are rejected for not meeting the individualized persecution bar, which de facto absolves the origin state of responsibility. They also retain labor and remittance flows from citizens who cannot exit, and avoid the diplomatic friction of being labeled persecutor states if the bar is pitched high enough that few can prove persecution.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, countries_of_origin, beneficiary,
    moderate, biographical, constrained, national).

% UNHCR, Amnesty International, Human Rights Watch, and legal aid groups document that the restrictive reading excludes millions of people in mortal danger. They advocate for broader protection, argue non-state violence and generalized armed conflict create protection needs, and push for gender-sensitive 'particular social group' definitions. They are excluded from the formal determination process and their data is often treated as advocacy rather than evidence of protection gaps.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, humanitarian_organizations, excluded,
    organized, biographical, constrained, global).

% The European Court of Human Rights, UN Human Rights Committee, and other treaty-monitoring bodies interpret the Convention in context of evolving human rights standards and receive complaints about protection denials. They issue advisory opinions and decisions that often diverge from the restrictive reading, but have limited enforcement power over state asylum practices. Their role is observational from the perspective of a state administering the restrictive reading, though they are the institutional challengers to it.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, international_courts_and_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, wealthy_countries_controlling_admission).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a baseline humanitarian protection framework for persons fleeing state persecution, creating predictable asylum categories (persecution based on race, religion, nationality, political opinion, or membership in a particular social group) so that states operate from a shared definition rather than ad-hoc or purely discretionary criteria.
% TRANSFER_FUNCTION: Moves the protection obligation from all states equally to the wealthy, well-resourced states that can afford screening and resettlement, while permitting poorer and origin states to limit asylum and the burden of displacement. The restrictive reading narrows the victim set eligible for this transfer, concentrating protection resources on the smallest, most documentable persecution cases and excluding those fleeing non-state violence or generalized armed conflict.
% ABSENT_VOICES: Persons actually fleeing non-state violence, generalized violence, and gender-based persecution have no seat in the interpretation of the Convention that governs their eligibility. Humanitarian organizations document their cases but are structurally excluded from asylum determinations. The reading itself was developed in states that did not face high non-state violence or internally displaced populations fleeing such violence.
% DISAPPEARANCE_RATIONALE: If the restrictive sovereignty reading vanished—i.e., if states adopted the expansive humanitarian reading—protection admission would rise sharply (humanitarian estimates suggest 2–3× current volume), resettlement costs would reallocate from origin countries to wealthy states, and labor migration patterns would shift as persecution-fleers gained legal pathways. International relations would face new pressure on burden-sharing. The current reading's disappearance would rearrange the distribution of displacement costs.
% FOUNDING_PROBLEM: The post-WWII refugee crisis: states needed a shared definition of who was a refugee to manage massive displacement from Nazi persecution, Soviet expansion, and decolonization. The 1951 Convention codified protection for those fleeing state persecution to end ad-hoc admissions and create predictability.
% FOUNDING_PROBLEM_CORROBORATION: States argue the founding problem (state persecution causing forced migration) remains live. Humanitarian organizations and legal scholars argue the founding problem has evolved: the majority of forced displacement today is caused by non-state violence, generalized armed conflict, and climate-driven scarcity, not individual government persecution. UNHCR data shows 89% of the world's displaced population are in developing countries, and the largest displacement causes are armed conflict (not state persecution) and gang/criminal violence. No consensus exists on whether the Convention's founding problem persists in its original form or has been superseded by different displacement drivers.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects that the reading systematically excludes millions in mortal danger who have no recourse. The extraction is not crude exclusion of all asylum; rather, it is a narrow channeling of protection to small, documentable persecution cases where states can maintain full discretion. Suppression (0.72) is high because the reading suppresses alternative framings—humanitarian organizations' data, non-state persecution realities, generalized conflict dynamics—through definitional gates ('well-founded fear,' 'particular social group') that are presented as legally neutral but function as barriers. Theater_ratio (0.41) reflects that the restrictive reading maintains a public posture of humanitarian compliance while operationally excluding the majority of protection-seekers; the gap between rhetorical compliance and actual protection narrows over the interval (0.28→0.41) as states become more explicit about discretionary screening. Accessibility_collapse (0.63) is moderate: applicants fleeing non-state violence retain the theoretical option of claiming persecution, but the high evidentiary bar and narrow definitions collapse their practical alternatives. Resistance (0.58) indicates pushback from humanitarian organizations, legal scholars, and international courts, but states retain the definitional power to deflect this resistance by framing it as activist overreach. The measurement series track intensifying screening practices and rising theater as states become more explicit about the discretionary reading while maintaining formal Convention compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the state agenda-setter's position, the reading is legitimate sovereignty protection while respecting the Convention floor. From the applicant's position (trapped, immediate time horizon), the same reading is a definitional closure that excludes life-threatening persecution. The engine computes this perspectival divergence: an institutional actor with arbitrage options (state) and a powerless actor with trapped exit (applicant) should show radically different directional measures. States compute high beneficiary directionality (low d); applicants compute high target directionality (high d). The reading's operation creates this asymmetry by design.
 *
 * DIRECTIONALITY LOGIC:
 *   Nation-states benefit from discretionary control and low admission numbers; their directionality should be near beneficiary (d ~0.1–0.2). Wealthy destination states benefit from burden-avoidance; their d should be similarly low. Applicants fleeing non-state violence are systematically excluded by definition; their d should be near target (d ~0.85–0.95). The restrictive reading operates by establishing high evidentiary bars that only states can set and interpret, leaving applicants no mechanism to contest the definition. Humanitarian organizations are excluded from formal determination, so while they resist, they lack institutional power to change the directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading faces a foundational mandatrophy question: has the founding problem (state persecution causing mass refugee flows) been superseded by different displacement drivers (non-state violence, generalized armed conflict, climate scarcity)? If the founding problem is dead (status=dead) but the arrangement persists (disappearance_verdict=world_rearranges), the reading is a zombie constraint—maintaining institutional legitimacy by policing the definition of 'refugee' rather than responding to actual protection needs. The authored measurement series show extractiveness and suppression rising then plateauing (t=20+), which is consistent with mandatrophy: the constraint hits an operational ceiling once the definitional gates are fully enforced; further extraction requires changing the definition itself, which would break the legitimacy claim. The high theater_ratio at the end (0.41) also signals mandatrophy—states are increasingly transparent about discretionary screening, admitting the Convention is a floor, not a mandate. A reading that was truly coordinate would not need rising theater to explain its operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (state persecution causing refugee flows) been obsoleted by post-Cold-War displacement drivers (non-state violence, generalized armed conflict, climate-driven displacement)?',
    'Comparative historical analysis: what share of forced displacement worldwide is caused by individualized state persecution versus non-state violence, armed conflict, and climate scarcity? UNHCR data (since 2006) permits quantification.',
    'If the founding problem is substantially dead, the reading is mandatrophy (zombie constraint): maintaining institutional legitimacy through definitional policing rather than responding to actual protection needs. If the founding problem remains live, the reading is coordinate: narrowly protecting the specific displacement type the Convention was designed for.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the Convention''s founding problem persists in its original form or has been superseded by different displacement drivers.').

omega_variable(
    non_state_persecution_logic,
    'Is the exclusion of non-state persecution logically necessary to maintain state sovereignty, or is it an arbitrary historical choice?',
    'Comparative law: do other human rights treaties (torture, slavery, discrimination conventions) exclude non-state perpetrators? Do other humanitarian frameworks protect persons fleeing non-state violence? If the answer is no, the restriction is choice; if yes, the restriction is structural.',
    'If arbitrary, the reading is imposing an unnecessary closure on the victim set and extracting without structural justification. If structural (state sovereignty requires state-action requirement), the reading is coordinate around sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_persecution_logic, conceptual, 'Whether non-state persecution exclusion is logically necessary or strategically chosen.').

omega_variable(
    social_group_definition_contest,
    'Does the narrow ''particular social group'' definition (immutable characteristics only) capture the groups the Convention intended to protect, or does it exclude persecution based on gender, sexual orientation, and clan membership that is equally systematic?',
    'Textual analysis: what did the drafters intend by ''particular social group'' in 1951? Historical record and subsequent jurisprudence from international courts (ECHR, international tribunals) show divergence. Empirical: what share of persecution claims worldwide are based on gender or sexual orientation? If high and excluded, the definition is restrictive choice.',
    'If the narrow definition conflicts with current evidence of persecution patterns, the reading is extractive by exclusion—systematically denying protection to documented persecution groups. If the definition aligns with drafting intent, the reading is coordinate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_group_definition_contest, empirical, 'Whether the narrow ''particular social group'' definition aligns with persecution realities and drafting intent.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (external barriers: evidentiary standards, border enforcement, offshore processing) or internalized (applicants accept the restrictive reading''s legitimacy and cease claiming non-state persecution)?',
    'Post-exclusion trajectory: do applicants rejected under the restrictive reading accept the outcome as legitimate, or do they continue to assert protection needs through other channels (domestic courts, humanitarian claim pathways, human trafficking, irregular migration)? High post-exclusion continuation suggests internalized suppression is low; applicants have not accepted the reading''s legitimacy.',
    'If suppression is structural only, alternative pathways (legal challenge, humanitarian reframing) remain theoretically available. If internalized, applicants carry the suppression with them: they cease advocating for alternative readings and become trapped by the reading''s definition. The reading''s persistence then depends on structural suppression alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Extent to which suppression is structural enforcement versus internalized acceptance of the restrictive reading''s legitimacy.').

omega_variable(
    kernel_reading_contention_locus,
    'Where exactly do the three kernel readings (restrictive, expansive, procedural) diverge in their interpretation of the Convention text, and is the divergence resolvable from the text or does it require committer-level choice?',
    'Close textual analysis and jurisprudential mapping: which specific Convention clauses are read differently by each reading? (E.g., ''well-founded fear'' is the primary locus.) Can one reading be said to be correct based on text alone, or does each reading require a normative choice about sovereignty, humanitarian protection, or procedural fairness?',
    'If the divergence is resolvable from text, one reading may be more ''legally correct'' and the restrictive reading may be flagged as a misreading. If resolvable only through committer choice, the readings are incommensurable and the contest is political, not legal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contention_locus, conceptual, 'Whether the kernel readings'' divergence is textually resolvable or requires committer-level choice about sovereignty and protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(refu_tr_t0, observed).
narrative_ontology:measurement(refu_tr_t5, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(refu_tr_t5, observed).
narrative_ontology:measurement(refu_tr_t10, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(refu_tr_t10, observed).
narrative_ontology:measurement(refu_tr_t15, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(refu_tr_t15, observed).
narrative_ontology:measurement(refu_tr_t20, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(refu_tr_t20, observed).
narrative_ontology:measurement(refu_tr_t25, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(refu_tr_t25, observed).
narrative_ontology:measurement(refu_tr_t30, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(refu_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(refu_be_t0, observed).
narrative_ontology:measurement(refu_be_t5, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(refu_be_t5, observed).
narrative_ontology:measurement(refu_be_t10, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(refu_be_t10, observed).
narrative_ontology:measurement(refu_be_t15, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(refu_be_t15, observed).
narrative_ontology:measurement(refu_be_t20, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(refu_be_t20, observed).
narrative_ontology:measurement(refu_be_t25, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(refu_be_t25, observed).
narrative_ontology:measurement(refu_be_t30, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(refu_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(refu_su_t0, observed).
narrative_ontology:measurement(refu_su_t5, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement_basis(refu_su_t5, observed).
narrative_ontology:measurement(refu_su_t10, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(refu_su_t10, observed).
narrative_ontology:measurement(refu_su_t15, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(refu_su_t15, observed).
narrative_ontology:measurement(refu_su_t20, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(refu_su_t20, observed).
narrative_ontology:measurement(refu_su_t25, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(refu_su_t25, observed).
narrative_ontology:measurement(refu_su_t30, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(refu_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__restrictive_sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% The refugee_convention_text kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of the 1951 Convention. The restrictive_sovereignty_reading (this file) interprets the Convention as a minimum floor permitting maximum state discretion; the expansive_humanitarian_reading interprets it as an unbendable humanitarian mandate; the procedural_integrity_reading prioritizes fair process over discretionary outcome. These are not the same constraint measured from different angles—they have different ε values, different victim sets, different institutional beneficiaries, and different empirical persistence conditions. The three readings are linked by shared textual kernel (the Convention itself) but stratified by reading choice (committer axis). Each story carries its own cs_structure.reading_relations to declare how it relates to its siblings: the restrictive reading forecloses the expansive reading (logically contradictory premises) but coexists with the procedural reading (both emphasize choice but differ on what choice is about). All three files must be present in the corpus for the kernel contest to be properly represented.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
