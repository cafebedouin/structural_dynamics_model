% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Universal Protective Floor in Armed Conflict (Common Article 3 plus Continuing Human Rights Law)
 *   domain: legal/international_humanitarian_law/armed_conflict
 *
 * SUMMARY:
 *   This file instantiates ONE reading — the universal rights reading — of
 *   the contested kernel 'who do Geneva protections cover in armed conflict.'
 *   Under this reading, protections attach to every person affected by armed
 *   conflict regardless of combatant status, with Common Article 3 and
 *   continuously applicable human rights law forming a single universal
 *   floor. The standing arrangement the story is about is the resulting
 *   restriction on state military operations: detention to a common standard,
 *   interrogation bounded, targeting reviewable, for every person in a
 *   state's hands. The state-centric and hybrid-proportionality readings are
 *   separate constraints in separate files; they appear here only as routed
 *   committer structure (omega variables, kernel_context, reading_relations),
 *   never inside this constraint's own classification. Claim and metrics are
 *   authored independently: the claim is tangled_rope; the metrics describe
 *   the arrangement's actual operation from the compiled structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.61).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.43).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.43).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Universal Protective Floor in Armed Conflict (Common Article 3 plus Continuing Human Rights Law)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "legal/international_humanitarian_law/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '69b05900-86c5-4118-a681-20b583108985').
narrative_ontology:cs_kernel_codification('69b05900-86c5-4118-a681-20b583108985', fixed_text).
narrative_ontology:cs_authority_grounding('69b05900-86c5-4118-a681-20b583108985', lineage).
narrative_ontology:cs_interpretation_layer_present('69b05900-86c5-4118-a681-20b583108985').
narrative_ontology:cs_reading_relation('69b05900-86c5-4118-a681-20b583108985', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('69b05900-86c5-4118-a681-20b583108985', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('69b05900-86c5-4118-a681-20b583108985', foundational, protection_attaches_to_personhood_not_status).
narrative_ontology:cs_axiom_status(protection_attaches_to_personhood_not_status, holdable).
narrative_ontology:cs_axiom_grounding('69b05900-86c5-4118-a681-20b583108985', protection_attaches_to_personhood_not_status, deontological).
narrative_ontology:cs_axiom('69b05900-86c5-4118-a681-20b583108985', foundational, common_article_three_is_customary_floor).
narrative_ontology:cs_axiom_status(common_article_three_is_customary_floor, holdable).
narrative_ontology:cs_axiom_grounding('69b05900-86c5-4118-a681-20b583108985', common_article_three_is_customary_floor, conventional).
narrative_ontology:cs_reference_frame('69b05900-86c5-4118-a681-20b583108985', status_blind_universal_floor).
narrative_ontology:cs_drift_state('69b05900-86c5-4118-a681-20b583108985', contemporary_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69b05900-86c5-4118-a681-20b583108985', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, captured_detainees_all_categories).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_armed_forces).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, military_intelligence_and_interrogation_units).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, common_article_3_minimum_standard_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, continuing_applicability_of_human_rights_law_in_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Plan and execute military operations under rules requiring humane treatment of every person they detain or capture, whatever affiliation the person carries. They must run detention facilities to a common standard, limit interrogation methods, accept review of targeting decisions, and answer in domestic and international forums when treatment falls short. Leaving the arrangement would mean repudiating treaties their own doctrine, training curricula, and alliance commitments are built on.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_armed_forces, payer,
    institutional, generational, constrained, global).

% Conduct questioning of captured persons under boundaries that do not vary with the detainee's status: coercive techniques usable against unlawful fighters under other readings are off-limits here, and treatment records are discoverable in litigation, inquiries, and treaty-body review. Their tradecraft is shaped by the floor even when the adversary observes no reciprocal limits.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, military_intelligence_and_interrogation_units, payer,
    powerful, biographical, constrained, national).

% Fight outside any uniformed structure for territory or cause. When their fighters fall into state hands they receive the same detention protections as privileged prisoners, without having worn insignia or carried arms openly. The same rules nominally bind how they treat their own captives, but almost no tribunal reaches them, so the obligation rarely bites. They cannot sign or renounce the conventions; the rules arrive with the conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    organized, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, payer).

% Live where the fighting happens. The floor is what stands between them and summary execution, torture, starvation sieges, and arbitrary detention; humanitarian agencies cite it to negotiate access, and it is the hook on which evacuation, family-contact, and aid pipelines hang. They cannot move out of the war and cannot petition anyone to release them from its protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, generational, trapped, regional).

% Held by a party to the conflict — regular soldier, insurgent, intercepted civilian, or person whose status nobody has determined. The floor is what entitles them to registration, humane conditions, contact with families, and proceedings before a court offering real guarantees before any sentence. Their category is decided by their captor; their protection, under this reading, is not.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, captured_detainees_all_categories, beneficiary,
    powerless, biographical, trapped, regional).

% Negotiates access to detention facilities in every conflict it can reach, registers prisoners, relays family messages, and files confidential reports to detaining authorities. Its working method presupposes that whoever is held is owed the visit — it does not sort detainees by status before approaching the facility. Its leverage rests on confidentiality and on states valuing the relationship.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, icrc_protection_division, agenda_setter,
    institutional, generational, constrained, global).

% Prosecute individuals for mistreatment of protected persons; their caselaw, from the ad hoc tribunals through the permanent court, treats the threshold question of who counts as protected as largely settled in favor of broad coverage. Their dockets expand or contract with how widely the floor is read.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_tribunals, agenda_setter,
    institutional, generational, constrained, global).

% Serve in forces whose adversaries are often irregular groups. Their own safety in captivity depends on those groups honoring rules the groups rarely acknowledge and no court effectively enforces against them. They are not represented in the diplomatic conferences, treaty-body sessions, or scholarly debates where the scope of protection is argued; their interest — enforceable reciprocity — has no seat.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_service_members_at_risk_of_capture, excluded,
    moderate, biographical, trapped, global).

% Interpret human rights treaties as continuing to apply alongside the law of armed conflict, examine state reports on detention practices in war zones, and publish findings that reinforce the floor. They observe and opine; they hold no enforcement power of their own.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, un_human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__universal_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets one minimum standard of humane treatment that every party to any armed conflict owes every person in its hands, replacing a patchwork in which protections depended on conflict classification and combatant qualification. Any commander, detainee, or intermediary can invoke the same floor anywhere.
% TRANSFER_FUNCTION: Moves operational discretion from state military commands — choices about interrogation methods, detention regimes, and targeting of persons of ambiguous status — to the persons in their custody and battlespace, who acquire claims enforceable through courts, monitoring bodies, and diplomatic pressure. It also moves adjudication of wartime conduct from command discretion into legal forums.
% ABSENT_VOICES: State service members exposed to capture by irregular groups would object that the floor prices their safety as an unenforceable promise; victims of non-state group atrocities would object that protections flow to their attackers. Both stand outside the conference rooms and treaty-body sessions where the reading is elaborated.
% DISAPPEARANCE_RATIONALE: Detention regimes in every active conflict would re-sort around status determinations within months; interrogation boundaries would loosen wherever oversight is weakest; humanitarian access negotiations would lose their common reference point; and the several hundred thousand people currently held in connection with armed conflicts would hold whatever protections their captor's category scheme allowed.
% FOUNDING_PROBLEM: Through the interwar period and 1949 it became clear that protections written for uniformed armies of recognized states evaporated in civil wars, occupations, and partisan fighting — precisely the settings producing the worst captive mortality of the century. Common Article 3 was drafted to guarantee a minimum floor from 'each Party to the conflict,' whatever its character.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the United States Supreme Court in Hamdan v. Rumsfeld (2006) — an institution of the paying side — held Common Article 3 governs the conflict with al-Qaeda; successive UN commissions of inquiry document status-determination abuses in current conflicts; ICRC annual reports record access negotiations in dozens of non-international conflicts; and state military manuals across rival blocs incorporate the floor. No major party to any recent conflict asserts the founding problem is dead.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.61, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.61 reflects real, recurring costs to state operations — building and running detention to common standards, foreclosed interrogation techniques, litigation exposure — discounted by the reciprocity and legitimacy returns states draw from the same floor. Suppression 0.43: enforcement machinery (tribunals, universal jurisdiction, treaty-body scrutiny, conditionality) is real but selective and slow; nothing resembling comprehensive coercion holds the arrangement up. Theater 0.33: ICRC registration, visits, and family messaging are functional core activity, while a meaningful minority of compliance activity is report-writing and inquiry choreography. Accessibility_collapse 0.35: the competing readings remain live, ratification gaps and interpretive reservations persist, so alternatives have not closed. Resistance 0.60: sustained — status-determination regimes, reservations to Additional Protocol I, contested human-rights-in-war doctrine. All three tracked metrics share one time grid (1949/1977/1986/1998/2006/2025). Receipt surface: gain_flow is authored 'diffuse' after checking every seat — the arrangement's gains dissolve into protected status spread across detainee, civilian, and fighter classes; non-state armed groups take the most concentrated share per capita, but they are a small fraction of the protected population and convert no gain into private advantage, so no single seat captures. fixing_cost 'prohibitive': unwinding the floor would require overturning customary-law consensus, decades of integrated doctrine, and allied treaty commitments, against uncertain operational benefit.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently and do so from the same facts. From state armed forces and interrogation units the arrangement arrives as externally imposed restriction, litigated in forums they do not control. From detainees, civilians, and non-state fighters it arrives as the only enforceable shield in the battlespace. The ICRC experiences it as mandate; tribunals as docket; the excluded service-member seat experiences the reciprocity asymmetry — protection promised by parties no court can reach. The engine computes per-seat classifications from the structural data; this story's claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   State armed forces and military intelligence units are declared victims with constrained exit (doctrine, alliance, and customary-law lock-in), placing them near the full-target end. Non-state armed groups are declared beneficiaries with no exit at all — the rules arrive with the conflict — but their effective burden stays near the beneficiary end because enforcement against them is rare; the secondary payer role records the nominal obligation without moving their position far. Captured detainees and civilian populations sit nearest the beneficiary pole: maximal protection, zero reciprocal duty. The ICRC and tribunals administer rather than collect; their positions derive from administration, not from the beneficiary array. Global spatial scope amplifies effective extraction on the paying seats (verification across theaters is hard), while suppression stays unscaled as a raw structural property.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure coordination would bury the enforcement asymmetry that lets non-state fighters collect protections while owing duties no forum enforces; reading it as pure extraction would erase the floor that is, in fact, the last enforceable protection for most people in custody in most conflicts. The tangled_rope claim keeps both halves visible and lets the engine price the asymmetry per seat. The founding problem — status-blind floors for non-international conflict — is live in every current insurgency, so no obsolescence gap opens: founding_problem_status 'live' paired with disappearance_verdict 'world_rearranges' produces no mismatch flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the universal_rights_reading of the geneva_conventions_protective_scope kernel; what changes structurally if a sibling reading is adopted instead?',
    'Treaty practice, ICJ or national apex-court holdings, or a diplomatic conference adopting a different scope formula would signal sibling adoption; monitor status-determination doctrine and reservation behavior across state parties.',
    'Adopting the state-centric reading collapses the victim set to Article 4-qualified combatants — extraction on state operations drops sharply and non-state fighters lose coverage; adopting the hybrid reading makes the floor scale with conflict classification, lowering the burden in classified non-international conflicts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the protective-scope kernel; sibling adoption would rewrite the victim set and payer burden.').

omega_variable(
    reciprocity_offset_question,
    'Does the floor''s return flow to states — protection of their own captured personnel, reduced escalation, legitimacy — offset enough of the operational cost that the arrangement is nearer pure coordination than the tangled_rope claim?',
    'Comparative conflict study measuring reciprocity incidence: whether parties honoring the floor systematically receive better treatment for their own captives than parties that do not.',
    'If reciprocity reliably materializes, the measured extraction is largely coordination cost and the classification shifts toward rope; if it does not, the asymmetry is structural and the tangled_rope reading firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_offset_question, empirical, 'Whether state-side returns offset the operational extraction.').

omega_variable(
    enforcement_asymmetry_origin,
    'Is the asymmetry — robust enforcement against states, negligible enforcement against non-state armed groups — intrinsic to the arrangement or an artifact of current enforcement capacity?',
    'Track whether new enforcement channels aimed at non-state groups (sanctions regimes keyed to detainee treatment, specialized tribunals) materially close the gap; compare eras with and without such channels.',
    'If intrinsic, the tangled_rope classification stabilizes; if a capacity artifact, maturing enforcement pushes the arrangement toward rope as the nominal symmetry becomes real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_origin, empirical, 'Origin of the state/non-state enforcement asymmetry.').

omega_variable(
    customary_vs_treaty_basis,
    'Is the universal floor now customary international law binding all parties regardless of consent, or does it still rest on treaty obligation that parties could in principle shed?',
    'Survey state practice and opinio juris — military manuals, official statements, voting patterns — as courts and the ICRC already compile them; watch for any state asserting a right of unilateral opt-out from the core floor.',
    'Customary status removes the last formal exit and raises effective suppression; treaty-only status preserves a constrained-exit channel and caps how far the arrangement can tighten.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_vs_treaty_basis, empirical, 'Legal basis of the floor: custom (no exit) versus treaty (constrained exit).').

omega_variable(
    ihrl_applicability_in_war,
    'Does international human rights law actually continue to govern state conduct during armed conflict (the second pillar of this reading), or does the law of armed conflict displace it as the more specific regime?',
    'Apex-court and treaty-body jurisprudence convergence: whether detention, interrogation, and lethal-force decisions in conflicts are reviewed under human rights standards or exclusively under conduct-of-hostilities rules.',
    'If human rights law is displaced, the floor rests on Common Article 3 alone — thinner coverage, lower extraction on state operations, and the reading loses half its textual foundation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ihrl_applicability_in_war, conceptual, 'Viability of the human-rights pillar of the universal floor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_universal_floor_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement_basis(geneva_universal_floor_tr_t1949, observed).
narrative_ontology:measurement(geneva_universal_floor_tr_t1977, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1977, 0.24).
narrative_ontology:measurement_basis(geneva_universal_floor_tr_t1977, observed).
narrative_ontology:measurement(geneva_universal_floor_tr_t1986, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1986, 0.27).
narrative_ontology:measurement_basis(geneva_universal_floor_tr_t1986, observed).
narrative_ontology:measurement(geneva_universal_floor_tr_t1998, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1998, 0.31).
narrative_ontology:measurement_basis(geneva_universal_floor_tr_t1998, observed).
narrative_ontology:measurement(geneva_universal_floor_tr_t2006, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2006, 0.36).
narrative_ontology:measurement_basis(geneva_universal_floor_tr_t2006, observed).
narrative_ontology:measurement(geneva_universal_floor_tr_t2025, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2025, 0.33).
narrative_ontology:measurement_basis(geneva_universal_floor_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(geneva_universal_floor_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.4).
narrative_ontology:measurement_basis(geneva_universal_floor_be_t1949, observed).
narrative_ontology:measurement(geneva_universal_floor_be_t1977, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1977, 0.48).
narrative_ontology:measurement_basis(geneva_universal_floor_be_t1977, observed).
narrative_ontology:measurement(geneva_universal_floor_be_t1986, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1986, 0.52).
narrative_ontology:measurement_basis(geneva_universal_floor_be_t1986, observed).
narrative_ontology:measurement(geneva_universal_floor_be_t1998, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1998, 0.57).
narrative_ontology:measurement_basis(geneva_universal_floor_be_t1998, observed).
narrative_ontology:measurement(geneva_universal_floor_be_t2006, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2006, 0.63).
narrative_ontology:measurement_basis(geneva_universal_floor_be_t2006, observed).
narrative_ontology:measurement(geneva_universal_floor_be_t2025, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2025, 0.61).
narrative_ontology:measurement_basis(geneva_universal_floor_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(geneva_universal_floor_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.15).
narrative_ontology:measurement_basis(geneva_universal_floor_su_t1949, observed).
narrative_ontology:measurement(geneva_universal_floor_su_t1977, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1977, 0.22).
narrative_ontology:measurement_basis(geneva_universal_floor_su_t1977, observed).
narrative_ontology:measurement(geneva_universal_floor_su_t1986, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1986, 0.28).
narrative_ontology:measurement_basis(geneva_universal_floor_su_t1986, observed).
narrative_ontology:measurement(geneva_universal_floor_su_t1998, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1998, 0.38).
narrative_ontology:measurement_basis(geneva_universal_floor_su_t1998, observed).
narrative_ontology:measurement(geneva_universal_floor_su_t2006, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2006, 0.45).
narrative_ontology:measurement_basis(geneva_universal_floor_su_t2006, observed).
narrative_ontology:measurement(geneva_universal_floor_su_t2025, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2025, 0.43).
narrative_ontology:measurement_basis(geneva_universal_floor_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'Geneva protections' covers three structurally distinct scope claims. This file authors the universal-rights member (status-blind floor; widest victim set; highest extraction on state operations). The state-centric member gates coverage on Article 4 criteria (narrowest victim set; lowest extraction on state operations; unprivileged belligerents unprotected). The hybrid member scales coverage by conflict classification (victim set varies by conflict type). The upstream universal claim influences the other two: once the floor is accepted as customary, the hybrid's scaling operates only above it and the state-centric reading's exclusions shrink to a contested residue. Each member carries its own epsilon, beneficiaries, and victims; none averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
