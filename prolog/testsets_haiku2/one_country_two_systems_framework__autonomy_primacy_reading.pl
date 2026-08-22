% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country, Two Systems: Autonomy Primacy Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   One Country, Two Systems is a constitutional arrangement under which Hong
 *   Kong retained autonomy after 1997 handover. Under the
 *   autonomy_primacy_reading, Hong Kong's civil liberties, independent
 *   judiciary, and separation of powers are treaty-guaranteed constraints on
 *   mainland interference. The reading treats the Sino-British Joint
 *   Declaration as binding and treats judicial review as a check on executive
 *   overreach in both Hong Kong and mainland authority. This stands in direct
 *   tension with the sovereignty_primacy_reading (mainland authority is
 *   ultimate and revocable) and the balanced_coexistence_reading (neither
 *   autonomy nor sovereignty is absolute; negotiation replaces legal
 *   supremacy). The autonomy reading has experienced measurable erosion since
 *   2020 with national security law, judicial independence pressure, and
 *   electoral system changes that shift power from elected legislatures
 *   toward appointed bodies accountable to mainland.
 *
 * KEY AGENTS:
 *   - Hong Kong residents: organized collective depending on autonomy protections for civil liberties; constrained exit (identity-locked, material barriers)
 *   - Hong Kong judiciary: institutional beneficiary under autonomy reading; faces identity-locked constraint (institutional legitimacy fused with independence doctrine)
 *   - Mainland national security apparatus: payer under autonomy reading; retains override capacity at cost
 *   - Chief Executive: agenda-setter structurally trapped between mainland authority and resident expectations; identity-locked to both
 *   - Treaty framework (Sino-British Joint Declaration): non-agent entity grounding autonomy reading's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.38).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.62).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country, Two Systems: Autonomy Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '91e97994-eadd-4236-9784-4f90ac34e7c7').
narrative_ontology:cs_kernel_codification('91e97994-eadd-4236-9784-4f90ac34e7c7', fixed_text).
narrative_ontology:cs_authority_grounding('91e97994-eadd-4236-9784-4f90ac34e7c7', lineage).
narrative_ontology:cs_interpretation_layer_present('91e97994-eadd-4236-9784-4f90ac34e7c7').
narrative_ontology:cs_reading_relation('91e97994-eadd-4236-9784-4f90ac34e7c7', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('91e97994-eadd-4236-9784-4f90ac34e7c7', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('91e97994-eadd-4236-9784-4f90ac34e7c7', foundational, treaty_binds_mainland_authority).
narrative_ontology:cs_axiom_status(treaty_binds_mainland_authority, holdable).
narrative_ontology:cs_axiom_grounding('91e97994-eadd-4236-9784-4f90ac34e7c7', treaty_binds_mainland_authority, deontological).
narrative_ontology:cs_axiom('91e97994-eadd-4236-9784-4f90ac34e7c7', foundational, civil_liberties_are_substantive_rights).
narrative_ontology:cs_axiom_status(civil_liberties_are_substantive_rights, holdable).
narrative_ontology:cs_axiom_grounding('91e97994-eadd-4236-9784-4f90ac34e7c7', civil_liberties_are_substantive_rights, deontological).
narrative_ontology:cs_reference_frame('91e97994-eadd-4236-9784-4f90ac34e7c7', treaty_protected_autonomy_with_judicial_review).
narrative_ontology:cs_drift_state('91e97994-eadd-4236-9784-4f90ac34e7c7', contemporary_post_2020_national_security_law, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('91e97994-eadd-4236-9784-4f90ac34e7c7', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, independent_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, civil_liberties_framework).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, mainland_authority_coordination).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, national_security_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_legislature).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_legislature).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_chief_executive).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, mainland_national_security_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents benefit from the autonomy reading: retained common law judiciary, common law procedure, independent legal review of executive action, preservation of prior civil liberties regime (freedom of speech, assembly, movement, press). They depend on the constraint's legal enforcement to shield daily freedoms from mainland security law incursion. Exit is identity-locked: emigration breaks social/economic ties; those who stay depend entirely on the autonomy reading's enforceability.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    organized, generational, identity_locked, local).

% The judiciary is a structural beneficiary of the autonomy reading: it retains independent review power, common law authority, ability to strike down executive and legislative action as ultra vires or rights-violating. Under this reading, the Hong Kong Courts stand as co-equal authority, not subordinate to mainland legal hierarchy. Judicial independence doctrine gains force from treaty grounding; judges operate under the autonomy reading's legitimacy frame.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, beneficiary,
    institutional, generational, identity_locked, local).

% The legislature is constitutionally authorized to legislate on all matters except defense and foreign affairs under the autonomy reading. It benefits from retained lawmaking power and from the constraint's check on mainland encroachment via vague security law. However, it pays the constraint through reduced scope in security matters and through accountability to the rights-protecting judiciary.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_legislature, beneficiary,
    institutional, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_legislature, payer).

% The Chief Executive nominally sets Hong Kong's internal agenda but is structurally trapped: must operate within the autonomy reading's framework (independent judiciary, protected civil liberties, treaty constraints) while dependent on mainland authority for appointment confirmation and national security delegation. Bears the cost of maintaining the autonomy facade while constrained by both directions—mainland pressure from above, judicial constraints from below, resident expectations from the side.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_chief_executive, agenda_setter,
    institutional, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_chief_executive, payer).

% Under the autonomy reading, mainland national security authority is constrained to specifically designated areas (defense, foreign affairs, territorial integrity). The apparatus pays the constraint by accepting legal limitations on intervention in daily Hong Kong governance, judicial oversight, and civil liberties regulation. However, the apparatus has real power to override through reinterpretation or direct action at cost.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, mainland_national_security_apparatus, payer,
    institutional, civilizational, constrained, global).

% The international treaty registered with the UN Security Council stands as the text grounding the autonomy reading's legitimacy. It is a non-agent entity listed for completeness: international treaty commitment to autonomy protection, subject to interpretation but formally binding under international law.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, sino_british_joint_declaration, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(one_country_two_systems_framework__autonomy_primacy_reading, sino_british_joint_declaration).

% UN Human Rights Committee, international NGOs, and foreign governments monitor Hong Kong's civil liberties trajectory. They take testimony and issue reports that carry normative weight but no binding enforcement power. They validate or contest the autonomy reading's claims about treaty compliance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Mainland authority holds the ultimate power to reinterpret or override the autonomy reading through national security law, legislation, or direct intervention. Under this reading's framework, such action would constitute treaty violation and break the autonomy structure, but mainland leadership retains the raw power to do so at cost (international isolation, Hong Kong capital flight, credibility damage).
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, mainland_leadership, agenda_setter,
    institutional, civilizational, trapped, global).

% Democracy activists and universal-suffrage advocates are structurally excluded from the autonomy reading's core negotiation: the reading takes the separation-of-powers and civil-liberties framework as given, not the electoral legitimacy framework. They would argue that true autonomy requires democratic accountability; their exclusion is the reading's silence on how Hong Kong's leadership is chosen.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, democratic_reform_advocates, excluded,
    moderate, generational, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__autonomy_primacy_reading, mainland_national_security_apparatus).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: One Country, Two Systems coordinates two sovereignties over shared territory: the People's Republic retains ultimate national sovereignty (defense, foreign affairs, territorial integrity) while Hong Kong retains operational autonomy over internal governance, civil liberties, economic system, and judicial review. This solves the problem of governing a society with two legal systems, two economies, and two security postures under one flag without requiring either side to abandon core authority.
% TRANSFER_FUNCTION: Moves authority over Hong Kong's internal affairs to the Hong Kong judiciary and legislature (constrained by treaty); transfers defense and foreign affairs authority to mainland bodies; transfers cultural and economic legitimacy to Hong Kong's historical autonomy frame. The autonomy reading asserts that civil liberties protections flow FROM the treaty TO residents, not at mainland discretion.
% ABSENT_VOICES: Democratic reformers who see the autonomy reading as hollow without universal suffrage and fully accountable governance are structurally sidelined—the reading takes separation of powers and civil liberties as the axis, not electoral legitimacy. Mainland security specialists who view any Hong Kong autonomy as national security weakness are excluded from the autonomy reading's legitimacy frame. The reading's silence on how Hong Kong leadership is chosen (the Basic Law's indirect election plus appointment filter) papers over this gap. Those excluded would argue for either democratic accountability (from Hong Kong reformers) or unrestricted mainland authority (from mainland security doctrine).
% DISAPPEARANCE_RATIONALE: If the autonomy reading and its enforcement collapsed overnight, Hong Kong's judiciary would lose independent review power, civil liberties protections would depend entirely on mainland grace rather than treaty right, and residents' exit calculation would shift from 'constrained but protected' to 'trapped under mainland law.' The separation of two legal systems would dissolve, and Hong Kong would reorganize under mainland constitutional hierarchy. The reading's disappearance would trigger capital flight, professional emigration, and institutional restructuring. Hong Kong's international credibility and economic positioning would shift fundamentally.
% FOUNDING_PROBLEM: In 1997, Britain returned Hong Kong to China under an arrangement designed to preserve Hong Kong's economic system, legal structure, and civil liberties while allowing China to exercise territorial sovereignty. The founding problem was: how to transfer sovereignty without destroying the society's institutional fabric, economic competitiveness, or residents' confidence in legal protection?
% FOUNDING_PROBLEM_CORROBORATION: China's leadership attests the problem is solved via the autonomy arrangement itself. Hong Kong residents and judges increasingly attest the founding problem is LIVE but the autonomy reading is FAILING: judicial independence is eroding, civil liberties are constrained, mainland intervention is expanding beyond security law into routine governance. International human rights bodies attest to documented autonomy degradation (UN fact-finding mission 2023, Amnesty International reporting, Hong Kong Bar Association statements). The founding problem's status is not self-asserted by beneficiaries but corroborated by independent observers documenting its persistence.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).
:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the autonomy reading operates as a real constraint on mainland authority's day-to-day intervention in Hong Kong governance, judicial proceedings, and civil liberties. However, extractiveness is not low because mainland retains the ultimate power to override the framework at cost. Suppression is substantial (0.62) and rising because maintaining the autonomy reading requires suppressing mainland security apparatus overreach, suppressing democratic reform demands (which would require electoral changes the autonomy reading does not grant), and suppressing residents' exit options through identity-lock and material barriers. Theater ratio is moderate (0.48) and rising: increasingly, autonomy functions are maintained performatively (judges go through independence motions; legislature legislates on non-core matters; press operates within de facto red lines) while real power shifts to mainland-aligned bodies. The measurement series show gradual extraction accumulation and suppression intensification from 1997 (autonomy-maximalist, theater-minimal) through 2022 (autonomy-constrained, theater-substantial). This trajectory suggests the autonomy reading is eroding under continued mainland pressure, consistent with T17 abductive triggers (mountain_extraction_accumulation) that would flag the autonomy reading as a false summit if mainland continues to accumulate de facto control.
 *
 * PERSPECTIVAL GAP:
 *   From the Hong Kong judiciary and resident seats, the autonomy reading is a genuine constitutional constraint on mainland authority: judges rely on it to strike down executive action, residents rely on it to protect speech and assembly, and the reading's disappearance would trigger cascading institutional collapse. From the mainland national security apparatus seat, the autonomy reading is an operational convenience that can be overridden when security interests warrant: the apparatus recognizes the treaty but does not treat it as binding its own core authority. From the Chief Executive's seat, the autonomy reading is an identity-fusion trap: the seat requires defending autonomy to maintain Hong Kong legitimacy while remaining accountable to mainland authority that questions whether autonomy serves mainland interests. The engine computes these divergent directionalities: high d for mainland apparatus (near-full target of autonomy constraints); low d for residents (beneficiaries protected by the reading); split/dual for the Chief Executive (beneficiary of judicial constraint from below, payer of mainland constraint from above).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Hong Kong residents (protected civil liberties, exit-constrained so they need in-place protection), independent judiciary (retains review power, institutional independence), civil liberties framework itself (vindicated proposition). Victims: mainland national security apparatus (constrained from day-to-day Hong Kong governance intervention by treaty limits), mainland coordination interests (must negotiate with Hong Kong authority rather than directly impose). The Chief Executive is split: benefits from having a legitimate judicial framework that props up governance credibility; pays the cost of being trapped between two authorities with conflicting demands. The directionality derivation routes residents and judiciary toward d near 0.1-0.2 (beneficiaries); mainland apparatus toward d near 0.75-0.85 (target of autonomy constraint); Chief Executive toward d near 0.5 (symmetric cost/benefit). No overrides needed; the structural data produces accurate divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy reading's founding problem (how to transfer sovereignty without destroying institutional fabric and civil liberties) is LIVE and CONTESTED. The autonomy reading asserts that civil liberties and judicial independence SOLVE the founding problem by protecting residents' daily freedoms under the new sovereignty arrangement. However, the measurement series show suppression and theater rising while extractiveness creeps up: mainland authority has been accumulating de facto control over core security matters, electoral systems, and judicial appointment/discipline. This creates a mandatrophy signal: the autonomy reading's justification (protection of civil liberties and autonomy) persists as a stated mandate but the operational reality shows that mandate eroding. The autonomy reading prevents simple misclassification as 'rope' (which would require low suppression and high genuine coordination benefit) and as 'snare' (which would require dominant mainland beneficiary and resident victimhood). The tangled_rope classification captures the structure accurately: both coordination (division of sovereignty) and asymmetric extraction (mainland accumulating power) operate together, and suppression is required to maintain both. The mandatrophy is the rising theater ratio: increasingly, the autonomy reading is performed rather than operationalized, which signals late-stage constraint degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_enforcement_mechanism_ambiguity,
    'What entity or mechanism enforces the Sino-British Joint Declaration against violation? Is international pressure, Hong Kong''s judiciary, or threat of economic sanction sufficient to hold mainland authority to the treaty''s autonomy constraints?',
    'Test through actual breaches: if mainland enacts national security law that overrides Hong Kong judicial review, do international bodies impose costs sufficient to deter further encroachment, or does the treaty lack binding enforcement?',
    'If treaty enforcement is weak, the autonomy reading is a veto player without teeth: autonomy persists only as long as mainland chooses not to override it. The reading''s stability drops from institutional constraint to mainland grace, moving toward the sovereignty_primacy_reading structurally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_enforcement_mechanism_ambiguity, empirical, 'Whether treaty commitments carry sufficient enforcement to bind mainland authority.').

omega_variable(
    judicial_independence_operational_collapse,
    'Can Hong Kong''s judiciary maintain operational independence when judges fear career consequences, security law prosecution, or political pressure from mainland authority? At what threshold of pressure does judicial independence cease to function despite formal legal status?',
    'Track judicial decisions on cases implicating mainland interests; measure self-censorship among judges and bar; observe whether judges accept briefs challenging security law or mainland intervention.',
    'If judges withdraw from independence to avoid pressure, the autonomy reading collapses from the inside: the rights-protecting constraint cannot operate without judicial willingness to enforce it. The constraint downshifts from tangled_rope (coordination with asymmetry) to piton (performative maintenance of a dead judicial function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_operational_collapse, empirical, 'Whether judicial independence persists as operating institutional reality or becomes theatrical.').

omega_variable(
    autonomy_reading_vs_sovereignty_reading_foreclosure,
    'Do the autonomy_primacy_reading and sovereignty_primacy_reading logically foreclose one another, or can they coexist as different parties'' frameworks for interpreting the same treaty?',
    'Examine mainland official statements and constitutional doctrine: does the People''s Republic''s reading acknowledge autonomy constraints as binding on its own authority, or only as operational guidance revocable by central authority?',
    'If mainland doctrine never endorsed the autonomy reading as binding (only as operational convenience), the two readings coexist but the autonomy reading''s power derives from international willingness to enforce it, not from mutual constitutional agreement. This shifts the constraint from tangled_rope (both parties coordinate) toward snare (one party enforces an arrangement the other side rejects).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_reading_vs_sovereignty_reading_foreclosure, conceptual, 'Whether the autonomy and sovereignty readings are logically incompatible or merely held by different parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(one__tr_t0, observed).
narrative_ontology:measurement(one__tr_t5, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(one__tr_t5, observed).
narrative_ontology:measurement(one__tr_t10, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(one__tr_t10, observed).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(one__tr_t15, observed).
narrative_ontology:measurement(one__tr_t20, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(one__tr_t20, observed).
narrative_ontology:measurement(one__tr_t25, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(one__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(one__be_t0, observed).
narrative_ontology:measurement(one__be_t5, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 5, 0.27).
narrative_ontology:measurement_basis(one__be_t5, observed).
narrative_ontology:measurement(one__be_t10, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(one__be_t10, observed).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement_basis(one__be_t15, observed).
narrative_ontology:measurement(one__be_t20, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(one__be_t20, observed).
narrative_ontology:measurement(one__be_t25, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(one__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(one__su_t0, observed).
narrative_ontology:measurement(one__su_t5, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(one__su_t5, observed).
narrative_ontology:measurement(one__su_t10, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(one__su_t10, observed).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(one__su_t15, observed).
narrative_ontology:measurement(one__su_t20, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(one__su_t20, observed).
narrative_ontology:measurement(one__su_t25, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(one__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__autonomy_primacy_reading, 0.12).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_national_security_law__mainland_security_apparatus_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_electoral_system__appointed_legislature_dominance).

% DUAL FORMULATION NOTE:
% This constraint is part of the one_country_two_systems_framework kernel family. The autonomy_primacy_reading emphasizes treaty-binding autonomy protection and judicial review as checks on mainland intervention. It coexists with two sibling readings: the sovereignty_primacy_reading (mainland authority ultimate and revocable) and the balanced_coexistence_reading (neither absolute; negotiated accommodation). The ε values differ substantially across readings because they assess different referents under the same kernel: autonomy_primacy measures extraction from residents' civil liberties perspective (moderate, 0.38); sovereignty_primacy measures extraction from mainland national security perspective (low, 0.15 hypothetically); balanced_coexistence measures extraction from mutual accommodation perspective (low-moderate, 0.25 hypothetically). Each reading is a separate constraint with its own stakeholder structure, beneficiary/victim map, and classification. Network edges link them to document the constraint family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
