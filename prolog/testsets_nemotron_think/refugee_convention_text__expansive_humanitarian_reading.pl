% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Refugee Convention — Expansive Humanitarian Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The 1951 Refugee Convention and 1967 Protocol constitute a kernel text.
 *   The expansive humanitarian reading interprets the Convention as an
 *   unbendable humanitarian mandate: 'well-founded fear' includes generalized
 *   violence and non-state persecution; 'particular social group' encompasses
 *   gender, LGBTQ+, and clan-based persecution; interdiction and offshore
 *   processing violate non-refoulement; states have a duty to assess all
 *   claims substantively. This reading has driven the evolution of
 *   international protection through UNHCR guidelines and human rights court
 *   jurisprudence. It claims the Convention's object and purpose require
 *   broad protection. The structural reality: states of asylum bear
 *   escalating costs, restrictionist states resist through deterrence and
 *   externalization, and the coordination function (burden-sharing) remains
 *   underdeveloped. The claimed type is tangled_rope — genuine coordination
 *   (non-refoulement floor, UNHCR supervision) plus asymmetric extraction
 *   (states pay, asylum seekers benefit, restrictionist states are coerced
 *   into compliance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.62).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.48).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention — Expansive Humanitarian Reading").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0').
narrative_ontology:cs_kernel_codification('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', fixed_text).
narrative_ontology:cs_authority_grounding('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', lineage).
narrative_ontology:cs_interpretation_layer_present('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0').
narrative_ontology:cs_reading_relation('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', refugee_convention_text__restrictive_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', foundational, convention_object_purpose_requires_broad_protection).
narrative_ontology:cs_axiom_status(convention_object_purpose_requires_broad_protection, holdable).
narrative_ontology:cs_axiom_grounding('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', convention_object_purpose_requires_broad_protection, deontological).
narrative_ontology:cs_axiom('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', foundational, non_refoulement_applies_extraterritorially).
narrative_ontology:cs_axiom_status(non_refoulement_applies_extraterritorially, holdable).
narrative_ontology:cs_axiom_grounding('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', non_refoulement_applies_extraterritorially, empirically_contingent).
narrative_ontology:cs_axiom('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', secondary, particular_social_group_social_perception_test).
narrative_ontology:cs_axiom_status(particular_social_group_social_perception_test, holdable).
narrative_ontology:cs_axiom_grounding('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', particular_social_group_social_perception_test, conventional).
narrative_ontology:cs_reference_frame('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', convention_object_purpose_humanitarian_protection).
narrative_ontology:cs_drift_state('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', contemporary_externalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('94d8e90b-8756-4f76-a0e7-aa6a1ca57cb0', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_generalized_violence).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_non_state_persecution).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_gender).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_lgbtq).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_clan).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, unhcr_international_protection_mandate).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, states_of_asylum_burdened).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, states_of_origin_non_cooperative).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, states_interdicting_offshore).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, humanitarian_protection_primacy_over_sovereignty).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_as_jus_cogens).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, particular_social_group_evolutionary_interpretation).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, well_founded_fear_objective_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee war zones, collapsed states, or pervasive criminal violence without individualized persecution. Under this reading, they qualify for protection based on objective risk of serious harm. They have no exit from the harm they flee; the constraint is their only pathway to safety. Their claims are often rejected under restrictive readings.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_generalized_violence, beneficiary,
    powerless, biographical, trapped, global).

% Targeted by gangs, militias, traffickers, or family members where the state is unable or unwilling to protect. This reading treats non-state persecution as equivalent to state persecution for well-founded fear. They cannot access state protection in their home country; the Convention is their only recourse. Exit from the persecution is blocked by the non-state actor's control.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_non_state_persecution, beneficiary,
    powerless, biographical, trapped, global).

% Women and girls fleeing gender-based violence: honor killings, forced marriage, female genital mutilation, domestic violence as persecution. Gender is an immutable characteristic; exit from the identity is impossible. This reading recognizes gender as a particular social group. Their claims depend entirely on the expansive interpretation; restrictive readings exclude them.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_gender, beneficiary,
    powerless, biographical, identity_locked, global).

% LGBTQ+ individuals fleeing criminalization, violence, or death penalty for sexual orientation or gender identity. Sexual orientation and gender identity are immutable; exit from the identity is impossible. This reading recognizes LGBTQ+ as a particular social group. In restrictive jurisdictions, their claims are denied or they are returned to persecution.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_lgbtq, beneficiary,
    powerless, biographical, identity_locked, global).

% Members of clans, tribes, or kinship groups targeted in clan-based conflicts (e.g., Somalia, South Sudan). Clan membership is immutable and socially determinative; exit from the group is impossible. This reading recognizes clan as a particular social group. They face collective targeting that restrictive readings treat as generalized violence rather than persecution.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_clan, beneficiary,
    powerless, biographical, identity_locked, regional).

% States that receive large numbers of asylum claims under the expansive interpretation. They bear the costs of reception, adjudication, integration, and non-refoulement obligations. They cannot easily exit the Convention (treaty obligation, reputational cost). Some resist through deterrence policies, interdiction, offshore processing, or restrictive domestic legislation. Their exit is constrained by international law and domestic courts.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, states_of_asylum_burdened, payer,
    institutional, generational, constrained, national).

% States whose nationals flee in large numbers; they may benefit from remittances and reduced internal pressure but face diplomatic pressure to readmit returnees. They can exit cooperation with the regime (non-cooperation on readmission, refusing documentation). Some actively persecute the groups this reading protects. They hold agenda-setting power in their own territory but are payers in the international protection framework.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, states_of_origin_non_cooperative, payer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, states_of_origin_non_cooperative, agenda_setter).

% States that interdict migrants at sea, process claims offshore, or externalize borders (e.g., Australia's Pacific Solution, EU-Turkey deal, US Migrant Protection Protocols). They treat interdiction as consistent with the Convention; this reading treats it as refoulement violation. They bear costs of offshore arrangements but avoid onshore obligations. They have mobility to shift policies; their exit from the expansive reading's constraints is policy change, not treaty withdrawal.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, states_interdicting_offshore, payer,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, states_interdicting_offshore, agenda_setter).

% UNHCR supervises the Convention, issues guidelines (e.g., on particular social group, gender-related claims, non-state persecution), and advocates for expansive interpretation. It collects no rents but its institutional legitimacy depends on the Convention's vitality. It has analytical exit (can reform mandate) but is structurally bound to the treaty. It is both agenda-setter and beneficiary of the expansive reading's normative force.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr_international_protection_mandate, agenda_setter,
    institutional, generational, analytical, global).

% ECtHR, IACtHR, African Court, national supreme courts adjudicate Convention claims. They interpret the treaty authoritatively; their jurisprudence drives the expansive reading's evolution (e.g., ECtHR on non-refoulement extraterritorially, IACtHR on gender-based violence). They are observers in the sense of not bearing costs or collecting benefits, but their rulings are the enforcement machinery.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_courts_tribunals, observer,
    analytical, generational, analytical, global).

% Governments that reject the expansive reading and push for restrictive interpretation or treaty reform. They are excluded from the interpretive community that drives the expansive reading (UNHCR, human rights courts, NGOs). They would object to the broad victim set and the refoulement findings on interdiction. Their exclusion is structural: the interpretive community treats their views as non-compliant rather than as a competing legitimate reading.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, restrictionist_governments, excluded,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of international refugee protection: without a common framework, states would free-ride, refugees would be returned to persecution, and protection would be a lottery of geography. The Convention coordinates burden-sharing (imperfectly), sets a non-refoulement floor, and creates a supervisory role for UNHCR.
% TRANSFER_FUNCTION: Moves protection obligations and material costs from asylum seekers (who bear the harm of persecution) to states of asylum (who bear reception, adjudication, and integration costs). Also transfers sovereignty costs: states cede discretion over entry and removal to treaty obligations and supervisory bodies. Non-state persecutors are deprived of impunity when their acts trigger protection obligations.
% ABSENT_VOICES: Restrictionist governments that reject the expansive reading are structurally excluded from the interpretive community (UNHCR guidelines, human rights court jurisprudence, NGO advocacy networks) that treats their views as non-compliant. Also absent: potential asylum seekers who never reach territory due to interdiction — their voices are excluded by the very practices this reading condemns as refoulement.
% DISAPPEARANCE_RATIONALE: If the expansive reading disappeared overnight, states would revert to restrictive interpretations: generalized violence and non-state persecution would no longer qualify, gender/LGBTQ+/clan claims would be denied, interdiction and offshore processing would be normalized. Millions currently protected would lose status; the international protection framework would shrink to individualized state persecution only. Burden-sharing would collapse further as states compete to restrict.
% FOUNDING_PROBLEM: Post-WWII displacement revealed that states would not protect refugees voluntarily; the 1951 Convention created a minimal floor. The expansive reading emerged from the 1967 Protocol (removing temporal/geographic limits), UNHCR's supervisory evolution, and human rights court jurisprudence recognizing that the Convention's object and purpose — preventing refoulement to serious harm — requires interpretation that tracks evolving persecution patterns (gender, sexuality, non-state actors, generalized violence).
% FOUNDING_PROBLEM_CORROBORATION: UNHCR's 2002 Guidelines on International Protection (gender-related persecution, particular social group) and 2011 Guidelines (non-state persecution) attest the expansive reading from within the supervisory body. ECtHR jurisprudence (e.g., M.S.S. v. Belgium & Greece, Hirsi Jamaa v. Italy) and IACtHR advisory opinions corroborate from judicial seats outside the beneficiary set. Restrictionist states (Australia, Hungary, US under certain administrations) and the procedural_integrity_reading proponents contest that the founding problem has shifted to managed migration and that the expansive reading exceeds the Convention's text.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the asymmetric cost burden: states of asylum bear material and sovereignty costs that have grown as the protected population expands (generalized violence, non-state actors, social groups). Suppression (0.48) is moderate: the constraint relies on treaty obligation, supervisory pressure, and court rulings rather than overt coercion, but interdiction states face legal condemnation and reputational costs. Theater ratio (0.22) is low-moderate: the protection function is real, but a growing share of state activity is performative compliance (deterrence policies that maintain formal adherence while avoiding substantive obligations). Accessibility collapse (0.35) is modest: alternatives (restrictive readings, regional arrangements, non-entrée policies) persist and are actively pursued. Resistance (0.58) is significant: restrictionist states, interdiction practices, and the procedural_integrity_reading all contest the expansive interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat classifications from the structural data. From asylum seeker seats (powerless, trapped/identity_locked), the constraint should compute as rope or mountain (high benefit, no extraction, alternatives collapsed). From state of asylum seats (institutional, constrained), it should compute as snare or tangled_rope (high extraction, constrained exit, active resistance). From interdiction state seats (institutional, mobile), it may compute as snare (they resist, have policy mobility). From UNHCR seat (institutional, analytical), it should compute as rope (coordination function, low extraction). The divergence between seats IS the measurement — the expansive reading claims humanitarian mandate (rope/mountain) but operates as tangled_rope from the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum seekers (all categories) are structural beneficiaries: the constraint channels protection to them, they bear no costs, and their exit from persecution depends on it (d near 0.0). States of asylum are structural payers: they bear costs, have constrained exit (treaty withdrawal is costly), and face enforcement pressure (d near 1.0). States interdicting offshore are payers with agenda-setter capacity: they bear offshore costs but actively shape the constraint through policy innovation that tests its boundaries (d ~0.7). UNHCR is an agenda-setter with analytical exit: it drives the reading but its institutional survival depends on the Convention's vitality (d ~0.15). International courts are analytical observers (d=0.5). Restrictionist governments are excluded: they would be payers if the reading binds them, but they contest its validity (d undefined in derivation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII protection gap) is contested as live vs. shifted. The expansive reading claims the problem persists and has evolved (new persecution forms). Restrictionist states and the procedural_integrity_reading argue the problem has shifted to managed migration and the expansive reading exceeds the Convention's mandate. Mandatrophy is unresolved: the coordination function (burden-sharing) has atrophied while the extraction function (state obligations) has expanded. The constraint persists because no alternative framework exists and withdrawal costs are prohibitive for states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the expansive humanitarian reading a structurally distinct constraint from the restrictive_sovereignty_reading and procedural_integrity_reading, or a contestable interpretation of a single constraint?',
    'Apply the ε-invariance test: if the three readings author different extractiveness values for the same referent (the Convention''s standing arrangement), they are distinct constraints. This reading authors ε=0.62; the restrictive reading would author ε≈0.2 (minimal obligations); the procedural reading would author ε≈0.35 (process costs only). The divergence confirms distinct constraints.',
    'If distinct constraints, each gets its own classification and the kernel is a family linked by network.affects_constraints. If one constraint, the framework must model observable-dependent classification (which it rejects per DP-001).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three readings instantiate one constraint or three distinct constraints in the kernel family.').

omega_variable(
    burden_sharing_coordination_failure,
    'Does the Convention''s coordination function (burden-sharing) exist substantively, or is it a cover story for the extraction of protection costs onto frontline states?',
    'Measure actual burden-sharing outcomes: resettlement quotas, financial contributions, responsibility-sharing mechanisms. If outcomes track the expansive reading''s victim set expansion, coordination is real. If frontline states bear disproportionate costs while others free-ride, the coordination function is nominal.',
    'If coordination is nominal, the constraint is snare (extraction masquerading as coordination). If coordination is real but asymmetric, it remains tangled_rope. The engine''s Boltzmann coupling test will flag this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_sharing_coordination_failure, empirical, 'Whether the Convention''s burden-sharing coordination function is genuine or a cover for asymmetric extraction.').

omega_variable(
    non_refoulement_extraterritorial_scope,
    'Does non-refoulement apply extraterritorially to interdiction at sea and offshore processing, or only at the territorial border?',
    'Track jurisprudence: ECtHR (Hirsi Jamaa), IACtHR, UNHRC views, ICJ advisory proceedings. Convergence on extraterritorial application would establish the expansive reading''s structural claim; divergence would support the restrictive reading.',
    'If extraterritorial, interdiction states are payers under this reading (refoulement violation). If territorial only, interdiction states are not constrained by this reading''s refoulement prohibition — the victim set shrinks and extraction on those states drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_refoulement_extraterritorial_scope, empirical, 'The spatial scope of non-refoulement under the expansive reading — determines whether interdiction states are victims/payers.').

omega_variable(
    social_group_immutability_vs_social_perception,
    'Does ''particular social group'' require immutability (innate characteristics) or social perception (group recognized by society), and how does this affect the victim set?',
    'Compare UNHCR guidelines (social perception test) with restrictive state jurisprudence (immutability test). The expansive reading uses social perception, capturing gender, LGBTQ+, clan. The restrictive reading uses immutability + state awareness, excluding many social groups.',
    'If social perception prevails, the victim set is broad (this reading''s claim). If immutability prevails, the victim set narrows toward the restrictive reading. The epsilon value tracks the victim set breadth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_group_immutability_vs_social_perception, conceptual, 'The definitional boundary of ''particular social group'' — the core structural delta between expansive and restrictive readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t1967, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t1980, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t1990, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t2000, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t2010, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t2020, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t2025, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t1967, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t1980, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t1990, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t2000, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t2010, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t2020, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t2025, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t1967, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1967, 0.25).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t1980, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t1990, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t2000, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t2010, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t2020, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2020, 0.47).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t2025, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__expansive_humanitarian_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the refugee_convention_text kernel family (three readings). The expansive reading claims the Convention's object and purpose require broad protection; the restrictive reading claims sovereign discretion; the procedural reading claims process integrity. They share the same kernel text but instantiate different constraints with different ε, different victim/payer structures, and different classifications. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__expansive_humanitarian_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
