% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Hybrid Complementarity Jurisdiction
 *   domain: international law / treaty interpretation / institutional authority
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_complementarity_reading of the
 *   rome_statute_jurisdiction kernel. Under this reading, the Rome Statute
 *   establishes an international criminal court with residual universal
 *   authority that is operationally constrained by the complementarity
 *   principle, whereby the Court defers to national jurisdictions that are
 *   willing and able to prosecute core international crimes genuinely. The
 *   reading holds that jurisdiction exists by virtue of both treaty consent
 *   and universal legal aspiration, but enforcement depends on state
 *   cooperation and the admissibility apparatus. The constraint governs the
 *   allocation of criminal jurisdiction between national courts and the ICC,
 *   creating a hierarchy that protects sovereign primacy in the first
 *   instance while retaining an international backstop. Sibling readings
 *   include the sovereigntist_reading (strict consent-conditional framework)
 *   and the universalist_reading (universal mandate transcending consent).
 *
 * KEY AGENTS:
 *   - international_criminal_court: Primary agenda-setter (institutional/constrained) â adjudicates admissibility and operates the complementarity mechanism
 *   - state_parties: Primary beneficiaries (organized/constrained) â receive sovereignty protection through complementarity deference
 *   - non_party_states: Primary targets (organized/trapped) â bear sovereignty costs via UNSC referral without treaty consent
 *   - atrocity_victims_in_non_cooperating_states: Secondary targets (powerless/trapped) â bear justice-delay costs when complementarity shields national inaction
 *   - un_security_council: Secondary beneficiary (institutional/constrained) â offloads enforcement responsibility while retaining political gatekeeping
 *   - international_legal_community: Analytical observer (analytical/analytical) â evaluates whether the hybrid framework achieves its aims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.63).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.66).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Hybrid Complementarity Jurisdiction").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international law / treaty interpretation / institutional authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e').
narrative_ontology:cs_kernel_codification('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', fixed_text).
narrative_ontology:cs_authority_grounding('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', lineage).
narrative_ontology:cs_interpretation_layer_present('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e').
narrative_ontology:cs_reading_relation('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', foundational, complementarity_as_constitutive_legitimation).
narrative_ontology:cs_axiom_status(complementarity_as_constitutive_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', complementarity_as_constitutive_legitimation, conventional).
narrative_ontology:cs_axiom('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', foundational, universal_reach_as_natural_legal_principle).
narrative_ontology:cs_axiom_status(universal_reach_as_natural_legal_principle, holdable).
narrative_ontology:cs_axiom_grounding('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', universal_reach_as_natural_legal_principle, deontological).
narrative_ontology:cs_reference_frame('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', hybrid_universal_sovereign_framework).
narrative_ontology:cs_drift_state('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', contemporary_enforcement_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5a1a7b4e-33a3-45ed-8d69-d5a1c4c4347e', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_in_non_cooperating_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, un_security_council).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the complementarity assessment under Articles 17 and 20 of the Rome Statute, determining whether national courts are unwilling or unable to genuinely prosecute. Its functioning depends on state cooperation for arrests, evidence transfer, and enforcement of sentences. It interprets the statutory text to balance universal jurisdiction claims with deference to national proceedings.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% Ratified the Rome Statute and participate in the Assembly of States Parties. They receive the procedural right to primary jurisdiction under complementarity, meaning the ICC can only step in if they fail to act genuinely. They contribute funding and cooperate with the Court, but retain sovereignty over domestic prosecutions.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties, beneficiary,
    organized, generational, constrained, national).

% Have not consented to the Rome Statute but may be subjected to ICC jurisdiction through UN Security Council referral or ad hoc declarations. They bear the cost of international prosecutorial authority operating over their territory or nationals without their treaty consent.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_party_states, payer,
    organized, generational, trapped, national).

% Populations in conflict zones where national authorities claim jurisdiction but conduct sham or absent proceedings, invoking complementarity to block ICC intervention. They experience delayed or denied justice when the admissibility process prolongs impunity.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_in_non_cooperating_states, payer,
    powerless, biographical, trapped, local).

% Can refer situations to the ICC prosecutor under Article 13(b), activating jurisdiction over non-party states. It thereby offloads atrocity response to the ICC while retaining political control over which situations enter the system.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, un_security_council, beneficiary,
    institutional, immediate, constrained, global).

% Academic institutions, nongovernmental organizations, and practicing lawyers who monitor ICC jurisprudence, debate complementarity interpretations, and assess whether the hybrid framework achieves its stated aims.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_legal_community, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__hybrid_complementarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates jurisdiction between national criminal courts and the International Criminal Court for genocide, crimes against humanity, war crimes, and aggression, ensuring the most appropriate forum prosecutes while respecting state sovereignty and preventing impunity.
% TRANSFER_FUNCTION: Moves prosecutorial authority and legitimacy from non-cooperating states to the ICC via admissibility rulings; moves sovereignty protection to state parties through complementarity deference; moves enforcement responsibility from the UN Security Council to the ICC via referral.
% ABSENT_VOICES: Victims in situations where states invoke complementarity as a procedural shield; non-party populations subjected to ICC jurisdiction without consent; local justice practitioners advocating for traditional or community-based accountability mechanisms that the ICC framework does not recognize.
% DISAPPEARANCE_RATIONALE: If the hybrid complementarity framework vanished, the structured allocation of jurisdiction between national and international courts would collapse. State parties would lose the sovereignty-protective deferral mechanism; the ICC would lose its residual authority anchor; non-party states would face either pure UNSC ad hoc tribunals or absolute impunity; and victims would lose the backstop forum that exists when national systems fail.
% FOUNDING_PROBLEM: How to end impunity for core international crimes while respecting state sovereignty and avoiding the legitimacy deficits, cost, and selectivity of ad hoc international criminal tribunals like Nuremberg and the ICTY/ICTR.
% FOUNDING_PROBLEM_CORROBORATION: International criminal law scholars outside the ICC and state-party structures attest that ad hoc tribunals were unsustainable and the Rome Statute was designed to solve the sovereignty-impunity tension. Human rights organizations corroborate that impunity remains live in non-cooperating jurisdictions. Conversely, the African Union and certain state parties attest that the sovereignty cost now exceeds the impunity benefit, indicating contested rather than settled status.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.63, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.63) is substantial because the constraint systematically transfers prosecutorial authority and sovereignty costs from non-party states and victims to the ICC and state-party system. Suppression (0.66) is higher than extraction because the constraint's persistence depends on actively suppressing non-party objections, UNSC referral override of consent, and resistance from states that challenge ICC authority. Theater ratio (0.55) is elevated: the complementarity mechanism generates extensive admissibility proceedings and jurisdictional rhetoric that often outpaces actual enforcement, particularly where arrest and cooperation remain absent. Accessibility collapse (0.50) is moderate because ad hoc tribunals, hybrid courts, and national justice remain partial alternatives, though the Rome Statute has become the default international criminal law framework. Resistance (0.55) reflects active African Union opposition, US bilateral immunity agreements and ASPA sanctions, and Russian withdrawal from the Statute's signature.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (ICC) and the beneficiary seat (state parties) should compute as tangled_rope or rope: they experience the constraint as a necessary coordination mechanism that solves impunity-sovereignty tensions. The payer seats (non-party states and victims in non-cooperating jurisdictions) should compute closer to snare: they experience the same structure as coercive sovereignty extraction or justice denial. The engine derives this divergence from the structural asymmetry in exit options (trapped vs. constrained) and the beneficiary-victim split.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and the ICC are declared beneficiaries: state parties receive sovereignty protection and procedural primacy, while the ICC receives institutional mandate and residual authority. Non-party states and atrocity victims in non-cooperating states are declared payers: non-parties bear sovereignty intrusion via UNSC referral, and victims bear the cost of impunity prolonged by complementarity shields. The ICC sits at institutional power with constrained exit; victims sit at powerless with trapped exit. This asymmetry drives high effective extraction for the victim seat and moderate extraction for the non-party state seat, while dampening extraction into subsidy for the state-party and ICC seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â ending impunity while respecting sovereignty â remains contested rather than dead. If the problem were dead, the constraint would risk piton classification. However, ongoing impunity in non-cooperating jurisdictions (e.g., Sudan, Syria analogues) and active resistance from states asserting sovereignty costs indicate the mandate is still contested in its function. The moderate theater ratio (0.55) reflects some performative maintenance, but the constraint is not purely inertial: genuine coordination function (jurisdictional allocation) and genuine extraction (sovereignty override, justice delay) both remain live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_shield_or_sword,
    'Does the complementarity mechanism primarily function to protect genuine state sovereignty over criminal justice, or does it operate as a procedural shield for states wishing to avoid accountability?',
    'Systematic review of admissibility proceedings where states claim willingness but fail to prosecute, correlated with subsequent ICC intervention or perpetuation of impunity.',
    'If the mechanism operates as a shield, extraction on victims is higher than the structural measure suggests and the constraint edges toward snare; if it operates as a sword protecting sovereignty, extraction on state parties is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_shield_or_sword, empirical, 'Whether complementarity protects sovereignty or shields impunity').

omega_variable(
    non_party_jurisdiction_legitimacy,
    'Does UNSC referral of non-party states to the ICC represent legitimate universal jurisdiction or coercive sovereignty extraction without consent?',
    'Comparative analysis of UNSC referral practice against general international law consent principles and state reactions to such referrals.',
    'If coercive extraction, non_party_states directionality moves closer to full target and the constraint''s extractiveness is amplified for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_party_jurisdiction_legitimacy, conceptual, 'Legitimacy of ICC jurisdiction over non-consenting states').

omega_variable(
    authority_grounding_nature,
    'Is the hybrid authority better understood as treaty-based positivist consent or natural law universalism, and does this framing change the constraint''s classification?',
    'Doctrinal analysis of ICC jurisprudence on sources of jurisdiction and authoritative statements by the Court and Assembly of States Parties.',
    'If natural law dominates, the constraint appears more mountain-like; if positivist consent dominates, it appears more rope-like. The hybrid reading depends on both.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_nature, conceptual, 'Nature of authority grounding in the hybrid reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_hybrid_tr_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rome_hybrid_tr_t4, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(rome_hybrid_tr_t8, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(rome_hybrid_tr_t12, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(rome_hybrid_tr_t16, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 16, 0.5).
narrative_ontology:measurement(rome_hybrid_tr_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(rome_hybrid_tr_t22, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 22, 0.55).

% Extraction over time
narrative_ontology:measurement(rome_hybrid_be_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(rome_hybrid_be_t4, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(rome_hybrid_be_t8, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(rome_hybrid_be_t12, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(rome_hybrid_be_t16, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(rome_hybrid_be_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(rome_hybrid_be_t22, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 22, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(rome_hybrid_su_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(rome_hybrid_su_t4, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(rome_hybrid_su_t8, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(rome_hybrid_su_t12, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(rome_hybrid_su_t16, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(rome_hybrid_su_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(rome_hybrid_su_t22, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 22, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the rome_statute_jurisdiction kernel. The hybrid reading is distinguished by moderate extraction (0.63) from non-parties and shielded victims, while the universalist reading would likely exhibit lower extraction (treating universal mandate as coordination) and the sovereigntist reading would exhibit higher extraction from international authority (treating strict consent as a snare on the Court). Each reading has distinct epsilon values, stakeholder directionalities, and failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
