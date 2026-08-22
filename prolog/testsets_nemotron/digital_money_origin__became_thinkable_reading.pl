% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Emergence — Became Thinkable Reading
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story captures the 'became thinkable' reading of digital
 *   money's origin: the moment when the concept of purely digital monetary
 *   representation became technically and institutionally conceivable —
 *   roughly the 1960s–1970s with the advent of electronic funds transfer
 *   research, central bank settlement automation, and the first messaging
 *   standards. This reading places the origin earlier than the 'first held'
 *   reading (which requires practical consumer use) or the 'regulatory
 *   recognition' reading (which requires formal statistical incorporation).
 *   The constraint set includes the conceptual and regulatory barriers that
 *   preceded widespread implementation: the definition of what counts as
 *   money, the standards for interoperability, the legal framework for
 *   electronic claims. Beneficiaries are the early institutional architects
 *   who defined the thinkable; victims are those whose monetary practices
 *   were rendered illegible by the new conceptual frame.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.62).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.48).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Emergence — Became Thinkable Reading").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, 'a93853d8-c87b-4d72-bb05-781852764b2a').
narrative_ontology:cs_kernel_codification('a93853d8-c87b-4d72-bb05-781852764b2a', distributed).
narrative_ontology:cs_authority_grounding('a93853d8-c87b-4d72-bb05-781852764b2a', practice).
narrative_ontology:cs_interpretation_layer_present('a93853d8-c87b-4d72-bb05-781852764b2a').
narrative_ontology:cs_reading_relation('a93853d8-c87b-4d72-bb05-781852764b2a', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('a93853d8-c87b-4d72-bb05-781852764b2a', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('a93853d8-c87b-4d72-bb05-781852764b2a', foundational, digital_money_is_conceptually_prior_to_implementation).
narrative_ontology:cs_axiom_status(digital_money_is_conceptually_prior_to_implementation, holdable).
narrative_ontology:cs_axiom_grounding('a93853d8-c87b-4d72-bb05-781852764b2a', digital_money_is_conceptually_prior_to_implementation, conventional).
narrative_ontology:cs_axiom('a93853d8-c87b-4d72-bb05-781852764b2a', secondary, monetary_form_follows_institutional_legibility).
narrative_ontology:cs_axiom_status(monetary_form_follows_institutional_legibility, holdable).
narrative_ontology:cs_axiom_grounding('a93853d8-c87b-4d72-bb05-781852764b2a', monetary_form_follows_institutional_legibility, instrumental).
narrative_ontology:cs_reference_frame('a93853d8-c87b-4d72-bb05-781852764b2a', pre_digital_settlement_chaos).
narrative_ontology:cs_drift_state('a93853d8-c87b-4d72-bb05-781852764b2a', universal_digital_default, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a93853d8-c87b-4d72-bb05-781852764b2a', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_bank_research_units).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, clearing_house_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_fintech_standard_setters).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, cash_dependent_populations).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, informal_economy_participants).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, peripheral_banking_regions).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, monetary_innovation_requires_institutional_legibility).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, digital_money_is_a_claim_structure_not_a_technology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commissioned the foundational research on electronic funds transfer, designed the conceptual architecture of interbank settlement systems, and set the standards that defined what would count as digital money. They benefit from controlling the legitimating framework and the data infrastructure. Exit is arbitrage-grade: they can shift to new frameworks they themselves design.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_bank_research_units, agenda_setter,
    institutional, generational, arbitrage, national).

% Built the operational layers (ACH, wire networks, settlement protocols) that made the concept practically executable. They collect rents from transaction volume and standard-setting authority. Exit is mobile — their skills transfer to any digital payment rail, but they benefit from the installed base.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, clearing_house_architects, beneficiary,
    organized, biographical, mobile, national).

% Defined the messaging standards (e.g., SWIFT, ISO 8583) and data formats that made interoperability possible. They benefit from network effects and licensing. Exit is constrained — their expertise is specific to the standard ecosystem they helped create.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_fintech_standard_setters, beneficiary,
    moderate, biographical, constrained, global).

% Rely on physical cash for daily transactions, lack access to banking infrastructure, and are excluded from the conceptual framing that treats digital representation as the natural form of money. Their costs rise as cash infrastructure erodes and digital becomes default. Exit is trapped — they cannot opt into the digital system on equal terms.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, cash_dependent_populations, payer,
    powerless, immediate, trapped, local).

% Operate in cash-based informal sectors where digital traceability threatens livelihood. The conceptual shift to digital money as the legitimate form renders their economic activity invisible or criminal. Exit is trapped — formalization carries prohibitive costs and surveillance.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, informal_economy_participants, payer,
    powerless, immediate, trapped, local).

% Regions where banking infrastructure arrived late or incompletely. The conceptual framework designed in financial centers assumes universal digital readiness, making peripheral regions structurally dependent on expensive correspondent relationships. Exit is constrained — they can adopt the standards but on terms set elsewhere.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, peripheral_banking_regions, payer,
    moderate, biographical, constrained, regional).

% Study the genealogy of digital money concepts and the contest over origin narratives. They see the structural asymmetry between the architects who defined the thinkable and the populations excluded from that framing. Analytical exit — they can shift interpretive frames but cannot change the material constraint.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared conceptual and technical framework for representing monetary value in digital form, enabling interoperable settlement across institutions and geographies — solves the coordination problem of how distinct ledgers can recognize each other's claims.
% TRANSFER_FUNCTION: Moves definitional authority and infrastructural control from diverse monetary practices (cash, local credit, commodity money) to a centralized institutional architecture (central banks, clearing houses, standard bodies). The transfer is cognitive first: what counts as money becomes what fits the digital framework.
% ABSENT_VOICES: Cash-dependent populations, informal economy participants, and peripheral banking regions are structurally excluded from the conceptual framing process. They would object to the naturalization of digital representation as the only legitimate form of money, but the constraint's architecture treats their practices as legacy rather than as legitimate alternatives.
% DISAPPEARANCE_RATIONALE: If the conceptual constraint that digital money is the natural form of money vanished, cash and informal monetary practices would regain legitimacy as co-equal forms, peripheral regions could develop context-appropriate monetary infrastructures, and the institutional architects would lose their definitional monopoly. The monetary system would reorganize around plural forms.
% FOUNDING_PROBLEM: Post-war monetary systems faced coordination failures in interbank settlement: incompatible ledgers, slow physical clearing, and no shared representation for electronic value transfer. The founding problem was creating a universal language for monetary claims across institutional boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Central bank archives and clearing house records attest the founding problem as live (ongoing need for faster, more interoperable settlement). Monetary historians and development economists outside the benefiting institutions attest the problem was substantially solved by the 1990s and the constraint persists as path dependency — the conceptual framework now excludes alternatives it once merely preceded.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the cognitive and infrastructural capture: the framework extracts definitional authority from alternative monetary forms. Suppression (0.48) is moderate — the constraint does not primarily use force but rather definitional exclusion and infrastructural lock-in. Theater ratio (0.22) is low — the coordination function (interoperable settlement) is genuine and the extraction is not primarily performative. Accessibility collapse (0.71) is high — once the digital frame is accepted, alternatives become structurally hard to imagine. Resistance (0.38) is moderate — cash and informal practices persist but are increasingly marginalized. The claim of tangled_rope reflects genuine coordination (interbank settlement) combined with asymmetric extraction (definitional capture).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is a necessary coordination infrastructure — the natural evolution of monetary technology. From the payer seats, it is a cognitive enclosure that renders their monetary practices invisible and costly. The engine computes this divergence from the declared power/exit structures: institutional arbitrage vs. trapped exit produces the directionality spread.
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank research units and clearing house architects are structural beneficiaries (d near 0.0–0.2): they designed the framework and collect its rents. Early fintech standard setters are moderate beneficiaries (d ~0.3): they benefit from network effects but operate within the frame. Cash-dependent populations and informal economy participants are full targets (d near 0.9–1.0): they bear the costs of exclusion with no exit. Peripheral banking regions are constrained targets (d ~0.7): they must adopt on terms set elsewhere. Monetary historians are analytical observers (d=0.5 symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interbank settlement coordination) was genuinely solved, but the conceptual framework that solved it has expanded beyond its solution domain. It now defines what money IS, excluding forms that don't fit the digital representation. This is mandatrophy: the mandate (coordinate settlement) has outlived its function (the coordination is achieved) but the constraint (digital-as-natural-form) persists and expands. The constraint prevents recognizing that the coordination problem is solved and the extraction (definitional monopoly) is now the primary function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the ''became thinkable'' reading genuinely identify a distinct constraint, or is it a retrospective projection that reads later institutional arrangements back into early conceptual work?',
    'Compare the 1960s–1970s institutional discourse (central bank reports, clearing house minutes, standards body proceedings) against the later claim that digital money ''emerged'' at this point. If the actors at the time did not conceive of themselves as originating a new form of money but merely automating existing settlement, the reading is retrospective.',
    'If retrospective, the constraint''s origin date shifts later, its extractiveness at origin is lower (the extraction was not yet designed in), and the beneficiary structure changes from ''architects who defined the thinkable'' to ''actors who captured the framework later''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the reading''s origin claim is contemporary or retrospective.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination function (interoperable settlement) structurally separable from the extraction function (definitional monopoly over what counts as money)?',
    'Examine whether alternative settlements systems (e.g., bilateral correspondent banking, decentralized ledger protocols) can achieve the coordination without the definitional monopoly. Historical counterfactual: if SWIFT/ACH standards had been open protocols without central governance, would coordination have been achieved?',
    'If separable, the constraint is a tangled_rope where extraction rides on coordination. If inseparable, the extraction may be the necessary price of coordination (rope with high floor) or the coordination story may be cover for extraction (snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components can be disentangled.').

omega_variable(
    victim_exclusion_mechanism,
    'Is the exclusion of cash-dependent and informal economy participants an intended design feature of the digital money framework, or an unanticipated consequence of a framework designed for interbank efficiency?',
    'Analyze the design documents and policy debates of the 1960s–1980s: did central bank architects discuss financial inclusion, or was the frame explicitly restricted to institutional settlement?',
    'If intended, the constraint is more snare-like (extraction by design). If unanticipated, it is more tangled_rope-like (coordination with emergent extraction). Affects the mandatrophy assessment: design intent vs. path dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_exclusion_mechanism, empirical, 'Intentionality of the constraint''s exclusionary effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1965, 1995).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1965, digital_money_origin__became_thinkable_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__became_thinkable_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(digi_tr_t1975, digital_money_origin__became_thinkable_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(digi_tr_t1980, digital_money_origin__became_thinkable_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(digi_tr_t1985, digital_money_origin__became_thinkable_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__became_thinkable_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(digi_tr_t1995, digital_money_origin__became_thinkable_reading, theater_ratio, 1995, 0.22).

% Extraction over time
narrative_ontology:measurement(digi_be_t1965, digital_money_origin__became_thinkable_reading, base_extractiveness, 1965, 0.25).
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__became_thinkable_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(digi_be_t1975, digital_money_origin__became_thinkable_reading, base_extractiveness, 1975, 0.41).
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__became_thinkable_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(digi_be_t1985, digital_money_origin__became_thinkable_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__became_thinkable_reading, base_extractiveness, 1990, 0.59).
narrative_ontology:measurement(digi_be_t1995, digital_money_origin__became_thinkable_reading, base_extractiveness, 1995, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1965, digital_money_origin__became_thinkable_reading, suppression_requirement, 1965, 0.15).
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__became_thinkable_reading, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(digi_su_t1975, digital_money_origin__became_thinkable_reading, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__became_thinkable_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(digi_su_t1985, digital_money_origin__became_thinkable_reading, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__became_thinkable_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(digi_su_t1995, digital_money_origin__became_thinkable_reading, suppression_requirement, 1995, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_origin__became_thinkable_reading, 0.02).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the digital_money_origin kernel. The became_thinkable_reading claims the earliest origin (conceptual conceivability), the first_held_reading claims the practical implementation moment (consumer use), and the regulatory_recognition_reading claims the formal incorporation moment (statistical/regulatory recognition). Each reading has different beneficiaries, victims, extractiveness, and claimed_type. They are linked by affects_constraints to form the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__became_thinkable_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
