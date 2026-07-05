% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money Origin — First-Held Practical Store of Value
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the 'first-held' reading of the digital money
 *   origin kernel: digital money is dated to the moment ordinary individuals
 *   began holding non-physical monetary instruments (bank deposit balances
 *   treated as spendable wealth, card-linked funds, early e-money) as their
 *   practical, everyday store of value. This is a later, narrower, and more
 *   infrastructure-contingent origin claim than the 'became thinkable'
 *   reading (which dates origin to conceptual/technical possibility) and
 *   earlier and less formal than the 'regulatory recognition' reading (which
 *   dates origin to when monetary authorities counted digital instruments in
 *   official aggregates). The three readings are separate constraints with
 *   separate ε values and separate beneficiary/victim sets, linked via
 *   network.affects_constraints — this file covers only the possession-based
 *   claim.
 *
 * KEY AGENTS:
 *   - early_adopter_account_holders: primary beneficiary (moderate/mobile) — first practical holders whose experience defines the origin moment
 *   - incumbent_payment_infrastructure_operators: agenda_setter/beneficiary (institutional/arbitrage) — built and control the access rails that determined who could hold digitally first
 *   - unbanked_and_infrastructure_excluded_populations: primary payer (powerless/trapped) — excluded from the founding cohort by infrastructure access, not by choice
 *   - cash_dependent_informal_economy_workers: secondary payer (powerless/constrained) — structurally locked out of the origin-defining possession
 *   - monetary_historians: analytical observer (analytical/analytical) — adjudicate which possession events count as origin-constitutive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.46).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.4).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin — First-Held Practical Store of Value").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, 'a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3').
narrative_ontology:cs_kernel_codification('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', distributed).
narrative_ontology:cs_authority_grounding('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', distributed).
narrative_ontology:cs_reading_relation('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', foundational, possession_constitutes_monetary_reality).
narrative_ontology:cs_axiom_status(possession_constitutes_monetary_reality, holdable).
narrative_ontology:cs_axiom_grounding('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', possession_constitutes_monetary_reality, empirically_contingent).
narrative_ontology:cs_axiom('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', secondary, access_asymmetry_is_intrinsic_to_origin_dating).
narrative_ontology:cs_axiom_status(access_asymmetry_is_intrinsic_to_origin_dating, holdable).
narrative_ontology:cs_axiom_grounding('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', access_asymmetry_is_intrinsic_to_origin_dating, conventional).
narrative_ontology:cs_reference_frame('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', possession_based_monetary_realism).
narrative_ontology:cs_drift_state('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', post_digital_banking_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a36a9e4a-bdf4-4426-a5fc-6d7861e24ef3', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopter_account_holders).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, incumbent_payment_infrastructure_operators).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_and_infrastructure_excluded_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, cash_dependent_informal_economy_workers).
narrative_ontology:constraint_vindicates(digital_money_origin__first_held_reading, possession_based_monetary_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% First individuals with bank access, employment, and address stability to hold electronic deposit balances, card-linked funds, or e-money wallets as their actual working store of value rather than a paper abstraction. They benefit from convenience, security against theft, and access to credit history built on transaction records — the origin-marking event is their lived possession, not a technical proof-of-concept.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopter_account_holders, beneficiary,
    moderate, biographical, mobile, national).

% Banks, card networks, and clearing systems that built and control the rails on which 'first holding' became possible. They set account minimums, KYC requirements, and fee structures that determine who can practically hold digital balances, and they collect fees and interest float on every held balance. Their infrastructure decisions retroactively define who counts as an origin-point holder.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, incumbent_payment_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, incumbent_payment_infrastructure_operators, beneficiary).

% People without documentation, credit history, or geographic access to branches or reliable connectivity who were excluded from the first cohort of practical holders. They pay in the form of continued reliance on costlier cash-based alternatives, exclusion from the credit and convenience benefits accruing to early holders, and later must adopt on infrastructure terms already set without their input.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_and_infrastructure_excluded_populations, payer,
    powerless, biographical, trapped, national).

% Workers paid in cash whose livelihoods are structured around physical currency because digital holding was never practically available to them at the origin moment. As the first-held reading becomes the canonical origin story, subsequent policy and financial-history narratives treat their exclusion as a lag to be corrected rather than a structural cost imposed by infrastructure rollout order.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, cash_dependent_informal_economy_workers, payer,
    powerless, biographical, constrained, regional).

% Scholars who adjudicate which moment counts as the origin of digital money. Under this reading they anchor the date to individual possession and use, which shifts the historical record later than conceptual-availability readings and foregrounds access inequality as constitutive of the origin event rather than incidental to it.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a verifiable, possession-based criterion for 'digital money exists' lets historians, economists, and regulators agree on a concrete empirical marker (first practical individual holding) rather than arguing over diffuse conceptual or institutional timelines.
% TRANSFER_FUNCTION: Moves narrative and evidentiary authority toward institutions that built the earliest access-granting infrastructure (banks, card networks) and toward the individuals who could use it, while moving the origin-attribution cost onto populations excluded from that infrastructure at the time — their absence from the record becomes definitional rather than incidental.
% ABSENT_VOICES: The unbanked and cash-dependent populations of the era in question have no voice in how the origin moment is dated; their lack of access is precisely what disqualifies them from the founding cohort under this reading, and no historical record centers their experience of continued cash dependency as a competing origin-relevant fact.
% DISAPPEARANCE_RATIONALE: If the first-held criterion vanished as the accepted origin marker, monetary history would not physically rearrange, but the attributed origin date would shift earlier (toward conceivability) or later (toward regulatory recognition), changing which institutions and populations are credited as originators — a matter of interpretive authority, not material infrastructure, hence contested rather than a clean rearrange/unchanged split.
% FOUNDING_PROBLEM: Historians and economists needed a non-arbitrary, empirically observable criterion for dating the emergence of digital money, distinct from vague claims about when the idea became possible or when regulators noticed.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent payment operators and early-adopter narratives attest that possession is the only rigorous empirical marker. Development economists and financial-inclusion researchers, outside the beneficiary set, corroborate that the founding problem (needing a dating criterion) persists but argue the possession criterion itself encodes and naturalizes access inequality rather than resolving the dating question neutrally.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, contested).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.46, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).
:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.46) and rises modestly over the measured interval: the possession-based criterion itself does not extract wealth directly, but it retroactively legitimizes the fee, float, and access-gating practices of the infrastructure operators who first enabled holding, and those practices deepen as digital holding becomes the presumed default. Suppression is moderate (0.40): access barriers (KYC, minimum balances, branch geography, connectivity) are real structural exclusions, not merely narrative ones, though less coercive than an enforced monopoly. Theater ratio stays low (rising 0.10 to 0.20) because the coordination function — establishing a workable empirical dating criterion — remains substantively real throughout; theatrical drift is present but not dominant.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent operator and early-adopter seats, the first-held criterion looks like a neutral empirical marker — coordination around a genuine, observable fact. From the excluded populations' seats, the same criterion operates as an extraction-adjacent mechanism: infrastructure access decisions made by institutional agenda-setters determine who gets credited as an originator, and that credit carries real downstream benefits (credit history, narrative legitimacy, policy attention) that excluded groups never receive.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopter holders and incumbent infrastructure operators sit near the beneficiary end: holders gain convenience and credit-building access, operators gain fee revenue, deposit float, and narrative authority over the origin story. Unbanked populations and cash-dependent workers sit near the target end: their structural exclusion from infrastructure access is the mechanism by which they are excluded from the founding cohort, and that exclusion is then treated as a historical footnote rather than a distributive cost of infrastructure rollout order.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (a concrete, falsifiable dating criterion for digital money's emergence) remains genuinely useful for monetary historians and does not appear to have outlived its purpose — hence founding_problem_status is contested rather than dead. What prevents this from being mislabeled pure extraction is that the possession criterion does real disambiguating work distinct from the conceptual and regulatory readings; what prevents it from being mislabeled pure coordination is the asymmetric access structure baked into who could practically satisfy the criterion first, which the tangled_rope classification is designed to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    possession_threshold_ambiguity,
    'What counts as ''practical holding'' — does a single deposit account balance suffice, or does the criterion require routine transactional use, and does this threshold choice change the origin date by decades?',
    'Comparative historical analysis of deposit account penetration rates versus transactional digital payment volume across candidate origin decades, cross-referenced with contemporaneous survey data on how account holders described their own money-holding practices.',
    'A low threshold (any balance held) pulls the origin date much earlier and closer to the became_thinkable_reading; a high threshold (routine transactional reliance) pulls it later, closer to or past the regulatory_recognition_reading, potentially collapsing the distinctiveness of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(possession_threshold_ambiguity, conceptual, 'Ambiguity in what counts as practical possession threshold for dating origin.').

omega_variable(
    access_exclusion_as_definitional_vs_incidental,
    'Is the exclusion of unbanked populations from the founding cohort a constitutive feature of the first-held reading (it defines origin by who had access) or an incidental historical fact that a fairer origin criterion should correct for?',
    'Would require normative adjudication among monetary historians and financial-inclusion scholars as to whether origin dating should be access-neutral (counterfactual: when could anyone have held digital money) or access-actual (when did real people first hold it).',
    'If constitutive, the tangled_rope classification is well-grounded — the coordination function and the exclusion are structurally fused. If incidental, the constraint''s extraction component would need to be attributed to the infrastructure operators alone, potentially reclassifying toward a cleaner rope-with-externality reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(access_exclusion_as_definitional_vs_incidental, conceptual, 'Whether access exclusion is intrinsic to the reading or a correctable historical accident.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the first-held reading favored in institutional and popular financial history because it best serves incumbent payment operators'' narrative interest in being credited as originators, relative to the became_thinkable and regulatory_recognition readings?',
    'Trace citation and framing patterns in central bank histories, financial industry retrospectives, and academic monetary history to see which reading each institutional source favors and whether favored readings correlate with institutional self-interest.',
    'If selection pressure is confirmed, the very choice of this reading as ''the'' origin story (rather than an equally valid alternative) is itself a site of extraction — institutional agenda-setters shaping which historical criterion becomes canonical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether reading selection itself is influenced by beneficiary institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__first_held_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(digi_tr_t8, digital_money_origin__first_held_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(digi_tr_t16, digital_money_origin__first_held_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(digi_tr_t24, digital_money_origin__first_held_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(digi_tr_t32, digital_money_origin__first_held_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__first_held_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__first_held_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(digi_be_t8, digital_money_origin__first_held_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(digi_be_t16, digital_money_origin__first_held_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(digi_be_t24, digital_money_origin__first_held_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(digi_be_t32, digital_money_origin__first_held_reading, base_extractiveness, 32, 0.44).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__first_held_reading, base_extractiveness, 40, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__first_held_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(digi_su_t8, digital_money_origin__first_held_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(digi_su_t16, digital_money_origin__first_held_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(digi_su_t24, digital_money_origin__first_held_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(digi_su_t32, digital_money_origin__first_held_reading, suppression_requirement, 32, 0.38).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__first_held_reading, suppression_requirement, 40, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_origin__first_held_reading, 0.05).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the digital_money_origin kernel. became_thinkable_reading dates origin to conceptual/technical possibility (earliest date, lowest extraction — closer to a rope/mountain-adjacent claim about technological feasibility). first_held_reading (this file) dates origin to individual practical possession (middle date, moderate extraction from access asymmetry — tangled_rope). regulatory_recognition_reading dates origin to formal incorporation into monetary aggregates and regulatory frameworks (latest date, extraction concentrated in who shaped the statistical/regulatory definitions). Each has a distinct ε and distinct beneficiary/victim structure; they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
