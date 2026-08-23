% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: ICC Jurisdiction (Sovereigntist Reading: Strict Consent Required)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint story models the sovereigntist reading of the Rome
 *   Statute's jurisdictional architecture: the ICC's authority is conditional
 *   on state consent, non-party nationals are immune absent UNSC referral,
 *   national courts retain primary authority, and complementarity operates as
 *   deference to national proceedings rather than an override. The reading
 *   treats the Statute as a voluntary association of consenting states rather
 *   than a universal mandate. The constraint is classified as a rope because
 *   it solves a genuine coordination problem (creating a functional
 *   international criminal court among willing states) with minimal coercive
 *   overhead; participants (states parties) are net beneficiaries, and
 *   alternatives (national prosecution, ad hoc tribunals, UNSC referrals) are
 *   not suppressed. The low extractiveness reflects that the constraint
 *   primarily limits the ICC's reach rather than extracting from subjects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.15).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.08).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "ICC Jurisdiction (Sovereigntist Reading: Strict Consent Required)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, 'f2e14c35-04d0-4cd3-bfe8-92e67fd07814').
narrative_ontology:cs_kernel_codification('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', formalized).
narrative_ontology:cs_authority_grounding('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', lineage).
narrative_ontology:cs_interpretation_layer_present('f2e14c35-04d0-4cd3-bfe8-92e67fd07814').
narrative_ontology:cs_reading_relation('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', foundational, jurisdiction_exclusively_from_state_consent).
narrative_ontology:cs_axiom_status(jurisdiction_exclusively_from_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', jurisdiction_exclusively_from_state_consent, conventional).
narrative_ontology:cs_axiom('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', foundational, complementarity_as_deference_to_national_primacy).
narrative_ontology:cs_axiom_status(complementarity_as_deference_to_national_primacy, holdable).
narrative_ontology:cs_axiom_grounding('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', complementarity_as_deference_to_national_primacy, conventional).
narrative_ontology:cs_reference_frame('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', rome_statute_as_voluntary_association).
narrative_ontology:cs_drift_state('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', contemporary_practice, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('f2e14c35-04d0-4cd3-bfe8-92e67fd07814', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_party_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_judicial_authorities).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, state_sovereignty_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutor_office).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, state_consent_foundation_of_international_law).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, complementarity_as_deference_not_override).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, national_courts_primary_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain full immunity for their nationals from ICC prosecution absent UNSC referral; the consent requirement protects their sovereign prerogative to shield officials and citizens from international judicial reach. They can engage with the ICC system selectively (e.g., accepting jurisdiction ad hoc) or ignore it entirely without legal consequence.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_states, beneficiary,
    powerful, generational, arbitrage, global).

% Remain the primary courts for prosecuting international crimes; the ICC acts only when they are genuinely unwilling or unable, and even then the consent gate limits ICC reach. Their institutional authority over criminal justice is preserved and the complementarity standard defers to their primacy.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, national_judicial_authorities, beneficiary,
    institutional, generational, analytical, national).

% The principle that international criminal jurisdiction derives exclusively from state consent is structurally entrenched by this reading; the Rome Statute becomes a voluntary association of consenting states rather than a universal mandate, reinforcing the Westphalian foundation of international law.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, state_sovereignty_doctrine, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(rome_statute_jurisdiction__sovereigntist_reading, state_sovereignty_doctrine).

% Operates under severe jurisdictional constraints: cannot investigate nationals of non-party states without UNSC referral; complementarity analysis must defer to national proceedings unless manifestly inadequate; resource allocation is skewed toward situations where consent exists, limiting docket scope and institutional relevance.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutor_office, payer,
    institutional, biographical, constrained, global).

% Have no pathway to ICC justice when their state is not a party and the UNSC does not refer; domestic courts may be unwilling or unable, but the consent gate blocks ICC access. Their claims are structurally invisible to the international system under this reading.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, victims_in_non_party_states, excluded,
    powerless, biographical, trapped, global).

% Analyze the jurisdictional architecture and its implications for accountability gaps; their interpretations feed into state practice, treaty body outputs, and the evolution of the complementarity doctrine. They do not collect from or pay into the constraint.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, consent-based framework for international criminal cooperation among willing states, avoiding the collective-action problem of universal jurisdiction disputes by limiting obligations to parties.
% TRANSFER_FUNCTION: Moves jurisdictional authority from a hypothetical universal court back to national courts and consenting states; the cost is borne by the ICC's operational scope (foregone cases) and by victims in non-party states (foregone remedy).
% ABSENT_VOICES: Victims in non-party states and civil society actors in those states who would seek ICC access are structurally excluded; the consent architecture makes their inclusion contingent on their own state's ratification or UNSC action, which is politically unlikely in many cases.
% DISAPPEARANCE_RATIONALE: If the strict consent requirement vanished, the ICC could assert jurisdiction over nationals of non-party states based on territoriality or UNSC referral alone, fundamentally altering the risk calculus for powerful non-party states and expanding the Court's docket — the international criminal justice landscape would reorganize around a more universal jurisdictional claim.
% FOUNDING_PROBLEM: Post-WWII and post-Cold War efforts to create a permanent international criminal court foundered on the unwillingness of major powers to submit their nationals to universal jurisdiction without consent; the Rome Statute's consent architecture was the compromise that allowed the treaty to be adopted.
% FOUNDING_PROBLEM_CORROBORATION: The negotiation history (Preparatory Committee records, Rome Conference documentation) corroborates that the consent compromise was essential for adoption; however, the Rome Statute's preamble and Article 1's language about 'the most serious crimes of concern to the international community as a whole' are cited by universalist proponents as evidence the founding aspiration transcended consent. States that ratified (123 parties) attest the problem of impunity remains live; major non-parties (US, China, Russia) attest the consent compromise remains essential to their non-participation.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).
:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the consent gate restricts the ICC's operational scope rather than extracting resources from governed populations; the 'cost' is foregone jurisdiction, not affirmative transfer. Suppression is very low (0.08) because non-party states face no legal penalty for non-participation and the ICC has no enforcement mechanism against them — the constraint's persistence depends on state consent, not coercion. Theater ratio is low (0.12) because the Court's proceedings are substantively functional for situations within its jurisdiction; performative elements (e.g., diplomatic pressure on non-parties) exist but are marginal to the core operation. Accessibility collapse is moderate (0.35) because alternative accountability pathways (national courts, universal jurisdiction in third states, UNSC referrals) remain legally available and are occasionally used. Resistance is low-moderate (0.25) because the constraint's main opposition comes from universalist advocates seeking expanded jurisdiction, not from subjects resisting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC Prosecutor's seat, the consent constraint appears as a structural limitation on the Court's mandate — a coordination failure that leaves accountability gaps. From the non-party state seat, the same constraint appears as a necessary protection of sovereign equality and a legitimate limit on international judicial power. The engine computes this divergence from the structural data (power, exit, beneficiary/payer roles); the claimed type (rope) reflects the authoring seat's assessment that the coordination function is genuine and not extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-party states and national judicial authorities are structural beneficiaries: they retain sovereign prerogative and primary jurisdiction respectively. The ICC Prosecutor's Office is a structural payer: its institutional capacity and docket are constrained by the consent architecture. State sovereignty doctrine (a non-agent proposition) is a vindicated beneficiary — it collects no rents but is structurally reinforced. Victims in non-party states are excluded: they bear the cost of the jurisdictional gap but have no voice in the constraint's maintenance. The directionality derivation from beneficiary/victim declarations plus exit options yields low d for beneficiaries (arbitrage/analytical exit) and higher d for the ICC Office (constrained exit), which the engine will scale into effective extraction differentials.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a court acceptable to major powers) is contested as still live: ratifying states see ongoing impunity as evidence the problem persists; non-parties see the consent architecture as the only viable foundation. The constraint has not atrophied into a piton because it actively enables the ICC's operation among 123 states parties; the consent gate is the functional core of the treaty, not a vestigial remnant. No mandatrophy declaration is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_universality_tension,
    'Does the Rome Statute''s consent architecture represent a stable coordination equilibrium among willing states, or an unresolved tension between the Statute''s universalist preamble and its consent-based operative provisions?',
    'Longitudinal analysis of state practice: if non-parties gradually ratify or accept ad hoc jurisdiction, the consent gate erodes toward universality; if major powers permanently opt out and the ICC''s docket remains concentrated in Africa and conflict zones, the tension is structural and stable.',
    'If the tension is structural and stable, the rope classification holds (genuine coordination among a subset). If the universalist preamble generates persistent pressure to expand jurisdiction beyond consent, the constraint may drift toward tangled_rope (coordination + asymmetric pressure on non-parties).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_universality_tension, conceptual, 'Whether the consent gate is a stable feature or a contested compromise under pressure.').

omega_variable(
    complementarity_operational_meaning,
    'Does complementarity-as-deference (this reading) produce materially different ICC case selection outcomes than complementarity-as-override (universalist/hybrid readings)?',
    'Comparative analysis of ICC admissibility decisions: count cases where the Prosecutor deferred to national proceedings versus cases where the Court asserted jurisdiction over national objections; correlate with the complementarity standard articulated in each decision.',
    'If deference is the dominant operational mode, the rope''s coordination function is genuine (states trust the Court won''t second-guess them). If override is common, the consent gate is partially illusory and extraction (foregone sovereignty) is higher than this reading assumes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complementarity_operational_meaning, empirical, 'Whether the complementarity standard operates as deference or override in practice.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the sovereigntist reading''s core premise (jurisdiction exclusively from consent) logically foreclose the universalist reading''s core premise (universal mandate transcending consent) within any single legal framework, or do they coexist as competing interpretations?',
    'Doctrinal analysis: can a single court or treaty body coherently apply both premises to the same situation? If the premises are mutually exclusive in application (e.g., a case involving a non-party national on a party''s territory), foreclosure holds; if different bodies can apply different premises without systemic contradiction, coexistence holds.',
    'If foreclosure: the kernel has a deep structural split and the readings are rival frameworks. If coexistence: the kernel tolerates pluralism and the constraint family is stable. This determines the reading_relations assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between sovereigntist and universalist premises within a single commitment framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.05).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_tr_t2002, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2002, 0.08).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_tr_t2010, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_tr_t2015, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2015, 0.11).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_tr_t2020, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_tr_t2024, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.1).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_be_t2002, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2002, 0.12).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_be_t2010, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2010, 0.13).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_be_t2015, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2015, 0.14).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_be_t2020, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_be_t2024, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.02).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_su_t2002, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2002, 0.04).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_su_t2010, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2010, 0.06).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_su_t2015, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2015, 0.07).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_su_t2020, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2020, 0.08).
narrative_ontology:measurement(rome_statute_jurisdiction_sovereigntist_su_t2024, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2024, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__sovereigntist_reading, 0.1).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, icc_complementarity_doctrine).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, unsc_referral_mechanism).

% DUAL FORMULATION NOTE:
% This story is one member of the rome_statute_jurisdiction constraint family. The sovereigntist reading (this file) models the consent gate as a genuine coordination mechanism (rope). The universalist reading models the same Statute as a universal mandate with substantial extraction from non-consenting states (likely tangled_rope or snare). The hybrid reading models complementarity as a balancing mechanism (likely tangled_rope). All three share the kernel_id rome_statute_jurisdiction but instantiate different constraints with different ε, beneficiaries, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__sovereigntist_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
