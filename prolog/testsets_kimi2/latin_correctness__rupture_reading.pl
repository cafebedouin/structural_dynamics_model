% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__rupture_reading, []).

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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Classical Latin Reconstruction Standard (Rupture Reading)
 *   domain: historical/linguistic/intellectual
 *
 * SUMMARY:
 *   The rupture reading treats classical Latin as a fixed textual standard
 *   recoverable only through reconstruction from ancient sources, and
 *   delegitimizes medieval Latin as corruption. This constraint operates
 *   through humanist editorial practice, educational curricula, and
 *   ecclesiastical adoption of purified Latin. It creates a hierarchical
 *   distinction between those with access to classical training and those
 *   confined to medieval or vernacular forms, extracting participation rights
 *   from the latter to concentrate legitimacy in the former.
 *
 * KEY AGENTS:
 *   - renaissance_humanists: Primary agenda-setter (organized/arbitrage) â reconstruct and enforce the classical standard
 *   - classical_philologists: Primary beneficiary (moderate/mobile) â collect prestige and patronage from emendation work
 *   - ecclesiastical_institutions: Secondary beneficiary (institutional/constrained) â adopt purified Latin for legitimacy
 *   - medieval_scholars: Primary payer (moderate/constrained) â bear delegitimization of their training
 *   - vernacular_technical_writers: Secondary payer (powerless/trapped) â excluded from scholarly legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.82).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.78).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Classical Latin Reconstruction Standard (Rupture Reading)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "historical/linguistic/intellectual").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, '9664a34e-a437-43d8-8d0d-7b92f4db0788').
narrative_ontology:cs_kernel_codification('9664a34e-a437-43d8-8d0d-7b92f4db0788', fixed_text).
narrative_ontology:cs_authority_grounding('9664a34e-a437-43d8-8d0d-7b92f4db0788', lineage).
narrative_ontology:cs_interpretation_layer_present('9664a34e-a437-43d8-8d0d-7b92f4db0788').
narrative_ontology:cs_reading_relation('9664a34e-a437-43d8-8d0d-7b92f4db0788', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('9664a34e-a437-43d8-8d0d-7b92f4db0788', latin_correctness__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('9664a34e-a437-43d8-8d0d-7b92f4db0788', foundational, classical_latin_fixity).
narrative_ontology:cs_axiom_status(classical_latin_fixity, holdable).
narrative_ontology:cs_axiom_grounding('9664a34e-a437-43d8-8d0d-7b92f4db0788', classical_latin_fixity, empirically_contingent).
narrative_ontology:cs_axiom('9664a34e-a437-43d8-8d0d-7b92f4db0788', foundational, medieval_latin_corruption).
narrative_ontology:cs_axiom_status(medieval_latin_corruption, holdable).
narrative_ontology:cs_axiom_grounding('9664a34e-a437-43d8-8d0d-7b92f4db0788', medieval_latin_corruption, conventional).
narrative_ontology:cs_reference_frame('9664a34e-a437-43d8-8d0d-7b92f4db0788', classical_roman_linguistic_norm).
narrative_ontology:cs_drift_state('9664a34e-a437-43d8-8d0d-7b92f4db0788', late_medieval_scholastic_period, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9664a34e-a437-43d8-8d0d-7b92f4db0788', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, renaissance_humanists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, ecclesiastical_institutions).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_scholars).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_technical_writers).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, classical_supremacy_doctrine).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, textual_reconstruction_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reconstruct ancient texts, define classical norms, and train correctors to purge medievalisms. They set the editorial standard and benefit from the prestige of controlling the legitimate language.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, renaissance_humanists, agenda_setter,
    organized, generational, arbitrage, continental).

% Build scholarly careers on emendation and textual criticism. Their expertise is demanded as long as the classical standard holds; they collect status and patronage without administering the enforcement directly.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, classical_philologists, beneficiary,
    moderate, generational, mobile, continental).

% Adopt humanist Latin to claim continuity with ancient authority rather than medieval barbarism. The purified language serves as an international prestige marker and legitimacy signal.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, ecclesiastical_institutions, beneficiary,
    institutional, generational, constrained, continental).

% Trained in scholastic and medieval Latin traditions; their manuscripts are marked as corrupt, their methods dismissed. They face pressure to relearn humanist norms or be excluded from scholarly discourse.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_scholars, payer,
    moderate, biographical, constrained, regional).

% Write practical and technical works in regions without classical educational infrastructure. Their Latin is deemed impure, so their contributions are excluded from the legitimate scholarly record and censored from print.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_technical_writers, payer,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, diffuse).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, pan-European language for scholarship, diplomacy, and theology by reconstructing a fixed ancient norm.
% TRANSFER_FUNCTION: Moves linguistic legitimacy and access to scholarly participation from medieval-trained scholars and vernacular writers to humanist-trained elites and institutions that adopt the classical standard.
% ABSENT_VOICES: Medieval commentators who viewed their own Latin as continuous with antiquity, and vernacular technical practitioners who could not speak but would have contested the purity standard if admitted to the council.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished overnight, medieval Latin texts would be re-evaluated on their own terms, vernacular technical writing would gain legitimacy, and the humanist editorial apparatus would lose its gatekeeping functionâscholarly communication would reorganize around continuity or hybrid norms.
% FOUNDING_PROBLEM: The fragmentation of Latin in late antiquity and the medieval period created barriers to precise scholarly and theological communication across regions; also, the desire to bypass medieval barbarism and reconnect directly with ancient authority.
% FOUNDING_PROBLEM_CORROBORATION: Humanists assert the problem persists, but medieval scholars and modern linguists attest that communication was already functional and that the barbarism was a constructed crisis; corroboration from historical sociolinguistics outside the benefiting parties supports the continuity reading.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness rises from 0.45 to 0.82 over the interval as humanist institutions consolidate control over print, education, and ecclesiastical language. Suppression (0.78) is high because the constraint depends on active correction, editorial gatekeeping, and the exclusion of medieval manuscripts from the legitimate canon. Theater ratio (0.45) reflects that while genuine philological recovery occurs, an increasing share of activity is performative purity policing that exceeds the needs of communication. Accessibility collapse (0.70) is high because once the humanist frame is accepted, medieval alternatives are read as mere error rather than viable registers. Resistance (0.55) is moderate: medieval scholars and regional writers push back, but they lack the institutional scale to offset the humanist-ecclesiastical coalition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as a recovery of clarity and a legitimate return to origins. The payer seats experience the same structure as an arbitrary barrier that strips their existing competence of value. The engine will compute divergent per-seat classifications from this structural asymmetry: low directionality for humanists, high directionality for medieval scholars and vernacular writers.
 *
 * DIRECTIONALITY LOGIC:
 *   Renaissance humanists and classical philologists are declared beneficiaries: they collect status, careers, and gatekeeping power from the standard, pushing their effective extraction downward (subsidy side). Ecclesiastical institutions are secondary beneficiaries: they trade adoption for legitimacy. Medieval scholars and vernacular technical writers are declared victims: their linguistic capital is devalued and their exit is constrained or trapped, amplifying their effective extraction upward.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfragmented communication and a desire to bypass medieval barbarismâis dead by the interval end. The arrangement persists because it has become a vehicle for status accumulation and institutional legitimacy. The framework prevents mislabeling this as pure coordination by requiring declared victims and active enforcement for the tangled_rope gate. The presence of excluded payers and the rising theater ratio signal that the coordination story is partially cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_continuity_framing,
    'Does the rupture between classical and medieval Latin reflect an actual linguistic break, or is it a humanist ideological construction?',
    'Comparative historical linguistics and sociolinguistic analysis of textual communities across the transition.',
    'If the break is constructed, the constraint is more extractive than its coordination framing suggests; if real, the reconstruction standard has stronger empirical grounding and lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_continuity_framing, conceptual, 'Whether the classical-medieval rupture is empirical or ideological.').

omega_variable(
    enforcement_scope_ambiguity,
    'To what extent does the constraint''s enforcement rely on institutional power (church, universities, print censorship) versus informal prestige dynamics?',
    'Archival study of editorial mandates, university curricula, and print licensing records.',
    'High institutional enforcement raises suppression and supports a snare classification; prestige-only enforcement suggests lower suppression and a possible rope-like coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_scope_ambiguity, empirical, 'Institutional versus informal enforcement mechanisms.').

omega_variable(
    classical_fixity_empirical_status,
    'Is classical Latin as a fixed standard itself a coherent object, given register and diachronic variation among ancient sources?',
    'Textual criticism of the ancient corpus itself; sociolinguistic analysis of ancient register variation.',
    'If ancient Latin was itself variable, the fixed standard is a constructed piton or snare rather than a recovered mountain, increasing the effective theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classical_fixity_empirical_status, empirical, 'Whether the classical standard is internally coherent or a constructed idealization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lati_tr_t20, latin_correctness__rupture_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(lati_tr_t40, latin_correctness__rupture_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(lati_tr_t60, latin_correctness__rupture_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(lati_tr_t80, latin_correctness__rupture_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(lati_tr_t100, latin_correctness__rupture_reading, theater_ratio, 100, 0.43).
narrative_ontology:measurement(lati_tr_t120, latin_correctness__rupture_reading, theater_ratio, 120, 0.45).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__rupture_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lati_be_t20, latin_correctness__rupture_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(lati_be_t40, latin_correctness__rupture_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(lati_be_t60, latin_correctness__rupture_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(lati_be_t80, latin_correctness__rupture_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(lati_be_t100, latin_correctness__rupture_reading, base_extractiveness, 100, 0.8).
narrative_ontology:measurement(lati_be_t120, latin_correctness__rupture_reading, base_extractiveness, 120, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__rupture_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lati_su_t20, latin_correctness__rupture_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(lati_su_t40, latin_correctness__rupture_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(lati_su_t60, latin_correctness__rupture_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(lati_su_t80, latin_correctness__rupture_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(lati_su_t100, latin_correctness__rupture_reading, suppression_requirement, 100, 0.77).
narrative_ontology:measurement(lati_su_t120, latin_correctness__rupture_reading, suppression_requirement, 120, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the latin_correctness kernel. The epsilon-invariance principle requires decomposing the kernel into separate constraints per reading because the sibling readings instantiate different structural claims with different epsilon values and stakeholder configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
