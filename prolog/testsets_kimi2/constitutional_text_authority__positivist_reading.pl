% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Positivist Reading of Constitutional Text Authority
 *   domain: constitutional_law/legal_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the positivist reading of the
 *   constitutional_text_authority kernel: constitutional validity derives
 *   exclusively from formal enactment procedures and institutional
 *   recognition, maintaining a strict separation between law and morality. It
 *   converges with originalism on text-fidelity but diverges by rejecting any
 *   natural-law moorings for validity. The constraint coordinates
 *   constitutional interpretation by supplying a clear validity test, but
 *   asymmetrically extracts argumentative authority from natural-law
 *   advocates and substantive-rights movements by delegitimizing their core
 *   claims in constitutional discourse. As a kernel reading, it is generated
 *   as a clean Îµ-invariant constraint; sibling readings (originalist, living
 *   constitutionalist) are separate files in the constraint family.
 *
 * KEY AGENTS:
 *   - constitutional_courts: Primary agenda-setter (institutional/analytical) â enforces the law-morality boundary through doctrine
 *   - legal_positivist_academia: Primary beneficiary (organized/mobile) â supplies the intellectual framework and benefits from its institutional centrality
 *   - legislative_drafters: Secondary beneficiary (institutional/constrained) â procedural enactments are treated as self-validating
 *   - natural_law_jurists: Primary payer (moderate/constrained) â arguments systematically excluded from validity discourse
 *   - substantive_rights_movements: Secondary payer (organized/constrained) â moral claims channeled away from constitutional validity arguments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.56).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.62).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Positivist Reading of Constitutional Text Authority").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "constitutional_law/legal_theory").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '11d9d203-d9ca-4d6d-a8b0-c3567f54b058').
narrative_ontology:cs_kernel_codification('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', formalized).
narrative_ontology:cs_authority_grounding('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', expertise).
narrative_ontology:cs_interpretation_layer_present('11d9d203-d9ca-4d6d-a8b0-c3567f54b058').
narrative_ontology:cs_reading_relation('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', foundational, validity_from_enactment_procedures).
narrative_ontology:cs_axiom_status(validity_from_enactment_procedures, holdable).
narrative_ontology:cs_axiom_grounding('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', validity_from_enactment_procedures, conventional).
narrative_ontology:cs_axiom('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', foundational, law_morality_separation).
narrative_ontology:cs_axiom_status(law_morality_separation, holdable).
narrative_ontology:cs_axiom_grounding('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', law_morality_separation, conventional).
narrative_ontology:cs_reference_frame('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', enactment_procedural_supremacy).
narrative_ontology:cs_drift_state('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', contemporary_moral_constitutionalism_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('11d9d203-d9ca-4d6d-a8b0-c3567f54b058', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_positivist_academia).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legislative_drafters).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, natural_law_jurists).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, substantive_rights_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determine constitutional validity by reference to formal enactment procedures and institutional pedigree. They maintain the law-morality boundary through doctrine, treating moral arguments as non-cognizable in validity disputes while channeling them into interpretation or political venues.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Supplies the conceptual framework that grounds constitutional validity in social facts and enactment procedures. Their scholarly tradition, curricular control, and professional authority depend on the continued dominance of the social-source thesis in legal education.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_positivist_academia, beneficiary,
    organized, generational, mobile, global).

% Enact constitutional provisions and ordinary legislation. Their outputs are treated as self-validating when procedures are followed, insulating legislative choices from direct moral invalidity challenges and reducing the need to justify enactments against independent moral standards.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legislative_drafters, beneficiary,
    institutional, generational, constrained, national).

% Argue that constitutional validity must answer to objective moral truth. Their arguments are systematically ruled out of bounds in constitutional validity discourse, forcing them to adopt positivist methodological conventions or remain marginalized in mainstream adjudication and scholarship.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_jurists, payer,
    moderate, biographical, constrained, national).

% Seek constitutional protection for moral claims such as dignity, equality, and justice directly as questions of validity. The positivist constraint channels their arguments into political or interpretive venues, constraining their ability to challenge constitutional norms on overt moral grounds in court.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, substantive_rights_movements, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables constitutional order in morally plural societies without requiring agreement on comprehensive moral truth, by supplying a publicly verifiable procedure for identifying valid constitutional norms through enactment history and institutional acceptance.
% TRANSFER_FUNCTION: Moves authority over constitutional validity from moral philosophers, natural law advocates, and substantive justice movements to legal institutions and procedural gatekeepers; moral disagreement is channeled into political rather than juridical venues.
% ABSENT_VOICES: Natural law philosophers and theological ethicists who would ground constitutional validity in objective moral order are present in the academy but structurally excluded from constitutional validity discourse; their objections are treated as category errors or political philosophy rather than legal arguments.
% DISAPPEARANCE_RATIONALE: Courts would lose the methodological boundary for rejecting moral invalidity challenges; constitutional adjudication would need to engage directly with moral foundations; legal education and professional socialization would reorganize around moral philosophy and substantive justice rather than institutional pedigree.
% FOUNDING_PROBLEM: How to establish stable, predictable constitutional authority in pluralistic societies lacking shared comprehensive moral commitments; how to distinguish valid law from moral opinion or political preference.
% FOUNDING_PROBLEM_CORROBORATION: Political historians and sociologists of law attest that formal enactment procedures historically resolved authority crises in divided societies; critical legal scholars and moral philosophers attest that pluralism is better addressed through substantive dialogue than procedural exclusion, and that the framing of the problem was partly constructed to insulate state power from moral critique. Corroboration is split across beneficiary and payer lines.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.56, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.56) because the constraint genuinely coordinates pluralistic constitutional orders, but it imposes a real cost on moral-legal challengers by rendering their arguments categorically inadmissible in validity disputes. Suppression is moderate-high (0.62) because the constraint requires active judicial and academic enforcement to maintain the law-morality boundary; without this enforcement, natural law arguments would re-enter constitutional discourse. Theater ratio is low (0.18) because the positivist framework is a deeply held methodological commitment rather than a theatrical performance. Accessibility collapse is substantial (0.68) because once the positivist framework is adopted, natural law alternatives become conceptually invisible as legal arguments. Resistance is moderate (0.48) because natural law jurists and critical scholars actively contest the framework. The temporal series show extraction peaking mid-interval and slightly declining as inclusive positivism and moral constitutionalism exert pressure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (courts) and beneficiary seats (academia, drafters) experience the constraint as enabling genuine coordination and legal certainty. The payer seats (natural law jurists, rights movements) experience the same structure as an enforced exclusion of their foundational argumentative tools. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the structural combination of coordination and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Courts and legislative drafters sit near the beneficiary end: they control the procedural gates and their authority is enhanced by the constraint. Legal positivist academia also benefits intellectually and professionally. Natural law jurists and substantive rights movements sit near the target end: the constraint extracts from them by delegitimizing the moral foundations of their arguments within constitutional validity discourse. No override is needed because the structural derivation (beneficiary/victim + exit options) captures these relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction to be present for tangled_rope classification. Here, the coordination function is real: the positivist rule of recognition supplies a shared validity test in morally plural societies. The extraction is also real: the same structure that coordinates also excludes, transferring authority from moral claimants to institutional gatekeepers. If the coordination function were absent, the constraint would be a snare (pure exclusion); if the extraction were absent, it would be a rope (pure coordination). Neither pure label fits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inclusive_exclusive_boundary,
    'Does this reading represent exclusive positivism (morality strictly irrelevant to validity) or inclusive positivism (morality relevant only when recognized by social sources)?',
    'Doctrinal analysis of the specific judicial and scholarly texts instantiating this reading.',
    'If inclusive, the constraint may be a rope (coordination without victims); if exclusive, it is a tangled rope with real argumentative exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inclusive_exclusive_boundary, conceptual, 'Ambiguity between inclusive and exclusive positivism').

omega_variable(
    natural_law_exclusion_scope,
    'Do courts actually exclude all natural law arguments, or do they merely relabel them as interpretation or construction?',
    'Empirical survey of constitutional court decisions rejecting moral invalidity challenges.',
    'If natural law arguments persist under other doctrinal labels, the suppression metric overstates actual exclusion and the constraint is less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_exclusion_scope, empirical, 'Whether natural law arguments are fully excluded or doctrinally relabeled').

omega_variable(
    kernel_reading_stability,
    'Is the positivist reading stable, or does it collapse into originalism or inclusive positivism under pressure?',
    'Track scholarly drift and judicial citation patterns over the interval.',
    'If unstable, the constraint may function as a scaffold (transitional methodological support) rather than a persistent tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Structural stability of the positivist reading against sibling pressures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t16, constitutional_text_authority__positivist_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(cons_tr_t32, constitutional_text_authority__positivist_reading, theater_ratio, 32, 0.16).
narrative_ontology:measurement(cons_tr_t48, constitutional_text_authority__positivist_reading, theater_ratio, 48, 0.18).
narrative_ontology:measurement(cons_tr_t64, constitutional_text_authority__positivist_reading, theater_ratio, 64, 0.19).
narrative_ontology:measurement(cons_tr_t80, constitutional_text_authority__positivist_reading, theater_ratio, 80, 0.18).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cons_be_t16, constitutional_text_authority__positivist_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(cons_be_t32, constitutional_text_authority__positivist_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(cons_be_t48, constitutional_text_authority__positivist_reading, base_extractiveness, 48, 0.6).
narrative_ontology:measurement(cons_be_t64, constitutional_text_authority__positivist_reading, base_extractiveness, 64, 0.57).
narrative_ontology:measurement(cons_be_t80, constitutional_text_authority__positivist_reading, base_extractiveness, 80, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cons_su_t16, constitutional_text_authority__positivist_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(cons_su_t32, constitutional_text_authority__positivist_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(cons_su_t48, constitutional_text_authority__positivist_reading, suppression_requirement, 48, 0.68).
narrative_ontology:measurement(cons_su_t64, constitutional_text_authority__positivist_reading, suppression_requirement, 64, 0.66).
narrative_ontology:measurement(cons_su_t80, constitutional_text_authority__positivist_reading, suppression_requirement, 80, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__positivist_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_text_authority kernel. It decomposes the general concept of constitutional authority into a specific positivist claim about validity grounded in enactment procedures. See sibling constraints for originalist and living constitutionalist readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
