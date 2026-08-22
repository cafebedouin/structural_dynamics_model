% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Meaning
 *   domain: constitutional/law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the originalist_reading of the
 *   us_constitution_meaning kernel. It holds that the meaning of the U.S.
 *   Constitution was fixed at the moment of ratification (and amendment) by
 *   its public meaning, and that judges are bound to discover and apply that
 *   fixed meaning. Sibling readings include living_constitutionalist_reading
 *   (meaning evolves with social circumstances) and positivist_reading
 *   (validity derives from enactment procedure). The structural delta for
 *   this reading is high suppression of non-originalist methodologies, with
 *   concentrated benefits flowing to counter-majoritarian constraint
 *   advocates and concentrated costs borne by rights claimants whose claims
 *   lack eighteenth-century historical support. The claim/metric independence
 *   principle is observed: the constraint is claimed as tangled_rope (genuine
 *   interpretive coordination coupled with asymmetric extraction) and the
 *   metrics are authored to reflect substantial extraction and suppression
 *   without tuning to match the claim.
 *
 * KEY AGENTS:
 *   - counter_majoritarian_advocates: Primary beneficiary (organized/institutional power) â collects legitimacy and doctrinal victories from the constraint's operation
 *   - originalist_judiciary: Agenda-setter and secondary beneficiary (institutional/constrained exit) â administers the constraint and gains methodological clarity while sacrificing interpretive flexibility
 *   - unsupported_rights_claimants: Primary target (powerless/trapped) â bears the extraction through foreclosed constitutional claims
 *   - non_originalist_jurists: Secondary target (institutional/constrained) â bears suppression of preferred methodologies
 *   - constitutional_historians: Analytical observer (moderate/analytical exit) â supplies evidence without capturing gains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.72).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.78).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Reading of Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional/law/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '0f77e821-66e8-4f64-a322-18ace38a6a4f').
narrative_ontology:cs_kernel_codification('0f77e821-66e8-4f64-a322-18ace38a6a4f', fixed_text).
narrative_ontology:cs_authority_grounding('0f77e821-66e8-4f64-a322-18ace38a6a4f', lineage).
narrative_ontology:cs_interpretation_layer_present('0f77e821-66e8-4f64-a322-18ace38a6a4f').
narrative_ontology:cs_reading_relation('0f77e821-66e8-4f64-a322-18ace38a6a4f', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('0f77e821-66e8-4f64-a322-18ace38a6a4f', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('0f77e821-66e8-4f64-a322-18ace38a6a4f', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('0f77e821-66e8-4f64-a322-18ace38a6a4f', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('0f77e821-66e8-4f64-a322-18ace38a6a4f', foundational, historical_public_meaning_recoverable).
narrative_ontology:cs_axiom_status(historical_public_meaning_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('0f77e821-66e8-4f64-a322-18ace38a6a4f', historical_public_meaning_recoverable, empirically_contingent).
narrative_ontology:cs_reference_frame('0f77e821-66e8-4f64-a322-18ace38a6a4f', ratification_public_meaning).
narrative_ontology:cs_drift_state('0f77e821-66e8-4f64-a322-18ace38a6a4f', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0f77e821-66e8-4f64-a322-18ace38a6a4f', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, unsupported_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, non_originalist_jurists).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, fixed_meaning_thesis).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, counter_majoritarian_difficulty_resolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legal and political actors who argue that judicial review must be constrained by fixed historical meaning to prevent arbitrary governance. They benefit from the institutionalization of originalism through judicial appointments, academic prestige, and doctrinal victories that limit progressive constitutional claims.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_advocates, beneficiary,
    organized, generational, mobile, national).

% Federal judges and justices who apply an originalist methodology. They are bound to discover and apply historical public meaning rather than contemporary values. They gain methodological clarity and institutional legitimacy but lose the flexibility to adapt doctrine to unforeseen social problems.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, originalist_judiciary, beneficiary).

% Individuals and groups bringing constitutional claims that lack clear analogues in eighteenth-century law and practice. Their claims are disadvantaged at the threshold of constitutional analysis because the interpretive framework treats historical absence as dispositive.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, unsupported_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Judges, scholars, and practitioners who view constitutional interpretation as legitimately evolving. Their preferred methodologies are treated as illegitimate within an originalist framework; they dissent, but originalist majorities control outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, non_originalist_jurists, payer,
    institutional, biographical, constrained, national).

% Academic proponents of evolving constitutional meaning and progressive constitutionalism. Their frameworks are structurally excluded from controlling constitutional doctrine when originalism dominates the bench.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_scholars, excluded,
    organized, generational, constrained, national).

% Historians who research founding-era meaning. Their findings are instrumentalized by originalist and non-originalist litigants alike, but they do not directly benefit from the constraint's operation.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, constitutional_historians, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates constitutional interpretation by fixing semantic content to a historical moment, reducing judicial discretion and providing an inter-temporal anchor for doctrinal stability.
% TRANSFER_FUNCTION: Moves interpretive authority from evolutionary jurists and unsupported rights claimants to originalist institutions and counter-majoritarian advocates; transfers doctrinal legitimacy toward historical fidelity and away from contemporary moral claims.
% ABSENT_VOICES: Living constitutionalist scholars and unsupported rights claimants are formally heard but structurally foreclosed; their methodological premises are treated as outside legitimate constitutional argument.
% DISAPPEARANCE_RATIONALE: Constitutional doctrine would shift toward evolutionary frameworks, previously foreclosed rights claims would become justiciable on non-historical grounds, and the institutional power of originalist networks would decline.
% FOUNDING_PROBLEM: Judicial arbitrariness and the counter-majoritarian difficulty: unelected judges exercising legislative power by reading their own values into open-textured constitutional language.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and institutions attest the problem remains live. Civil rights organizations and living constitutionalist jurists outside the beneficiary set attest that the problem has been superseded by democratic maturation and that originalism now functions to freeze historical hierarchies; empirical studies of judicial behavior corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically disadvantages entire categories of constitutional claims based on historical absence rather than contemporary moral weight or textual ambiguity. Suppression (0.78) is higher because the constraint's persistence depends on actively excluding non-originalist methodologies from controlling doctrine through appointment politics, academic gatekeeping, and doctrinal foreclosure. Theater_ratio (0.45) reflects the growing performative dimension of originalist jurisprudence, where historical evidence is selectively instrumentalized to reach predetermined outcomes. Accessibility_collapse (0.75) captures the near-total foreclosure of non-originalist argument once originalism is accepted as the legitimate frame. Resistance (0.70) reflects sustained academic and jurisprudential opposition.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judiciary and counter-majoritarian advocates experience the constraint as a necessary bulwark against arbitrary judicial power and a source of methodological clarity. Unsupported rights claimants and non-originalist jurists experience the same structure as an extractive mechanism that freezes eighteenth-century values into present law. The engine computes this divergence from the structural asymmetry in power, exit options, and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Counter-majoritarian advocates and the originalist judiciary sit near the beneficiary end (low d): the constraint subsidizes their institutional position and methodological commitments. Unsupported rights claimants sit near the full-target end (high d): the constraint extracts from them by foreclosing their claims. Non-originalist jurists also sit at high d: their interpretive autonomy is suppressed. The engine will compute high effective extraction for the payer seats and low or negative extraction for the beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â judicial arbitrariness â may have been genuine at the constraint's emergence. However, the constraint now shows signs of mandatrophy: the originalist methodology persists and intensifies even where it produces counter-intuitive outcomes that lack historical certainty (e.g., ambiguous founding-era evidence treated as dispositive). The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) flags the possibility that the coordination function has atrophied while the extraction function has intensified. If the problem were truly live and the coordination genuine, resistance would be lower and theater_ratio lower than observed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (originalist_reading) of the contested kernel us_constitution_meaning. How would classification change if the kernel were read through living_constitutionalist_reading or positivist_reading?',
    'Kernel decomposition is already performed; compare this JSON with sibling constraint stories for the same kernel.',
    'Living constitutionalist reading would likely reclassify as tangled_rope or rope with a different victim set (originalist institutions becoming payers); positivist reading might reclassify as rope or mountain depending on its extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega documenting this reading''s place in the kernel contest.').

omega_variable(
    historical_meaning_indeterminacy,
    'Is the historical record capable of yielding a single determinate public meaning for constitutional provisions, or does it support irreducible semantic pluralism?',
    'Empirical historical and corpus linguistics research into specific constitutional clauses; consensus among historians of the founding era.',
    'If indeterminacy is high, originalism''s coordination function collapses into arbitrary selection among historical possibilities, raising extraction and theater_ratio; if determinacy is high, the coordination function is validated and extraction is better justified as necessary cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_indeterminacy, empirical, 'Whether historical public meaning is sufficiently determinate to support the constraint''s coordination claim.').

omega_variable(
    originalism_political_cover,
    'Does originalism function primarily as a genuine interpretive methodology, or has it become a performative cover for substantive political outcomes that would be reached by other means?',
    'Outcome-pattern analysis: compare originalist judicial outcomes across partisan lines with the outcomes predicted by non-originalist methodologies; measure theater_ratio and extraction accumulation over time.',
    'If primarily performative cover, classification shifts toward snare (coordination story is cover); if genuinely methodological, tangled_rope remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_political_cover, conceptual, 'Ambiguity between genuine methodology and political cover story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_con_orig_tr_t0, us_constitution_meaning__originalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_con_orig_tr_t8, us_constitution_meaning__originalist_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(us_con_orig_tr_t16, us_constitution_meaning__originalist_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(us_con_orig_tr_t24, us_constitution_meaning__originalist_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(us_con_orig_tr_t32, us_constitution_meaning__originalist_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(us_con_orig_tr_t40, us_constitution_meaning__originalist_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(us_con_orig_be_t0, us_constitution_meaning__originalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_con_orig_be_t8, us_constitution_meaning__originalist_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(us_con_orig_be_t16, us_constitution_meaning__originalist_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(us_con_orig_be_t24, us_constitution_meaning__originalist_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(us_con_orig_be_t32, us_constitution_meaning__originalist_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(us_con_orig_be_t40, us_constitution_meaning__originalist_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(us_con_orig_su_t0, us_constitution_meaning__originalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(us_con_orig_su_t8, us_constitution_meaning__originalist_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(us_con_orig_su_t16, us_constitution_meaning__originalist_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(us_con_orig_su_t24, us_constitution_meaning__originalist_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(us_con_orig_su_t32, us_constitution_meaning__originalist_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(us_con_orig_su_t40, us_constitution_meaning__originalist_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the us_constitution_meaning kernel. The kernel decomposes into at least three structurally distinct constraints (originalist, living constitutionalist, positivist) because the natural-language label 'constitutional meaning' conflates distinct claims about fixation, evolution, and validity. Each reading has a different epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
