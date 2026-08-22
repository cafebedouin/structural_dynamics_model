% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Divine Right Legitimacy
 *   domain: political/constitutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the monarchical reading of the
 *   sovereign_legitimacy kernel: legitimate authority flows downward from the
 *   sovereign through inherited right, grounded in divine sanction,
 *   tradition, and bloodline continuity. It is one of three readings; the
 *   republican reading (authority from the people) and the constitutional
 *   hybrid reading (dual-sourced authority) are sibling constraints. The
 *   hereditary ruling class and aristocratic hierarchy are the structural
 *   beneficiaries, while subjects excluded from authority participation bear
 *   the costs. The constraint persists through active suppression of
 *   alternative legitimacy claims and through ritual continuity, though it
 *   remains vulnerable to succession contests.
 *
 * KEY AGENTS:
 *   - Hereditary sovereign: Primary agenda-setter and beneficiary (institutional/arbitrage) â holds and enforces the authority structure
 *   - Aristocratic hierarchy: Secondary beneficiary (powerful/constrained) â collects privilege through delegated status
 *   - Clerical establishment: Legitimacy validator and beneficiary (organized/constrained) â supplies divine sanction and receives institutional protection
 *   - Excluded subjects: Primary target (powerless/trapped) â bear extraction and exclusion from authority
 *   - Republican dissidents: Excluded voice (moderate/trapped) â suppressed alternative legitimacy claims
 *   - Constitutional theorists: Analytical observer (analytical/analytical) â compares legitimacy models from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.79).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.88).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Divine Right Legitimacy").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, 'fe09e048-1eee-4c30-9fd4-5444373e4b7e').
narrative_ontology:cs_kernel_codification('fe09e048-1eee-4c30-9fd4-5444373e4b7e', fixed_text).
narrative_ontology:cs_authority_grounding('fe09e048-1eee-4c30-9fd4-5444373e4b7e', lineage).
narrative_ontology:cs_interpretation_layer_present('fe09e048-1eee-4c30-9fd4-5444373e4b7e').
narrative_ontology:cs_reading_relation('fe09e048-1eee-4c30-9fd4-5444373e4b7e', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('fe09e048-1eee-4c30-9fd4-5444373e4b7e', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('fe09e048-1eee-4c30-9fd4-5444373e4b7e', foundational, authority_derives_from_divine_bloodline).
narrative_ontology:cs_axiom_status(authority_derives_from_divine_bloodline, holdable).
narrative_ontology:cs_axiom_grounding('fe09e048-1eee-4c30-9fd4-5444373e4b7e', authority_derives_from_divine_bloodline, theological).
narrative_ontology:cs_axiom('fe09e048-1eee-4c30-9fd4-5444373e4b7e', foundational, succession_legitimacy_requires_ritual_continuity).
narrative_ontology:cs_axiom_status(succession_legitimacy_requires_ritual_continuity, holdable).
narrative_ontology:cs_axiom_grounding('fe09e048-1eee-4c30-9fd4-5444373e4b7e', succession_legitimacy_requires_ritual_continuity, conventional).
narrative_ontology:cs_reference_frame('fe09e048-1eee-4c30-9fd4-5444373e4b7e', divine_right_bloodline_continuity).
narrative_ontology:cs_drift_state('fe09e048-1eee-4c30-9fd4-5444373e4b7e', age_of_democratic_revolutions, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('fe09e048-1eee-4c30-9fd4-5444373e4b7e', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_sovereign).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, clerical_establishment).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, excluded_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds supreme authority by inherited right and divine sanction, sets laws and succession rules, and receives loyalty and surplus from subjects. Cannot exit the structure without dissolving it; arbitrage is limited to playing factions against one another within the bloodline framework.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_sovereign, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, hereditary_sovereign, beneficiary).

% Derives privilege, land, and delegated authority from the sovereign, justified by bloodline proximity and tradition. Benefits from the structural exclusion of commoners from governance. Exit is constrained because aristocratic status depends on monarchical recognition and cannot be ported into republican frameworks without loss.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, constrained, national).

% Validates the sovereign's divine sanction through coronation, anointing, and doctrinal support. Receives state protection, tithes, and institutional authority in exchange for legitimizing the bloodline. Exit is constrained because its authority is fused with the monarchical framework; secularization dissolves its privilege.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, clerical_establishment, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of taxation, conscription, and total exclusion from political authority. Compliance is secured through suppression of alternative legitimacy claims and habitual deference. Exit options are effectively trapped: emigration is costly or forbidden, and internal resistance is treated as treason or heresy.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, excluded_subjects, payer,
    powerless, biographical, trapped, national).

% Advance alternative legitimacy models based on popular sovereignty or merit. Structurally excluded from the official legitimacy conversation; their movements are suppressed, their voices absent from coronation rituals and succession councils.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, republican_dissidents, excluded,
    moderate, biographical, trapped, national).

% Analyze the structural function of hereditary legitimacy and its alternatives from outside the system of divine right. They document the extraction mechanism and compare it to republican and hybrid models without being bound to any single throne.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, diffuse).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents violent succession contests by fixing leadership selection to bloodline continuity, establishing a single recognized authority source for a territory and reducing civil war among competing claimants.
% TRANSFER_FUNCTION: Transfers political authority, economic surplus, and social status from the broad subject population to a hereditary sovereign and aristocratic hierarchy, underwritten by claims of divine sanction and traditional ritual.
% ABSENT_VOICES: Republican claimants, popular-sovereignty movements, and subjects who reject bloodline primacy are structurally excluded from the legitimacy conversation; their alternative authority models are suppressed as treasonous or heretical.
% DISAPPEARANCE_RATIONALE: If monarchical legitimacy vanished overnight, succession rules would collapse, aristocratic privilege would lose its grounding, and authority would shift to alternative legitimacy claims such as popular sovereignty or constitutional delegation; the political order would fundamentally reorganize.
% FOUNDING_PROBLEM: Preventing violent succession contests and establishing a stable, recognized authority structure in a territory with multiple armed factions or competing claimants after the death of a ruler.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists outside the benefiting parties attest that hereditary succession reduced certain succession conflicts; republican and democratic theorists attest the problem is better solved by institutionalized rotation and popular consent, corroborating that the original problem is either solved by other means or persists in modified form.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.79, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.79) because the arrangement systematically transfers authority and surplus to a narrow bloodline. Suppression is higher (0.88) because the constraint's persistence depends on actively excluding republican and democratic alternatives, criminalizing dissent, and controlling legitimacy discourse. Theater ratio is moderate (0.35): coronation rituals and divine symbolism are partly performative, but the enforcement apparatus (guards, treason laws, state religion) is functional. Accessibility collapse is high (0.80) because alternative legitimacy models are rendered cognitively and politically inaccessible to most subjects. Resistance is moderate (0.55): revolts and republican movements exist but are suppressed. The measurement series tracks intensifying extraction and suppression as Enlightenment and democratic ideas emerge over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The sovereign and aristocratic seats experience the constraint as natural order and divine duty; the exclusion of commoners is framed as protecting harmony. The excluded subject seat experiences the same structure as arbitrary extraction and blocked advancement. The engine computes this divergence from the same structural data: low directionality for the bloodline beneficiaries, high directionality for the trapped payer population.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary sovereign sits near the full-beneficiary end (subsidized by the constraint), as do the aristocratic hierarchy and clerical establishment. Excluded subjects sit near the full-target end: they are identity-locked and trapped, with suppression amplified by national scope. Republican dissidents are excluded rather than coordinated; their suppression is the enforcement object. The observer seat sits at analytical distance with no directionality stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating the genuine coordination function (succession stability) from the extraction function (bloodline rent). A pure coordination reading would ignore the victim set; a pure extraction reading would ignore the civil-war-prevention mechanism. Declaring both beneficiaries and victims, and requiring active enforcement, forces the tangled_rope classification rather than allowing rope-washing or snare-inflation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_relation,
    'This constraint is the monarchical reading of the sovereign_legitimacy kernel; how would sibling readings (republican, constitutional_hybrid) reassign beneficiaries and victims?',
    'Comparison across the constraint family: the republican reading would eliminate hereditary beneficiaries and dissolve aristocratic extraction; the constitutional hybrid reading would split the beneficiary seat between crown and elected institutions.',
    'Determines whether the extraction is intrinsic to the kernel or a function of this specific reading''s authority distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relation, conceptual, 'Sibling reading structural delta for monarchical legitimacy').

omega_variable(
    coordination_vs_extraction_nature,
    'Is the hereditary succession mechanism primarily a coordination device preventing civil war, or primarily an extraction mechanism enriching a bloodline?',
    'Historical counterfactual analysis comparing succession-conflict rates in hereditary monarchies versus elective monarchies or republics with institutionalized leadership rotation.',
    'If coordination dominates, the constraint sits near the rope/tangled_rope boundary; if extraction dominates, it is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_nature, empirical, 'Ambiguity between succession coordination and class extraction').

omega_variable(
    suppression_internalization,
    'Is subject compliance secured primarily by structural coercion (guards, treason laws) or by internalized deference (divine right ideology, habitual obedience)?',
    'Measure resistance recurrence after structural suppression is removed; if deference persists post-revolution, suppression was partially internalized.',
    'Internalized suppression raises effective extraction beyond the structural measure; post-monarchical cultures may retain the constraint''s behavioral pattern without its formal institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sove_tr_t25, sovereign_legitimacy__monarchical_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__monarchical_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(sove_tr_t75, sovereign_legitimacy__monarchical_reading, theater_ratio, 75, 0.32).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__monarchical_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(sove_be_t25, sovereign_legitimacy__monarchical_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__monarchical_reading, base_extractiveness, 50, 0.74).
narrative_ontology:measurement(sove_be_t75, sovereign_legitimacy__monarchical_reading, base_extractiveness, 75, 0.77).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__monarchical_reading, base_extractiveness, 100, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(sove_su_t25, sovereign_legitimacy__monarchical_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__monarchical_reading, suppression_requirement, 50, 0.82).
narrative_ontology:measurement(sove_su_t75, sovereign_legitimacy__monarchical_reading, suppression_requirement, 75, 0.85).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__monarchical_reading, suppression_requirement, 100, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
