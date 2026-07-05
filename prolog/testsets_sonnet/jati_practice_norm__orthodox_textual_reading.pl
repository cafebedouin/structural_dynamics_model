% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Orthodox Textual Reading: Jati Boundaries as Fixed Scriptural Varna Order
 *   domain: social/religious/political_economy
 *
 * SUMMARY:
 *   This story instantiates the orthodox textual reading of the
 *   jati_practice_norm kernel: the claim that jati boundaries are direct,
 *   fixed derivations of the scriptural varna framework, such that
 *   occupational assignment, endogamy, and ritual status form an immutable
 *   hierarchy and any deviation constitutes pollution requiring correction.
 *   This is one of three structurally distinct readings of the same kernel —
 *   the colonial_census_reading (external administrative reification) and the
 *   localized_practice_reading (continuous local renegotiation) are separate
 *   constraints with their own ε values, not alternative measurements of this
 *   one. Under the orthodox textual reading specifically, extraction is high
 *   because the framework assigns a fixed set of jatis to the most degrading
 *   and worst-compensated labor and forecloses exit by treating occupation as
 *   constitutive of ritual identity rather than incidental to it; the theater
 *   ratio rises over the measured interval as legal and social pressure
 *   increasingly forces ritual maintenance into more purely performative
 *   registers (temple-entry symbolism, purification rites) even as the
 *   underlying economic and marital exclusions persist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.87).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.91).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Orthodox Textual Reading: Jati Boundaries as Fixed Scriptural Varna Order").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social/religious/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, 'd29d545a-a0cf-4f1e-98ef-cc2add8d98f7').
narrative_ontology:cs_kernel_codification('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', fixed_text).
narrative_ontology:cs_authority_grounding('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', lineage).
narrative_ontology:cs_interpretation_layer_present('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7').
narrative_ontology:cs_reading_relation('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_reading_relation('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_axiom('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', foundational, scriptural_hierarchy_is_ontologically_fixed).
narrative_ontology:cs_axiom_status(scriptural_hierarchy_is_ontologically_fixed, holdable).
narrative_ontology:cs_axiom_grounding('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', scriptural_hierarchy_is_ontologically_fixed, theological).
narrative_ontology:cs_axiom('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', foundational, occupation_constitutes_ritual_status).
narrative_ontology:cs_axiom_status(occupation_constitutes_ritual_status, holdable).
narrative_ontology:cs_axiom_grounding('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', occupation_constitutes_ritual_status, theological).
narrative_ontology:cs_axiom('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', secondary, boundary_transgression_requires_ritual_remedy).
narrative_ontology:cs_axiom_status(boundary_transgression_requires_ritual_remedy, holdable).
narrative_ontology:cs_axiom_grounding('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', boundary_transgression_requires_ritual_remedy, conventional).
narrative_ontology:cs_reference_frame('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', vedic_varna_cosmological_order).
narrative_ontology:cs_drift_state('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d29d545a-a0cf-4f1e-98ef-cc2add8d98f7', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmanical_priesthood).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, landowning_dominant_jatis).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, orthodox_ritual_authorities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, manual_scavenging_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, leatherworking_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, sanitation_labor_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, intercaste_marriage_couples).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, varna_scriptural_immutability_doctrine).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, ritual_purity_hierarchy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and transmits the scriptural varna framework, adjudicates disputes over ritual status, and performs the rites that certify purity and pollution. Holds interpretive monopoly over the kernel text, collects fees and deference for ritual services, and occupies the apex position the framework itself declares fixed and unchangeable.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmanical_priesthood, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, brahmanical_priesthood, beneficiary).

% Control agricultural land and local labor markets; the varna-derived jati order supplies a captive, categorically-bound labor pool for the most undesirable and lowest-paid work. Can invoke ritual-pollution rhetoric to enforce compliance without needing direct coercive apparatus, since refusal risks social excommunication of the laborer, not the landowner.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, landowning_dominant_jatis, beneficiary,
    powerful, generational, mobile, regional).

% Maintain temple entry rules, commensality restrictions, and purification rituals that operationalize the scriptural hierarchy in daily practice. Enforce boundary maintenance through social sanction — exclusion from temples, wells, and shared eating — treating any deviation as pollution requiring ritual remedy.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, orthodox_ritual_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Assigned hereditary occupation classified as maximally polluting under the scriptural schema; barred from temple entry, shared water sources, and commensality with higher jatis. Occupational mobility is blocked because the scriptural framework treats the occupation itself as constitutive of ritual status, not merely descriptive of it — leaving the trade forecloses caste identity but rarely improves material position given continued social exclusion.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, manual_scavenging_jatis, payer,
    powerless, generational, trapped, local).

% Hereditarily assigned work with dead animal hides, classified as polluting by scriptural injunction. Face residential segregation and exclusion from common facilities; economic exit is nominally available but socially punished, and children inherit the same ascribed status regardless of the family's actual occupation in a given generation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, leatherworking_jatis, payer,
    powerless, generational, trapped, local).

% Perform latrine-cleaning and waste-disposal work justified by the scriptural framework as their appropriate ritual station. Attempts to enter other occupations or intermarry are treated as violations requiring purification of the community, not accommodation of the individual.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, sanitation_labor_jatis, payer,
    powerless, generational, trapped, local).

% Violate the endogamy boundary the scriptural framework treats as load-bearing for the entire varna order. Face social boycott, sometimes violence, and family excommunication; the orthodox reading has no mechanism to accommodate boundary-crossing except reclassification as expelled or fallen, which the framework declares a form of ritual death rather than a permissible transition.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, intercaste_marriage_couples, payer,
    powerless, biographical, constrained, local).

% Argue the scriptural texts are being read selectively or literalistically to justify a social order the texts themselves do not mandate in fixed hereditary form. Excluded from orthodox ritual authority structures and denied standing to reinterpret the kernel text within mainstream orthodox institutions.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, reformist_religious_movements, excluded,
    organized, generational, constrained, national).

% Adjudicate cases where the orthodox reading collides with anti-discrimination law. Document testimony from affected jatis and religious authorities but operate outside the scriptural interpretive community, applying a secular legal framework that does not recognize ritual pollution as a valid basis for exclusion.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, transmitted schema for assigning ritual status, occupation, and marriage eligibility across a very large population without requiring case-by-case negotiation — in principle reducing disputes over social position by appeal to a fixed, shared text.
% TRANSFER_FUNCTION: Moves labor, ritual deference, and social standing from jatis assigned polluting occupations to jatis positioned as ritually pure or dominant; moves enforcement cost from any centralized authority onto diffuse community-level social sanction (exclusion, boycott, violence).
% ABSENT_VOICES: Reformist and heterodox interpreters who argue the scriptural corpus supports occupational fluidity or metaphorical (not hereditary) reading of varna are excluded from orthodox interpretive authority. The jatis assigned polluting work are rarely granted standing to reinterpret their own scriptural classification; their objections are treated as pollution-adjacent rather than as valid textual argument.
% DISAPPEARANCE_RATIONALE: If the orthodox textual reading's authority collapsed overnight, occupational assignment would no longer carry scriptural sanction, temple and commensality exclusions would lose their doctrinal justification, and intercaste marriage would no longer trigger ritual-death framing — labor markets, marriage markets, and local social hierarchies organized around hereditary ritual status would need to re-form on some other basis (economic, legal, or negotiated local practice).
% FOUNDING_PROBLEM: Claimed to solve the problem of social and cosmological order: assigning every person a fixed station consistent with dharmic duty, preventing status competition and instability by grounding hierarchy in an unchangeable scriptural source rather than contestable local negotiation.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox ritual authorities attest the founding problem (cosmological and social order) remains live and the scriptural reading is its necessary solution. Constitutional courts, reformist religious movements, and independent historians of religion attest the problem framing itself instrumentalizes a labor and status allocation system, and that the 'fixed and unchangeable' claim is a much later doctrinal hardening rather than an original, static feature of the source texts — corroboration for the doctrine's own genealogy claim comes only from within the orthodox interpretive tradition itself.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.87, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.87) reflects the concentration of labor extraction and status subordination onto specific hereditary groups (manual scavenging, leatherworking, sanitation) whose exit is blocked not by law but by the doctrine's own claim that occupation IS ritual status — leaving the occupation doesn't exit the categorization, it just adds 'fallen' or 'expelled' to it. Suppression (0.91) is authored high and rising because enforcement is diffuse, socially distributed, and largely self-reinforcing (boycott, exclusion from wells/temples, violence) rather than centrally administered — which makes it harder to dismantle than a state-enforced rule, since no single office can simply repeal it. Theater ratio starts moderate (0.20) and climbs to 0.42 as legal reform and urbanization strip away much of the doctrine's original economic function while orthodox authorities intensify ritual performance to compensate for eroding practical enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmanical priesthood and orthodox ritual authorities sit at the beneficiary/agenda-setter end: they hold interpretive monopoly over the kernel text and collect deference, fees, and status from its operation, with arbitrage-grade exit (their own position is never the one classified as polluting). Landowning dominant jatis benefit materially from a captive, categorically-bound labor supply without needing direct coercive apparatus of their own — the doctrine does the enforcement work for them via social sanction. The payer seats (manual scavenging, leatherworking, sanitation jatis) are trapped specifically because the framework declares occupation and ritual status coextensive, which is a stronger lock than ordinary economic immobility — even successful economic exit does not neutralize the ascribed status. Intercaste couples are constrained rather than fully trapped because their violation is an act rather than an ascribed birth status, but the doctrine treats the transgression as requiring 'ritual death' reclassification rather than offering a legitimate transitional category.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing (cosmological/social order via fixed station) is contested precisely because the orthodox reading treats the doctrine's function as permanently live and unchangeable — that is the doctrine's own defining move. But corroboration from outside the beneficiary set (historians of religion, constitutional courts, reformist movements within the same textual tradition) indicates the strict hereditary-occupation reading is a later doctrinal hardening layered onto more fluid textual and historical material, not an original static feature. This divergence between the claimed function (cosmological order) and the corroborated function (labor and status allocation with self-reinforcing enforcement) is exactly the mandatrophy signal: a constraint whose founding justification has been retrospectively fixed to prevent the arrangement from being read as revisable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fixity_vs_doctrinal_hardening,
    'Do the scriptural source texts themselves mandate a fixed, hereditary, occupation-bound jati hierarchy, or does the orthodox reading impose a later doctrinal hardening onto more fluid or contested source material?',
    'Philological and historical analysis of the textual corpus across periods, comparing earliest strata to classical commentarial layers, cross-referenced against independent (non-orthodox) textual scholarship and epigraphic/inscriptional evidence of actual occupational mobility in different historical periods.',
    'If the fixity claim is a later hardening, the orthodox reading''s ''emerges naturally from scripture'' framing is substantially weakened, supporting classification as constructed extraction rather than transmitted natural order — this bears directly on whether the constraint should be read as closer to a false-summit mountain claim (natural/textual necessity) or as a snare wearing textual legitimation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_fixity_vs_doctrinal_hardening, empirical, 'Whether scriptural fixity is original or a later doctrinal accretion.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three readings of the jati_practice_norm kernel (orthodox_textual, colonial_census, localized_practice) best characterizes the actual generative mechanism of jati boundaries in a given historical and regional context, and can a single social formation be governed by more than one reading simultaneously?',
    'Comparative regional historiography examining whether jati boundary rigidity correlates more strongly with proximity to orthodox textual/priestly institutions, with colonial administrative classification regimes, or with local economic/political renegotiation dynamics, across multiple regions and periods.',
    'If regions show strong correlation with only one reading, that reading dominates locally and the others are less applicable there; if multiple readings operate simultaneously in the same region (e.g., orthodox doctrine providing legitimating vocabulary for what is functionally continuous local renegotiation, further hardened by colonial census categories), all three constraints may be co-present and mutually reinforcing rather than mutually exclusive alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the three kernel readings are mutually exclusive characterizations or co-present layers.').

omega_variable(
    reformist_reinterpretation_standing,
    'Do reformist and heterodox interpretive traditions carry genuine textual authority to challenge the orthodox reading from within the same scriptural corpus, or are they external critiques with no standing inside the tradition the orthodox reading claims to represent?',
    'Analysis of whether reformist reinterpretations draw on textual material contemporaneous with or predating the orthodox commentarial tradition, versus material developed primarily as external counter-argument.',
    'If reformist readings have genuine internal textual standing, the orthodox reading''s claim to sole legitimate transmission is weakened, supporting the doctrinal-hardening hypothesis and strengthening the case for reclassifying the orthodox reading as constructed/extractive rather than naturally transmitted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_reinterpretation_standing, conceptual, 'Whether reformist reinterpretation has genuine textual standing or is purely external critique.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(jati_tr_t0, observed).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__orthodox_textual_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(jati_tr_t20, observed).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__orthodox_textual_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(jati_tr_t40, observed).
narrative_ontology:measurement(jati_tr_t60, jati_practice_norm__orthodox_textual_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement_basis(jati_tr_t60, observed).
narrative_ontology:measurement(jati_tr_t80, jati_practice_norm__orthodox_textual_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement_basis(jati_tr_t80, observed).
narrative_ontology:measurement(jati_tr_t100, jati_practice_norm__orthodox_textual_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(jati_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement_basis(jati_be_t0, observed).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement_basis(jati_be_t20, observed).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement_basis(jati_be_t40, observed).
narrative_ontology:measurement(jati_be_t60, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 60, 0.86).
narrative_ontology:measurement_basis(jati_be_t60, observed).
narrative_ontology:measurement(jati_be_t80, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 80, 0.87).
narrative_ontology:measurement_basis(jati_be_t80, observed).
narrative_ontology:measurement(jati_be_t100, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 100, 0.87).
narrative_ontology:measurement_basis(jati_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(jati_su_t0, observed).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement_basis(jati_su_t20, observed).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement_basis(jati_su_t40, observed).
narrative_ontology:measurement(jati_su_t60, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement_basis(jati_su_t60, observed).
narrative_ontology:measurement(jati_su_t80, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 80, 0.9).
narrative_ontology:measurement_basis(jati_su_t80, observed).
narrative_ontology:measurement(jati_su_t100, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 100, 0.91).
narrative_ontology:measurement_basis(jati_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__orthodox_textual_reading, 0.08).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, colonial_census_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, localized_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'jati boundaries and their origin.' orthodox_textual_reading claims scriptural derivation with fixed hereditary occupation-status binding (high ε, snare); colonial_census_reading claims administrative reification via external governance apparatus (distinct ε, distinct mechanism, distinct beneficiary set — colonial administrators and successor state bureaucracies rather than religious authorities); localized_practice_reading claims the boundaries are continuously renegotiated local coordination norms with proliferation rather than fixity (likely much lower ε, closer to rope/tangled_rope). All three are linked via affects_constraints because each reading's persistence affects the plausibility and enforcement cost of the others — e.g., colonial administrative fixing of categories may have hardened what the orthodox reading treats as eternally fixed, and both may suppress the localized_practice_reading's capacity to actually renegotiate boundaries on the ground.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
