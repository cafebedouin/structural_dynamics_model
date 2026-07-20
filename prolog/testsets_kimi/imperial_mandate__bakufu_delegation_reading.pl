% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Imperial Mandate: Bakufu Delegation Reading
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   The Tokugawa bakufu and the broader samurai class organized Japanese
 *   political life around the doctrine that the emperor possessed supreme
 *   sacred legitimacy but delegated actual governance to the shogunate and
 *   warrior houses. This reading of the imperial mandate kernel bifurcated
 *   sovereignty: the emperor remained the ritual source of divine authority,
 *   while the shogun exercised administrative and military power. The
 *   constraint solved the coordination problem of legitimizing military rule
 *   within a cosmology that reserved ultimate sanctity to the imperial line,
 *   but it also extracted political agency from the imperial court and
 *   concentrated it in the samurai stratum. The constraint is historically
 *   bounded; it was ultimately repudiated by the Meiji Restoration, which
 *   restored the loyalist reading of unmediated imperial sovereignty.
 *
 * KEY AGENTS:
 *   - Bakufu shogunate (agenda-setter): Institutional power, constrained exit â enforces the delegation structure and suppresses imperial political involvement.
 *   - Samurai class (beneficiary): Organized power, identity-locked exit â derives governing legitimacy from the delegated mandate.
 *   - Imperial court (payer): Moderate power, trapped exit â retains ritual authority but is stripped of political governance.
 *   - Loyalist scholars (excluded): Moderate power, constrained exit â advocate direct imperial rule and are marginalized by the bakufu discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.68).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.7).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate: Bakufu Delegation Reading").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '5adacab8-44da-499e-9c09-1ba009055aaf').
narrative_ontology:cs_kernel_codification('5adacab8-44da-499e-9c09-1ba009055aaf', fixed_text).
narrative_ontology:cs_authority_grounding('5adacab8-44da-499e-9c09-1ba009055aaf', extraction).
narrative_ontology:cs_interpretation_layer_present('5adacab8-44da-499e-9c09-1ba009055aaf').
narrative_ontology:cs_reading_relation('5adacab8-44da-499e-9c09-1ba009055aaf', imperial_mandate__loyalist_restoration_reading, forecloses).
narrative_ontology:cs_axiom('5adacab8-44da-499e-9c09-1ba009055aaf', foundational, mandate_delegation_principle).
narrative_ontology:cs_axiom_status(mandate_delegation_principle, holdable).
narrative_ontology:cs_axiom_grounding('5adacab8-44da-499e-9c09-1ba009055aaf', mandate_delegation_principle, conventional).
narrative_ontology:cs_axiom('5adacab8-44da-499e-9c09-1ba009055aaf', foundational, samurai_governing_stratum).
narrative_ontology:cs_axiom_status(samurai_governing_stratum, holdable).
narrative_ontology:cs_axiom_grounding('5adacab8-44da-499e-9c09-1ba009055aaf', samurai_governing_stratum, conventional).
narrative_ontology:cs_reference_frame('5adacab8-44da-499e-9c09-1ba009055aaf', delegated_mandate_framework).
narrative_ontology:cs_drift_state('5adacab8-44da-499e-9c09-1ba009055aaf', late_tokugawa_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5adacab8-44da-499e-9c09-1ba009055aaf', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, bifurcated_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the actual apparatus of government, sets the terms of imperial delegation, and enforces the suppression of direct imperial political action. Derives its authority from the claim that the emperor has delegated governance to the warrior class, and is constrained by its own need to maintain that symbolic dependence.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate, agenda_setter,
    institutional, generational, constrained, national).

% Governs at central and local levels under the authority of the shogunate. Its identity as a legitimate ruling stratum is constituted by the doctrine that divine mandate has been delegated to the warrior houses through the imperial institution. Exit from this identity would mean surrendering political privilege and social role.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_class, beneficiary,
    organized, generational, identity_locked, national).

% Retains hereditary position as the sacred source of legitimacy but is structurally barred from exercising political authority. Performs ritual functions while the governing function is appropriated by the shogunate. Cannot abandon the imperial role, yet is denied the power conventionally associated with sovereignty.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court, payer,
    moderate, generational, trapped, national).

% Advocate for the unmediated exercise of imperial sovereignty and argue that delegating the mandate corrupts its sacred character. Their voices are marginalized from the legitimating discourse of the warrior government because their position threatens the bifurcated structure.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_scholars, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of legitimizing military rule in a political cosmology that reserves supreme sacred authority to a hereditary imperial line, by delegating administrative and coercive authority to the warrior class while preserving the emperor as the ritual source of ultimate legitimacy.
% TRANSFER_FUNCTION: Transfers actual political, military, and administrative authority from the imperial court to the shogunate and samurai retainers; transfers the symbolic legitimation function from the warrior rulers back to the emperor, who ritually confirms the shogun's office.
% ABSENT_VOICES: Loyalist scholars, radical court nobles, and kokugaku thinkers who argue that sovereignty must be exercised directly by the emperor and that delegation corrupts the mandate; they are excluded from the bakufu's legitimating discourse and often surveilled or suppressed.
% DISAPPEARANCE_RATIONALE: Without the delegation fiction, the shogunate could not claim non-coercive legitimacy; the samurai class's privileged governing status would lose its cosmological warrant; the imperial court would either be thrust into active governance or abolished; the entire constitutional order of Tokugawa Japan would reorganize around either direct imperial restoration or naked military domination.
% FOUNDING_PROBLEM: How to secure stable, legitimate governance for a warrior class that lacks traditional aristocratic divine sanction, in the aftermath of prolonged civil war, without destroying the imperial institution that provides cosmological continuity.
% FOUNDING_PROBLEM_CORROBORATION: Tokugawa legal documents (buke shohatto, kinchu narabini kuge shohatto) and contemporary samurai political writings attest the need to regulate warrior houses and derive authority from the court. Modern historians outside the samurai beneficiary class corroborate that the bifurcation solved the Sengoku legitimacy crisis, though they attest the problem was obsolete well before the Meiji Restoration ended the arrangement.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint systematically transfers governing authority from the imperial court to the warrior class while returning only symbolic legitimation. Suppression (0.70) reflects the active enforcement required to prevent the emperor and loyalist factions from reclaiming political power. Theater ratio (0.60) is elevated because the emperor's role became increasingly performative over the interval, with the governing function fully appropriated by the shogunate. Accessibility collapse (0.65) indicates that alternatives such as direct imperial rule were structurally suppressed though never fully erased. Resistance (0.55) captures persistent loyalist agitation and court factionalism. The measurement series share one time grid (0â30) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The bakufu shogunate and samurai class experience the constraint as a necessary constitutional order that prevents civil war and provides cosmological continuity; their directionality sits near the beneficiary end. The imperial court experiences the same structure as the active suppression of its political agency; its directionality sits near the full-target end. The engine computes this divergence from the structural data rather than from any authored type claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the bakufu_shogunate and samurai_class: they collect governing legitimacy and administrative authority through the delegation mechanism. Victim declaration maps to the imperial_court: it bears the cost of political exclusion and ritual reduction. The shogunate's exit is constrained because its authority depends on the imperial symbol it cannot safely abolish; the samurai class is identity-locked because its social role is constituted by this delegated mandate; the imperial court is trapped because its hereditary identity is fixed and exit from the ritual role is impossible. These declarations yield low d for beneficiaries and high d for the court, which the engine scales into effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating the coordination function (preventing civil war by providing a legitimacy mechanism for warrior rule) from the extraction function (stripping the emperor of political power). The founding problemâhow to legitimate military rule after the Sengoku civil warsâwas substantially solved by the mid-Tokugawa period, yet the delegation structure persisted and became increasingly theatrical. The R5 genealogy (founding_problem_status: dead, disappearance_verdict: world_rearranges) flags mandatrophy: the arrangement outlived its problem and persisted by inertia and enforcement until external crisis and repudiation resolved it. This historical trajectory distinguishes a tangled rope that aged into piton-like behavior from a pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delegation_naturalness,
    'Is the bifurcation of legitimacy and governance a necessary feature of East Asian political cosmology, or a historically constructed constraint designed to solve the specific problem of warrior-class legitimacy?',
    'Comparative analysis of other military regimes that adopted versus rejected symbolic delegation; philological study of classical Chinese and Japanese political texts for indigenous theories of divided sovereignty.',
    'If purely constructed, the constraint is a tangled_rope or snare; if cosmologically necessary within this tradition, it carries more mountain-like durability despite its social origin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_naturalness, conceptual, 'Whether the delegation doctrine is natural to the political tradition or contingent.').

omega_variable(
    imperial_suppression_modality,
    'Was the emperor''s political exclusion maintained primarily by active bakufu enforcement (surveillance, edicts, threat of force) or by the imperial court''s internalized acceptance of its purely ritual role?',
    'Archival study of bakufu-court correspondence, imperial diary entries, and records of court nobles'' political initiatives to determine the ratio of external coercion to internalized resignation.',
    'If suppression was primarily internalized, effective extraction exceeds the structural measure; if external, the constraint was more brittle and depended on continuous enforcement investment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imperial_suppression_modality, empirical, 'Structural versus internalized suppression of imperial political agency.').

omega_variable(
    kernel_reading_anachronism,
    'Does the bakufu delegation reading project modern Westphalian concepts of bifurcated sovereignty onto a pre-modern order that may not have recognized such a separation, rendering the constraint''s epsilon a misattribution?',
    'Philological recovery of indigenous terms for authority in primary sources; comparison with sibling loyalist reading''s textual evidence.',
    'If the reading is anachronistic, the structural classification may be tracking a modern historiographical construct rather than a historically operative constraint; both readings may be false projections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_anachronism, conceptual, 'Whether the delegation reading is a modern imposition on pre-modern political thought.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(impe_tr_t5, imperial_mandate__bakufu_delegation_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(impe_tr_t10, imperial_mandate__bakufu_delegation_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(impe_tr_t15, imperial_mandate__bakufu_delegation_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(impe_tr_t20, imperial_mandate__bakufu_delegation_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(impe_tr_t25, imperial_mandate__bakufu_delegation_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement(impe_tr_t30, imperial_mandate__bakufu_delegation_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(impe_be_t5, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(impe_be_t10, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(impe_be_t15, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(impe_be_t20, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(impe_be_t25, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(impe_be_t30, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(impe_su_t5, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(impe_su_t10, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(impe_su_t15, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(impe_su_t20, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(impe_su_t25, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(impe_su_t30, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, identity_coordination).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the imperial_mandate kernel, decomposed from the loyalist_restoration_reading per the epsilon-invariance principle. The two readings have incompatible epsilon profiles, different beneficiary-victim structures, and incompatible failure modes. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
