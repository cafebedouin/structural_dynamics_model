% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native Generational Transmission as Sole Criterion for Living Language Status
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint story captures the 'native generation reading' of the
 *   contested kernel 'living language status.' It asserts that a language is
 *   living only if transmitted generationally as a mother tongue in daily
 *   life, explicitly framing liturgical recitation as 'preservation of a
 *   corpse.' This reading is enforced by state language policy institutions
 *   and serves as a pillar of secular nationalist legitimacy. The constraint
 *   has a genuine coordination function (providing an operational definition
 *   for policy) but also extracts legitimacy and resources from
 *   liturgical-only communities, making it a tangled rope. The metrics
 *   reflect moderate base extractiveness that has risen over a century as the
 *   definition became entrenched in law and education, rising suppression as
 *   liturgical communities are denied recognition, and a modest but growing
 *   theater ratio as the coordination rationale (policy operability) is
 *   increasingly performed while the extractive core solidifies.
 *
 * KEY AGENTS:
 *   - secular_nationalist_movement: Primary beneficiary (organized/mobile) — gains legitimacy via linguistic sovereignty
 *   - liturgical_only_communities: Primary victim (moderate/identity_locked) — framed as preserving death, denied resources
 *   - state_language_policy_institutions: Agenda setter (institutional/constrained) — enforces the definition, allocates resources
 *   - sociolinguists: Observer (analytical/analytical) — provides competing evidence but no policy power
 *   - literary_continuity_advocates: Excluded (moderate/constrained) — sibling reading, marginalized in policy
 *   - liturgical_preservation_advocates: Excluded (moderate/identity_locked) — sibling reading, existentially threatened by the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.55).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.7).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native Generational Transmission as Sole Criterion for Living Language Status").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '9b9d7864-9148-44ba-88df-b22242a1b1da').
narrative_ontology:cs_kernel_codification('9b9d7864-9148-44ba-88df-b22242a1b1da', distributed).
narrative_ontology:cs_authority_grounding('9b9d7864-9148-44ba-88df-b22242a1b1da', extraction).
narrative_ontology:cs_interpretation_layer_present('9b9d7864-9148-44ba-88df-b22242a1b1da').
narrative_ontology:cs_reading_relation('9b9d7864-9148-44ba-88df-b22242a1b1da', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('9b9d7864-9148-44ba-88df-b22242a1b1da', living_language_status__literary_continuity_reading, forecloses).
narrative_ontology:cs_axiom('9b9d7864-9148-44ba-88df-b22242a1b1da', foundational, native_generational_transmission_necessary_for_vitality).
narrative_ontology:cs_axiom_status(native_generational_transmission_necessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('9b9d7864-9148-44ba-88df-b22242a1b1da', native_generational_transmission_necessary_for_vitality, empirically_contingent).
narrative_ontology:cs_axiom('9b9d7864-9148-44ba-88df-b22242a1b1da', secondary, liturgical_recitation_is_not_vitality).
narrative_ontology:cs_axiom_status(liturgical_recitation_is_not_vitality, holdable).
narrative_ontology:cs_axiom_grounding('9b9d7864-9148-44ba-88df-b22242a1b1da', liturgical_recitation_is_not_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('9b9d7864-9148-44ba-88df-b22242a1b1da', native_transmission_norm).
narrative_ontology:cs_drift_state('9b9d7864-9148-44ba-88df-b22242a1b1da', contemporary_nationalist_policy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9b9d7864-9148-44ba-88df-b22242a1b1da', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movement).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, language_vitality_requires_native_transmission).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, liturgical_recitation_is_not_living_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains legitimacy and political sovereignty by defining the national language as living only through native generational transmission; uses this definition to claim authentic representation of the nation and to direct state resources toward national language programs.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movement, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, secular_nationalist_movement, agenda_setter).

% Maintain the language solely through liturgical recitation and study; denied living language status and associated resources (funding, education, official recognition) because they do not transmit it as a mother tongue in daily life; their identity is fused with the liturgical use of the language, making exit from the constraint's framing nearly impossible.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    moderate, generational, identity_locked, national).

% Administer language policy, allocate funding, and set educational curricula based on the native-generation definition; enforce the constraint by recognizing only languages with native speaker transmission as 'living' for official purposes; bound by law and political mandate to maintain the criterion.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, state_language_policy_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Study language vitality from multiple theoretical perspectives; provide expert testimony but do not set policy; their research on language revitalization (e.g., Hebrew, Māori) challenges the exclusivity of the native-generation criterion.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, sociolinguists, observer,
    analytical, biographical, analytical, global).

% Promote the language as a living literary medium (Haskalah periodicals, modern Hebrew literature); argue that literary production demonstrates vitality, but are excluded from the policy framework that requires native transmission; their reading is a sibling constraint in the kernel contest.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, literary_continuity_advocates, excluded,
    moderate, generational, constrained, national).

% Maintain that continuous liturgical recitation and ritual use constitutes living transmission; excluded from the policy framework that defines vitality exclusively by native generational transmission; their identity and religious practice are bound to the liturgical use, making the constraint's framing an existential threat.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_preservation_advocates, excluded,
    moderate, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, operationalizable criterion for language vitality that enables state institutions to allocate resources (education, funding, official status) and to distinguish living languages from heritage or liturgical languages.
% TRANSFER_FUNCTION: Moves legitimacy, funding, and institutional recognition from liturgical-only communities (and literary continuity communities) to the secular nationalist movement and state apparatus that champions native generational transmission as the sole marker of linguistic life.
% ABSENT_VOICES: Liturgical preservation advocates and literary continuity advocates are structurally excluded from the policy conversation that adopts the native-generation criterion; they would argue that their modes of transmission sustain the language in meaningful ways, but the constraint's definition renders their practices invisible to official recognition.
% DISAPPEARANCE_RATIONALE: Without the native-generation criterion, the state would lack its current operational definition of a living language; liturgical and literary communities could claim living status and access resources, forcing a reorganization of language policy and nationalist legitimacy.
% FOUNDING_PROBLEM: The need for a standardized, state-actionable definition of language vitality to replace ad-hoc or religiously influenced criteria, enabling modern nation-building and resource allocation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historians of nationalism and language policy (e.g., the role of language standardization in 19th-20th century nation-states) outside the nationalist movement; however, the claim that native-generation transmission is the only valid solution is contested by sociolinguists and religious communities.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate because the constraint redirects substantial state resources and symbolic capital to the nationalist movement while imposing real costs on liturgical communities (loss of funding, education, recognition). Suppression (0.7) is high because the constraint's persistence depends on active exclusion of alternative vitality criteria from policy frameworks. Theater ratio (0.3) reflects that the coordination function (policy operability) is real but increasingly performed; the definition is treated as a technical necessity while its normative exclusivity does the extractive work. Accessibility collapse (0.8) is high because once the native-generation criterion is codified in law and bureaucracy, alternative framings (liturgical, literary) are structurally invisible to official recognition. Resistance (0.5) is moderate: liturgical communities resist but are identity-locked, and sociolinguistic critique exists but has limited policy uptake.
 *
 * PERSPECTIVAL GAP:
 *   From the state institution seat, the constraint appears as a neutral coordination tool (rope-like). From the liturgical community seat, it is a snare that defines their core practice as death. From the nationalist movement seat, it is a legitimate sovereignty claim (beneficiary). The engine will compute these per-seat divergences from the structural data: beneficiaries and agenda_setters have low directionality (d), payers have high d, excluded seats have no beneficiary/victim declaration so their d derives from exit_options (identity_locked → high d).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: secular_nationalist_movement (collects legitimacy, mobile exit → low d). Victim: liturgical_only_communities (bear costs, identity_locked exit → high d). Agenda_setter: state_language_policy_institutions (administers constraint, constrained exit → moderate d). Excluded advocates have no beneficiary/victim declaration; their directionality will be derived from exit_options (identity_locked for liturgical → high d, constrained for literary → moderate d). Observer has analytical exit → d=0.5. The engine's derivation chain uses these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (standardized vitality definition for nation-building) remains contested: the problem of defining vitality is live, but the native-generation solution is increasingly challenged by revitalization successes (Hebrew, Māori) that began from liturgical or literary bases. The constraint persists not because the founding problem is solved, but because the nationalist movement and state institutions extract legitimacy from the fixed definition — a classic mandatrophy pattern where the mandate (defining vitality) has outlived its functional justification (policy operability could be served by pluralistic criteria) but is maintained for its extractive benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading of the contested kernel ''living_language_status''; what are the structural consequences of treating this reading as a standalone constraint versus modeling the kernel contest as a single constraint with measurement-dependent ε?',
    'Compare classification outcomes when the three readings are authored as separate constraints linked by network.affects_constraints versus a single constraint with a measurement_basis parameter (which the schema forbids). The ε-invariance principle requires separate stories; this omega documents that the decomposition was performed and the structural deltas (beneficiary/victim sets, ε bins) are as declared.',
    'If the kernel were modeled as one constraint, ε would be ambiguous (moderate for this reading, low for literary, high for liturgical?) and the classification would be unstable. Separate stories make the extraction structure of each reading visible and allow the engine to compute per-seat types for each reading''s enforcement context.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment to the ε-invariance decomposition: one kernel, three constraints, linked by network edges.').

omega_variable(
    coordination_necessity_of_exclusion,
    'Does the coordination function (operational definition for policy) genuinely require the exclusion of liturgical and literary transmission modes, or could a pluralistic criterion (e.g., UNESCO''s vitality framework) achieve the same coordination with less extraction?',
    'Policy analysis of alternative frameworks (e.g., UNESCO Atlas of the World''s Languages in Danger uses multiple factors including domains of use, not solely native transmission). If pluralistic criteria are operationally viable, the exclusion is extractive overhead, not coordination necessity.',
    'If pluralistic criteria are viable, the constraint''s extractiveness is higher than its coordination floor, strengthening the tangled_rope classification and supporting reform. If exclusion is necessary, the extraction is the price of coordination, moving the constraint toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_of_exclusion, empirical, 'Whether the native-generation criterion''s exclusivity is a coordination requirement or an extractive choice.').

omega_variable(
    identity_lock_mechanism_liturgical,
    'Is the liturgical_only_communities'' identity_locked exit status driven by religious doctrine (theological necessity), communal cohesion (social identity), or the constraint''s own framing (being defined as ''preserving a corpse'' creates a self-fulfilling exit barrier)?',
    'Comparative study of liturgical communities under different policy regimes: where the native-generation criterion is not enforced, do liturgical communities maintain transmission without identity lock? If exit options improve when the constraint is relaxed, the lock is partly constraint-induced.',
    'If identity lock is partly produced by the constraint, the effective suppression is higher than structural barriers alone suggest — the constraint creates the very exit barrier it exploits. This would increase computed χ for the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_liturgical, empirical, 'Origin of identity-locked exit for liturgical communities: endogenous vs. constraint-induced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lls_native_gen_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(lls_native_gen_tr_t0, observed).
narrative_ontology:measurement(lls_native_gen_tr_t20, living_language_status__native_generation_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(lls_native_gen_tr_t20, observed).
narrative_ontology:measurement(lls_native_gen_tr_t40, living_language_status__native_generation_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(lls_native_gen_tr_t40, observed).
narrative_ontology:measurement(lls_native_gen_tr_t60, living_language_status__native_generation_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement_basis(lls_native_gen_tr_t60, observed).
narrative_ontology:measurement(lls_native_gen_tr_t80, living_language_status__native_generation_reading, theater_ratio, 80, 0.29).
narrative_ontology:measurement_basis(lls_native_gen_tr_t80, observed).
narrative_ontology:measurement(lls_native_gen_tr_t100, living_language_status__native_generation_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(lls_native_gen_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(lls_native_gen_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(lls_native_gen_be_t0, observed).
narrative_ontology:measurement(lls_native_gen_be_t20, living_language_status__native_generation_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(lls_native_gen_be_t20, observed).
narrative_ontology:measurement(lls_native_gen_be_t40, living_language_status__native_generation_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement_basis(lls_native_gen_be_t40, observed).
narrative_ontology:measurement(lls_native_gen_be_t60, living_language_status__native_generation_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement_basis(lls_native_gen_be_t60, observed).
narrative_ontology:measurement(lls_native_gen_be_t80, living_language_status__native_generation_reading, base_extractiveness, 80, 0.53).
narrative_ontology:measurement_basis(lls_native_gen_be_t80, observed).
narrative_ontology:measurement(lls_native_gen_be_t100, living_language_status__native_generation_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement_basis(lls_native_gen_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(lls_native_gen_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(lls_native_gen_su_t0, observed).
narrative_ontology:measurement(lls_native_gen_su_t20, living_language_status__native_generation_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(lls_native_gen_su_t20, observed).
narrative_ontology:measurement(lls_native_gen_su_t40, living_language_status__native_generation_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(lls_native_gen_su_t40, observed).
narrative_ontology:measurement(lls_native_gen_su_t60, living_language_status__native_generation_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement_basis(lls_native_gen_su_t60, observed).
narrative_ontology:measurement(lls_native_gen_su_t80, living_language_status__native_generation_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement_basis(lls_native_gen_su_t80, observed).
narrative_ontology:measurement(lls_native_gen_su_t100, living_language_status__native_generation_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement_basis(lls_native_gen_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__native_generation_reading, 0.08).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the native_generation_reading of the living_language_status kernel. It forecloses the liturgical_preservation_reading and literary_continuity_reading within any single policy framework. The three readings form a constraint family linked by mutual affects_constraints edges. The ε values differ: this reading moderate (0.55), liturgical reading lower (liturgical communities are beneficiaries there), literary reading moderate but with different victim/beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
