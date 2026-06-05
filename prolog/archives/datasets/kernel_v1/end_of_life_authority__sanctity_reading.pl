% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Life's Intrinsic Sanctity in End-of-Life Authority (Sanctity Reading)
 *   domain: medical_ethics/bioethics/constitutional_law
 *
 * SUMMARY:
 *   The end-of-life authority constraint exhibits a fundamental kernel: the
 *   determination of which principles ground legitimate decision-making about
 *   intentional life-ending. This constraint story instantiates ONE READING
 *   of that kernel — the sanctity reading, which asserts that human life
 *   possesses intrinsic, non-delegable sacred value such that intentional
 *   life-ending is categorically impermissible. The sanctity reading competes
 *   with two sibling readings: (1) the autonomy reading, which grounds
 *   legitimate authority in the individual's right to self-determination over
 *   their own death, and (2) the dignity reading, which grounds authority in
 *   the preservation of human dignity through choice and control. All three
 *   readings operate within the same institutional domain (law, medicine,
 *   bioethics) and generate different constraint structures from the same
 *   observable phenomenon: patients approaching death with the question 'Who
 *   decides how I die?' Under the sanctity reading, the answer is: no one;
 *   the decision is foreclosed by the intrinsic value of life itself. This
 *   reading creates clear beneficiaries (the moral community affirming
 *   sanctity) and clear victims (patients denied relief, families
 *   constrained). The constraint exhibits tangled rope classification because
 *   it simultaneously coordinates a genuine moral commitment (preventing
 *   commodification of death, establishing a non-negotiable floor) and
 *   extracts from those it governs (enforcing a particular metaphysical
 *   premise against competing interpretations).
 *
 * KEY AGENTS:
 *   - Terminally Ill Patient: Primary victim (powerless/trapped) — subject to prohibition on relief-seeking; bears full cost of prolonged suffering without exit option or voice in decision
 *   - Family/Caregiver: Secondary victim (moderate/constrained) — witnesses suffering, prohibited from relief-enabling action, experiences moral distress and complicity; legal sanctions deter action
 *   - Medical Providers: Secondary actor (institutional/constrained) — constrained by law and ethics codes to preserve life; benefits from liability protection; experiences moral distress from constraint enforcement
 *   - Sanctity-Affirming Religious/Moral Community: Primary beneficiary (institutional/arbitrage) — constraint codifies and enforces core moral commitment; benefits from legal recognition; has cultural and political influence on policy
 *   - Analytical Observer: Disinterested analyst (analytical/analytical) — maps structural relationships across all positions without endorsement; instantiates this reading specifically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.58).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.62).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Life's Intrinsic Sanctity in End-of-Life Authority (Sanctity Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/constitutional_law").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3').
narrative_ontology:cs_kernel_codification('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', fixed_text).
narrative_ontology:cs_authority_grounding('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', lineage).
narrative_ontology:cs_interpretation_layer_present('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3').
narrative_ontology:cs_reading_relation('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', end_of_life_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', end_of_life_authority__dignity_reading, influences).
narrative_ontology:cs_axiom('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', foundational, life_intrinsic_sacred_value).
narrative_ontology:cs_axiom_status(life_intrinsic_sacred_value, holdable).
narrative_ontology:cs_axiom_grounding('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', life_intrinsic_sacred_value, deontological).
narrative_ontology:cs_axiom('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', foundational, intentional_ending_categorically_impermissible).
narrative_ontology:cs_axiom_status(intentional_ending_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', intentional_ending_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', sanctity_authority_framework).
narrative_ontology:cs_drift_state('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', contemporary_pluralistic_democracies, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('845c5aab-82ee-4ebc-85c8-5baa8f0cd3b3', '2026-02-26T14:32:15Z').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, moral_community_sanctity_upholders).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_patients_denied_relief).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, families_of_dying_persons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DYING PATIENT (SNARE) — Trapped by law and medical protocol in terminal suffering; no exit option exists within legal bounds. The constraint extracts the cost of prolonged dying without the patient's consent. Physical and existential agency are both foreclosed. Maximum experienced extraction; zero alternatives.
constraint_indexing:constraint_classification(end_of_life_authority__sanctity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILY (SNARE/CONSTRAINED) — Witnesses the patient's suffering with legal prohibition on relief-enabling action. Constrained by law, social judgment, and medical gatekeeping. High extraction: emotional cost of helplessness, moral distress, complicity in prolonged suffering. Some structural mobility (illegal exit exists) but carries severe legal and social penalties.
constraint_indexing:constraint_classification(end_of_life_authority__sanctity_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDICAL PROVIDERS (TANGLED ROPE) — Constrained by law and professional ethics codes that mandate sanctity preservation. Benefits from the constraint insofar as it protects them from legal liability and moral hazard (avoiding encouragement of euthanasia). Also bears extraction: moral distress from witnessing preventable suffering, violation of autonomy-based professional norms, participation in constraint enforcement against their judgment. Mixed coordination (palliative care protocols) and extraction (forced life-extension beyond patient preference).
constraint_indexing:constraint_classification(end_of_life_authority__sanctity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SANCTITY-AFFIRMING RELIGIOUS/MORAL COMMUNITY (ROPE) — Primary beneficiary. The constraint codifies and enforces the core moral commitment: all human life has intrinsic, non-delegable sacred value. Benefits from legal recognition of this principle; experiences the constraint as coordination of moral community around shared sanctity norm. Low extraction experienced by beneficiaries; arbitrage options exist through cultural and political influence on policy.
constraint_indexing:constraint_classification(end_of_life_authority__sanctity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the constraint as simultaneously coordinating a genuine moral commitment (sanctity principle) and extracting from those it governs (patients denied relief, families constrained). The constraint has real coordination function (prevents commodification of death, establishes a clear moral floor) and real extraction (enforces a particular metaphysical premise about life's meaning against competing readings). Perspectival classification reflects both functions.
constraint_indexing:constraint_classification(end_of_life_authority__sanctity_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(end_of_life_authority__sanctity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(end_of_life_authority__sanctity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The sanctity reading imposes a clear prohibition that prevents a large population (terminally ill persons with unbearable suffering) from accessing a service they autonomously desire. The extraction is not maximal (like a snare) because genuine coordination exists: the constraint does prevent commodification and establishes a moral floor valued by many. But the extraction is substantial because the constraint enforces a particular metaphysical claim (life is sacred) that competing readings (autonomy, dignity) reject. The rising trajectory from 0.48 to 0.58 reflects increasing moral distress over time as palliative care improves yet intentional relief remains prohibited—the justification (prevent suffering) increasingly diverges from the means (prohibit all intentional ending). Suppression (0.62): Moderate-high. Multiple enforcement mechanisms: law criminalizes assisted dying, medical protocols require life-extension, social stigma constrains discourse, religious authority frames relief-seeking as moral transgression. Barrier to exit is substantial—families cannot openly access relief without legal consequences; patients cannot leave the jurisdiction easily; medical providers cannot openly assist without losing licensure. Theater ratio (0.45): Moderate. The sanctity reading's enforcement has genuine functional content (it does prevent certain deaths) and genuine coordination function (it does establish a shared moral boundary). Less theatrical than purely performative constraint, but some theater exists: the categorical prohibition persists even where patient suffering is maximal and relief-seeking is clearly autonomous, suggesting the constraint's force derives partly from the norm itself rather than the empirical justification for it.
 *
 * PERSPECTIVAL GAP:
 *   The sanctity reading generates profound perspectival gaps. The sanctity-affirming community sees rope (coordination of shared moral commitment). The dying patient sees snare (absolute prohibition without exit). The family sees extraction masked as moral principle. The medical provider sees conflicted tangled rope (protecting patients from harm while enforcing a constraint that causes suffering). The analytical observer sees tangled rope because the constraint simultaneously serves coordination (preventing commodification) and extraction (enforcing a metaphysical premise against competing readings). The gap is not resolvable by additional information—it reflects that the sanctity reading and the autonomy/dignity readings operate from incommensurable foundational axioms. One reading's coordination is another reading's coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from the agent's structural position relative to the constraint. The dying patient is maximum victim (d ≈ 0.95): terminal trapped powerless agent bearing full cost with no exit. The family is moderate-high victim (d ≈ 0.70): constrained moderate agent bearing moral and emotional cost but with some structural mobility (illegal options exist, though costly). Medical providers are mixed (d ≈ 0.55): benefits from liability protection (low d component) but constrained by law and distressed by enforcement (high d component)—tangled rope reflects the mixed directionality. The sanctity community is maximum beneficiary (d ≈ 0.05): institutional arbitrage agent benefiting from legal enforcement of their core norm with no cost. The analytical observer adopts neutral position (d ≈ 0.72 canonical for analytical power)—observes structure without endorsement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in end-of-life authority arises because all three readings claim to preserve something essential: sanctity (life itself), autonomy (self-governance), dignity (human worth and control). The sanctity reading resolves the mandatrophy by asserting that these values are not in tension—they coexist because autonomy is properly exercised only in service of the good (sanctity), and dignity is properly maintained by refusing to treat life as a commodity. However, this resolution is itself contested. The autonomy reading asserts that dignity REQUIRES choice, and sanctity WITHOUT choice becomes violation of dignity. The dignity reading attempts to hold both but prioritizes dignity-through-control, which aligns more closely with autonomy. The analytical observer's tangled rope classification reflects that none of these resolutions is empirically forced—they are axiological choices. The sanctity reading's mandatrophy resolution rests on a deontological axiom: life has non-negotiable value. This axiom is not empirically resolvable. Therefore, the mandatrophy is real and persistent within this framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctity_grounding_status,
    'Is sanctity a discoverable metaphysical property of human life (deontological foundation) or a conventional norm chosen by a moral community (conventional foundation)?',
    'Philosophical analysis of whether sanctity claims rest on intrinsic moral properties or on enacted community commitments. Cross-cultural comparison of sanctity interpretations. Legal history of how courts ground sanctity doctrine.',
    'If metaphysical (deontological): sanctity reading is universally binding; autonomy reading is categorically foreclosed within this framework. If conventional: sanctity reading coexists_with autonomy and dignity readings as competing community standards; the constraint becomes governance of plural values rather than enforcement of discovered truth.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctity_grounding_status, conceptual, 'Whether sanctity is metaphysical or conventional').

omega_variable(
    slippery_slope_empirical_status,
    'Does legalizing assisted dying in bounded contexts (terminal illness, persistent unbearable suffering, informed consent) actually lead to expansion toward non-voluntary or convenience euthanasia?',
    'Comparative jurisdictional analysis (Netherlands, Belgium, Oregon, Canada, Switzerland). Longitudinal tracking of legal scope expansion. Correlation analysis between legal boundaries and actual practice drift. Qualitative analysis of authorization logic and decision-maker variation.',
    'If slope is empirically steep: sanctity reading''s suppression argument (prohibition necessary to prevent cascade) is evidence-grounded. If slope is shallow or absent: suppression argument relies on speculative moral hazard rather than observed pattern, weakening the extraction justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(slippery_slope_empirical_status, empirical, 'Whether legalizing assisted dying causes scope expansion').

omega_variable(
    alternative_reading_mutual_foreclosure,
    'Do the sanctity reading (life''s intrinsic value forecloses intentional ending) and the autonomy reading (self-determination forecloses non-consensual life-extension) logically foreclose each other within a single coherent framework, or can a unified framework hold both?',
    'Formal analysis of axiomatic compatibility. Examination of jurisdictions attempting to hold both (e.g., Canada''s MAiD framework that restricts autonomy on sanctity grounds for some populations while permitting it for others). Identification of whether resolution requires prioritizing one axiom over the other or whether a higher-order framework can contain both.',
    'If mutually foreclosing: readings are incompatible in one framework; the engine should classify relation as forecloses. If compatible: coexists_with is the correct relation; both readings are simultaneously live in different parties'' frameworks. Determines whether this constraint is a genuine natural law (foreclosure is universal) or a governance mechanism (coexistence reflects plural values).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_mutual_foreclosure, conceptual, 'Whether sanctity and autonomy readings mutually foreclose').

omega_variable(
    patient_suffering_measurement_validity,
    'Are current end-of-life suffering measurements (pain scales, existential distress assessments, quality-of-life indices) sufficiently reliable and valid to distinguish unbearable suffering (justifying relief under autonomy reading) from normal terminal symptoms (to be managed with palliative care under sanctity reading)?',
    'Meta-analysis of suffering measurement instruments. Empirical comparison of suffering reported by patients accessing assisted dying vs those in purely palliative care. Prospective study of suffering trajectories and decision-maker variation.',
    'If measurement is valid and reliable: suffering becomes an objective constraint on the sanctity reading''s applicability — patients with measurably unbearable suffering demonstrate an empirical gap between the principle (preserve life) and its justification (preservation serves the person''s good). If measurement is unreliable: suffering becomes hermeneutic and contestable; the sanctity reading retains interpretive authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_suffering_measurement_validity, empirical, 'Validity of end-of-life suffering measurement').

omega_variable(
    proxy_decision_authority_scope,
    'In the sanctity reading, does the prohibition on intentional life-ending extend to surrogate decisions by families/guardians for incompetent persons, or only to autonomous self-decisions? Does the axiom bind the person (their life is sacred) or their choices (intentional ending is impermissible)?',
    'Textual analysis of sanctity doctrine''s scope (does it apply to persons or to agents'' choices?). Jurisdictional comparison of surrogate decision authority under sanctity vs autonomy frameworks. Examination of cases where patient previously expressed desire for death and surrogate seeks to honor it.',
    'If axiom binds the person: surrogate cannot authorize life-ending even with prior patient consent; extraction extends to competent persons'' autonomy foreclosed by incompetence. If axiom binds the choice: surrogate might honor patient''s prior autonomous choice; sanctity constrains the agent''s intentional action rather than the life itself. Determines victim set and extraction scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_decision_authority_scope, conceptual, 'Scope of sanctity axiom: person or choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_sanctity_tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eol_sanctity_tr_t15, end_of_life_authority__sanctity_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(eol_sanctity_tr_t30, end_of_life_authority__sanctity_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(eol_sanctity_be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(eol_sanctity_be_t15, end_of_life_authority__sanctity_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(eol_sanctity_be_t30, end_of_life_authority__sanctity_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eol_sanctity_su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(eol_sanctity_su_t15, end_of_life_authority__sanctity_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(eol_sanctity_su_t30, end_of_life_authority__sanctity_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__sanctity_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__dignity_reading).

% DUAL FORMULATION NOTE:
% The end-of-life authority constraint splits into three structurally distinct readings: sanctity_reading (ε=0.58, Tangled Rope, emphasis on categorical prohibition), autonomy_reading (ε=0.52, Tangled Rope, emphasis on self-determination), and dignity_reading (ε=0.55, Tangled Rope, emphasis on choice and control). Each reading has the same base observables (terminal illness, autonomous desire for death, prohibition or permission in law) but generates different constraint structures through different foundational axioms. The network links represent mutual influence: changes in legal adoption of one reading affect the epistemic standing and institutional adoption of the others. All three affect the medical practice constraint directly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__sanctity_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
