% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Temple Sacrifice Commitment: Symbolic Transformation Reading
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   The rabbinic tradition, following the destruction of the Second Temple,
 *   progressively authorized prayer and Torah study as the legitimate
 *   instantiation of the biblical sacrificial command, rather than as mere
 *   temporary substitutes for a suspended practice. This constraint story
 *   models that interpretive framework as a standing arrangement: the
 *   rabbinic authority structure claims the power to redefine material divine
 *   commands into symbolic practice, normative Jewish communities receive
 *   coordinated religious continuity, and literalist traditionalistsâwho
 *   hold that material altar sacrifice is non-negotiableâbear the cost of a
 *   commitment declared fulfilled by proxy. The reading claims authorized
 *   transformation; the metrics independently assess the structural
 *   extraction involved in concentrating interpretive authority and
 *   marginalizing material alternatives.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Agenda-setter (institutional/constrained) â claims authorized transformative power over divine command
 *   - normative_communities: Beneficiary (organized/constrained) â receives coordinated practice without material Temple
 *   - literalist_traditionalists: Payer (moderate/identity_locked) â bears cost of commitment transformation they reject
 *   - priestly_descendants: Excluded (moderate/identity_locked) â lost institutional voice in transformation
 *   - religious_studies_observers: Observer (analytical/analytical) â tracks ritual adaptation and authority migration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.7).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.65).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.7).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Temple Sacrifice Commitment: Symbolic Transformation Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, 'e60586c0-ab3d-41a6-a485-4d9315b73650').
narrative_ontology:cs_kernel_codification('e60586c0-ab3d-41a6-a485-4d9315b73650', fixed_text).
narrative_ontology:cs_authority_grounding('e60586c0-ab3d-41a6-a485-4d9315b73650', lineage).
narrative_ontology:cs_interpretation_layer_present('e60586c0-ab3d-41a6-a485-4d9315b73650').
narrative_ontology:cs_reading_relation('e60586c0-ab3d-41a6-a485-4d9315b73650', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('e60586c0-ab3d-41a6-a485-4d9315b73650', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('e60586c0-ab3d-41a6-a485-4d9315b73650', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_axiom('e60586c0-ab3d-41a6-a485-4d9315b73650', foundational, rabbinic_transformative_authority).
narrative_ontology:cs_axiom_status(rabbinic_transformative_authority, holdable).
narrative_ontology:cs_axiom_grounding('e60586c0-ab3d-41a6-a485-4d9315b73650', rabbinic_transformative_authority, conventional).
narrative_ontology:cs_axiom('e60586c0-ab3d-41a6-a485-4d9315b73650', foundational, symbolic_instantiation_validity).
narrative_ontology:cs_axiom_status(symbolic_instantiation_validity, holdable).
narrative_ontology:cs_axiom_grounding('e60586c0-ab3d-41a6-a485-4d9315b73650', symbolic_instantiation_validity, deontological).
narrative_ontology:cs_reference_frame('e60586c0-ab3d-41a6-a485-4d9315b73650', symbolic_instantiation_framework).
narrative_ontology:cs_drift_state('e60586c0-ab3d-41a6-a485-4d9315b73650', contemporary_restoration_challenges, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e60586c0-ab3d-41a6-a485-4d9315b73650', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, normative_communities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, literalist_traditionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims authorized power to transform the material sacrificial command into prayer and Torah study; adjudicates halakhic validity through textual interpretation and jurisprudential precedent; maintains institutional continuity by defining the new instantiation as legitimate fulfillment rather than emergency substitute.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority, agenda_setter,
    institutional, generational, constrained, global).

% Accept rabbinic authority's interpretive framework; fulfill the sacrificial commitment through daily prayer, festival liturgy, and Talmud study; maintain covenantal relationship without material Temple or functioning priesthood; receive religious continuity and coherence.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, normative_communities, beneficiary,
    organized, biographical, constrained, global).

% Hold that the divine command requires material altar sacrifice and cannot be authoritatively transformed into symbolic practice; bear the cognitive and spiritual cost of seeing the commitment declared fulfilled by proxy; marginalized by mainstream halakhic consensus; unable to perform the command due to Temple absence and halakhic prohibition on extra-temple sacrifice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, literalist_traditionalists, payer,
    moderate, generational, identity_locked, global).

% Inherited religious role historically centered on material sacrificial performance; marginalized by the transformation that renders their function largely ceremonial; present in the textual tradition but structurally excluded from the new institutional power configuration.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, priestly_descendants, excluded,
    moderate, generational, identity_locked, global).

% Analyze the transformation as a case of ritual adaptation under institutional rupture; track how interpretive authority migrates from priestly to rabbinic classes; observe the authorization mechanism without halakhic commitment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, religious_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__symbolic_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Israelite/Jewish covenantal practice after the loss of the central sacrificial site; channels devotional energy into sustainable diaspora and post-Temple practices; prevents religious dissolution following the cultic rupture of 70 CE.
% TRANSFER_FUNCTION: Transfers the locus of divine service from material altar offerings to verbal prayer and textual study; transfers interpretive authority to the rabbinic class to adjudicate the transformation; transfers the 'sacrifice' designation to non-material acts.
% ABSENT_VOICES: Priestly orders whose institutional role depended on material sacrifice; literalist sects and restoration movements insisting on altar reconstruction; Samaritan communities maintaining sacrificial practice at their shrine.
% DISAPPEARANCE_RATIONALE: If the symbolic transformation framework vanished, normative Jewish practice would lose its primary mechanism for fulfilling the sacrificial command; communities would face pressure to either reconstruct the Temple and restore slaughter-offerings or abandon the covenantal framework entirely; the interpretive authority of the rabbinic class would be profoundly destabilized.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the legitimate site for commanded sacrifice, creating a crisis of covenantal practice: how to fulfill a divine command requiring material performance when the material conditions are impossible.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic literature (Mishnah, Tosefta, Talmud) attests the crisis and the pivot to study and prayer. Modern academic historians corroborate the Temple destruction as historical rupture. However, the 'authorized transformation' framing is primarily attested by the rabbinic beneficiaries themselves; literalist traditionalists and some modern scholars read the transformation as retrospective authorization of practical necessity rather than revealed jurisprudence.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70 at interval end) is high because the rabbinic authority concentrates the power to redefine commanded practice, and literalist alternatives are structurally barred. Suppression (0.65) reflects the halakhic prohibition on extra-Temple sacrifice and the marginalization of literalist voices, though modern pluralism slightly tempers active enforcement. Theater_ratio (0.42) captures the routinized 'as-if' quality of prayer-as-sacrifice after two millennia, where genuine spirituality mixes with performed substitution. Accessibility_collapse (0.88) is very high: once inside the rabbinic framework, material sacrifice becomes practically and conceptually inaccessible. Resistance (0.52) reflects persistent literalist dissent and modern Temple restoration movements. The trajectory shows extraction rising through the medieval codification period and stabilizing slightly lower as modern challenges emerge. Measurements share one time grid to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rabbinic_authority) experiences the constraint as legitimate jurisprudential development preserving Israel's covenant; the payer seat (literalist_traditionalists) experiences it as usurpation of divine command by human authority. The beneficiary seat (normative_communities) sits betweenâreceiving genuine coordination benefit while accepting an interpretive framework that resolves the crisis. The engine should compute these seats differently: the authority near the beneficiary end, literalists near the target end, and communities in the middle.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic_authority is the declared beneficiary of interpretive power and institutional continuity; it sits near the low-d end but not at zero because the authority is itself constrained by textual tradition. Normative_communities receive coordination benefit (low-moderate d). Literalist_traditionalists are explicitly in the victim array and identity-locked to the material command, placing them near the full-target end (high d). Their exit options are severely constrained by both the Temple's physical absence and halakhic prohibition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling because it retains a genuine coordination function: without some transformation mechanism, Jewish practice would have faced collapse after 70 CE. However, the persistence of the framework for two millenniaâlong after the original crisisâcombined with the concentration of authority in the rabbinic class and the exclusion of priestly/literalist alternatives, indicates the coordination function has accreted extractive overhead. The tangled_rope classification captures both faces: it is not merely a rope (the authority claim is not pure coordination) nor merely a snare (the crisis response was genuine and necessary). The founding_problem_status is 'contested' because the original problem (Temple destruction) is historical fact, but whether the transformation was the authorized solution or an opportunistic power transfer remains disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_authority_legitimacy,
    'Is the rabbinic claim of authorized transformation grounded in genuine jurisprudential continuity, or is it retrospective rationalization of practical necessity?',
    'Historical-philological analysis of early rabbinic literature (Mishnah, Tosefta, Talmud) for claims of Sinaitic authorization versus crisis-response rhetoric; comparison with other ancient Near Eastern ritual adaptation frameworks.',
    'If retrospective rationalization, the constraint''s extraction is unauthorized drift and classification shifts toward snare; if genuine continuity, the tangled_rope reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_authority_legitimacy, empirical, 'Whether rabbinic transformative authority is legitimately continuous or post-hoc').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of literalist sacrifice practice structural (halakhic prohibition on non-Temple sacrifice) or internalized (theological acceptance that prayer is spiritually equivalent or superior)?',
    'Survey of communities with Temple restoration ideology for persistence of sacrifice desire despite halakhic prohibition; analysis of whether literalists experience external coercion or internalized blockage.',
    'If internalized, effective suppression exceeds structural measure; if purely structural, extraction may be lower than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression of literal sacrifice commitment').

omega_variable(
    coordination_extraction_boundary,
    'Can the genuine coordination functionâmaintaining Jewish religious continuity after Temple destructionâbe separated from the concentration of interpretive authority in the rabbinic class?',
    'Counterfactual analysis of priestly-led or decentralized alternatives to rabbinic hegemony; examination of Samaritan and Karaite frameworks for sacrifice fulfillment without rabbinic authority concentration.',
    'If inseparable, some extraction is the necessary cost of coordination; if separable, the authority concentration is extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Separability of coordination benefit from authority concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_st_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tsc_st_tr_t20, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 20, 0.35).
narrative_ontology:measurement(tsc_st_tr_t40, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 40, 0.48).
narrative_ontology:measurement(tsc_st_tr_t60, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 60, 0.52).
narrative_ontology:measurement(tsc_st_tr_t80, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 80, 0.45).
narrative_ontology:measurement(tsc_st_tr_t100, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(tsc_st_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(tsc_st_be_t20, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(tsc_st_be_t40, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(tsc_st_be_t60, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(tsc_st_be_t80, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 80, 0.72).
narrative_ontology:measurement(tsc_st_be_t100, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 100, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(tsc_st_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tsc_st_su_t20, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(tsc_st_su_t40, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(tsc_st_su_t60, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(tsc_st_su_t80, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(tsc_st_su_t100, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_commitment kernel, decomposed per the Îµ-invariance principle. Sibling readings instantiate structurally distinct claims about the same kernel and are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
