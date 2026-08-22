% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Continuationist Reading: Divine Marriage Command as Unrescinded
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint is the continuationist reading of the
 *   divine_marriage_command kernel within Mormon theological history. The
 *   kernel is the contested status of the original revelation authorizing
 *   plural marriage. This reading holds that the 1890 Manifesto was a
 *   prudential suspension under federal duress, not a doctrinal rescission,
 *   and that polygamy therefore remains doctrinally valid. Fundamentalist
 *   splinter communities organize around this claim, maintaining
 *   institutional separation from the mainstream LDS Church. The constraint
 *   is actively enforced through religious authority (excommunication,
 *   marriage arrangement, shunning) and amplified by external federal
 *   criminalization, which raises the cost of adherence and intensifies
 *   communal boundary maintenance.
 *
 * KEY AGENTS:
 *   - Fundamentalist splinter leadership (institutional/agenda_setter): interprets the Manifesto, enforces doctrinal boundaries, holds concentrated religious authority.
 *   - Continuationist practitioners (moderate/beneficiary-payer): gain theological legitimacy and continuity but bear legal and social costs.
 *   - Women in plural marriage (powerless/payer): bear asymmetric domestic and legal vulnerability with trapped exit.
 *   - Displaced youth males (powerless/payer): bear the demographic cost of marriage-market exclusion.
 *   - Mainstream LDS Church and federal authorities (structural counter-pressure): external resistance that amplifies extraction without being governed by the doctrinal constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.58).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Continuationist Reading: Divine Marriage Command as Unrescinded").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, 'fcab13d5-5906-4148-8412-6fc0fc43605a').
narrative_ontology:cs_kernel_codification('fcab13d5-5906-4148-8412-6fc0fc43605a', fixed_text).
narrative_ontology:cs_authority_grounding('fcab13d5-5906-4148-8412-6fc0fc43605a', lineage).
narrative_ontology:cs_interpretation_layer_present('fcab13d5-5906-4148-8412-6fc0fc43605a').
narrative_ontology:cs_reading_relation('fcab13d5-5906-4148-8412-6fc0fc43605a', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('fcab13d5-5906-4148-8412-6fc0fc43605a', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('fcab13d5-5906-4148-8412-6fc0fc43605a', foundational, original_polygamy_command_unrescinded).
narrative_ontology:cs_axiom_status(original_polygamy_command_unrescinded, holdable).
narrative_ontology:cs_axiom_grounding('fcab13d5-5906-4148-8412-6fc0fc43605a', original_polygamy_command_unrescinded, theological).
narrative_ontology:cs_axiom('fcab13d5-5906-4148-8412-6fc0fc43605a', foundational, manifesto_lacks_doctrinal_force).
narrative_ontology:cs_axiom_status(manifesto_lacks_doctrinal_force, holdable).
narrative_ontology:cs_axiom_grounding('fcab13d5-5906-4148-8412-6fc0fc43605a', manifesto_lacks_doctrinal_force, theological).
narrative_ontology:cs_reference_frame('fcab13d5-5906-4148-8412-6fc0fc43605a', original_revelation_continuity).
narrative_ontology:cs_drift_state('fcab13d5-5906-4148-8412-6fc0fc43605a', post_manifesto_mainstream_abandonment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fcab13d5-5906-4148-8412-6fc0fc43605a', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, continuationist_practitioners).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, women_in_plural_marriage).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, displaced_youth_males).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, continuationist_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the 1890 Manifesto as temporary political accommodation rather than doctrinal rescission; arranges plural marriages, administers excommunication for Manifesto-as-supersession belief, and enforces communal boundary against mainstream LDS affiliation. Holds concentrated religious authority and the power to legitimate or delegitimate family units within the community.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinter_leadership, agenda_setter,
    institutional, generational, constrained, regional).

% Receive theological legitimacy and continuity with founding revelation by entering plural marriage; their family structure is sacramentally validated within the splinter community. Simultaneously bear legal jeopardy, asset forfeiture risk, and social ostracism outside the community because their marital practice is felonious in most jurisdictions.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, continuationist_practitioners, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, continuationist_practitioners, payer).

% Enter religious unions with no legal standing, limited economic autonomy, and heavy dependence on the community for housing, childcare, and spiritual meaning. Exiting typically means leaving children, family networks, and promised eternal salvation; many have no secular education or credit history.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, women_in_plural_marriage, payer,
    powerless, biographical, trapped, local).

% In communities where older men monopolize plural wives, adolescent males are expelled or pressured to leave to reduce marriage competition. They are spiritually blacklisted if they dissent doctrinally, and materially unsupported once outside, bearing the demographic cost of the marriage system's arithmetic.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, displaced_youth_males, payer,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__continuationist_reading, fundamentalist_splinter_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__continuationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains religious continuity and communal boundary for splinter groups who hold that the founding revelation on plural marriage was not rescinded; provides a unified theological framework for family structure, group identity, and separation from the mainstream institutional church.
% TRANSFER_FUNCTION: Moves theological legitimacy, marital status, and communal belonging from the mainstream institutional church to splinter communities; moves legal risk and social stigma from the collective doctrine onto women, children, and households who bear the direct consequences of felonious marital practice.
% ABSENT_VOICES: Mainstream LDS historians and apologists who frame the Manifesto as divine rescission; federal prosecutors who reduce the practice to purely criminal behavior without doctrinal content; women who have exited and repudiate the theological framing â their absence from continuationist discourse is enforced by shunning, spiritual threat, and communal economic dependency.
% DISAPPEARANCE_RATIONALE: The fundamentalist splinters exist specifically to maintain this doctrinal claim; if the constraint vanished overnight, plural marriages within these communities would lose their theological warrant, the boundary with the mainstream church would collapse, and the community's organizing principle and marriage arrangements would reorganize or dissolve.
% FOUNDING_PROBLEM: The 1890 Manifesto created a crisis of continuity: how to remain faithful to the original divine marriage command after the institutional church publicly abandoned plural marriage under federal military and economic pressure.
% FOUNDING_PROBLEM_CORROBORATION: Federal court records, congressional hearings, and territorial documents from 1887-1897 attest to the external coercion (seizure of church property, imprisonment of leaders). Independent academic historians such as D. Michael Quinn and B. Carmon Hardy, writing from outside the continuationist beneficiary community, corroborate the pressure and the doctrinal non-rescission interpretation held by dissenters, though they do not endorse its theological validity.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the doctrinal constraint imposes heavy legal and social costs on practitioners, especially women and displaced youth, while providing genuine theological coordination to the community. Suppression (0.72) is high because the constraint actively suppresses alternative readings of the Manifesto through shunning and excommunication, and external law suppresses the practice itself, which paradoxically hardens internal suppression. Theater_ratio (0.42) captures the oscillation between public defiance (early era, mid-century raids) and secrecy, with modern media-era performative maintenance of identity. Accessibility_collapse (0.78) is high because once inside the doctrinal framework, empirical alternatives (mainstream historiography, legal marriage, secular support networks) are theologically delegitimated. Resistance (0.85) is very high because the constraint meets sustained federal enforcement, mainstream ecclesiastical opposition, and internal generational dissent. The temporal series show cyclical extraction driven by federal enforcement waves (1910s, 1950s, 2000s) and corresponding internal discipline intensifications, not monotonic drift.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (splinter leadership) and the beneficiary-payer seat (male practitioners) experience the constraint as legitimate continuity with divine authority and necessary communal defense. The pure payer seats (women in plural marriage, displaced youth) experience the same structure as asymmetric extraction of life chances, legal vulnerability, and familial exclusion. The engine computes this divergence from the structural data: identical spatial scope and overlapping community membership, but radically differentiated exit options (constrained vs trapped) and power (moderate vs powerless).
 *
 * DIRECTIONALITY LOGIC:
 *   Fundamentalist leadership derives structural subsidy from the constraint (low d): their authority is constituted by their role as guardians of the unrescinded command. Continuationist practitioners sit near symmetric but slightly toward beneficiary because they gain spiritual goods they judge to exceed legal costs. Women in plural marriage and displaced youth sit at full-target (high d): they bear concentrated costs without commensurate authority or communal voice, and their spatial scope is contracted to local/regional cages. Federal authorities are external to the directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy interview prevents mislabeling this constraint as pure extraction (snare) because the founding problem â how to maintain doctrinal continuity after 1890 â remains live for the beneficiary community and is corroborated by external historical archives. However, the temporal measurements and high theater ratio prevent mislabeling it as pure coordination (rope) because the enforcement history shows repeated waves of extraction accumulation and performative boundary maintenance that exceed what genuine coordination would require.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of exit from this doctrinal constraint primarily structural (legal persecution, economic dependency, geographic isolation) or internalized (fear of spiritual damnation, identity fusion with communal salvation)?',
    'Post-exit trajectory analysis: if practitioners who physically leave continue to experience psychological suppression, identity loss, and compulsive return attempts, the mechanism is substantially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because the target carries the suppression mechanism after physical exit, amplifying computed extraction for the trapped seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in a religious identity coordination constraint').

omega_variable(
    practitioner_net_benefit,
    'Do continuationist male practitioners net-benefit from this constraint, or do they bear concentrated legal and economic costs that exceed their theological and social gains?',
    'Comparative life-outcome analysis of continuationist practitioners versus matched monogamous religious populations, measuring incarceration rates, asset stability, educational attainment, and reported subjective religiosity.',
    'If male practitioners are net payers rather than net beneficiaries, the beneficiary set collapses toward leadership alone, shifting the classification toward snare and away from tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practitioner_net_benefit, empirical, 'Whether practitioner benefits exceed costs in the continuationist community').

omega_variable(
    kernel_framing_alternative,
    'Is the constraint better framed as a doctrinal commitment to polygamy, or as a resistance narrative against federal and mainstream ecclesiastical coercion?',
    'Discourse analysis of continuationist theological literature: if the majority of doctrinal production centers on the invalidity of the Manifesto rather than affirmative polygamy theology, the constraint is primarily a resistance narrative.',
    'If the alternative framing is adopted, the coordination_type shifts from identity_coordination toward enforcement_mechanism, altering the Boltzmann floor and coupling threshold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Framing under-determination between doctrinal commitment and resistance narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__continuationist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__continuationist_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__continuationist_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(divi_tr_t60, divine_marriage_command__continuationist_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(divi_tr_t80, divine_marriage_command__continuationist_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement(divi_tr_t100, divine_marriage_command__continuationist_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(divi_tr_t130, divine_marriage_command__continuationist_reading, theater_ratio, 130, 0.42).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__continuationist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__continuationist_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__continuationist_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(divi_be_t60, divine_marriage_command__continuationist_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(divi_be_t80, divine_marriage_command__continuationist_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(divi_be_t100, divine_marriage_command__continuationist_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement(divi_be_t130, divine_marriage_command__continuationist_reading, base_extractiveness, 130, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__continuationist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__continuationist_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__continuationist_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(divi_su_t60, divine_marriage_command__continuationist_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(divi_su_t80, divine_marriage_command__continuationist_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(divi_su_t100, divine_marriage_command__continuationist_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement(divi_su_t130, divine_marriage_command__continuationist_reading, suppression_requirement, 130, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the divine_marriage_command kernel, instantiated as continuationist_reading. It shares the referent (the standing doctrinal arrangement regarding plural marriage) with substitutionist_reading and coercion_visibility_reading, but each reading authors a distinct epsilon and structural profile per the epsilon-invariance principle. Decomposition is required because evaluating the kernel through the continuationist reading yields a substantially different extractiveness and victim structure than evaluating it through the substitutionist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
