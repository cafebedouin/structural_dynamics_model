% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: 1890 Manifesto: Hybrid Pragmatic Reading
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Manifesto issued by the Church of Jesus Christ of Latter-day
 *   Saints announced the cessation of plural marriage. The hybrid pragmatic
 *   reading interprets this document not as pure prophetic revelation
 *   (endogenous) nor as mere federal capitulation (exogenous), but as
 *   strategic institutional adaptation: prophetic authority deployed
 *   instrumentally to manage an existential exogenous crisis while preserving
 *   core theological commitments through deliberate scope ambiguity. This
 *   reading identifies institutional leadership as the primary beneficiary of
 *   the ambiguity, which grants them doctrinal flexibility and federal
 *   compliance simultaneously, while rank-and-file members and post-Manifesto
 *   plural families bear the costs of interpretive uncertainty, legitimacy
 *   ambiguity, and legal exposure.
 *
 * KEY AGENTS:
 *   - institutional_leadership (agenda_setter/beneficiary, institutional/arbitrage): Retains exclusive control over interpretive ambiguity, preserving doctrinal flexibility and federal compliance simultaneously.
 *   - rank_and_file_members (payer, moderate/identity_locked): Bear the cognitive and social costs of interpretive uncertainty about eternal marriage doctrine while expected to conform publicly to monogamy.
 *   - post_manifesto_plural_families (payer, powerless/trapped): Secretly authorized into plural marriage after 1890, publicly denied, bearing legal and existential risk while leadership maintains deniability.
 *   - academic_interpreters (observer, analytical/analytical): External analytical seat assessing the historical and theological structural function of the Manifesto.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.62).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "1890 Manifesto: Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '667e0639-ade1-4d22-a237-81fcefb24878').
narrative_ontology:cs_kernel_codification('667e0639-ade1-4d22-a237-81fcefb24878', fixed_text).
narrative_ontology:cs_authority_grounding('667e0639-ade1-4d22-a237-81fcefb24878', lineage).
narrative_ontology:cs_interpretation_layer_present('667e0639-ade1-4d22-a237-81fcefb24878').
narrative_ontology:cs_reading_relation('667e0639-ade1-4d22-a237-81fcefb24878', marriage_commitment_legitimacy__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('667e0639-ade1-4d22-a237-81fcefb24878', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('667e0639-ade1-4d22-a237-81fcefb24878', foundational, prophetic_authority_as_strategic_instrument).
narrative_ontology:cs_axiom_status(prophetic_authority_as_strategic_instrument, holdable).
narrative_ontology:cs_axiom_grounding('667e0639-ade1-4d22-a237-81fcefb24878', prophetic_authority_as_strategic_instrument, instrumental).
narrative_ontology:cs_axiom('667e0639-ade1-4d22-a237-81fcefb24878', foundational, scope_ambiguity_as_preservation_mechanism).
narrative_ontology:cs_axiom_status(scope_ambiguity_as_preservation_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('667e0639-ade1-4d22-a237-81fcefb24878', scope_ambiguity_as_preservation_mechanism, conventional).
narrative_ontology:cs_reference_frame('667e0639-ade1-4d22-a237-81fcefb24878', pragmatic_doctrinal_preservation).
narrative_ontology:cs_drift_state('667e0639-ade1-4d22-a237-81fcefb24878', post_manifesto_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('667e0639-ade1-4d22-a237-81fcefb24878', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, post_manifesto_plural_families).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_survival_imperative).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prophetic_pragmatism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued and subsequently interpreted the Manifesto, retaining exclusive control over whether it represented a divine revelation, a political necessity, or both. This ambiguity allows them to simultaneously claim federal compliance and preserve doctrinal space for future theological developments, while disciplining members who interpret the Manifesto too narrowly or too broadly.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, beneficiary).

% Expected to conform to monogamous marriage norms post-1890 while hearing conflicting messages about the eternal status of plural marriage. Their theological certainty is sacrificed so that leadership can maintain negotiating flexibility with federal authorities and internal dissenters. Exit means abandoning salvation framework, family networks, and community identity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    moderate, biographical, identity_locked, national).

% Entered plural marriages after the 1890 Manifesto based on private assurances from leaders that the Manifesto applied only to public practice, not to the principle itself. Publicly denied and legally nonexistent, they bear the full risk of prosecution and social erasure while the institutional leadership that authorized their marriages maintains public deniability.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, post_manifesto_plural_families, payer,
    powerless, biographical, trapped, regional).

% Assess the historical and theological structural function of the Manifesto without being bound by its authority claims. They trace the divergence between public statements and private practice, and evaluate the cost-shifting from leadership to members.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, academic_interpreters, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective action problem of institutional survival under existential federal pressure by providing a single authoritative statement that all members can point to as grounds for abandoning publicly visible plural marriage, thereby preventing schism and preserving the organized community.
% TRANSFER_FUNCTION: Transfers interpretive certainty and doctrinal stability from rank-and-file members to institutional leadership, who retain exclusive control over the ambiguous scope of the Manifesto and the theological meaning of 'cease to practice' while members bear the uncertainty of not knowing whether plural marriage remains an eternal principle.
% ABSENT_VOICES: Federal authorities, who enforced the external pressure but were excluded from the internal theological negotiation over the Manifesto's meaning; post-Manifesto plural wives, whose marriages were authorized privately but denied publicly; and fundamentalist dissenters, who were excommunicated for insisting the original doctrine remained binding.
% DISAPPEARANCE_RATIONALE: Without the Manifesto's scope ambiguity, the Church could not simultaneously satisfy federal demands for visible compliance and retain doctrinal flexibility on plural marriage as an eternal principle; the result would have been either explicit doctrinal repudiation (alienating the core), continued open resistance (destroying the institution), or immediate schism.
% FOUNDING_PROBLEM: Federal anti-polygamy legislation (Edmunds-Tucker Act, seizure of church property, imprisonment of leaders) threatened the survival of the LDS Church as an organized body in the late 19th century.
% FOUNDING_PROBLEM_CORROBORATION: Federal court records, congressional debates, and non-Mormon historical scholarship corroborate the severity of the anti-polygamy campaign. The claim that the Manifesto was the necessary and sufficient solution is attested primarily by institutional leadership; independent historians corroborate the federal pressure but dispute that the specific ambiguity was the only possible solution.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high) because the constraint transfers interpretive certainty from members to leadership, who retain exclusive control over the Manifesto's meaning while members bear ambiguity. Suppression is 0.62 because persistence depends on disciplining dissenters, excommunicating fundamentalists, and maintaining information asymmetry between public doctrine and private authorization. Theater ratio is 0.45 and rising: the public presentation of the Manifesto as a straightforward prophetic command masks a growing share of performative activity devoted to managing the gap between public monogamy and continued secret plural marriages by leaders. Resistance at 0.55 reflects sustained underground practice, apostate movements, and member confusion. The temporal series show extraction and theater accumulating as the ambiguity matured and the divergence between public and private practice widened from 1890 to 1910.
 *
 * PERSPECTIVAL GAP:
 *   The institutional leadership seat experiences the constraint as successful coordination: the Church survived federal assault, property was restored, statehood was achieved, and doctrinal space was preserved for future development. The rank-and-file and post-Manifesto plural family seats experience the same structure as extraction: their theological certainty, legal safety, and marital legitimacy were sacrificed to create maneuvering room for leadership. The engine computes this divergence from the structural dataâsame constraint, different directionality derived from beneficiary/victim position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is declared in beneficiaries and carries agenda_setter role with arbitrage-grade exit, placing d near the beneficiary end; effective extraction is damped and may invert to subsidy. Rank-and-file members are declared victims with identity_locked exit, placing d near the full-target end; effective extraction is amplified. Post-Manifesto plural families are victims with trapped exit, sitting at the extreme target end. Academic interpreters carry analytical exit and observer role; their directionality is neutral. The asymmetry is structural: the same ambiguity that subsidizes leadership extracts from members.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâexistential federal pressure under the Edmunds-Tucker regimeâwas dead by 1896 (Utah statehood) at the latest. Yet the constraint's ambiguity persisted beyond its founding crisis, with post-Manifesto plural marriages authorized by apostles as late as 1904. The R5 genealogy interview (founding_problem_status: dead, disappearance_verdict: world_rearranges) flags this as a mandatrophy candidate. However, during the authored interval the constraint retains a genuine coordination function (preventing schism, preserving the institutional body) alongside its extraction, which prevents premature piton classification. The hybrid reading specifically asserts institutional agency: the leadership chose ambiguity as a tool, which distinguishes this from pure inertial persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_intent_of_ambiguity,
    'Was the scope ambiguity of the 1890 Manifesto intentionally crafted by institutional leadership as a strategic instrument, or did it emerge organically from the political necessity of securing consensus among authorities holding divergent views?',
    'Historical archival discovery of private deliberations among the First Presidency and Quorum of the Twelve in 1889â1890.',
    'If intentional, extraction is higher (active management of member uncertainty); if organic, extraction is lower (coordination side effect of internal consensus-building).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_intent_of_ambiguity, empirical, 'Whether the Manifesto''s ambiguity was strategic design or emergent compromise.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (enforced by institutional information control, threat of excommunication, and social ostracism) or internalized (members believe uncertainty is a test of faith and voluntarily suppress their own doubts)?',
    'Post-exit narrative analysis: do former members report persistent uncertainty and institutional fear, or do they quickly develop clear alternative frameworks?',
    'If internalized, effective suppression is higher than structural measures suggest; if structural, the constraint weakens rapidly under information access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for rank-and-file members.').

omega_variable(
    kernel_reading_foreclosure,
    'Does adopting the hybrid pragmatic reading logically foreclose the endogenous reinterpretation reading (genuine prophetic revelation) within a single theological framework, or can both coexist as complementary descriptions of the same event?',
    'Analysis of whether the hybrid reading''s claim of ''strategic'' deployment of prophetic authority is logically compatible with the endogenous reading''s claim of genuine divine command.',
    'If foreclosure holds, the kernel is structurally fractured and the two readings are mutually exclusive; if coexistence holds, the disagreement is perspectival rather than logical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between hybrid pragmatic and endogenous revelation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t4, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(marr_tr_t8, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(marr_tr_t12, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(marr_tr_t16, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t4, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(marr_be_t8, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(marr_be_t12, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(marr_be_t16, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(marr_su_t4, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(marr_su_t8, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(marr_su_t12, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(marr_su_t16, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% The kernel marriage_commitment_legitimacy decomposes into three structurally distinct constraints (readings) because the natural-language label 'the Manifesto' conflates three different claims about authority, agency, and doctrinal stability. The exogenous reading centers on coercion and doctrinal rigidity; the endogenous reading centers on revelation and divine command; this hybrid reading centers on strategic ambiguity and institutional adaptation. Each has a distinct epsilon, beneficiary structure, and classification. This reading influences the exogenous reading by introducing institutional agency and doctrinal flexibility that complicate pure-coercion narratives, and coexists with the endogenous reading because both can accommodate prophetic authority in a single theological framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
