% ============================================================================
% CONSTRAINT STORY: maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maliki_reading, []).

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
 *   constraint_id: maliki_reading
 *   human_readable: Maliki Methodological Framework for Islamic Legal Reasoning
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The Maliki methodological framework represents one of four major
 *   approaches to Islamic legal reasoning (usul al-fiqh). It distinguishes
 *   itself by elevating Medinan transmitted practice ('amal ahl al-Madina) to
 *   independent evidentiary status alongside hadith, accepting unrestricted
 *   public interest (maslaha mursala) as a valid legal source, and
 *   systematically integrating regional custom ('urf) where it does not
 *   contradict explicit textual rulings. The framework emerged in 8th-century
 *   Medina and spread across North and West Africa, al-Andalus, and parts of
 *   the Arabian Peninsula. From the Maliki perspective, this methodological
 *   structure solves a genuine coordination problem: how to derive legal
 *   rulings when textual sources (Qur'an and hadith) are incomplete,
 *   ambiguous, or silent on novel cases. The elevation of Medinan practice
 *   grounds law in the transmitted consensus of the Prophetic community;
 *   maslaha provides a rational framework for addressing new circumstances;
 *   custom integration prevents legal alienation from community norms. The
 *   framework's extractiveness (0.28) reflects moderate institutional
 *   consolidation and gatekeeping around madhab boundaries, but remains
 *   substantially lower than centralized legal systems because authority is
 *   distributed across textual sources, transmitted practice, and juristic
 *   reasoning rather than concentrated in a single institution. The theater
 *   ratio (0.22) indicates relatively low performative content — the
 *   methodological principles remain functionally engaged with legal
 *   problem-solving rather than maintained purely through ritual adherence.
 *
 * KEY AGENTS:
 *   - Regional Customary Practitioners: Primary beneficiaries (moderate/mobile) — their existing practices gain authoritative legal status through 'urf integration
 *   - Medinan Legal Tradition Bearers: Primary beneficiaries (institutional/constrained) — their transmitted practices are elevated to independent evidentiary weight
 *   - Local Community Norms: Beneficiary (collective good) — regional variation is legitimated rather than suppressed by universalist textualism
 *   - Maliki Jurists: Institutional actors (institutional/constrained) — operate within the framework to derive rulings; benefit from methodological flexibility but constrained by madhab boundaries
 *   - Textualist Schools: Inter-institutional observers (institutional/constrained) — see both coordination function and extraction risk in custom/maslaha elevation
 *   - Regional Political Authorities: Beneficiaries (powerful/arbitrage) — can leverage flexible legal framework for pragmatic governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maliki_reading, 0.28).
domain_priors:suppression_score(maliki_reading, 0.35).
domain_priors:theater_ratio(maliki_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maliki_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(maliki_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(maliki_reading, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maliki_reading, rope).
narrative_ontology:human_readable(maliki_reading, "Maliki Methodological Framework for Islamic Legal Reasoning").
narrative_ontology:topic_domain(maliki_reading, "islamic_jurisprudence/legal_theory/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maliki_reading, 'a67edb9d-7810-46ee-9a72-e87560e3b5b3').
narrative_ontology:cs_kernel_codification('a67edb9d-7810-46ee-9a72-e87560e3b5b3', formalized).
narrative_ontology:cs_authority_grounding('a67edb9d-7810-46ee-9a72-e87560e3b5b3', lineage).
narrative_ontology:cs_interpretation_layer_present('a67edb9d-7810-46ee-9a72-e87560e3b5b3').
narrative_ontology:cs_reading_relation('a67edb9d-7810-46ee-9a72-e87560e3b5b3', maliki_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('a67edb9d-7810-46ee-9a72-e87560e3b5b3', maliki_reading__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('a67edb9d-7810-46ee-9a72-e87560e3b5b3', maliki_reading__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('a67edb9d-7810-46ee-9a72-e87560e3b5b3', foundational, medinan_practice_independent_authority).
narrative_ontology:cs_axiom_status(medinan_practice_independent_authority, holdable).
narrative_ontology:cs_axiom_grounding('a67edb9d-7810-46ee-9a72-e87560e3b5b3', medinan_practice_independent_authority, conventional).
narrative_ontology:cs_axiom('a67edb9d-7810-46ee-9a72-e87560e3b5b3', foundational, maslaha_unrestricted_legitimacy).
narrative_ontology:cs_axiom_status(maslaha_unrestricted_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a67edb9d-7810-46ee-9a72-e87560e3b5b3', maslaha_unrestricted_legitimacy, instrumental).
narrative_ontology:cs_axiom('a67edb9d-7810-46ee-9a72-e87560e3b5b3', secondary, custom_legal_integration).
narrative_ontology:cs_axiom_status(custom_legal_integration, holdable).
narrative_ontology:cs_axiom_grounding('a67edb9d-7810-46ee-9a72-e87560e3b5b3', custom_legal_integration, conventional).
narrative_ontology:cs_reference_frame('a67edb9d-7810-46ee-9a72-e87560e3b5b3', medinan_prophetic_practice_continuity).
narrative_ontology:cs_drift_state('a67edb9d-7810-46ee-9a72-e87560e3b5b3', contemporary_revival_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a67edb9d-7810-46ee-9a72-e87560e3b5b3', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maliki_reading, regional_customary_practitioners).
narrative_ontology:constraint_beneficiary(maliki_reading, medinan_legal_tradition_bearers).
narrative_ontology:constraint_beneficiary(maliki_reading, local_community_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MALIKI JURIST (ROPE) — Experiences the methodological framework as coordination solving a genuine problem: how to derive law when textual sources are incomplete or ambiguous. The elevation of Medinan practice and maslaha provides legitimate pathways for addressing novel cases without fabricating hadith or forcing texts. Constrained exit because switching madhahib carries reputational and institutional costs, but the constraint genuinely solves the under-determination problem.
constraint_indexing:constraint_classification(maliki_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL COMMUNITY MEMBER (ROPE) — Benefits from the integration of local custom into authoritative legal reasoning. The framework validates existing practices that might be rejected under stricter textualist methodologies. Mobile exit because individuals can relocate to regions following different madhahib if local rulings become oppressive, but in practice most experience the framework as accommodating rather than extractive.
constraint_indexing:constraint_classification(maliki_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: TEXTUALIST SCHOOL (TANGLED ROPE) — Sees both coordination and extraction. Coordination: the Maliki method prevents anarchic proliferation of unsupported opinions by grounding rulings in transmitted Medinan consensus. Extraction: the elevation of practice and custom above explicit textual evidence creates opportunities for regional elites to naturalize their preferences as 'custom' and for jurists to introduce innovations under the cover of maslaha. Constrained by institutional investment in alternative methodologies.
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: REGIONAL POLITICAL AUTHORITY (ROPE) — Benefits from a legal framework that can accommodate administrative pragmatism and regional variation without requiring central textual authority for every ruling. Arbitrage exit because political elites can forum-shop between madhahib or appoint compliant jurists. Experiences the framework as coordination enabling governance rather than as extraction.
constraint_indexing:constraint_classification(maliki_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — Classifies the Maliki framework as genuine coordination addressing the under-determination problem in legal reasoning. The methodological choices (Medinan practice, maslaha, custom) solve real problems: how to derive rulings when texts are silent, how to adapt law to regional variation, how to prevent juristic paralysis or hadith fabrication. The framework's extractiveness is low because it distributes authority across transmitted practice, public interest reasoning, and textual sources rather than concentrating it in any single institution. The suppression is moderate because madhhab affiliation carries institutional weight, but cross-madhab mobility exists and methodological critique remains possible within the tradition.
constraint_indexing:constraint_classification(maliki_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maliki_reading_tests).
:- end_tests(maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The framework creates some institutional gatekeeping around madhab affiliation and juristic authority, but extractiveness is limited by several structural factors: (1) distributed authority across multiple sources (text, practice, custom, reason) prevents concentration; (2) cross-madhab mobility exists both geographically and through scholarly critique; (3) the methodological principles genuinely solve coordination problems rather than merely concentrating rents. The modest extractiveness reflects institutional consolidation over time and the potential for maslaha/custom to be captured by elite preferences, but does not reach the levels characteristic of centralized legal monopolies. Suppression (0.35): Moderate. Madhab affiliation carries institutional weight — switching schools involves reputational costs, scholarly retraining, and potential loss of institutional position. However, suppression is limited by: (1) historical examples of scholars moving between madhahib; (2) ongoing inter-madhab dialogue and critique; (3) modern ijtihad revival reducing boundary rigidity. Higher than pure coordination would produce, but substantially lower than coercive legal systems. Theater ratio (0.22): Low. The methodological framework remains functionally engaged with legal problem-solving. Maslaha reasoning, custom integration, and appeal to Medinan practice are used to derive actual rulings addressing real cases, not merely performed as madhab loyalty rituals. Some theater exists in the formalization of madhab boundaries and performative adherence to school positions, but the core coordination function persists.
 *
 * PERSPECTIVAL GAP:
 *   The critical perspectival gap is between the Maliki jurist/community member (rope — genuine coordination solving under-determination) and the textualist school (tangled rope — coordination function mixed with extraction risk through custom/maslaha). The Maliki perspective experiences the framework as solving real problems: how to derive rulings when texts are silent, how to maintain connection to Prophetic community practice, how to accommodate legitimate regional variation. The textualist perspective acknowledges the coordination function but sees structural vulnerability: custom integration and unrestricted maslaha can become vehicles for elite preference naturalization or unconstrained juristic discretion. The analytical observer classification as rope reflects the assessment that the coordination function is genuine and the extraction risks, while real, are moderated by distributed authority and inter-madhab critique. The gap is not about whether coordination exists (all perspectives acknowledge it) but about whether the methodological flexibility creates systematic capture opportunities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and power/exit combinations. Regional customary practitioners and Medinan tradition bearers are primary beneficiaries — the framework elevates their practices to authoritative status, giving them low directionality values (d → 0) and negative or minimal effective extraction. Local community norms benefit as a collective good. Maliki jurists as institutional actors with constrained exit experience moderate directionality — they operate within and benefit from the framework but face institutional costs for exit. Textualist schools as inter-institutional observers with constrained exit see mixed coordination and extraction, resulting in moderate directionality. Regional political authorities with arbitrage exit experience low effective extraction — they can forum-shop and benefit from legal flexibility. No victims are declared because the framework does not systematically extract from identifiable groups; the textualist critique identifies potential rather than actual victims (those who might be harmed if custom/maslaha is captured, but this remains an omega uncertainty rather than a demonstrated structural extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing methodological framework (rope — genuine coordination addressing under-determination in legal reasoning) from potential misuse of that framework (extraction through custom capture or maslaha abuse, tracked as omega uncertainties). The Maliki method's coordination function is real: it prevents both juristic paralysis (when texts are silent) and hadith fabrication (when jurists feel pressured to find textual support for every ruling). The elevation of Medinan practice, maslaha, and custom provides legitimate pathways for legal reasoning in the inevitable gaps and ambiguities of textual sources. The framework does not mandate that custom or public interest override clear texts — both are explicitly subordinate to Qur'an and established Sunnah. The extraction risks identified by textualist critics are structural vulnerabilities (captured custom, unbounded maslaha) rather than inherent features. Whether these vulnerabilities are systematically exploited is an empirical question documented in the omegas, not a classification certainty. Current metrics reflect that exploitation is possible but not dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    custom_capture_risk,
    'Does the integration of ''urf (custom) as an independent source create systematic capture risk where regional elite preferences are naturalized as ''custom'' and thereby gain legal authority?',
    'Historical case analysis: track rulings justified by custom across different Maliki regions; identify whether ''custom'' disproportionately reflects elite vs. common practice; compare Maliki custom-based rulings to Shafi''i or Hanbali rejection of the same practices as unsupported innovation.',
    'If systematic capture exists: extractiveness should be revised upward (0.28 → 0.45+) and textualist perspective shifts from tangled_rope toward snare. If custom genuinely represents distributed community practice: current rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_capture_risk, empirical, 'Whether custom integration enables elite preference naturalization').

omega_variable(
    maslaha_boundary_ambiguity,
    'Where does maslaha mursala (unrestricted public interest) end and unconstrained juristic preference begin? Is there a stable boundary or does the concept collapse into ''what the jurist thinks is good''?',
    'Doctrinal analysis of maslaha criteria across Maliki authorities (al-Shatibi''s constraints vs. later expansions); empirical tracking of maslaha invocations that were later rejected vs. those that achieved consensus; comparison to utilitarian legal reasoning in other traditions.',
    'If stable boundary exists with inter-subjective verification: framework remains coordination (rope). If boundary collapses: maslaha becomes a blank check for juristic discretion, raising extractiveness and potentially shifting analytical classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_boundary_ambiguity, conceptual, 'Stability of maslaha mursala boundary criteria').

omega_variable(
    kernel_framing_under_determination,
    'Is the Maliki reading one instantiation of a contested usul al-fiqh kernel, or is each madhab reading a structurally independent constraint with separate kernels? Does ''Islamic legal methodology'' constitute one kernel with four readings, or four kernels?',
    'Cross-madhab analysis: do the methodologies share a common founding problem (deriving law from revelation) with different solutions, or do they define the problem itself differently? Historical analysis: did early jurists perceive themselves as offering competing readings of a shared question or as establishing independent frameworks?',
    'If one kernel: the committer structure applies and reading_relations are meaningful structural facts. If separate kernels: the readings are independent constraints and the kernel framing is a modern scholarly imposition that should be dissolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether madhahib are readings of one kernel or independent constraints').

omega_variable(
    medinan_practice_transmission_reliability,
    'Is ''amal ahl al-Madina genuinely transmitted communal practice, or is it a constructed tradition retrospectively attributed to the Medinan community to legitimate Maliki methodological preferences?',
    'Historical source criticism: evaluate earliest attestations of specific Medinan practices; cross-reference with non-Maliki sources from the same period; identify practices claimed as Medinan that have no pre-Maliki attestation.',
    'If genuinely transmitted: the framework''s coordination function is real (solves the problem of legal continuity with Prophetic community). If constructed: the ''Medinan practice'' claim is a legitimation strategy and extractiveness should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_practice_transmission_reliability, empirical, 'Historical reliability of transmitted Medinan practice claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maliki_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maliki_tr_t0, maliki_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(maliki_tr_t300, maliki_reading, theater_ratio, 300, 0.2).
narrative_ontology:measurement(maliki_tr_t600, maliki_reading, theater_ratio, 600, 0.22).
narrative_ontology:measurement(maliki_tr_t900, maliki_reading, theater_ratio, 900, 0.25).
narrative_ontology:measurement(maliki_tr_t1100, maliki_reading, theater_ratio, 1100, 0.22).

% Extraction over time
narrative_ontology:measurement(maliki_be_t0, maliki_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(maliki_be_t300, maliki_reading, base_extractiveness, 300, 0.25).
narrative_ontology:measurement(maliki_be_t600, maliki_reading, base_extractiveness, 600, 0.28).
narrative_ontology:measurement(maliki_be_t900, maliki_reading, base_extractiveness, 900, 0.32).
narrative_ontology:measurement(maliki_be_t1100, maliki_reading, base_extractiveness, 1100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maliki_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(maliki_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The Maliki reading is one of four methodological readings of the usul al-fiqh kernel. Each reading has its own extractiveness value reflecting its institutional structure and gatekeeping mechanisms. The coordination function (solving under-determination in legal reasoning) is shared across readings; the extraction mechanisms (madhab consolidation, authority concentration) differ. The readings are linked because they compete for institutional authority and because jurists historically engaged in cross-madhab critique and methodology borrowing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
