% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Progressive Restriction Reading of Quranic Naskh (Abrogation)
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   The progressive-restriction reading of Quranic naskh (abrogation)
 *   interprets textual contradictions between permissive earlier verses and
 *   restrictive later verses as evidence of divine pedagogical intent, not as
 *   textual invalidation. Earlier permissions are reframed as temporary
 *   accommodations guiding the community toward final restrictions that
 *   represent God's ultimate will. This reading benefits contemporary legal
 *   interpreters seeking to anchor restrictive contemporary rulings in
 *   theological necessity rather than contextual preference. It disadvantages
 *   literalists who cite early permissions as permanently valid and powerless
 *   practitioners whose lived practice relies on those permissions. The
 *   constraint operates as a tangled rope: genuine coordination function
 *   (resolving apparent textual contradictions into a coherent narrative),
 *   active enforcement through institutional gatekeeping of hermeneutical
 *   legitimacy, and asymmetric extraction (restricting whose interpretive
 *   voice counts).
 *
 * KEY AGENTS:
 *   - Evolutionary legal interpreters (beneficiary, organized): frame restrictions as final divine intent, gain hermeneutical authority
 *   - Literalist traditionalists (payer, organized): cite earlier permissions as valid, displaced from institutional authority
 *   - Contemporary practitioners (payer, powerless): practice according to earlier permissions, lose textual legitimacy
 *   - Quranic exegesis institutions (agenda setter, institutional): enforce progressive-restriction reading through curriculum and peer review
 *   - Classical abrogation proponents (excluded, organized): rival interpretive methodology, marginalized from authority structures
 *   - Contextual harmonization advocates (excluded, moderate): alternative resolution strategy, excluded from institutional discourse
 *   - Contemporary Muslim publics (beneficiary, powerless): benefit from coherent narrative but depend on interpreters
 *   - Historical revelation scholarship (observer, analytical): studies chronological revelation and historical context
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.68).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.71).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.68).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Progressive Restriction Reading of Quranic Naskh (Abrogation)").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, 'c800b89f-8b24-4b09-b175-f9d4bc015edb').
narrative_ontology:cs_kernel_codification('c800b89f-8b24-4b09-b175-f9d4bc015edb', fixed_text).
narrative_ontology:cs_authority_grounding('c800b89f-8b24-4b09-b175-f9d4bc015edb', lineage).
narrative_ontology:cs_interpretation_layer_present('c800b89f-8b24-4b09-b175-f9d4bc015edb').
narrative_ontology:cs_reading_relation('c800b89f-8b24-4b09-b175-f9d4bc015edb', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('c800b89f-8b24-4b09-b175-f9d4bc015edb', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('c800b89f-8b24-4b09-b175-f9d4bc015edb', foundational, divine_pedagogical_progression).
narrative_ontology:cs_axiom_status(divine_pedagogical_progression, holdable).
narrative_ontology:cs_axiom_grounding('c800b89f-8b24-4b09-b175-f9d4bc015edb', divine_pedagogical_progression, deontological).
narrative_ontology:cs_axiom('c800b89f-8b24-4b09-b175-f9d4bc015edb', secondary, restriction_as_final_intent).
narrative_ontology:cs_axiom_status(restriction_as_final_intent, holdable).
narrative_ontology:cs_axiom_grounding('c800b89f-8b24-4b09-b175-f9d4bc015edb', restriction_as_final_intent, deontological).
narrative_ontology:cs_reference_frame('c800b89f-8b24-4b09-b175-f9d4bc015edb', quranic_pedagogical_coherence).
narrative_ontology:cs_drift_state('c800b89f-8b24-4b09-b175-f9d4bc015edb', contemporary_institutional_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c800b89f-8b24-4b09-b175-f9d4bc015edb', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, evolutionary_legal_interpreters).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, contemporary_reformist_scholars).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, literalist_traditionalists).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, earlier_permissive_ruling_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, contemporary_muslim_publics).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, contemporary_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legal scholars and jurists who advance the progressive-restriction reading of Quranic contradictions as theological pedagogy. They benefit from institutional adoption of this methodology because it provides them hermeneutical authority and legitimacy in academic and fatwa-setting institutions. They set the interpretive agenda by training new generations of scholars, publishing commentaries, and controlling scholarly publishing standards. Their professional identity is fused with this interpretive methodology.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, evolutionary_legal_interpreters, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, evolutionary_legal_interpreters, agenda_setter).

% Scholars and communities committed to reading the Quranic text according to classical abrogation or other traditionalist methodologies. They bear the cost of institutional marginalization: their interpretive methods are increasingly treated as methodologically inferior despite their historical grounding and continued scholarly validity. They cannot exit because their professional and religious identity is bound to these methodologies, and abandoning them would mean departing from their scholarly tradition.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, literalist_traditionalists, payer,
    organized, generational, identity_locked, global).

% Individuals and communities who practice according to earlier permissive Quranic rulings that the progressive-restriction reading reframes as transcended pedagogical accommodations. They lose textual legitimacy for their practices when those practices are reinterpreted as evolutionarily superseded rather than permanently valid. They have no hermeneutical voice in institutional discourse and cannot exit their cultural and religious identity.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contemporary_practitioners, payer,
    powerless, biographical, trapped, global).

% Scholars who argue that later Quranic verses abrogate earlier verses through explicit or implicit chronological cancellation, without the pedagogical framing of the progressive-restriction reading. They maintain this is a valid and simpler methodology but are excluded from dominant institutional positions when progressive-restriction scholars control curriculum, publication, and fatwa-setting. They could exit by adopting the progressive-restriction reading but this would require abandoning their methodological commitments.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_abrogation_proponents, excluded,
    organized, generational, constrained, global).

% Scholars arguing that apparent Quranic contradictions are resolved through contextual specification rather than chronological progression or abrogation. They maintain earlier and later verses coexist in their specific contexts. They are excluded from institutional authority in regions where progressive-restriction scholars dominate, though they may maintain institutional positions in other regions or in smaller scholarly networks.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextual_harmonization_advocates, excluded,
    moderate, generational, constrained, regional).

% Universities, seminaries, fatwa councils, and Islamic legal institutions that establish and enforce standards for legitimate Quranic interpretation and jurisprudence. They enforce the progressive-restriction reading through hiring, curriculum design, journal peer review, and fatwa issuance. They benefit from the methodological clarity this reading provides (one authoritative interpretation path) and maintain institutional authority by controlling which interpretive methods are taught as standard.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, quranic_exegesis_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Lay Muslims seeking guidance on contemporary Islamic practice. They benefit from the progressive-restriction reading because it provides a coherent theological narrative of divine pedagogical intent progressing toward final restrictions, which can anchor contemporary legal rulings in theological necessity rather than contextual preference or scholarly opinion. They are dependent on institutional interpreters to understand this narrative and have no independent way to evaluate competing hermeneutical claims.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contemporary_muslim_publics, beneficiary,
    powerless, biographical, identity_locked, global).

% Academic historians, philologists, and comparative religionists studying the chronology of Quranic revelation, historical contexts of early Islam, and textual analysis. They take no institutional role in the constraint but observe and comment on its operation through scholarly publication and cross-disciplinary review. Their historical and philological findings inform but do not control the hermeneutical constraint.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, historical_revelation_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__progressive_restriction, quranic_exegesis_institutions).
narrative_ontology:fixing_cost_class(naskh_principle__progressive_restriction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a coherent interpretive methodology for reading the Quranic text as a unified divine pedagogical arc rather than as a collection of contextual rulings or chronologically superseded prescriptions. Reduces hermeneutical indeterminacy by providing a systematic rule: restrictions understood as final intent, permissions as transitional accommodations.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from those citing earlier permissive verses to those advancing the progressive-restriction narrative. Moves interpretive legitimacy from literalist and contextual readings to evolutionary-legal frameworks. In institutional settings, transfers control of fatwa-setting and curriculum from traditionalists to reformist scholars.
% ABSENT_VOICES: Practitioners relying on earlier permissive rulings (the powerless users of those permissions) are structurally absent from hermeneutical discourse; classical-abrogation and contextual-harmonization scholars are excluded from contemporary institutional authority structures. The reading assumes contemporary practitioners and institutions should align with the progressive-restriction framework without consulting those whose practice depends on earlier permissions.
% DISAPPEARANCE_RATIONALE: If this hermeneutical reading disappeared, the Quranic text would remain unchanged, but the interpretive apparatus for resolving apparent contradictions would collapse. Traditionalists argue the world rearranges: earlier permissions regain validity without a declared abrogation. Progressivists argue the world unchanged: other methodologies (classical abrogation, harmonization) would fill the space. The contest is over which interpretive absence leaves practice most coherent.
% FOUNDING_PROBLEM: Early Islamic jurisprudence required a method to address apparent contradictions between Quranic verses that seemed to permit and restrict the same practice. The founding problem was how to maintain textual coherence when the same community received conflicting directives at different revelation moments.
% FOUNDING_PROBLEM_CORROBORATION: All competing readings (classical abrogation, contextual harmonization, and progressive restriction) attest the founding problem is live: apparent textual contradictions require methodological resolution. The progressive-restriction reading uniquely claims that resolution happens through pedagogical progression rather than chronological cancellation or contextual specification. This claim is corroborated by contemporary reformist legal scholarship and institutional authority, but is contested by traditionalist exegetes and historians who argue earlier permissions remain valid or that contextual methods suffice.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, contested).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness (0.68) is high because the progressive-restriction reading concentrates hermeneutical authority in institutional hands and denies competing readings equal legitimacy status. Suppression is substantial (0.71) because enforcement operates through controlling who can publish Quranic exegesis, which scholars are trained, and which interpretations count as legitimate jurisprudence. Theater ratio rises over the interval (0.28→0.42) because as the reading becomes institutionally dominant, more of the work shifts from genuine hermeneutical innovation to performance of consensus and enforcement of methodological conformity. Accessibility collapse (0.63) is moderate: alternatives exist (classical abrogation, harmonization) but require exiting institutional academic structures. Resistance (0.58) reflects significant pushback from traditionalist and contextual-harmonization scholars who maintain their interpretive methods despite institutional pressure. The measurement series demonstrates extractiveness accumulation: the reading gains institutional dominance over time, requiring increasing suppression of alternatives. One shared time grid: every metric is authored at t0, 3, 6, 9, 12, 15 (15-year interval representing the modern period of progressive-restriction institutionalization in academic jurisprudence).
 *
 * PERSPECTIVAL GAP:
 *   Beneficiary seats (evolutionary interpreters, exegesis institutions, contemporary Muslim publics) experience this constraint as coherent theological pedagogy resolving textual indeterminacy. From these seats, the reading appears as discovery of God's progressive intent. Payer seats (literalists, practitioners of early permissions) experience it as hermeneutical coercion: their valid interpretive methods and their living practices are delegitimized by fiat. The engine will compute different per-seat types from this structural asymmetry: beneficiaries see rope (coordination with incidental benefit), payers see snare (extraction dressed as methodology). The claim anchors this divergence in the reading's own theological logic; the metrics capture its institutional operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Evolutionary interpreters and exegesis institutions hold d near the beneficiary end (0.1-0.3): they benefit from institutional authority and face no exit pressure. Literalist traditionalists hold d near the target end (0.7-0.85): they bear the cost of delegitimization but are identity-locked to their methodological commitments, increasing their d. Powerless contemporary practitioners hold d near full target (0.85-1.0): they practice according to invalidated rulings, have no hermeneutical voice, and are trapped by cultural and religious identity. Classical abrogation and contextual harmonization scholars hold d moderately high (0.6-0.75) because they are actively excluded from institutional authority structures despite maintaining scholarly competence. The progressive-restriction reading's claim rests on theological grounds (divine pedagogical intent), but its operation depends on institutional power to control which readings count as legitimate scholarship.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy detection hinges on the gap between founding problem and current function. The founding problem is genuine: early Islamic jurisprudence required a method for resolving apparent textual contradictions. The progressive-restriction reading offers one genuine solution: read contradictions as evidence of pedagogical progression. However, the constraint's current operation diverges from this solution function. At time 0, the constraint primarily serves coordination: scholars genuinely debate how to resolve textual contradictions using progressive-restriction logic. By time 15, the constraint has become gatekeeping: institutional power controls which interpretive methods are taught, funded, and published, independent of their hermeneutical coherence. The theater ratio tracks this shift: early performance reflects genuine scholarly engagement; later performance reflects enforcement of methodological conformity. Mandatrophy is not yet resolved (the founding problem persists as live), but accumulating performance cost (rising theater_ratio) signals that the constraint is increasingly maintained by institutional inertia rather than by its solution value. A true mandatrophy resolution would require asking whether the progressive-restriction reading has become a preferred interpretation among institutional actors primarily because of its institutional position, not because of its hermeneutical superiority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_intent_vs_textual_ambiguity,
    'Is the movement from permissive to restrictive verses evidence of divine pedagogical intent, or evidence of the Quranic text responding to contextual circumstances without a predetermined pedagogical arc?',
    'Historical analysis of the revelatory contexts for specific verse pairs (e.g., financial practices, marriage conditions) to determine whether restrictions followed contextual need or were intentional revisions of earlier permission. Literary analysis of the Quranic text for markers of pedagogical intentionality versus contextual responsiveness.',
    'If pedagogical intent is confirmed, the progressive-restriction reading gains hermeneutical authority and final restrictions are truly final. If contextual responsiveness dominates, earlier permissions remain conditionally valid and contextual harmonization becomes more plausible than progressive restriction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pedagogical_intent_vs_textual_ambiguity, conceptual, 'Whether the Quranic text exhibits intentional pedagogical progression or contextual responsiveness to historical circumstances.').

omega_variable(
    institutional_authority_vs_hermeneutical_merit,
    'Is the institutional dominance of the progressive-restriction reading due to its superior hermeneutical coherence and theological necessity, or due to its utility for institutional authority in enforcing unified legal doctrine?',
    'Comparative analysis of citation patterns in scholarly literature: if the reading dominates despite hermeneutical challenges, institutional utility is a primary driver. Survey of scholars across institutional and non-institutional settings to measure independent interpretive preference. Historical analysis of when institutional adoption occurred relative to the reading''s theoretical development.',
    'If institutional utility is primary, the constraint''s operation is primarily extractive (institutional gatekeeping) and should be classified as snare. If hermeneutical merit is primary, the constraint offers genuine coordination value. Current evidence suggests mixed causation; the measurement series tracks rising theater_ratio as institutional enforcement increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_vs_hermeneutical_merit, empirical, 'Whether the reading''s institutional dominance reflects hermeneutical superiority or institutional utility for authority consolidation.').

omega_variable(
    suppression_mechanism_internalized_or_structural,
    'To what extent is literalist scholars'' marginalization from institutional authority due to structural barriers (formal exclusion, funding control, publication gatekeeping) versus internalized constraints (acceptance of methodological inferiority, self-selected departure from institutional scholarship)?',
    'Post-exit analysis: if traditionalists who leave institutional academia continue productive hermeneutical work in non-institutional settings, suppression is primarily structural. If traditionalists exit and cease scholarly work, suppression is partially internalized. Qualitative interviews with traditionalist scholars about perceived barriers versus perceived methodological validity.',
    'If suppression is primarily structural, the constraint''s extractiveness is higher than the scalar metric suggests—the target continues bearing suppression after the institutional barrier is removed. If primarily internalized, the institutional reading has achieved cognitive capture of the target group. Mixed case (current evidence suggests 60-70% structural, 30-40% internalized) means the constraint has both institutional enforcement machinery and internalized professional identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_or_structural, empirical, 'Whether suppression of alternative readings operates through structural barriers or internalized belief in methodological invalidity.').

omega_variable(
    kernel_reading_constitution,
    'Is the progressive-restriction reading a reading of a single kernel (the Quranic text on contradiction resolution), or does it constitute a separate constraint alongside classical abrogation and contextual harmonization?',
    'Kernel test: if all three readings answer the same question (how to resolve apparent Quranic contradictions) from the same textual source (the Quranic verses), they are readings of a single kernel. If they diverge on what constitutes a ''contradiction'' or what counts as ''resolution,'' they are separate constraints. Contemporary jurisprudential authority claims all three are readings of the same methodological question; traditionalists argue they answer different questions (how to understand revelation''s relationship to practice).',
    'If single kernel with three readings, the constraint''s classification depends on which reading is instantiated in any given institutional context. If three separate constraints, the progressive-restriction constraint is one among competitive alternatives with no logical relationship. The kernel framing is adopted here per the committer frame''s instruction; a separate analysis would decompose into three constraint files per the ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_constitution, conceptual, 'Whether the three abrogation methodologies are readings of a single hermeneutical kernel or three separate constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(nask_tr_t0, observed).
narrative_ontology:measurement(nask_tr_t3, naskh_principle__progressive_restriction, theater_ratio, 3, 0.32).
narrative_ontology:measurement_basis(nask_tr_t3, observed).
narrative_ontology:measurement(nask_tr_t6, naskh_principle__progressive_restriction, theater_ratio, 6, 0.37).
narrative_ontology:measurement_basis(nask_tr_t6, observed).
narrative_ontology:measurement(nask_tr_t9, naskh_principle__progressive_restriction, theater_ratio, 9, 0.4).
narrative_ontology:measurement_basis(nask_tr_t9, observed).
narrative_ontology:measurement(nask_tr_t12, naskh_principle__progressive_restriction, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(nask_tr_t12, observed).
narrative_ontology:measurement(nask_tr_t15, naskh_principle__progressive_restriction, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(nask_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(nask_be_t0, observed).
narrative_ontology:measurement(nask_be_t3, naskh_principle__progressive_restriction, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(nask_be_t3, observed).
narrative_ontology:measurement(nask_be_t6, naskh_principle__progressive_restriction, base_extractiveness, 6, 0.63).
narrative_ontology:measurement_basis(nask_be_t6, observed).
narrative_ontology:measurement(nask_be_t9, naskh_principle__progressive_restriction, base_extractiveness, 9, 0.66).
narrative_ontology:measurement_basis(nask_be_t9, observed).
narrative_ontology:measurement(nask_be_t12, naskh_principle__progressive_restriction, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(nask_be_t12, observed).
narrative_ontology:measurement(nask_be_t15, naskh_principle__progressive_restriction, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(nask_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(nask_su_t0, observed).
narrative_ontology:measurement(nask_su_t3, naskh_principle__progressive_restriction, suppression_requirement, 3, 0.62).
narrative_ontology:measurement_basis(nask_su_t3, observed).
narrative_ontology:measurement(nask_su_t6, naskh_principle__progressive_restriction, suppression_requirement, 6, 0.66).
narrative_ontology:measurement_basis(nask_su_t6, observed).
narrative_ontology:measurement(nask_su_t9, naskh_principle__progressive_restriction, suppression_requirement, 9, 0.69).
narrative_ontology:measurement_basis(nask_su_t9, observed).
narrative_ontology:measurement(nask_su_t12, naskh_principle__progressive_restriction, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(nask_su_t12, observed).
narrative_ontology:measurement(nask_su_t15, naskh_principle__progressive_restriction, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(nask_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__progressive_restriction, 0.12).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__contextual_harmonization).

% DUAL FORMULATION NOTE:
% The naskh_principle kernel decomposes into three structurally distinct constraints per the ε-invariance principle: classical_abrogation (chronological supersession, lower extraction, traditionalist operation), progressive_restriction (this story; pedagogical arc, moderate extraction, institutional operation), and contextual_harmonization (context-sensitive validity, lower extraction, pluralist operation). Each reading instantiates a different constraint with different ε values and different beneficiary/victim structures. The readings are linked by network.affects_constraints to represent their hermeneutical interdependence: the dominance of one reading affects the viability and institutional position of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__progressive_restriction, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
