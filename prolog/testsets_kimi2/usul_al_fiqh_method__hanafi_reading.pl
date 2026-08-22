% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Usul al-Fiqh: Expansive Qiyas and Jurist Authority
 *   domain: legal/religious/islamic_jurisprudence
 *
 * SUMMARY:
 *   The Hanafi reading of usul al-fiqh treats qiyas (analogical reasoning) as
 *   expansively applicable wherever textual sources are silent, supplements
 *   analogy with ra'y (reasoned opinion) at its limits, and permits istihsan
 *   (juristic preference) to depart from strict analogy for public interest.
 *   This reading concentrates interpretive authority in a rationalist-trained
 *   jurist class and systematically overrides textualist claims that would
 *   restrict innovation. The constraint is claimed as coordination (filling
 *   legal gaps) but structurally operates as tangled rope: genuine
 *   coordination function paired with asymmetric extraction of authority to
 *   the jurist class.
 *
 * KEY AGENTS:
 *   - Rationalist jurist class (agenda_setter/beneficiary): Institutional power, identity-locked exit, administers the interpretive framework and captures authority through qiyas and istihsan.
 *   - Textualist scholars (payer): Moderate power, constrained exit, bear the cost of methodological marginalization as their claim to limit innovation is overridden.
 *   - Abbasid state judiciary (beneficiary): Institutional power, constrained exit, benefits from flexible legal tools for governance but depends on the jurist class.
 *   - Lay Muslim public (excluded): Powerless, constrained exit, subject to rulings but absent from methodological debate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.65).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.6).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Usul al-Fiqh: Expansive Qiyas and Jurist Authority").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "legal/religious/islamic_jurisprudence").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, 'bd1aa471-fd6b-428d-ab93-80ddee864a6b').
narrative_ontology:cs_kernel_codification('bd1aa471-fd6b-428d-ab93-80ddee864a6b', fixed_text).
narrative_ontology:cs_authority_grounding('bd1aa471-fd6b-428d-ab93-80ddee864a6b', lineage).
narrative_ontology:cs_interpretation_layer_present('bd1aa471-fd6b-428d-ab93-80ddee864a6b').
narrative_ontology:cs_reading_relation('bd1aa471-fd6b-428d-ab93-80ddee864a6b', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd1aa471-fd6b-428d-ab93-80ddee864a6b', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_reading_relation('bd1aa471-fd6b-428d-ab93-80ddee864a6b', usul_al_fiqh_method__hanbali_reading, influences).
narrative_ontology:cs_axiom('bd1aa471-fd6b-428d-ab93-80ddee864a6b', foundational, expansive_qiyas_as_valid_source).
narrative_ontology:cs_axiom_status(expansive_qiyas_as_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('bd1aa471-fd6b-428d-ab93-80ddee864a6b', expansive_qiyas_as_valid_source, conventional).
narrative_ontology:cs_axiom('bd1aa471-fd6b-428d-ab93-80ddee864a6b', foundational, istihsan_juristic_preference_authority).
narrative_ontology:cs_axiom_status(istihsan_juristic_preference_authority, holdable).
narrative_ontology:cs_axiom_grounding('bd1aa471-fd6b-428d-ab93-80ddee864a6b', istihsan_juristic_preference_authority, conventional).
narrative_ontology:cs_reference_frame('bd1aa471-fd6b-428d-ab93-80ddee864a6b', hanafi_school_authority_framework).
narrative_ontology:cs_drift_state('bd1aa471-fd6b-428d-ab93-80ddee864a6b', classical_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd1aa471-fd6b-428d-ab93-80ddee864a6b', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, rationalist_jurist_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, abbasid_state_judiciary).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_scholars).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, istihsan_validity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, ra_y_as_legal_source).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, expansive_qiyas_scope).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in the Hanafi school's rationalist methodology, they administer the framework of qiyas, ra'y, and istihsan. They determine the boundaries of textual silence, construct analogies, and decide when public interest warrants departure from strict analogy. Their professional authority, institutional positions in madrasas, and judicial appointments depend on the continued dominance of this interpretive framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rationalist_jurist_class, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, rationalist_jurist_class, beneficiary).

% Uphold the claim that textual sources should maximally restrict jurist innovation. They bear the cost of methodological marginalization as the Hanafi framework expands analogical reasoning into domains they consider textually regulated. Their objections are systematically overridden by Hanafi jurists invoking istihsan or ra'y, and their access to courts and teaching positions is limited in Hanafi-dominated institutions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_scholars, payer,
    moderate, generational, constrained, national).

% Relies on the Hanafi jurist class to produce workable legal rulings for governance across diverse territories. Benefits from the flexibility of expansive qiyas and istihsan to adapt law to administrative and fiscal needs without appearing to violate textual sources directly. Switching to a textualist framework would create immediate ungovernability in novel cases.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, abbasid_state_judiciary, beneficiary,
    institutional, generational, constrained, national).

% Subject to rulings produced by the Hanafi framework but excluded from the methodological debates that determine how textual silence is filled. They lack the training to engage in qiyas or ra'y and must accept the jurist's determination of public interest as authoritative, with limited recourse to challenge the methodological premises behind the ruling.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_muslim_public, excluded,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, rationalist_jurist_class).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic method for deriving legal rulings in cases where Quranic and authenticated hadith texts are silent, using analogy, reasoned opinion, and juristic preference to maintain legal coherence across novel administrative, commercial, and social situations in a rapidly expanding polity.
% TRANSFER_FUNCTION: Moves interpretive authority from the fixed textual sources to the trained rationalist jurist class, transferring the power to declare textual silence, construct analogies, and override strict analogy in the name of public interest.
% ABSENT_VOICES: Textualist scholars and the lay Muslim public are largely absent from methodological formulation; textualist objections to expansive analogy are overridden by the jurist class's invocation of ra'y and istihsan, while the public lacks the training to contest the framework.
% DISAPPEARANCE_RATIONALE: If the Hanafi methodological framework vanished overnight, the Abbasid judiciary would lose its primary legal tool for ruling on unprecedented cases, madrasa curricula would collapse, and textualist scholars would regain ground in defining the scope of legitimate innovation. The distribution of jurist authority would reorganize around stricter textual or traditionalist methods.
% FOUNDING_PROBLEM: The rapid expansion of the Islamic polity under the Abbasids generated countless legal questions not explicitly addressed by Quranic revelation or authenticated hadith, requiring a scalable, systematic method of legal derivation that could keep pace with administrative and commercial novelty.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of early Abbasid administrative and judicial expansion attest to the volume of unprecedented legal cases. Corroboration from outside the benefiting rationalist jurist class comes from state chronicles and non-Hanafi historians documenting the administrative pressure for flexible legal tools.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects substantial authority concentration in the jurist class through control of analogical scope. Suppression (0.60) captures the active marginalization of textualist alternatives via institutional control of courts and madrasas. Theater_ratio (0.30) acknowledges the genuine legal reasoning function while recognizing that a share of juristic activity defends methodological autonomy rather than solves novel cases. Accessibility_collapse (0.50) is moderate because textualist alternatives survive but are institutionally disadvantaged. Resistance (0.55) reflects ongoing textualist counter-argumentation and sporadic state challenges to jurist independence.
 *
 * PERSPECTIVAL GAP:
 *   From the rationalist jurist seat, the arrangement is necessary coordination solving genuine textual silence in an expanding empire. From the textualist seat, it is an extraction mechanism that constructs silence to expand jurist power. The state judiciary experiences it as functional flexibility diffusely beneficial to governance. The engine computes this divergence from the structural data without adjudicating the theological merits of either seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist jurists are beneficiaries (low d) because the constraint subsidizes their authority, income, and institutional position. Textualist scholars are targets (high d) because the constraint extracts from their claim to limit innovation and suppresses their methodological alternative through curriculum and appointment control. The state judiciary sits nearer symmetric (moderate d) â it benefits from flexibility but pays the cost of dependence on the jurist class for legal administration.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination (genuine gap-filling in unprecedented cases) and extraction (authority concentration in an identifiable jurist class). Without the coordination component, it would be a snare of pure jurist domination; without the extraction component, it would be a rope of shared interpretive method. The founding problem (legal gaps in imperial expansion) remains live, preventing piton classification, though institutional commentary traditions have drifted from the founder's direct practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jurist_authority_vs_textual_fidelity,
    'Does the Hanafi methodology solve genuine textual silence, or does it construct silence to expand jurist authority?',
    'Comparative historical analysis of qiyas applications: cataloging cases where qiyas produced rulings versus cases where texts were available but interpreted as silent through Hanafi hermeneutic techniques.',
    'If silence is largely constructed, extraction exceeds coordination and the tangled rope classification leans toward snare; if genuine, the coordination function is validated and the classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurist_authority_vs_textual_fidelity, empirical, 'Whether textual silence is discovered or manufactured to authorize jurist expansion.').

omega_variable(
    textualist_marginalization_mechanism,
    'Is the suppression of textualist alternatives achieved through institutional exclusion from courts and madrasas, or through internalized acceptance of Hanafi methodological superiority?',
    'Analysis of judicial appointment records and madrasa curricula versus textualist scholars'' own discursive self-representation in polemical literature.',
    'If institutional, suppression is structural and measurable; if internalized, effective suppression exceeds structural measures because textualists carry the constraint with them even when institutional barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_marginalization_mechanism, empirical, 'Structural versus internalized suppression of textualist methodological dissent.').

omega_variable(
    sibling_reading_boundary,
    'Is the Hanafi reading''s expansive qiyas structurally separable from the textualist kernel, or is it an inevitable interpretive layer that any functioning legal system would develop?',
    'Study of pre-Hanafi juristic practice and non-Hanafi textualist court systems to determine whether strict textualism can function without analogous reasoning at all.',
    'If separable, the reading is a genuine alternative constraint with distinct epsilon; if inevitable, the kernel readings may collapse into a single commitment system with different emphasis, reducing the structural delta between siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary, conceptual, 'Whether the Hanafi reading is an alternative constraint or an inevitable interpretive layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_hanafi_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(usul_hanafi_tr_t8, usul_al_fiqh_method__hanafi_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(usul_hanafi_tr_t16, usul_al_fiqh_method__hanafi_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(usul_hanafi_tr_t24, usul_al_fiqh_method__hanafi_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(usul_hanafi_tr_t32, usul_al_fiqh_method__hanafi_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(usul_hanafi_tr_t40, usul_al_fiqh_method__hanafi_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(usul_hanafi_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(usul_hanafi_be_t8, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(usul_hanafi_be_t16, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(usul_hanafi_be_t24, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(usul_hanafi_be_t32, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(usul_hanafi_be_t40, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(usul_hanafi_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_hanafi_su_t8, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(usul_hanafi_su_t16, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(usul_hanafi_su_t24, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(usul_hanafi_su_t32, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(usul_hanafi_su_t40, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% The usul_al_fiqh_method kernel decomposes into four sibling constraints (Hanafi, Maliki, Shafi'i, Hanbali readings) because the natural-language label 'usul al-fiqh method' conflates structurally distinct claims about the scope of qiyas, the validity of istihsan, and the weight of textual versus rational sources. Each reading has a distinct epsilon, beneficiary/victim structure, and classification. This story links to the Shafi'i and Hanbali readings as downstream reactions influenced by Hanafi rationalism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
