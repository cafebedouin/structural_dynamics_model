% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: classical_latin_standard__continuity_reading
 *   human_readable: Living-Practice (Continuity) Reading of the Classical Latin Standard
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the CONTINUITY reading of the contested
 *   classical_latin_standard kernel: the position that correct Latin is
 *   whatever form has been transmitted through unbroken institutional
 *   practice (chancery, liturgy, monastic scholarship), and that drift from
 *   Classical-era norms is legitimate development rather than corruption.
 *   This reading is structurally distinct from, and coexists alongside, two
 *   sibling readings authored as separate constraints: reconstruction_reading
 *   (correct Latin is only the philologically-recoverable Classical form, and
 *   medieval drift is degradation) and hybrid_reading (correctness requires
 *   both Classical textual fidelity and recognition of legitimate
 *   post-Classical technical/ecclesiastical development). Per the
 *   ε-invariance principle, these are not one constraint measured three ways
 *   — they are three constraints with different ε values, different
 *   beneficiary/victim structures, and different classifications, linked here
 *   only by network reference.
 *
 * KEY AGENTS:
 *   - ecclesiastical_latin_institutions: primary beneficiary and agenda-setter (institutional/arbitrage) — their accumulated usage is validated as correct
 *   - monastic_and_curial_scribes: secondary beneficiary (organized/constrained) — daily practitioners whose working fluency is legitimated
 *   - living_tradition_pedagogues: beneficiary (moderate/mobile) — teach Latin as living practice
 *   - classical_philologists: excluded voice (organized/mobile) — reconstructionist objection has no purchase within this reading
 *   - vernacular_writers_using_barbarisms: narrow excluded/marginal group (powerless/trapped) — the only real exclusion boundary
 *   - comparative_linguists: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__continuity_reading, 0.38).
domain_priors:suppression_score(classical_latin_standard__continuity_reading, 0.22).
domain_priors:theater_ratio(classical_latin_standard__continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__continuity_reading, rope).
narrative_ontology:human_readable(classical_latin_standard__continuity_reading, "Living-Practice (Continuity) Reading of the Classical Latin Standard").
narrative_ontology:topic_domain(classical_latin_standard__continuity_reading, "historical_linguistics/philology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__continuity_reading, '0feca996-7582-4d57-af95-d4a93136775f').
narrative_ontology:cs_kernel_codification('0feca996-7582-4d57-af95-d4a93136775f', distributed).
narrative_ontology:cs_authority_grounding('0feca996-7582-4d57-af95-d4a93136775f', practice).
narrative_ontology:cs_interpretation_layer_present('0feca996-7582-4d57-af95-d4a93136775f').
narrative_ontology:cs_reading_relation('0feca996-7582-4d57-af95-d4a93136775f', classical_latin_standard__reconstruction_reading, forecloses).
narrative_ontology:cs_reading_relation('0feca996-7582-4d57-af95-d4a93136775f', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('0feca996-7582-4d57-af95-d4a93136775f', foundational, drift_constitutes_legitimate_development).
narrative_ontology:cs_axiom_status(drift_constitutes_legitimate_development, holdable).
narrative_ontology:cs_axiom_grounding('0feca996-7582-4d57-af95-d4a93136775f', drift_constitutes_legitimate_development, conventional).
narrative_ontology:cs_axiom('0feca996-7582-4d57-af95-d4a93136775f', foundational, unbroken_practice_is_sufficient_warrant_for_correctness).
narrative_ontology:cs_axiom_status(unbroken_practice_is_sufficient_warrant_for_correctness, holdable).
narrative_ontology:cs_axiom_grounding('0feca996-7582-4d57-af95-d4a93136775f', unbroken_practice_is_sufficient_warrant_for_correctness, conventional).
narrative_ontology:cs_reference_frame('0feca996-7582-4d57-af95-d4a93136775f', unbroken_institutional_transmission).
narrative_ontology:cs_drift_state('0feca996-7582-4d57-af95-d4a93136775f', post_humanist_classicizing_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0feca996-7582-4d57-af95-d4a93136775f', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__continuity_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, ecclesiastical_latin_institutions).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, monastic_and_curial_scribes).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, living_tradition_pedagogues).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, unbroken_transmission_legitimacy).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, linguistic_drift_as_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Church and its chancery/liturgical apparatus have used continuously-spoken and continuously-written Latin for administration, liturgy, and scholarship for over a millennium. They set what counts as acceptable Latin usage within their domains by ongoing practice rather than by appeal to a fixed Classical corpus, and they benefit from a standard that validates their own accumulated linguistic innovations (new vocabulary, syntax shifts, loanwords) as legitimate rather than corrupt.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, ecclesiastical_latin_institutions, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, ecclesiastical_latin_institutions, beneficiary).

% Copyists, notaries, and administrators who write functional Latin daily. Their working competence is validated by the continuity reading, since they are not required to reconstruct Ciceronian forms to be considered correct — their trained, inherited usage IS the standard. Exit from this framework would mean their practical fluency no longer counts as authoritative.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, monastic_and_curial_scribes, beneficiary,
    organized, generational, constrained, regional).

% Teachers who instruct students in Latin as an evolving, spoken-adjacent medium (as in medieval schools and some living-Latin movements today) rather than as a dead reconstructed language. They can move between institutions or traditions relatively freely since their expertise is in practice and use, not in specialized philological credentialing.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, living_tradition_pedagogues, beneficiary,
    moderate, biographical, mobile, regional).

% Scholars committed to reconstructing Classical-era usage from textual evidence view the continuity reading as tolerating or legitimizing what they consider corruptions and errors accumulated over centuries. They are not victims in a material sense — they retain full academic standing elsewhere — but their central claim, that medieval and later drift represents degradation rather than development, has no purchase within this reading's framework and they are not consulted in setting continuity-reading norms.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, classical_philologists, excluded,
    organized, generational, mobile, global).

% Writers whose Latin deviates so far from any recognized line of unbroken institutional practice (idiosyncratic vernacular admixture, uncorrected error, non-institutional usage) are labeled barbarisms and excluded from the standard even under this permissive reading. This is the reading's only real exclusion boundary, and it is narrow and marginal rather than systematic.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, vernacular_writers_using_barbarisms, excluded,
    powerless, immediate, trapped, local).

% Historical linguists studying the diachronic development of Latin into Romance languages treat the continuity reading as one hypothesis among several about what counts as 'correct' Latin at a given moment, without a stake in its institutional legitimacy.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__continuity_reading, ecclesiastical_latin_institutions).
narrative_ontology:fixing_cost_class(classical_latin_standard__continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working standard of correctness for Latin users embedded in continuous institutional practice (chancery, liturgy, scholarship), so that administrators, scribes, and teachers across generations and regions can produce and recognize mutually intelligible, authoritative Latin without needing to independently verify every usage against a fixed ancient corpus.
% TRANSFER_FUNCTION: Confers legitimacy and institutional authority onto the Latin usage of whichever bodies maintain unbroken transmission (Church, chancery, monastic schools), at the expense of legitimacy for reconstructionist claims that this usage is corrupted; the transfer is primarily reputational/authority-conferring rather than material.
% ABSENT_VOICES: Classical philologists who hold that post-Classical drift constitutes degradation, not development, are structurally outside this reading's framework — their objection is definitionally unanswerable from within the continuity reading, since the reading's founding premise is precisely that drift is legitimate.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished as the operative standard, ecclesiastical and administrative institutions that built centuries of practice on it would need to either defend their historical usage as merely 'imperfect Classical Latin' or cede legitimacy to reconstructionist standards — a real reorganization for those institutions. But because the underlying linguistic practice (spoken/written medieval and ecclesiastical Latin) would continue regardless of which reading is declared 'correct,' philologists and comparative linguists would say the world of actual usage is largely unchanged; only the legitimating framework around it would shift.
% FOUNDING_PROBLEM: As spoken and written Latin diverged over centuries from Classical-era texts, institutions that depended on Latin for continuous administrative and liturgical function needed a way to certify their own evolving usage as legitimate rather than as an ongoing failure to reproduce an ancient standard.
% FOUNDING_PROBLEM_CORROBORATION: Historians of medieval Latin (e.g., studies of Medieval Latin lexicography and the Mittellateinisches Wörterbuch tradition) corroborate, from outside the ecclesiastical institutions themselves, that continuous administrative and liturgical Latin usage genuinely diverged from Classical norms and that this divergence was functionally necessary for institutions to keep operating in Latin at all — the founding problem was real and is not merely a self-serving institutional narrative, though its current 'live' status is debated by philologists who consider the problem largely resolved by settled orthographic and grammatical convention.
narrative_ontology:disappearance_verdict(classical_latin_standard__continuity_reading, contested).
narrative_ontology:founding_problem_status(classical_latin_standard__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__continuity_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__continuity_reading_tests).
:- end_tests(classical_latin_standard__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) rather than low, because the continuity reading still gatekeeps institutional legitimacy — it is not neutral among all usages, it privileges usage transmitted through recognized institutional lines (Church, chancery) over usage that arose outside those lines, even when both are equally 'living.' Suppression is low (0.22) because the reading's defining feature is that it does NOT suppress drift — drift is precisely what it legitimizes; the only suppressed category is 'barbarism,' a narrow residual class of usage outside any recognized line of practice, not a systematic delegitimization of alternative practice as under the reconstruction reading. Accessibility collapse is moderate (0.35): institutional Latin practice remains a real, learnable, open tradition, not a closed reconstructed system requiring rare philological expertise. Resistance is low-moderate (0.30), coming mainly from philologists who dispute the legitimacy premise, not from within the practicing community itself.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (ecclesiastical institutions), this reads as a rope: a genuine, low-coercion coordination solution letting a living tradition function without requiring impossible fidelity to an ancient corpus. From the excluded philologist seat, the same structure looks like it quietly forecloses a legitimate scholarly claim (that drift is degradation) without ever having to argue against it — by definitional fiat rather than by engagement. The engine computes these as different seat experiences of the same structural data; the claim (rope) and the metrics (moderate extraction, low suppression) are authored independently and happen to sit close together here, which is itself informative — this reading is structurally closer to genuine coordination than either extractive sibling.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical and administrative institutions sit at the beneficiary end: the reading validates their historically accumulated usage without requiring them to defend it against a fixed ancient standard. Scribes and pedagogues benefit secondarily as practitioners whose competence is defined by use rather than by specialized reconstruction. Classical philologists are excluded rather than victimized — the reading does not extract from them, it simply has no mechanism to accommodate their central claim. The only quasi-victim class, vernacular writers using barbarisms, is small and marginal; this is why victims[] is authored empty rather than populated — the expected structural delta calls for a minimal-to-empty victim set, and on reflection the excluded barbarism-users are better modeled as an excluded stakeholder group than as extraction victims, since no rent is collected from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutions needed their own evolving Latin usage to count as legitimate, not as failed Classical Latin) remains live wherever ecclesiastical and administrative Latin practice continues, so this is not simple mandatrophy — the coordination function still operates. However, the founding_problem_status is authored contested because philologists reasonably argue the problem was substantively resolved once medieval Latin achieved its own settled descriptive grammars (e.g. Du Cange, the Mittellateinisches Wörterbuch), at which point continued institutional gatekeeping does less coordination work and more legitimacy-conferral work for the institutions themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drift_versus_corruption_boundary,
    'Is there a principled linguistic criterion distinguishing ''legitimate drift'' (validated by this reading) from ''barbarism'' (excluded even under this permissive reading), or is the line drawn purely by institutional recognition — i.e., usage is correct if and only if it occurred within a recognized line of transmission, regardless of its intrinsic linguistic character?',
    'Comparative historical-linguistic analysis of specific contested forms (e.g., certain Vulgar Latin constructions, ecclesiastical neologisms) to determine whether recognized and excluded innovations differ in kind or only in institutional provenance.',
    'If the line is purely institutional rather than linguistic, the continuity reading''s low suppression score understates a hidden gatekeeping function — the exclusion boundary would be doing more legitimacy-conferring work than the reading''s self-description admits, pushing it structurally closer to the hybrid or even reconstruction readings on the extraction axis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_versus_corruption_boundary, conceptual, 'Whether legitimate-drift/barbarism is a linguistic or purely institutional distinction.').

omega_variable(
    reading_selection_is_itself_contested,
    'This story adopts the continuity reading as its committed framing among three coexisting readings of the same kernel (classical_latin_standard). Is the choice to treat continuity as the primary or default reading itself a contestable act favoring institutional incumbents, or is it a neutral analytical starting point?',
    'Cross-reference against the reconstruction_reading and hybrid_reading sibling stories: compare which reading current major institutions (Vatican Latinitas Foundation, university classics departments, living-Latin pedagogical movements) actually operate under, and whether the choice of ''default'' reading in scholarship correlates with institutional funding sources.',
    'If institutional selection of the continuity reading as default correlates with institutional self-interest (ecclesiastical bodies preferring the reading that validates their own historical usage), that strengthens the case that beneficiaries authored here are not incidental but causally connected to which reading gets treated as authoritative in broader discourse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_is_itself_contested, conceptual, 'Whether treating continuity as a live, coequal reading (rather than a marginal or fringe position) already reflects institutional interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__continuity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clas_tr_t8, classical_latin_standard__continuity_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(clas_tr_t16, classical_latin_standard__continuity_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(clas_tr_t24, classical_latin_standard__continuity_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(clas_tr_t32, classical_latin_standard__continuity_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(clas_tr_t40, classical_latin_standard__continuity_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__continuity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clas_be_t8, classical_latin_standard__continuity_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(clas_be_t16, classical_latin_standard__continuity_reading, base_extractiveness, 16, 0.34).
narrative_ontology:measurement(clas_be_t24, classical_latin_standard__continuity_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(clas_be_t32, classical_latin_standard__continuity_reading, base_extractiveness, 32, 0.37).
narrative_ontology:measurement(clas_be_t40, classical_latin_standard__continuity_reading, base_extractiveness, 40, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(classical_latin_standard__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__continuity_reading, 0.1).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the classical_latin_standard kernel. classical_latin_standard__reconstruction_reading claims correctness is recoverable only via discontinuous philological return to Classical texts (highest suppression, most systematic delegitimization of medieval/ecclesiastical usage). classical_latin_standard__hybrid_reading requires both Classical fidelity AND recognition of post-Classical technical/ecclesiastical development (intermediate suppression and victim structure). This continuity_reading story has the lowest suppression and narrowest victim set of the three, consistent with its founding premise that drift is legitimate development rather than corruption. Per the ε-invariance principle each reading is authored as a structurally distinct constraint with its own stable ε, not as one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
