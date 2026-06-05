% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Reading: Divine Legitimacy as Pharaonic Revelation Monopoly
 *   domain: ancient_history/religious_studies/political_economy_of_belief
 *
 * SUMMARY:
 *   The Atenist monotheistic reading asserts that divine legitimacy flows
 *   exclusively through pharaonic revelation of Aten as the sole true deity;
 *   all other gods are false and their worship is heresy. This constraint
 *   represents a specific reading of the contested kernel 'divine legitimacy
 *   substrate' — one of three competing interpretive frameworks that emerged
 *   and competed in ancient Egypt. The Atenist reading (instantiated here)
 *   centralizes religious authority into an exclusive pharaonic monopoly and
 *   employs enforcement mechanisms (temple confiscation, suppression of
 *   alternative worship) to suppress competing readings (Amun polytheistic
 *   and folk syncretistic). The constraint exhibits high extractiveness
 *   (0.68), reflecting the pharaonic concentration of religious wealth and
 *   authority, and high suppression (0.75), reflecting coercive enforcement
 *   against alternative practice. However, the theater ratio (0.58) indicates
 *   that the Atenist reading combines genuine theological commitment with
 *   performative authority display — the theological content appears sincere
 *   (the Hymn to Aten), but the suppression and wealth consolidation serve
 *   political objectives. The constraint classifies as a Snare from the
 *   perspective of the suppressed priesthoods and folk practitioners, but as
 *   Rope from the perspective of pharaonic authority (the reading solves the
 *   problem of unified divine legitimacy) and as Tangled Rope from the
 *   perspective of the Atenist priesthood (who benefit from monopoly
 *   authority but remain subordinate to the throne). The analytical observer
 *   risks naturalizing this reading as a timeless metaphysical truth rather
 *   than recognizing it as a contingent institutional construction — the
 *   false summit detector will flag this naturalization.
 *
 * KEY AGENTS:
 *   - Pharaonic Authority (Akhenaten and successor pharaohs): Primary beneficiary (institutional/arbitrage) — consolidates religious legitimacy monopoly; extracts temple wealth and administrative control
 *   - Amun Priesthood: Primary victim (institutional/trapped) — loses temple lands, revenue, and interpretive authority; economically devastated by confiscation
 *   - Folk Religious Practitioners: Secondary victim (powerless/trapped) — lose legitimacy for household shrines and local protective deities; suppression through enforcement and condemnation
 *   - Atenist Priesthood: Beneficiary-victim hybrid (organized/constrained) — gains status and resources but remains subordinate to pharaonic will; dependent on continued royal favor
 *   - Regional Temple Economies: Tertiary victim (moderate/constrained) — disrupted by confiscation of lands and resources; constrained in re-organizing around alternative divine authority
 *   - Alternative Interpretive Communities (Amun polytheistic reading, folk syncretistic reading): Suppressed sibling readings — persist through underground practice and survival of institutional or folk bases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.68).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.75).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, snare).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Reading: Divine Legitimacy as Pharaonic Revelation Monopoly").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "ancient_history/religious_studies/political_economy_of_belief").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, 'd534256b-f6cb-468e-af64-dd548d04a999').
narrative_ontology:cs_kernel_codification('d534256b-f6cb-468e-af64-dd548d04a999', formalized).
narrative_ontology:cs_authority_grounding('d534256b-f6cb-468e-af64-dd548d04a999', extraction).
narrative_ontology:cs_interpretation_layer_present('d534256b-f6cb-468e-af64-dd548d04a999').
narrative_ontology:cs_reading_relation('d534256b-f6cb-468e-af64-dd548d04a999', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('d534256b-f6cb-468e-af64-dd548d04a999', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('d534256b-f6cb-468e-af64-dd548d04a999', foundational, aten_ontological_exclusivity).
narrative_ontology:cs_axiom_status(aten_ontological_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('d534256b-f6cb-468e-af64-dd548d04a999', aten_ontological_exclusivity, theological).
narrative_ontology:cs_axiom('d534256b-f6cb-468e-af64-dd548d04a999', foundational, pharaonic_revelation_monopoly).
narrative_ontology:cs_axiom_status(pharaonic_revelation_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('d534256b-f6cb-468e-af64-dd548d04a999', pharaonic_revelation_monopoly, deontological).
narrative_ontology:cs_reference_frame('d534256b-f6cb-468e-af64-dd548d04a999', aten_exclusive_divinity_pharaonic_monopoly).
narrative_ontology:cs_drift_state('d534256b-f6cb-468e-af64-dd548d04a999', post_akhenaten_restoration_period, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('d534256b-f6cb-468e-af64-dd548d04a999', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_authority).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, atenist_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_religious_practitioners).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, regional_temple_economies).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, alternative_interpretive_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMUN PRIESTHOOD (SNARE) — Economically and politically devastated by confiscation of temple lands and revenue. Trapped by institutional dependence on the temple economy with no alternative power base. No legitimate exit option within the Atenist reading framework — rejecting Aten means rejecting the only legitimate source of religious authority. Maximum experienced extraction: institutional wealth transferred to pharaonic treasury; institutional authority dissolved; priesthood reduced to powerlessness or forced collaboration with Atenist regime.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__atenist_monotheistic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FOLK RELIGIOUS PRACTITIONERS (SNARE) — Local shrine practices, household deities, and protective amulets are delegitimized as heretical. Suppression mechanism combines religious condemnation (only Aten is legitimate) with coercive enforcement (destruction of shrines, confiscation of sacred objects). Exit is psychologically unavailable — folk practitioners lack the interpretive authority to construct alternative legitimacy frameworks; they can only accept or hide their practice. Extraction: loss of ceremonial authority, spiritual authority, social standing; terror from enforcement campaigns.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__atenist_monotheistic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: PHARAONIC AUTHORITY (ROPE) — The Atenist reading consolidates religious legitimacy into an exclusive pharaonic monopoly. The pharaoh is the sole legitimate interpreter and mediator of Aten's will. This is coordination: the reading solves the problem of religious authority by centralizing it. The pharaoh benefits from theological monopoly (no competing priestly class can challenge authority; all religious legitimacy flows through the throne) and experiences the constraint as pure coordination. Effective extraction is negligible from this perspective — the pharaoh is simultaneously the beneficiary and the enforcement mechanism. The constraint appears as a legitimate reorganization of sacred order.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__atenist_monotheistic_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ATENIST PRIESTHOOD (TANGLED ROPE) — Organized agents who benefit from the Atenist monopoly (elevated status, access to pharaonic resources, exclusive interpretive authority) but also bear costs. The priesthood is constrained by total dependence on pharaonic patronage — they cannot develop independent institutional power without violating Atenist theology (which forbids competing priestly authority). They experience both coordination (solving the problem of unified divine interpretation) and extraction (subordination to pharaonic will, vulnerability to purge or demotion if they displease the throne). Exit is costly — abandoning Atenism means losing institutional position and authority.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__atenist_monotheistic_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational distance, an observer might treat the Atenist claim — that Aten's exclusive divinity is a timeless natural law rather than a contingent historical assertion — as an immutable metaphysical truth. This perspective reads the monotheistic innovation as discovered rather than constructed, legitimacy as inherent rather than enforced, and suppression of alternatives as necessary elimination of falsehood rather than political extraction. The engine's false summit detector will identify this as naturalization: the base properties reveal that beneficiaries exist (pharaonic authority, Atenist priesthood), victims exist (Amun priesthood, folk practitioners), enforcement is required, and suppression is high — all markers of a constructed constraint, not a natural law.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__atenist_monotheistic_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: POST-ATENIST TEMPLE BUREAUCRACY (PITON) — After Akhenaten's death, the Atenist reading is formally abandoned and Amun is re-elevated to supreme status. However, institutional structures created during the Atenist period (centralized temple administration, pharaonic control of religious appointment, suppression of independent priestly authority) persist into the post-Atenist era. The constraint becomes degraded — theaters of Atenist reverence are maintained or redacted, administrative procedures reflect the Atenist reorganization, but the underlying legitimacy claim (Aten as exclusive god) has been revoked. Theater_ratio is high (0.58 in base properties reflects the mixed degradation: some Atenist structures persist as vestigial forms while the regime has officially disowned the theological reading). Institutional inertia, not functional coordination or active extraction, maintains the constraint.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__atenist_monotheistic_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(divine_legitimacy_substrate__atenist_monotheistic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(divine_legitimacy_substrate__atenist_monotheistic_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, TR),
    TR >= 0.70.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The pharaonic authority consolidates religious wealth (temple lands, temple labor, votive offerings that formerly went to Amun temples are now redirected to Aten temples under pharaonic control) and eliminates competing institutional authority. The extraction is structured and sustained across the Atenist period (roughly 1353-1336 BCE), with a trajectory of increasing severity as the regime consolidates. The measurements show extraction rising from 0.15 (pre-Atenist baseline where religious authority was distributed) to 0.68 (peak Atenist extraction). It does not reach maximal snare levels (0.85+) because the pharaonic authority experiences the constraint as coordination (they are solving a real legitimacy problem, not purely predating) and because some alternative practice persists despite suppression. Suppression (0.75): High. Multiple enforcement mechanisms operate: theological delegitimization (declaring alternative gods false), institutional destruction (confiscation of temple properties, defacement of inscriptions mentioning other gods), economic extraction (redirection of temple revenues and offerings), and likely physical suppression of shrine practices. The measurements show suppression rising from 0.20 (pre-Atenist baseline, normal religious competition without state enforcement) to 0.75 (active enforcement campaigns, widespread institutional destruction). Theater ratio (0.58): Moderate-high. The Atenist reading contains genuine theological content (Aten hymns articulate a coherent theological vision of solar monotheism, different from the polytheistic theological corpus). However, the theatrical element is substantial: the suppression and wealth consolidation serve pharaonic power consolidation as much as theological truth; the restoration of Amun worship immediately after Akhenaten's death suggests the theological commitment was not entirely sincere or was politically contingent; and the intensity of enforcement campaigns suggests the regime was attempting to control belief rather than merely convince.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural phenomenon (centralization of religious authority, suppression of alternatives, confiscation of institutional wealth) is classified differently from different positions. The pharaonic authority sees the Atenist reading as Rope — it solves the coordination problem of unified religious legitimacy. The Atenist priesthood sees it as Tangled Rope — they gain authority but remain subordinate. The Amun priesthood sees it as Snare — they lose everything with no exit option. Folk practitioners see it as Snare — their spiritual practice is delegitimized and suppressed. The analytical observer at civilizational distance might see it as Mountain — treating Aten's exclusivity as a discovered truth rather than enforced claim — but the structural data (beneficiaries, victims, enforcement requirements) reveals this as a false summit (naturalization of a political construction). The perspectival gap reveals that the classification depends entirely on the observer's structural relationship to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the constraint. Pharaonic authority is a beneficiary with arbitrage exit options — they can maintain the Atenist reading or abandon it (as post-Atenist pharaohs did); d is low (~0.15), producing negative f(d) and low χ from their perspective. Amun priesthood is a victim with trapped exit — they cannot exit the suppression without accepting the constraint's legitimacy claim; d is high (~0.95), producing high f(d) and high χ. Folk practitioners are victims with trapped exit; d is high (~0.95). Atenist priesthood are beneficiaries who are also trapped by dependence on pharaonic patronage; their d is mixed (~0.50), producing moderate f(d) and moderate χ. The analytical observer at civilizational scope has d derived from observer-neutral position (~0.72), producing high f(d) and high χ in the mountain classification — but the false summit detector identifies the beneficiary/victim structure as contradicting the mountain claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The Atenist reading resolves the mandatrophy by showing that the Snare classification is correct despite the presence of a genuine coordination function (solving the problem of unified divine authority). The constraint is NOT a pure snare of extraction masked as coordination — the reading genuinely solves a coordination problem (unifying divine legitimacy authority) while simultaneously extracting resources and suppressing alternatives. This is the essence of the Tangled Rope type, and it appears in multiple perspectives (the Atenist priesthood sees Tangled Rope, the Amun priesthood sees Snare, the pharaonic authority sees Rope). The mandatrophy is resolved by recognizing that the classification depends on whether the observer is a beneficiary of the coordination (pharaoh, Atenist priest) or a victim of the extraction (Amun priest, folk practitioner). The analytical observer's false summit (attempting to naturalize the Atenist reading as a discovered truth) is the mandatrophy risk — treating institutional construction as natural law. This is resolved by the omega variables, which document the empirical uncertainty about the reading's sincerity and the conceptual uncertainty about whether it forecloses or merely suppresses competing readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_sincerity_vs_political_rationalization,
    'Was the Atenist reading a genuine theological commitment to monotheistic revelation, or a rationalization for pharaonic centralization of religious authority and confiscation of temple wealth?',
    'Historical analysis of Akhenaten''s theological texts (Hymn to Aten, boundary stelae inscriptions) for evidence of internal coherence; correlation between temple confiscations and Atenist theological innovations; post-Atenist reversal speed and completeness; comparison to other monotheistic movements for distinctive theological signatures vs. political utility signatures',
    'If sincere theology: legitimacy claim is epistemically defensible (though still contested and enforced). If political rationalization: Atenism is purely extractive apparatus disguised as revelation; classification remains Snare but the omega resolves the naturalization question. If mixed: the reading contains both genuine theological content and political opportunism, neither eliminating the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_sincerity_vs_political_rationalization, empirical, 'Whether Atenist monotheism was sincere theology or political rationalization for authority consolidation').

omega_variable(
    kernel_contestation_vs_single_framework,
    'Did the Atenist reading and the Amun polytheistic reading coexist as alternative legitimate readings within a single shared framework (the divine legitimacy substrate kernel), or did Atenism attempt to foreclose the polytheistic reading entirely by claiming it was not a reading but heresy?',
    'Analysis of Akhenaten''s texts: do they engage polytheism as an alternative-but-wrong interpretation of a shared kernel, or do they treat polytheism as ontologically false rather than interpretively divergent? Post-Atenist restoration texts: do they reframe Aten as a subordinate manifestation, or explicitly reject Atenism as incoherent? Examination of whether the sibling readings (Amun, folk syncretistic) existed before, during, and after the Atenist period.',
    'If Atenism forecloses polytheism: the reading_relations should be forecloses. If Atenism merely claims superiority while polytheism persists: the relation should be coexists_with. This affects the cs_structure.reading_relations field and determines whether the Atenist reading is a true epistemic foreclosure or a political suppression of a conceptually coherent alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contestation_vs_single_framework, conceptual, 'Whether Atenism forecloses polytheism logically or merely suppresses it politically').

omega_variable(
    suppression_severity_underestimation,
    'Is the suppression value (0.75) capturing the full scope of coercive enforcement, or is it underestimating the psychological and material terror of the suppression mechanism?',
    'Detailed analysis of enforcement mechanisms: destruction of Amun temple reliefs, defacement of inscriptions mentioning other gods, confiscation of household shrine objects, reports of priestly purges or forced relocations, accounts of folk-level harassment or shrine destruction. Comparison to other state-mandated religious suppressions (Christian persecution under Rome, witch hunts, religious reconquest) to calibrate the severity scale.',
    'If underestimated: suppression should be ≥0.85, potentially pushing the constraint toward a harder snare classification (higher χ). If accurate: suppression at 0.75 is appropriate for a high-extraction snare with mixed enforcement and some uncontrolled folk practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_severity_underestimation, empirical, 'Whether the suppression metric (0.75) accurately captures enforcement severity').

omega_variable(
    alternative_readings_persistence_mechanism,
    'How did the Amun polytheistic reading and folk syncretistic reading persist during the Atenist period despite active suppression? What structural conditions enabled survival?',
    'Archaeological evidence of Amun worship continuing underground during Akhenaten''s reign (shrine artifacts, hidden reliefs, evidence of clandestine practice); post-Atenist rapid restoration of Amun temples (indicating infrastructure survived); geographic variation in suppression effectiveness; analysis of which sibling readings had the strongest institutional or folk basis to resist erasure.',
    'If suppression was penetrating: the sibling readings should be classified as foreclosed or coexists_with but severely damaged. If suppression was incomplete: the sibling readings persisted as coexists_with throughout, and the Atenist reading merely suppressed rather than eliminated alternatives. This affects historical accuracy of the classification and indicates whether the false summit detector is missing active ongoing resistance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_persistence_mechanism, empirical, 'Mechanisms enabling sibling readings to survive Atenist suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aten_theater_pre_atenism, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aten_theater_mid_atenism, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(aten_theater_late_atenism, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 17, 0.58).

% Extraction over time
narrative_ontology:measurement(aten_extractiveness_pre_atenism, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(aten_extractiveness_mid_atenism, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(aten_extractiveness_late_atenism, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(aten_suppression_pre_atenism, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(aten_suppression_mid_atenism, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(aten_suppression_late_atenism, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% The Atenist monotheistic reading, Amun polytheistic reading, and folk syncretistic reading are three competing interpretations of the same contested kernel (divine legitimacy substrate). Each reading has its own constraint story with its own extractiveness, suppression, and classification values. The Atenist reading (this file) asserts exclusive pharaonic-mediated divine authority and suppresses the sibling readings through enforcement; it classifies as Snare from the perspective of the suppressed. The Amun polytheistic reading maintains that divine legitimacy is distributed among multiple gods and that pharaonic authority is legitimate insofar as it serves this polytheistic order; it would classify differently from the Atenist perspective (likely as Snare or Tangled Rope). The folk syncretistic reading maintains that divine legitimacy is localized and does not require centralized theological adjudication; it would also classify differently. Network links indicate that this reading affects (suppresses, forecloses, or influences) the sibling readings, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__atenist_monotheistic_reading, institutional, 0.15).
constraint_indexing:directionality_override(divine_legitimacy_substrate__atenist_monotheistic_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
