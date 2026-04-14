% ============================================================================
% CONSTRAINT STORY: biosignature_interpretation_reliability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biosignature_interpretation_reliability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biosignature_interpretation_reliability
 *   human_readable: Biosignature Interpretation Reliability in Exoplanet Characterization
 *   domain: astrobiology/planetary_science
 *
 * SUMMARY:
 *   The biosignature interpretation reliability constraint governs how
 *   exoplanet research teams evaluate whether spectroscopic features detected
 *   in distant atmospheres constitute evidence of life. This constraint
 *   exhibits all six DR types from different perspectives, making it a
 *   diagnostic exemplar for extractive institutional arrangements disguised
 *   as scientific method. The same structural phenomenon — the gap between
 *   spectroscopic observation and biological confirmation in exoplanet
 *   atmospheres — appears as an immutable scientific reality (mountain), a
 *   coordination mechanism for shared detection standards (rope), a mixed
 *   coordination-extraction hybrid that rewards early claims over
 *   confirmation (tangled rope), a degraded peer review ritual (piton), a
 *   temporary coordination problem being solved by formalized confirmation
 *   protocols (scaffold), or pure extraction from hypothesis integrity
 *   (snare), depending on the observer's structural position. Theater ratio
 *   has increased from 0.45 to 0.68 over the 8-year interval, reflecting the
 *   growing gap between performative confirmation (literature consensus
 *   around preliminary detections) and actual verification (ability to rule
 *   out abiotic explanations). The constraint's suppression (0.62) reflects
 *   substantial barriers to alternative hypotheses: funding pressure toward
 *   detection narratives, publication bias against null results, career risk
 *   for researchers who contradict early biosignature claims, and absence of
 *   ground truth for exoplanet verification.
 *
 * KEY AGENTS:
 *   - Early Detection Researchers: Primary beneficiary (institutional/arbitrage) — captures high-impact publications, funding priority, mission involvement during detection window
 *   - False Positive Hypothesis Integrity: Primary victim (powerless/trapped) — cannot exit the interpretation framework; bears full cost of confirmation errors without corrective mechanism
 *   - Mission Funding Agencies: Secondary beneficiary (institutional/arbitrage) — benefits from biosignature frameworks that justify continued funding and public engagement
 *   - Competing Research Programs: Secondary victim (moderate/constrained) — face funding disadvantage if they pursue skeptical or abiotic-explanation research; constrained by publication pressure to adopt detection-favorable narratives
 *   - Astrobiology Standards Consortium: Organized agents (organized/constrained) — NRC working groups, ISSOL committees, exoplanet consortia building formalized confirmation protocols with sunset logic
 *   - Peer Review Institutional Structure: Institutional actor (institutional/arbitrage) — maintains performative review ritual; cannot verify exoplanet biosignature claims against ground truth
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing technological limitation (current detection methods cannot distinguish biological from abiotic chemistry) as immutable scientific law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biosignature_interpretation_reliability, 0.48).
domain_priors:suppression_score(biosignature_interpretation_reliability, 0.62).
domain_priors:theater_ratio(biosignature_interpretation_reliability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biosignature_interpretation_reliability, extractiveness, 0.48).
narrative_ontology:constraint_metric(biosignature_interpretation_reliability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(biosignature_interpretation_reliability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biosignature_interpretation_reliability, tangled_rope).
narrative_ontology:human_readable(biosignature_interpretation_reliability, "Biosignature Interpretation Reliability in Exoplanet Characterization").
narrative_ontology:topic_domain(biosignature_interpretation_reliability, "astrobiology/planetary_science").

domain_priors:requires_active_enforcement(biosignature_interpretation_reliability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biosignature_interpretation_reliability, early_detection_researchers).
narrative_ontology:constraint_beneficiary(biosignature_interpretation_reliability, mission_funding_agencies).
narrative_ontology:constraint_victim(biosignature_interpretation_reliability, false_positive_hypothesis_integrity).
narrative_ontology:constraint_victim(biosignature_interpretation_reliability, competing_research_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FALSE POSITIVE DETECTION STANDARD (SNARE) — Cannot exit the interpretation framework; bears full cost of premature biosignature claims. The epistemic standard for 'biosignature' has no external oversight and no corrective mechanism. Maximum extraction from hypothesis integrity — false positives accumulate in the literature with no mandatory reversion.
constraint_indexing:constraint_classification(biosignature_interpretation_reliability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONFIRMATION-SEEKING RESEARCH TEAM (TANGLED ROPE) — Constrained by funding cycles and publication pressure, but also benefits from the biosignature interpretation framework through access to high-impact publication venues. Extracts reputational advantage by rapid claims; also gains genuine scientific coordination through shared biosignature definitions.
constraint_indexing:constraint_classification(biosignature_interpretation_reliability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SPACE AGENCY MANDATE FULFILLMENT (ROPE) — Benefits from biosignature detection frameworks that justify continued mission funding. Experiences the constraint as coordination: defining biosignature criteria enables mission design and public engagement. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(biosignature_interpretation_reliability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ASTROBIOLOGY STANDARDS CONSORTIUM (SCAFFOLD) — Organized agents (NRC biosignature working groups, ISSOL committees, exoplanet characterization consortia) see interpretation reliability as a temporary coordination problem with a sunset: machine learning-based false positive filtering, spectroscopic benchmark libraries, and multi-wavelength confirmation protocols are building alternative verification pathways. Theater_ratio declining as confirmation requirements formalize.
constraint_indexing:constraint_classification(biosignature_interpretation_reliability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EXOPLANET CHARACTERIZATION TEMPLATE (PITON) — Traditional biosignature interpretation frameworks (atmospheric oxygen + methane at disequilibrium, phosphine detection, 'technosignatures') persist through institutional adoption despite degraded verification pathways. The characterization ritual maintains a high theater ratio — most papers claiming biosignature detections undergo peer review that cannot actually verify the interpretation against ground truth (no accessible exoplanet samples).
constraint_indexing:constraint_classification(biosignature_interpretation_reliability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, biosignature interpretation is always subject to false positive risk: complex chemistry, abiotic pathways, and instrumental artifact are inherent to remote detection. The ambiguity between biological and non-biological origins is structurally irreducible. This perspective risks naturalizing what is actually a contingent limitation of current observational methods — confusing technological constraint with immutable physical law.
constraint_indexing:constraint_classification(biosignature_interpretation_reliability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biosignature_interpretation_reliability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biosignature_interpretation_reliability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biosignature_interpretation_reliability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biosignature_interpretation_reliability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biosignature_interpretation_reliability, TR),
    TR >= 0.70.

:- end_tests(biosignature_interpretation_reliability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, reflecting genuine incentive asymmetry. Early detection researchers gain reputational advantage, publication prestige, and mission involvement through rapid claims during the years-long confirmation window. The extraction is not as severe as atmospheric verification constraints (which would push this to 0.65+) because some biosignature claims do eventually receive independent confirmation. The 0.48 value reflects that extraction is substantial but not total — about one claim in three survives strengthened scrutiny. Suppression (0.62): High. Substantial barriers prevent alternative hypotheses from being heard: (1) publication bias toward detection narratives (null results and abiotic explanations receive fewer citations), (2) career risk for researchers who contradict preliminary biosignature claims (seen as 'not team players'), (3) funding concentration toward missions designed to find biosignatures (JWST biosignature characterization programs), (4) absence of ground truth — exoplanet atmospheres cannot be sampled, so no researcher can definitively prove an alternative hypothesis. Theater ratio (0.68): Elevated and rising. Most biosignature papers undergo peer review that cannot actually verify the spectroscopic interpretation — reviewers assess plausibility, novelty, instrumental quality, and whether conclusions follow from data, but they cannot access exoplanet samples to test whether the inferred biological explanation is correct. The ritual persists because it provides institutional legitimacy, but its verification capacity is degraded. The theater has increased over the interval as biosignature detection claims have accumulated faster than confirmation capability.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full six-type spectrum. Early detection researchers see pure coordination (Rope) — they are legitimately solving the problem of establishing shared biosignature standards. The astrobiology standards consortium sees a temporary coordination problem with formalized sunset (Scaffold) — machine learning false positive filters, multi-wavelength confirmation requirements, and spectroscopic benchmark libraries are moving adoption at measurable rates. The peer review institutional structure sees its own degraded ritual (Piton) — biosignature interpretation review persists through institutional adoption despite zero verification capacity for exoplanet atmospheres. Competing research programs see mixed coordination and extraction (Tangled Rope) — the system coordinates shared standards but rewards detection claims over skepticism. Hypothesis integrity sees pure extraction (Snare) — false positives accumulate without self-correction. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — spectroscopic ambiguity and false positive risk are inherent to remote detection — but the structural data reveals this as a false summit: the contingent institutional arrangements (publication bias, funding concentration, absence of verification mechanisms) are not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position. Early detection researchers with arbitrage exit options experience low effective extraction (d ≈ 0.15) — they benefit from the framework and can move to other high-impact domains if needed. The institutional funding agencies also have arbitrage (d ≈ 0.10) — biosignature narratives serve their mission mandate, but they could pivot to other exoplanet science. Competing research programs face constrained exit (d ≈ 0.60) — they must operate within the same biosignature framework even if they want to pursue skepticism, because publication and funding gatekeepers favor detection narratives. The false positive hypothesis integrity is trapped (d ≈ 0.95) — it cannot exit or self-correct; bears extraction with no agency. The sigma scope function applies here: global scope (σ=1.2) amplifies the extraction — because biosignature interpretation standards are globally enforced through international journal consensus and mission review boards, the asymmetry cannot be arbitraged away by geographic movement.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing how indexical classification captures different structural realities of the same phenomenon. The mandatrophy is not 'which type is correct?' but 'which perspective are you measuring from?'. The early detection researcher's Rope is their genuine experience — they are solving coordination problems. The astrobiology consortium's Scaffold is real — formalized confirmation protocols are being adopted. The peer review structure's Piton is real — the ritual persists through inertia. The competing program's Tangled Rope is real — they face both coordination benefits and extraction costs. Hypothesis integrity's Snare is real — false positives accumulate. The mountain perspective is a false summit — confusing technological constraint with immutable law. The presheaf over the observation site is the complete answer. The key diagnostic: if the mountain classification were correct, changing the funding structure or review mechanisms would not change the verification rate (it would be immutable). But the constraint's structural mechanisms (publication bias, funding concentration, career risk for skeptics) ARE changeable. This proves the mountain is a false summit and the constraint is institutionally contingent, not physically necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phosphine_detection_artifact_ambiguity,
    'Does atmospheric phosphine detection represent a genuine biosignature candidate or is it predominantly instrumental artifact and sulfur chemistry misattribution?',
    'Independent multi-wavelength reobservation with different instruments and spectral analysis methods; laboratory simulation of sulfur compound spectral profiles at Venus-like conditions',
    'If genuine biosignature: biosignature_interpretation_reliability classification remains Tangled Rope. If artifact: the phosphine detections exemplify maximum extraction from false positives, pushing classification toward Snare; reveals institutional failure of peer review to catch systematic errors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phosphine_detection_artifact_ambiguity, empirical, 'Whether phosphine detections represent biosignature or instrumental artifact').

omega_variable(
    oxygen_disequilibrium_abiotic_pathway_sufficiency,
    'Can abiotic photochemistry and hydrogen escape produce oxygen disequilibrium signatures that mimic biological oxygen production at detectable levels?',
    'Updated photochemical modeling with constraints from Venus, early Earth, and extrasolar analog simulations; measurement of hydrogen escape rates for rocky exoplanets in habitable zone',
    'If abiotic pathways sufficient: oxygen disequilibrium loses discriminatory power, pushing oxygen biosignature from Rope (reliable coordination marker) to Tangled Rope (unreliable extraction mechanism). If pathways insufficient: oxygen remains a high-confidence biosignature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oxygen_disequilibrium_abiotic_pathway_sufficiency, empirical, 'Whether abiotic pathways can mimic biological oxygen production').

omega_variable(
    publication_bias_correction_mechanism,
    'What fraction of claimed biosignature detections are retracted or substantially revised post-publication, and is the retraction rate captured by the published literature?',
    'Longitudinal analysis of all exoplanet biosignature claims (2010-2026); follow-up citation tracking; identification of retracted papers and substantial revisions; comparison of retraction rate to physics/geology baseline',
    'If retraction rate < 5%: peer review is functioning adequately and extractiveness is moderate (current 0.48). If retraction rate 15-30%: extraction is severe and suppression of alternative hypotheses is strong, pushing toward Snare (extractiveness → 0.60+). If retraction rate > 30%: the constraint is a verification catastrophe masquerading as science.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_bias_correction_mechanism, empirical, 'Publication bias and post-publication correction rate for biosignature claims').

omega_variable(
    observation_confirmation_protocol_adoption,
    'Are multi-wavelength confirmation protocols and machine learning false positive filtering being adopted at rates sufficient to justify the Scaffold sunset timeline?',
    'Survey of exoplanet characterization programs; measurement of confirmation requirement adoption rates in proposal review and publication standards; timeline to maturity of automated false positive filtering',
    'If adoption > 60% within 5 years: Scaffold perspective is empirically grounded. If adoption < 30%: sunset timeline is aspirational rather than structural, and the constraint remains Tangled Rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observation_confirmation_protocol_adoption, empirical, 'Adoption rate of multi-wavelength confirmation protocols').

omega_variable(
    identity_lock_in_early_detection_narrative,
    'To what extent are biosignature researchers identity-locked to the ''imminent detection'' narrative, making post-detection alternatives (null results, weak candidates, abiotic explanations) structurally unthinkable?',
    'Qualitative interview analysis with exoplanet characterization researchers; measurement of citation patterns favoring detection-oriented papers vs null-result papers; career trajectory analysis for researchers who shift from detection to skepticism',
    'If identity_lock is high: the constraint has cognitive capture dimension beyond material extraction. Removing funding incentives alone will not change behavior — researchers must rebuild their professional identity. Suppression value rises from 0.62 to 0.75+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_early_detection_narrative, empirical, 'Identity fusion with ''imminent biosignature detection'' narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biosignature_interpretation_reliability, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biosig_tr_t0, biosignature_interpretation_reliability, theater_ratio, 0, 0.45).
narrative_ontology:measurement(biosig_tr_t4, biosignature_interpretation_reliability, theater_ratio, 4, 0.58).
narrative_ontology:measurement(biosig_tr_t8, biosignature_interpretation_reliability, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(biosig_be_t0, biosignature_interpretation_reliability, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(biosig_be_t4, biosignature_interpretation_reliability, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(biosig_be_t8, biosignature_interpretation_reliability, base_extractiveness, 8, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biosignature_interpretation_reliability, information_standard).
narrative_ontology:boltzmann_floor_override(biosignature_interpretation_reliability, 0.05).
narrative_ontology:affects_constraint(biosignature_interpretation_reliability, exoplanet_atmospheric_characterization_precision).
narrative_ontology:affects_constraint(biosignature_interpretation_reliability, spectroscopic_abiotic_pathway_detection).

% DUAL FORMULATION NOTE:
% Biosignature interpretation reliability is downstream of both spectroscopic characterization precision and abiotic pathway chemistry. The upstream constraints determine what signals can be detected; this constraint determines how those signals are interpreted and whether false positives are suppressed or allowed to accumulate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biosignature_interpretation_reliability, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
