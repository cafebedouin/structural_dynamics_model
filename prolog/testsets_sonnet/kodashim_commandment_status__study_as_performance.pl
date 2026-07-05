% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Talmudic Study as Fulfillment of Sacrificial Commandments (Kodashim)
 *   domain: religious/halakhic/epistemic
 *
 * SUMMARY:
 *   This story instantiates one of three coexisting readings of the kernel
 *   'kodashim_commandment_status' — the question of what status the
 *   sacrificial commandments (Kodashim) hold now that the Temple and altar do
 *   not exist. Under this reading, intellectual study of the sacrificial laws
 *   IS the commandment's fulfillment: the kernel (the obligation) remains
 *   occupied not by physical performance but by sustained textual engagement.
 *   This is structurally distinct from the sibling reading 'performance_only'
 *   (which treats the commandment as a suspended husk pending a rebuilt
 *   altar) and from 'messianic_deferral' (which treats it as temporally
 *   suspended, with study serving only a readiness function, not a
 *   fulfillment function). Each reading is generated as its own constraint
 *   story with its own ε; they are not measurement perspectives on one
 *   constraint but three structurally distinct claims that happen to share a
 *   natural-language label ('the commandment status of Kodashim').
 *
 * KEY AGENTS:
 *   - yeshiva_scholars: Primary beneficiary/agenda-setter (organized/mobile) — treat study as the constraint's fulfillment mechanism
 *   - torah_study_institutions: Institutional beneficiary (institutional/mobile) — curricular and prestige benefit
 *   - observant_laypeople_seeking_full_commandment_access: Diffuse beneficiary (moderate/mobile) — psychological/theological completeness
 *   - temple_restorationist_communities: Excluded alternative-tradition holders (moderate/mobile) — do not share this reading's premise
 *   - halakhic_analysts: Analytical observer (analytical/analytical) — tracks the reading as one of several coexisting resolutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.03).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.08).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Talmudic Study as Fulfillment of Sacrificial Commandments (Kodashim)").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic/epistemic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '91756f9d-8659-4509-bdfb-e273955f9e40').
narrative_ontology:cs_kernel_codification('91756f9d-8659-4509-bdfb-e273955f9e40', fixed_text).
narrative_ontology:cs_authority_grounding('91756f9d-8659-4509-bdfb-e273955f9e40', lineage).
narrative_ontology:cs_interpretation_layer_present('91756f9d-8659-4509-bdfb-e273955f9e40').
narrative_ontology:cs_reading_relation('91756f9d-8659-4509-bdfb-e273955f9e40', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('91756f9d-8659-4509-bdfb-e273955f9e40', kodashim_commandment_status__messianic_deferral, influences).
narrative_ontology:cs_axiom('91756f9d-8659-4509-bdfb-e273955f9e40', foundational, study_constitutes_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('91756f9d-8659-4509-bdfb-e273955f9e40', study_constitutes_fulfillment, theological).
narrative_ontology:cs_axiom('91756f9d-8659-4509-bdfb-e273955f9e40', secondary, physical_performance_not_required_for_commandment_discharge).
narrative_ontology:cs_axiom_status(physical_performance_not_required_for_commandment_discharge, holdable).
narrative_ontology:cs_axiom_grounding('91756f9d-8659-4509-bdfb-e273955f9e40', physical_performance_not_required_for_commandment_discharge, conventional).
narrative_ontology:cs_reference_frame('91756f9d-8659-4509-bdfb-e273955f9e40', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('91756f9d-8659-4509-bdfb-e273955f9e40', post_destruction_rabbinic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('91756f9d-8659-4509-bdfb-e273955f9e40', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, yeshiva_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, torah_study_institutions).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, observant_laypeople_seeking_full_commandment_access).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, torah_study_equivalent_to_sacrifice_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, oral_substitution_for_temple_service_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study the tractate Kodashim (sacrificial law) as a full-time or part-time discipline, treating the intellectual reconstruction of sacrificial procedure as the commandment's present-day mode of fulfillment. They set the interpretive norm that study equals performance within their institutions, and this norm is itself the constraint's operating mechanism. Their exit option is genuine: they could instead treat the commandment as wholly suspended, but the study-as-performance reading is what makes their vocation religiously load-bearing rather than merely academic.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, yeshiva_scholars, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__study_as_performance, yeshiva_scholars, agenda_setter).

% Yeshivot and kollels structure curricula around Kodashim study partly because the study-as-performance reading gives this study independent religious weight rather than purely preparatory or theoretical value. This underwrites enrollment, funding, and prestige for institutions specializing in sacrificial law even though no sacrifice has occurred in two millennia.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, torah_study_institutions, beneficiary,
    institutional, generational, mobile, global).

% Individuals living without a functioning Temple who want to feel they are not permanently barred from a category of commandments. Under this reading, reading and studying the relevant sections (e.g. as part of daily liturgy or study cycles) lets them claim active fulfillment rather than passive waiting. No one is coerced into this reading; it is adopted because it is experientially and theologically satisfying.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, observant_laypeople_seeking_full_commandment_access, beneficiary,
    moderate, biographical, mobile, global).

% Groups oriented toward literal Temple rebuilding and resumed sacrificial practice may view the study-as-performance reading as reducing urgency for restoration — if study already fulfills the commandment, the felt need to rebuild the altar is diminished. Their objection is rarely voiced within mainstream study-as-performance institutions because the two positions are not adjudicated against each other; the reading simply predominates in non-restorationist study communities.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, temple_restorationist_communities, excluded,
    moderate, generational, mobile, regional).

% Scholars of comparative halakhic method who track how different communities resolve the status of commandments whose physical object (the Temple, the altar) no longer exists. They observe study-as-performance, performance-only, and messianic-deferral as coexisting, textually grounded traditions rather than a single settled doctrine.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a religious community's relationship to commandments whose physical performance is currently impossible, by relocating the site of fulfillment from the altar to the study hall — this keeps a body of law alive, transmitted, and practiced (as intellectual engagement) across generations when literal performance is unavailable.
% TRANSFER_FUNCTION: Moves religious value and communal prestige toward those who specialize in Kodashim study (yeshiva scholars, study institutions) and away from a purely deferred or dormant treatment of the commandment; no material resource is extracted from a victim class — this is a value-attribution shift, not an extraction of goods or labor from an identifiable payer.
% ABSENT_VOICES: Restorationist communities who hold that only literal sacrifice fulfills the commandment are not in active dialogue with study-as-performance communities within this reading's own framework; their objection would be that study assigns to itself a completion the text does not grant, but they are a different tradition-community rather than an excluded party within this one.
% DISAPPEARANCE_RATIONALE: If the study-as-performance doctrine vanished, the world of Kodashim-focused yeshivot would not disappear (the texts and the obligation to study Torah broadly persist), but the specific religious weight attached to that study — the sense that it independently 'completes' something rather than merely preparing or memorializing — would diminish, likely shifting some study emphasis and institutional prestige toward other tractates or toward the messianic-deferral framing. Whether this counts as the world rearranging or staying the same is itself disputed between the reading's adherents and its skeptics.
% FOUNDING_PROBLEM: After the Temple's destruction, sacrificial commandments could no longer be physically performed, threatening either the abandonment of an entire legal category or an indefinite dormancy that could erode its perceived authority and relevance across generations.
% FOUNDING_PROBLEM_CORROBORATION: Adherents within the beneficiary institutions (yeshiva scholars, study institutions) attest the founding problem remains fully live and is solved precisely by their ongoing study. Comparative halakhic analysts (an observer seat outside the beneficiary set) corroborate that the doctrine has ancient textual grounding (e.g., Talmudic statements equating study of sacrificial order with offering) but note this is one of several coexisting resolutions rather than a uniquely corroborated settlement; no fully external, non-adherent verification of the doctrine's theological correctness exists or could exist for a claim of this kind.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.02–0.03) because under this reading there is no performance gap to convert into extraction: the commandment is not suspended, dormant, or owed-and-unpaid — it is actively fulfilled through study, so there is no victim bearing the cost of non-performance. Suppression is low (0.08) because adoption of this reading is voluntary interpretive commitment within a tradition that also tolerates the sibling readings; no one is coerced into believing study fulfills the commandment. Theater ratio is low-to-moderate and drifts slightly upward (0.08 to 0.12) reflecting a mild, non-alarming possibility that some institutional study can become performative signaling of piety rather than genuine engagement with the legal content, but this is a minor drift, not a structural feature. Accessibility collapse is moderate (0.35) — the reading closes off the felt need to pursue literal restoration as urgently as the performance_only reading would, but it does not eliminate that alternative; restorationist communities persist alongside it. Resistance is low (0.15): the reading faces only the passive non-adoption of restorationist communities, not active opposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (scholars, institutions, laypeople) sit near the full-beneficiary end of directionality: the reading confers religious completeness and institutional legitimacy on their activity without requiring extraction from any identifiable victim. There is no victim group under this reading by design — the entire point of the reading is that non-performance harms no one because performance already occurred, in the mode of study. This is the structural delta relative to the sibling readings: performance_only would generate victims (those who feel permanently barred from fulfillment) and messianic_deferral would generate a different structure (readiness-without-fulfillment, a lesser but nonzero gap). Here the gap is closed entirely by definition of the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading specifically prevents the mislabeling of an ancient commandment as either abandoned (which would treat millennia of continuous legal transmission as pointless) or as pure extraction (a demand with no possible satisfaction, imposed on people who can never discharge it). By defining study as the fulfillment mode, the reading converts what could structurally resemble a Snare (an unpayable, permanently-owed debt) into a Rope (a genuinely satisfiable coordination function — sustaining legal knowledge across generations without an altar). The claim (rope) and the metrics (near-zero extraction, low suppression) are aligned here, which is itself notable: this is a case where the claimed type and the descriptive metrics converge, unlike constraints where divergence is the interesting signal. The convergence is expected precisely because the reading is authored to close the performance gap definitionally, and the metrics reflect that closure honestly rather than being tuned to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_equals_performance_ambiguity,
    'Does textual/intellectual engagement with sacrificial law actually satisfy the underlying commandment, or does this reading substitute a psychologically and institutionally convenient practice for an obligation that in fact remains unmet?',
    'No empirical resolution exists for a theological equivalence claim of this kind; the question can only be addressed by which reading a given halakhic authority or tradition-lineage endorses, and by internal textual argument (e.g., the Talmudic dicta cited in support of study-as-atonement). Tracking which authorities/communities adopt which reading over time, and whether restorationist activity (e.g., Temple Institute-style organizing) grows or shrinks in tandem with adoption of this reading, would be the closest available empirical proxy.',
    'If the study-as-performance reading is judged theologically insufficient by a corroborating body of authority, the constraint would need to be re-modeled with a nonzero performance gap and a reinstated victim class (those denied genuine fulfillment) — moving it structurally toward the messianic_deferral or performance_only siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_equals_performance_ambiguity, conceptual, 'Whether the study-equals-fulfillment equivalence is genuine or a convenient reinterpretation.').

omega_variable(
    reading_selection_pressure,
    'Is the predominance of study-as-performance within non-restorationist yeshiva culture a free interpretive choice, or is it structurally favored because it is the only reading that makes indefinite Temple-absence institutionally sustainable for full-time Torah scholars?',
    'Comparative analysis of whether communities with strong restorationist commitments (who have a live alternative use for their energy — actual rebuilding advocacy) adopt this reading at lower rates than communities without such commitments.',
    'If adoption tracks institutional convenience rather than independent textual conviction, the near-zero extractiveness score may understate a subtler extraction: the reading could be functioning to legitimate existing institutional arrangements (yeshiva economies) rather than purely closing a genuine theological gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_pressure, conceptual, 'Whether the reading''s popularity reflects theological conviction or institutional self-interest.').

omega_variable(
    cs_framing_kernel_vs_legitimacy_narrative,
    'Should the kernel here be framed as the sacrificial commandments themselves, or as the broader legitimacy narrative that rabbinic Judaism substitutes study/prayer for Temple service across all of Temple-dependent law (not just Kodashim)? Under the narrower framing, this is a distinct commandment-status claim; under the broader framing, this reading is one instance of a much larger substitution doctrine spanning purity law, pilgrimage, and priestly gifts.',
    'Would require tracing whether the same ''study equals performance'' argument is applied uniformly across all Temple-contingent commandment categories or whether Kodashim receives distinct treatment (e.g., because tractate Kodashim historically received disproportionate study attention as a genre marker of scholarly seriousness).',
    'Under the broader framing, this story would be one leaf of a much larger constraint family and its ε might need to be evaluated as part of a shared substitution-doctrine kernel; under the narrower framing (adopted here), it stands alone as a Kodashim-specific claim. The narrower framing was chosen because the source material specifically named Kodashim/sacrifice laws.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_legitimacy_narrative, conceptual, 'Whether the relevant kernel is Kodashim-specific or the broader rabbinic substitution doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.08).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__study_as_performance, theater_ratio, 20, 0.09).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__study_as_performance, theater_ratio, 40, 0.1).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__study_as_performance, theater_ratio, 60, 0.1).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__study_as_performance, theater_ratio, 80, 0.11).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__study_as_performance, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__study_as_performance, base_extractiveness, 20, 0.02).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__study_as_performance, base_extractiveness, 40, 0.03).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__study_as_performance, base_extractiveness, 60, 0.03).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__study_as_performance, base_extractiveness, 80, 0.03).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__study_as_performance, base_extractiveness, 100, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__study_as_performance, 0.1).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'the status of the Kodashim (sacrificial) commandments today.' Each sibling reading is a structurally distinct constraint with its own ε: study_as_performance (this story, ε≈0.03, empty victim set, Rope), performance_only (ε expected substantially higher, nonzero victim set of those denied fulfillment, likely Snare or Tangled Rope depending on suppression), and messianic_deferral (an intermediate ε, readiness-without-fulfillment). They are linked via affects_constraints rather than merged because the ε-invariance principle forbids assigning multiple ε values to what would otherwise be treated as 'one constraint measured differently' — these are three different claims sharing a label, not one claim measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
