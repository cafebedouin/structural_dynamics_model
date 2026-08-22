% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hybrid Continuity Reading of Hebrew Vitality: Substrate-and-Reconstruction Conjunction
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This story instantiates the hybrid_continuity_reading of the contested
 *   kernel hebrew_vitality: the claim that liturgical preservation of Hebrew
 *   across roughly seventeen centuries of post-spoken decline was a necessary
 *   enabler of the twentieth-century vernacular revival but was insufficient
 *   for it, and that vitality therefore required the conjunction of preserved
 *   substrate and deliberate reconstruction. The reading is an analytical
 *   synthesis rather than an actionable arrangement, and accordingly carries
 *   no beneficiary or victim structure: no party collects from the thesis's
 *   operation and no party pays into it. The colloquial kernel label 'Hebrew
 *   vitality' decomposes into three structurally distinct claims per the
 *   epsilon-invariance principle - this conjunctive-synthesis reading, a
 *   liturgical reading on which ritual continuity itself constitutes
 *   vitality, and a native-daily reading on which only native generational
 *   transmission counts - each authored as its own constraint file with its
 *   own epsilon and linked, not merged. Assumptions recorded: sibling
 *   constraint_ids are assumed to follow this file's prefix convention
 *   (hebrew_vitality__liturgical_reading,
 *   hebrew_vitality__native_daily_reading); the interval maps to the
 *   documented revival-and-assessment arc from the onset of organized
 *   vernacularization (1881) to the contemporary scholarly settlement (2020).
 *
 * KEY AGENTS:
 *   - comparative_revival_sociolinguists: Analytical observer (analytical/analytical) - holds, tests, and transmits the conjunctive thesis across revival cases
 *   - liturgical_maintaining_communities: Excluded voice (organized/identity_locked) - the theorized substrate producers, absent from the scholarly conversation that assigns them their enabling role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.12).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.06).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, mountain).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hybrid Continuity Reading of Hebrew Vitality: Substrate-and-Reconstruction Conjunction").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:emerges_naturally(hebrew_vitality__hybrid_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '29d035ab-b302-4d40-999b-6daa3b4c14bf').
narrative_ontology:cs_kernel_codification('29d035ab-b302-4d40-999b-6daa3b4c14bf', distributed).
narrative_ontology:cs_authority_grounding('29d035ab-b302-4d40-999b-6daa3b4c14bf', expertise).
narrative_ontology:cs_interpretation_layer_present('29d035ab-b302-4d40-999b-6daa3b4c14bf').
narrative_ontology:cs_reading_relation('29d035ab-b302-4d40-999b-6daa3b4c14bf', hebrew_vitality__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('29d035ab-b302-4d40-999b-6daa3b4c14bf', hebrew_vitality__native_daily_reading, influences).
narrative_ontology:cs_axiom('29d035ab-b302-4d40-999b-6daa3b4c14bf', foundational, vitality_requires_substrate_and_reconstruction).
narrative_ontology:cs_axiom_status(vitality_requires_substrate_and_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('29d035ab-b302-4d40-999b-6daa3b4c14bf', vitality_requires_substrate_and_reconstruction, empirically_contingent).
narrative_ontology:cs_axiom('29d035ab-b302-4d40-999b-6daa3b4c14bf', secondary, liturgical_preservation_insufficient_for_vernacular_life).
narrative_ontology:cs_axiom_status(liturgical_preservation_insufficient_for_vernacular_life, holdable).
narrative_ontology:cs_axiom_grounding('29d035ab-b302-4d40-999b-6daa3b4c14bf', liturgical_preservation_insufficient_for_vernacular_life, empirically_contingent).
narrative_ontology:cs_reference_frame('29d035ab-b302-4d40-999b-6daa3b4c14bf', conjunctive_substrate_reconstruction_vitality).
narrative_ontology:cs_drift_state('29d035ab-b302-4d40-999b-6daa3b4c14bf', contemporary_revisionist_challenge, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('29d035ab-b302-4d40-999b-6daa3b4c14bf', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, substrate_necessity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, reconstruction_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the comparative framework within which the conjunctive thesis is stated, tested, and revised. They code revival cases for substrate presence and reconstruction effort, run attribution studies on Hebrew's source strata, and adjudicate challenges such as the European-adstrate critique. Their exit is epistemic: they can abandon the conjunctive criterion if cross-case evidence undermines it, at the cost of reworking the field's comparative template. They collect citation and standing from the framework's adequacy; the historical events the thesis describes confer no benefit on them and impose no costs on them.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, comparative_revival_sociolinguists, observer,
    analytical, generational, analytical, global).

% Across roughly seventeen centuries of diaspora, rabbinic academies, print networks, and household practice maintained Hebrew for prayer, study, and legal writing after it ceased to be anyone's daily speech. The scholarly synthesis credits this maintenance as the necessary substrate of the later vernacular. The communities themselves largely opposed or stood apart from vernacularization, regarding secularized Hebrew as a desecration; they are not participants in the academic debate that assigns them their enabling role, and from where they stand the enabling framing repurposes a sacred practice as material for a national project they did not join. Leaving the maintenance practice was never a live option for them, since the practice constituted the community's continuity rather than serving an external goal.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_maintaining_communities, excluded,
    organized, generational, identity_locked, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared evaluative criterion for comparing language-revival cases: outcomes are classified by whether both preserved substrate and deliberate reconstruction were present, so that revival successes and failures can be explained within one framework.
% TRANSFER_FUNCTION: Moves explanatory credit and disciplinary attention: apportions recognition for Hebrew's vitality between liturgical-maintaining communities (substrate credit) and revivalist institutions (reconstruction credit), and directs scholarly citation and comparative attention toward conjunctive analyses.
% ABSENT_VOICES: The liturgical-maintaining communities are the principal absent voice - seated here as excluded: the synthesis theorizes their practice, but they are not in the scholarly conversation, and many would object to having sacred maintenance cast as mere enabling substrate. Traditionalist partisans of the liturgical reading and nativist partisans of the native-daily reading sit outside this file's frame by design (they are sibling constraints, not seats); within the synthesis's own venues, dissenting attribution researchers raising the European-adstrate challenge are present but marginal.
% DISAPPEARANCE_RATIONALE: Overnight removal of the conjunctive thesis would leave Hebrew speech, liturgical practice, and every revival project elsewhere untouched; only the comparative template of revival scholarship would lose its organizing criterion until an equivalent synthesis re-formed. Nothing is arranged around the thesis - no seat's situation depends on it - which is the operative sense in which this reading's constraint approaches natural-background status.
% FOUNDING_PROBLEM: To explain how a language that had ceased to be anyone's mother tongue for centuries nonetheless became, within two generations, the daily vernacular of a national community - specifically, to apportion explanation between the preserved liturgical corpus and the deliberate reconstruction effort, resolving the paradox that neither component alone accounts for the outcome.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside any benefiting party (the thesis's holders are academic analysts with no stake in the historical arrangement): comparative revival literature on Cornish, Manx, Maori, Welsh, Irish, and Hawaiian independently confronts the substrate-versus-reconstruction apportionment, and corpus-attribution studies of modern Hebrew's source strata (biblical, mishnaic, rabbinic, medieval, and European-adstrate layers) provide methodologically independent testimony to the empirical core. No corroboration originates from parties advantaged by the thesis, and traditionalist and nativist partisans explicitly dispute that the founding problem is closed - hence status contested rather than live or dead.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_vitality__hybrid_continuity_reading),
    narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.12) because the standing arrangement under contest - the joint substrate-reconstruction structure the thesis describes - is assessed by this reading's own lights as complementary contribution, not asymmetric transfer; the residual reflects the one asymmetry the reading acknowledges, that substrate-maintenance labor was supplied by populations who did not participate in and often opposed the vernacular outcome. Suppression is authored as a raw structural property (0.06) and is deliberately not contextualized; only extractiveness is scaled by the engine, and here there is no directional structure to scale. Theater is low (0.08): the thesis is maintained by evidential work, not performance. Accessibility collapse is high (0.80): within the reading's frame, once the conjunction is understood, the pure readings collapse as explanations - liturgy alone cannot explain why seventeen centuries of recitation produced no speech, and nativism alone cannot explain why the revival's lexicon was overwhelmingly inherited - though historical counterfactuals are softer than physical law, hence below the 0.85+ characteristic of genuine natural law. Resistance is moderate (0.30): traditionalist and nativist partisans, plus revisionist attribution challenges, actively contest the thesis without displacing it. Claim and metrics are independent authored facts: claimed_type is mountain because the thesis is held as a structural regularity that would persist without enforcement and from which no party collects; the metrics are the descriptive record. The measurement series run on one shared time grid; the mild extractiveness hump peaking at 1948 tracks the period when revival institutions maximally drew on the substrate while the maintaining communities remained apart, declining as native-born generations made the arrangement self-sustaining.
 *
 * PERSPECTIVAL GAP:
 *   With no seated payers or beneficiaries, computed seat divergence is thin by design; the live perspectival gap runs between readings rather than within seats. From the liturgical seat, this reading's insufficiency clause demotes ritual continuity from constitutive to instrumental - a move the traditionalist experiences as category error. From the nativist seat, the necessity clause dilutes the achievement of native generation by crediting a dead substrate. From the analytical seat, both objections are noise around a settled synthesis. The engine computes per-seat classifications from structural data; this story seats only the analytical observer and the excluded community voice, so the divergence surfaces as inter-file contrast with the sibling stories rather than intra-story seat spread.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionalities are derivable because no beneficiary or victim structure is declared: the derivation chain finds no structural data, and the constraint subsidizes no index and taxes no index. This absence is itself the descriptive claim of the hybrid reading - the substrate-reconstruction relationship is modeled as complementary, not transfer-bearing. The one candidate asymmetry (maintenance labor supplied by non-participating communities) is carried in commentary and in the substrate_composition_attribution omega rather than as a victim declaration, because the reading's own lights assess it as historical contribution rather than extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. As scaffold: the thesis has no sunset - it is not transitional support awaiting obsolescence but a standing explanatory frame, so has_sunset_clause stays false. As piton: nothing about the thesis is theatrically maintained - its low theater_ratio reflects evidential upkeep, and no administrator could cheaply revise it away since it stands on corpus evidence rather than institutional inertia. Mandatrophy is not declared resolved: the founding problem (accounting for how a language with no native speakers became a daily vernacular) remains contested, so the arrangement has not outlived its function. If the engine certifies the mountain claim, the thesis sits as fixed background for revival analysis; if the engine computes divergence toward constructed types on the strength of the historiography omega, that divergence is the measurement the corpus exists to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Which structural element of the hebrew_vitality kernel do the three readings disagree on, and what would each sibling change if adopted?',
    'Explicit criterion specification: force each reading to state the denotation of ''vitality'' (ritual continuity vs native generational transmission vs conjunctive achievement). The disagreement is located in the criterion, not in the historical record, which all three readings accept in outline.',
    'Adopting the liturgical reading relocates kernel satisfaction to ritual continuity and falsifies this reading''s insufficiency clause; adopting the native-daily reading strips the substrate clause of constitutive credit and reduces this reading to a causal footnote. This file instantiates the hybrid reading only; the siblings are separate constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: this constraint is one reading of kernel hebrew_vitality; the contest is located in the criterion of vitality.').

omega_variable(
    natural_law_vs_historiography,
    'Is the substrate-reconstruction conjunction a structural regularity of language revival generally, or a constructed narrative shaped by nationalist historiographic interests?',
    'Cross-case comparative test: code independent revival attempts (Cornish, Manx, Maori, Welsh, Irish, Catalan, Hawaiian) for substrate presence and reconstruction effort; if the conjunction predicts outcomes across cases, the thesis gains law-like status; if it fits only the Hebrew case, it is historiography.',
    'Law-like support stabilizes the mountain classification; case-specific fit demotes the thesis to contingent narrative and invites recomputation toward constructed constraint types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_historiography, empirical, 'Whether the conjunctive necessity claim is a natural regularity of revival or a constructed narrative.').

omega_variable(
    substrate_composition_attribution,
    'How much of modern Hebrew''s lexicon and morphosyntax derives from liturgically preserved strata versus deliberate invention, mishnaic and rabbinic sources, and European (especially Yiddish and Slavic) adstrates?',
    'Corpus-linguistic attribution studies quantifying source-stratum shares in the revived vernacular.',
    'A high liturgical share strengthens the necessity clause; a dominant European-adstrate share (the revisionist challenge) reweights the reading toward reconstruction-plus-adstrate and pressures the declared reference frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_composition_attribution, empirical, 'Empirical composition of the substrate the thesis credits.').

omega_variable(
    insufficiency_mode,
    'Is liturgical use insufficient for vitality contingently (diaspora conditions blocked vernacularization) or necessarily (register, pragmatics, and diglossic function make liturgical language unusable as daily speech)?',
    'Register-pragmatic analysis of liturgical corpora, plus comparison with sacred-language vernacularizations that did occur under comparable fixity.',
    'Contingent insufficiency weakens the law-like reading of the thesis; necessary insufficiency rooted in register structure hardens the mountain claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insufficiency_mode, conceptual, 'Mode of the insufficiency clause: contingent circumstance versus structural register limit.').

omega_variable(
    reframing_resolution_power,
    'Does the hybrid reframing dissolve the kernel contest (the siblings converge as limiting cases of the conjunctive criterion) or merely relabel it (the contest persists with the hybrid as median position)?',
    'Track sibling-reading adherence over time: if liturgical and nativist proponents increasingly argue in necessity-and-sufficiency terms, the reframing is absorbing the contest; if they retain their original criteria, it is relabeling.',
    'Dissolution would make this reading the terminal attractor of the kernel; persistence keeps all three readings as live constraints indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reframing_resolution_power, conceptual, 'Whether the reading''s reframing resolves or merely repositions the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 1881, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1881, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1881, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t1881, observed).
narrative_ontology:measurement(hebr_tr_t1922, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1922, 0.06).
narrative_ontology:measurement_basis(hebr_tr_t1922, observed).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1948, 0.07).
narrative_ontology:measurement_basis(hebr_tr_t1948, observed).
narrative_ontology:measurement(hebr_tr_t1975, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1975, 0.09).
narrative_ontology:measurement_basis(hebr_tr_t1975, observed).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(hebr_tr_t2000, observed).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2020, 0.08).
narrative_ontology:measurement_basis(hebr_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1881, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1881, 0.1).
narrative_ontology:measurement_basis(hebr_be_t1881, observed).
narrative_ontology:measurement(hebr_be_t1922, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1922, 0.13).
narrative_ontology:measurement_basis(hebr_be_t1922, observed).
narrative_ontology:measurement(hebr_be_t1948, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement_basis(hebr_be_t1948, observed).
narrative_ontology:measurement(hebr_be_t1975, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement_basis(hebr_be_t1975, observed).
narrative_ontology:measurement(hebr_be_t2000, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement_basis(hebr_be_t2000, observed).
narrative_ontology:measurement(hebr_be_t2020, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2020, 0.12).
narrative_ontology:measurement_basis(hebr_be_t2020, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__hybrid_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% 'Hebrew vitality' is a colloquial label covering three structurally distinct claims with different epsilons: this conjunctive-synthesis reading (epsilon 0.12 - the joint substrate-reconstruction arrangement assessed as complementary), the liturgical reading (which would author epsilon for ritual continuity as constitutive), and the native-daily reading (which would author epsilon for the nativeness criterion). The readings are linked, not merged: each is a separate file; this file links to both siblings via affects_constraints, and the pressure runs outward from this synthesis because it supplies the reframed necessity-and-sufficiency terms both siblings must answer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
