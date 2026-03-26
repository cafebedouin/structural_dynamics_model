% ============================================================================
% CONSTRAINT STORY: theatrical_neutrality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_theatrical_neutrality, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: theatrical_neutrality
 *   human_readable: Theatrical Neutrality: The View from Nowhere as Epistemic Extraction
 *   domain: epistemology/media/rhetoric
 *
 * SUMMARY:
 *   Theatrical neutrality represents a structural constraint where the
 *   framing of asymmetric claims as equivalent 'perspectives' systematically
 *   extracts the reader's ability to discern evidence quality. This
 *   constraint operates across journalism, academia, public health
 *   communication, and policy discourse. The 'View from Nowhere' — presenting
 *   oneself as neutral arbiter above contested terrain — is a rhetorical
 *   performance that enables the amplification of fringe or false claims
 *   without bearing epistemic cost. The constraint exhibits fundamentally
 *   extractive properties: those generating asymmetric claims benefit from
 *   false-balance framing (legitimacy without evidence cost), media
 *   institutions benefit from neutrality-as-identity (trust without
 *   fact-checking investment), while readers and the epistemic commons bear
 *   the cost of degraded truth discrimination. The theater_ratio (0.81)
 *   reflects that the performative content of 'neutrality' far exceeds any
 *   remaining coordination function. The constraint has intensified over the
 *   50-year interval as media volume has increased, making epistemic status
 *   differentiation more cognitively expensive and theatrical shortcuts more
 *   profitable.
 *
 * KEY AGENTS:
 *   - Reader/Epistemic Capacity: Primary victim (powerless/trapped) — seeks truth discrimination but is trapped within false-balance frame
 *   - Epistemic Commons: Primary victim (moderate/constrained) — collective good of shared factual grounding systematically degraded
 *   - Asymmetric Claim Generator: Primary beneficiary (institutional/arbitrage) — gains legitimacy and reach without epistemic cost of weak evidence
 *   - Neutral-Appearing Publication: Primary beneficiary (institutional/arbitrage) — gains trust and audience via neutrality identity while avoiding fact-checking investment
 *   - Fact-Checking Community: Secondary actor (organized/constrained) — both enabled (creates need for their work) and constrained (cannot shut down underlying extraction)
 *   - Journalistic Neutrality Norm: Institutional actor (institutional/arbitrage) — maintains performative 'both sides' framing despite degraded coordination function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks misidentifying contingent institutional choice as inherent communicative limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(theatrical_neutrality, 0.58).
domain_priors:suppression_score(theatrical_neutrality, 0.68).
domain_priors:theater_ratio(theatrical_neutrality, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(theatrical_neutrality, extractiveness, 0.58).
narrative_ontology:constraint_metric(theatrical_neutrality, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(theatrical_neutrality, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(theatrical_neutrality, snare).
narrative_ontology:human_readable(theatrical_neutrality, "Theatrical Neutrality: The View from Nowhere as Epistemic Extraction").
narrative_ontology:topic_domain(theatrical_neutrality, "epistemology/media/rhetoric").

domain_priors:requires_active_enforcement(theatrical_neutrality).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(theatrical_neutrality, asymmetric_claim_generators).
narrative_ontology:constraint_beneficiary(theatrical_neutrality, institutional_neutrality_performers).
narrative_ontology:constraint_victim(theatrical_neutrality, epistemic_commons).
narrative_ontology:constraint_victim(theatrical_neutrality, truth_discernment_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: READER EPISTEMIC CAPACITY (SNARE) — The reader seeking truth discrimination is trapped within the theatrical frame. Presenting a fringe claim and a well-evidenced claim as equivalent 'perspectives' actively extracts the reader's ability to discern quality of evidence. No exit: the reader cannot disengage from content consumption without losing access to information entirely. The suppression is total — alternatives (hierarchical evidence ranking, clear epistemic status labeling) are systematically avoided in favor of the false-balance frame.
constraint_indexing:constraint_classification(theatrical_neutrality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EPISTEMIC COMMONS (SNARE) — The collective good of shared truth-reference is systematically degraded. When readers cannot reliably distinguish well-evidenced claims from false equivalences, the epistemic commons becomes unreliable. Communities that depend on shared factual grounding (public health, climate policy, electoral integrity) experience extraction of their ability to coordinate on shared reality. Constrained exit: some communities can build alternative information channels, but at significant cost. Most are trapped within the dominant publication ecology.
constraint_indexing:constraint_classification(theatrical_neutrality, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ASYMMETRIC CLAIM GENERATOR (ROPE) — The agent making a fringe, false, or asymmetrically-sourced claim benefits from theatrical neutrality as a coordination mechanism. By framing their claim as one 'perspective' among others, they gain legitimacy and audience reach without bearing the epistemic cost of their claim's weakness. Arbitrage exit: they can abandon the claim if challenged and move to a new fringe position, always operating within the false-balance frame. The extraction runs toward this agent — they experience the constraint as coordination (we all get to present our views equally).
constraint_indexing:constraint_classification(theatrical_neutrality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NEUTRAL-APPEARING PUBLICATION (ROPE) — The media institution or journal that performs theatrical neutrality gains legitimacy and audience trust ('We present all sides') while avoiding the resource cost of rigorous fact-checking or epistemic status differentiation. Arbitrage exit: they can claim neutrality while editing content to favor particular asymmetric claims, always hiding behind the 'both sides' frame. The constraint serves as coordination cover for selective amplification. The institution experiences the constraint as a legitimate coordination mechanism.
constraint_indexing:constraint_classification(theatrical_neutrality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FACT-CHECKING COMMUNITY (TANGLED ROPE) — Organized fact-checkers, scientific communities, and epistemically rigorous publishers are both enabled and constrained by theatrical neutrality. The constraint creates the need for counter-speech and fact-checking (coordination function: the community exists to restore epistemic differentiation). But the constraint also extracts their labor — fact-checkers must constantly work to re-rank evidence and correct false balances, without being able to shut down the underlying extraction mechanism. Constrained exit: they can build alternative platforms, but most readers never see them because the dominant publication ecology remains theatrical. Active enforcement required: fact-checkers must continuously enforce epistemic standards against the theater.
constraint_indexing:constraint_classification(theatrical_neutrality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: JOURNALISTIC NEUTRALITY NORM (PITON) — The professional norm of 'balanced reporting' and 'presenting all sides' was originally a coordination solution: avoid bias, represent pluralism, give voice to underrepresented groups. The functional purpose — preventing institutional capture by particular interests — has largely atrophied. The norm now persists through institutional inertia and professional identity, despite its degradation into theatrical false balance. Piton classification: the theater_ratio is extremely high (0.81) because the performative content of 'neutrality' far exceeds any remaining coordination function. The norm is maintained because alternatives (rigorous evidence hierarchies, explicit epistemic status labels) would require institutional change, not because it works.
constraint_indexing:constraint_classification(theatrical_neutrality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some communicative ambiguity between claims of different epistemic status may seem inherent to language and pluralistic discourse itself. One might argue that distinguishing truth from falsehood is intrinsically difficult and that 'neutrality' reflects this irreducible uncertainty. However, the structural data contradicts this mountain classification. The theatrical neutrality constraint is not about inherent linguistic limits — it is about institutional choices to avoid the cost of epistemic status differentiation (fact-checking, evidence hierarchies, source evaluation). The false-summit detector will identify this as naturalization of a contingent choice.
constraint_indexing:constraint_classification(theatrical_neutrality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theatrical_neutrality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(theatrical_neutrality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theatrical_neutrality, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(theatrical_neutrality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(theatrical_neutrality, TR),
    TR >= 0.70.

:- end_tests(theatrical_neutrality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts the reader's ability to discern evidence quality by systematically treating asymmetric claims as equivalent 'perspectives.' The extraction is not total (some readers develop independent fact-checking practices, some communities build alternative information channels), but it is substantial and systematic. The measure reflects that theatrical neutrality operates through default cognitive load — distinguishing evidence quality requires explicit effort that the frame actively discourages. Suppression (0.68): High. Significant barriers to escaping theatrical neutrality include: dominance of neutral-appearing publications in information distribution, professional norms enforcing false balance, cognitive costs of independent fact-checking, and structural incentives (business models, legal liability) favoring neutrality performance. But suppression is not total — alternative epistemic frames exist (scientific literature, expert-consensus summaries), though most readers do not access them. Theater ratio (0.81): Very high. The performative content of 'neutrality' far exceeds the actual coordination function. The original purpose of neutrality norms — preventing institutional capture by particular interests — has largely atrophied. The norm now persists primarily to avoid charges of bias while enabling selective amplification under the cover of 'balance.' The theater_ratio has increased over the interval (0.52 → 0.81) as media fragmentation has made explicit evidence hierarchies appear less neutral and theatrical balance more profitable.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and reveals the extractive function. Asymmetric claim generators see Rope (coordination of diverse viewpoints, legitimate pluralism). Neutral-appearing publications see Rope (professional norm enabling trust-building). Readers see Snare (trapped within false balance, unable to discriminate). The fact-checking community sees Tangled Rope (enabled by the problem but extracted by inability to fix it). The journalistic norm sees Piton (professional identity maintained through degraded ritual). The analytical observer risks seeing Mountain (pluralism as inherent to language), but this is a false summit — the constraint is a contingent institutional choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to the constraint. Readers seeking truth discrimination are structurally positioned as trapped victims: they cannot exit content consumption without losing information access, and the constraint actively extracts their capacity. The epistemic commons (abstract collective good) experiences powerless-level extraction. Asymmetric claim generators and neutral-appearing publications are beneficiaries: they arbitrage the frame (can abandon weak positions and move to new ones while always hiding behind 'both sides' neutrality). The fact-checking community occupies a hybrid position: the constraint creates demand for their work (coordination function) but prevents them from addressing root causes (asymmetric exit: they can build alternatives but readers don't find them). Directionality overrides are not needed — the structural data directly implies the derived d values.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED at the Tangled Rope analysis: The constraint reveals how institutional coordination ('we present all sides') combines with systematic extraction ('readers cannot discern evidence quality'). The mandatrophy question is: 'Is this a legitimate coordination mechanism (Rope/Scaffold) or pure extraction (Snare)?' The resolution: the institutional actors benefit from the coordination framing while readers bear the extractive cost. This is precisely the structure of Tangled Rope — both genuine coordination function (enabling diverse voices) AND asymmetric extraction (benefiting asymmetric claim generators at reader expense). The piton perspective shows that the coordination function has atrophied over time — the original purpose (preventing institutional capture) no longer motivates the practice. The snare perspective from the reader's position is not wrong; it is the ground-truth view of extraction. The rope perspective from the beneficiary's position is also not wrong; it is their genuine experience of coordination. The system is extractive because the beneficiaries' coordination function is purchased at the victims' epistemic cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asymmetric_claim_definition,
    'What constitutes an ''asymmetric claim'' requiring explicit epistemic status ranking vs. a legitimate minority position deserving equal voice?',
    'Empirical correlation between claim evidential base and claim success rate in subsequent research; community consensus standards for evidence thresholds; comparative analysis of ''both sides'' framing in domains with clear outcome measures (medicine, climate, engineering) vs. interpretive domains (ethics, aesthetics)',
    'If clear threshold exists: theatrical neutrality is unambiguously extraction (treats false claims as equally valid). If threshold is blurry: some ''both sides'' framing may be legitimate pluralism, not extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_claim_definition, empirical, 'Definition boundary between asymmetric claims and legitimate minority positions').

omega_variable(
    reader_epistemic_capacity_degradation,
    'Does exposure to theatrical neutrality frames actually degrade readers'' ability to discern evidence quality, or does it merely reflect existing epistemic heterogeneity in the audience?',
    'Longitudinal cognitive studies comparing evidence-evaluation skills in readers exposed to hierarchical vs. theatrical-neutral frames; measurement of claim retention rates and confidence calibration across frame types; analysis of how readers update beliefs when presented with equal vs. weighted evidence',
    'If degradation is causal: theatrical neutrality extracts epistemic capacity (Snare confirmed). If it merely reflects pre-existing differences: the constraint is more Rope than Snare — it coordinates the communication of diverse views without actively harming discernment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_epistemic_capacity_degradation, empirical, 'Whether theatrical neutrality causally degrades reader epistemic capacity').

omega_variable(
    institutional_incentive_structure,
    'Is theatrical neutrality maintained primarily by professional norms (the degraded Piton), by business incentives (maximize audience without factual cost), or by liability concerns (avoid lawsuits by appearing balanced)?',
    'Analysis of editorial decision-making processes; interviews with journalists, editors, and publishers about neutrality rationales; comparison of neutrality practice across media organizations with different incentive structures (public broadcasting vs. commercial vs. nonprofit); time-series analysis of how neutrality frames respond to legal/reputational pressures',
    'If primarily norm-driven (Piton): change requires professional identity shift. If primarily incentive-driven (Snare with institutional beneficiaries): change requires structural incentive realignment. If mixed: both norm and incentive change needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_structure, empirical, 'Primary institutional driver of theatrical neutrality maintenance').

omega_variable(
    alternative_frame_effectiveness,
    'Do explicit epistemic status hierarchies (e.g., ''well-evidenced,'' ''emerging research,'' ''contested,'' ''false'') actually improve public understanding, or do they create new forms of extraction through authority claims?',
    'Randomized controlled trials comparing reader comprehension and evidence discrimination across neutral vs. hierarchical frames; longitudinal studies of communities using explicit ranking systems (scientific literature, medical guidelines) vs. those using theatrical balance; analysis of whether epistemic hierarchies reduce polarization or entrench competing hierarchies',
    'If hierarchies improve understanding: theatrical neutrality is unambiguously harmful extraction. If hierarchies create new problems: the constraint represents a genuine dilemma (no good frame exists), not pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_frame_effectiveness, empirical, 'Effectiveness of alternative epistemic status hierarchies vs. theatrical neutrality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(theatrical_neutrality, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theat_tr_t0, theatrical_neutrality, theater_ratio, 0, 0.52).
narrative_ontology:measurement(theat_tr_t25, theatrical_neutrality, theater_ratio, 25, 0.68).
narrative_ontology:measurement(theat_tr_t50, theatrical_neutrality, theater_ratio, 50, 0.81).

% Extraction over time
narrative_ontology:measurement(theat_be_t0, theatrical_neutrality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(theat_be_t25, theatrical_neutrality, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(theat_be_t50, theatrical_neutrality, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(theatrical_neutrality, information_standard).
narrative_ontology:affects_constraint(theatrical_neutrality, regulatory_capture_via_balance).
narrative_ontology:affects_constraint(theatrical_neutrality, discourse_polarization_asymmetry).
narrative_ontology:affects_constraint(theatrical_neutrality, public_health_distrust_cascade).

% DUAL FORMULATION NOTE:
% Theatrical neutrality as a constraint decomposes into multiple downstream constraints by domain. In climate discourse, it enables 'both sides' framing of settled science vs. fringe skepticism. In public health, it produces vaccine hesitancy by treating expert consensus and disproven claims as equivalent. In regulatory domains, it enables regulatory capture by treating industry and public-interest positions as equally valid 'perspectives.' Each domain exhibits the same structural constraint (false equivalence extraction) but with different beneficiaries and victims. Link to parent constraint for decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
