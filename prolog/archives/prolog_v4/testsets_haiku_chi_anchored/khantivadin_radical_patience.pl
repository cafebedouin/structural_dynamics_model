% ============================================================================
% CONSTRAINT STORY: khantivadin_radical_patience
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_khantivadin_radical_patience, []).

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
 *   constraint_id: khantivadin_radical_patience
 *   human_readable: The Teacher of Patience (Khantivadin)
 *   domain: religious/ethical/Buddhist_philosophy
 *
 * SUMMARY:
 *   The Khantivadin (Teacher of Patience) constraint models a structural
 *   extraction mechanism embedded in religious doctrine. The Jataka tale of
 *   Khantivadin depicts a bodhisattva who submits to absolute torture by a
 *   king without resistance, interpreting his suffering as spiritual practice
 *   (practicing Khanti, or radical patience). This constraint exemplifies how
 *   religious authority can extract the ultimate resources — body, autonomy,
 *   life itself — through a narrative frame that reconceptualizes
 *   victimization as virtue. The constraint operates across multiple levels:
 *   at the individual level, practitioners are taught that nonresistance to
 *   harm is enlightened behavior; at the institutional level, the Sangha
 *   enforces doctrinal compliance through hierarchy and authority; at the
 *   societal level, populations taught radical patience are easier to govern
 *   (kings and conquerors benefit from subjects who will not rebel). The
 *   extractiveness (0.78) is exceptionally high because the constraint
 *   targets the most fundamental human goods: bodily integrity and autonomy.
 *   The suppression (0.92) is equally extreme: alternatives (resistance,
 *   flight, appeal to justice) are foreclosed both practically and ethically.
 *   The theater ratio (0.58) indicates that modern implementations are
 *   increasingly performative — contemporary Buddhist communities rarely
 *   enforce absolute nonresistance, and reinterpretations emphasize
 *   psychological equanimity over physical submission — but the original
 *   doctrine retains narrative authority and institutional prestige. The
 *   constraint's classification as a snare is robust from almost all
 *   perspectives except the institutional beneficiaries, who frame it as a
 *   scaffold (temporary, with a sunset at enlightenment). The mandatrophy
 *   question is acute: is this a tragic trade (bodily suffering for spiritual
 *   liberation) or a pure extraction dressed in metaphysics? The analysis
 *   suggests the latter — the enlightenment framework appears post-hoc to
 *   rationalize extraction, not prior justification for consensual
 *   participation.
 *
 * KEY AGENTS:
 *   - Khantivadin (The Victim): Bodhisattva subject to absolute liquidation (powerless/trapped) — the exemplar whose story teaches others to accept torture as virtue
 *   - Buddhist Practitioners and Monastics: Believers taught that nonresistance is the path to enlightenment (organized/trapped) — experience the constraint as binding ethical obligation
 *   - Sangha Institutional Leadership: Monastic hierarchy enforcing doctrinal compliance (institutional/arbitrage) — benefit from practitioners' obedience and spiritual authority
 *   - Secular Rulers and Conquerers: Kings and empires who benefit from populations taught not to resist (powerful/arbitrage) — indirectly benefit from Khanti doctrine through reduced rebellion and increased governability
 *   - Modern Buddhist Communities: Contemporary practitioners reinterpreting Khanti as psychological rather than absolute (moderate/constrained) — experience mixed coordination and extraction
 *   - Analytical Observer: Structural view of the constraint as pure extraction mechanism (analytical/analytical) — sees how religious narratives naturalize and legitimize the snare
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(khantivadin_radical_patience, 0.78).
domain_priors:suppression_score(khantivadin_radical_patience, 0.92).
domain_priors:theater_ratio(khantivadin_radical_patience, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(khantivadin_radical_patience, extractiveness, 0.78).
narrative_ontology:constraint_metric(khantivadin_radical_patience, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(khantivadin_radical_patience, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(khantivadin_radical_patience, snare).
narrative_ontology:human_readable(khantivadin_radical_patience, "The Teacher of Patience (Khantivadin)").
narrative_ontology:topic_domain(khantivadin_radical_patience, "religious/ethical/Buddhist_philosophy").

domain_priors:requires_active_enforcement(khantivadin_radical_patience).

% --- Structural relationships ---
narrative_ontology:constraint_victim(khantivadin_radical_patience, khantivadin_victim).
narrative_ontology:constraint_victim(khantivadin_radical_patience, practitioners_of_nonresistance).
narrative_ontology:constraint_victim(khantivadin_radical_patience, bodhisattva_aspirants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KHANTIVADIN AS VICTIM (SNARE) — The teacher of patience stands before the king's executioners with no physical escape, no institutional protection, and a doctrinal prohibition on resistance. The constraint enforces absolute extraction: the victim's body, autonomy, and eventual life are taken. Suppression is maximal (0.92): alternatives (resistance, flight, appeal to law) are foreclosed by both circumstance and ethical doctrine. d≈0.98, f(d)≈1.45, σ=1.0 → χ≈1.13. Pure extraction with maximal coercion.
constraint_indexing:constraint_classification(khantivadin_radical_patience, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PRACTITIONERS OF NONRESISTANCE (SNARE) — Communities committed to Khanti (radical patience) experience the constraint as a structural entrapment: doctrine and example extract loyalty, body-autonomy, and life itself under the promise of karmic liberation. The constraint operates at the generational level — teaching bhikshus and bodhisattva aspirants that meeting violence with patience is the path to enlightenment. Suppression includes both enforcement (institutional discipline) and indoctrination (ideological commitment to nonresistance as virtue). d≈0.85, f(d)≈1.25, σ=1.0 → χ≈0.98. Organized agents experience the snare through collective enforcement and spiritual authority.
constraint_indexing:constraint_classification(khantivadin_radical_patience, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: SANGHA INSTITUTIONAL AUTHORITY (SCAFFOLD) — From the position of the monastic hierarchy, Khanti appears as a temporary coordination mechanism: the constraint holds practitioners in states of radical submission until enlightenment is achieved or reincarnation releases them from the ethical commitment. The Sangha leadership benefits from the structural obedience of practitioners and the narrative authority to define virtue. However, this perspective perceives a sunset clause: eventual enlightenment (nirvana) dissolves the need for ethical submission. The constraint is framed as temporary (lasting only until ultimate liberation). Theater is moderate (0.58) — the Sangha maintains genuine spiritual practices alongside performative ritual. d≈0.15, f(d)≈0.02, σ=1.0 → χ≈0.01. Minimal effective extraction from the institutional position because the sunset frame conceals the permanent extraction.
constraint_indexing:constraint_classification(khantivadin_radical_patience, scaffold,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HISTORICAL RELIGIOUS AUTHORITY (PITON) — From the perspective of institutional religious power centers (medieval Buddhist kingdoms, modern theocratic institutions), Khanti becomes a performative constraint. The doctrine of radical patience is maintained because it historically enabled religious institutions to coexist with secular power (kings valued subjects who would not rebel; conquerors valued populations taught not to resist). The functional purpose has largely atrophied — modern Buddhist institutions rarely enforce total nonresistance, and practitioners increasingly interpret Khanti as contextual rather than absolute. Theater ratio is moderate-low (0.58) because the doctrine retains some genuine metaphysical coherence (enlightenment narratives), but much of its force is now ceremonial. The constraint persists through institutional inertia and reverence for textual authority. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.005. Piton classification from theater_ratio=0.58 just below the gate (≥0.70), suggesting degradation in progress.
constraint_indexing:constraint_classification(khantivadin_radical_patience, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MODERN BUDDHIST COMMUNITIES (TANGLED ROPE) — Contemporary Buddhist practitioners and teachers often reinterpret Khanti as psychological patience (inner equanimity) rather than absolute nonresistance to physical harm. This perspective experiences both coordination (the community coalesces around shared ethical practice) and extraction (institutional pressure to accept traditional interpretations even when they conflict with safety and dignity). The constraint has both a genuine coordination function (building contemplative capacity) and asymmetric extraction (privileging institutional interpretations over individual safety). d≈0.60, f(d)≈0.80, σ=1.0 → χ≈0.52. Mixed experience: coordination at the level of practice, extraction at the level of institutional authority.
constraint_indexing:constraint_classification(khantivadin_radical_patience, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL VIEW (SNARE) — From a civilizational, universal analytical position, the Khantivadin constraint is a structural mechanism by which religious institutions extract ultimate submission (body, autonomy, life) from practitioners and populations under the guise of spiritual virtue. The constraint's extractiveness (0.78) reflects that the primary mechanism is not coordination but suppression: practitioners are foreclosed from resistance, self-defense, and exit. The claim that this is a 'path to enlightenment' is an ideological justification for the extraction, not a functional description of the constraint itself. The analytical observer sees the snare clearly: absolute suppression, absolute extraction, and a narrative that frames victimization as virtue. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈1.03.
constraint_indexing:constraint_classification(khantivadin_radical_patience, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(khantivadin_radical_patience_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(khantivadin_radical_patience, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(khantivadin_radical_patience, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(khantivadin_radical_patience, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(khantivadin_radical_patience, TR),
    TR >= 0.70.

:- end_tests(khantivadin_radical_patience_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Exceptionally high. The constraint extracts the ultimate human goods — bodily autonomy, physical safety, and life itself. The trajectory from 0.55 to 0.78 reflects the intensification of doctrinal commitment over time as practitioners internalize the teaching and become increasingly willing to accept suffering. The extraction is not accidental but structural: the religious institution's authority depends on the credibility of its martyrs, and practitioners who submit most completely to harm become the most effective teachers of the doctrine. Suppression (0.92): Maximal. Practitioners are foreclosed from resistance by both doctrine (nonresistance is presented as virtue) and institutional power (the Sangha enforces compliance). Exit is not merely difficult but conceptually forbidden — leaving the path of Khanti is framed as spiritual failure. Theater ratio (0.58): Moderate. The original Khantivadin doctrine has genuine metaphysical coherence (enlightenment narratives, karma, the promise of eventual liberation) that gives it more than purely performative content. However, the theater has increased over the interval (from 0.35 to 0.58) as modern communities have shifted toward psychological reinterpretations, treating the doctrine as metaphorical rather than prescriptive. The rising theater reflects institutional degradation: the constraint persists through narrative authority and institutional inertia rather than through genuine functional commitment to the original doctrine. Claimed type (snare): Robust from all perspectives except institutional beneficiaries. The constraint satisfies snare gates: ε ≥ 0.46 (yes, 0.78), suppression ≥ 0.60 (yes, 0.92), effective extraction χ ≥ 0.66 (yes, 1.03 for the primary victim, 0.98 for organized practitioners).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is extreme and reveals the mechanism of the snare. The primary victims (Khantivadin and practitioners) classify the constraint as pure snare: they bear maximal costs (bodily suffering, death, psychological trauma from internalized guilt for their resistance impulses) with no benefit. The institutional beneficiaries (Sangha leadership) classify the constraint as a temporary scaffold: they see Khanti as maintaining order until practitioners achieve enlightenment and the ethical obligation dissolves. This is a profound disagreement rooted in directionality: beneficiaries have arbitrage options (they can interpret Khanti contextually, use it to control practitioners, then relax it for themselves) while victims are trapped (they are taught to accept suffering absolutely). The analytical observer bridges this gap by recognizing the snare: the 'sunset at enlightenment' is a promissory note that never comes due because enlightenment remains perpetually beyond reach in the same way that the promised liberation of Marxist societies remained perpetually deferred. The modern practitioners' reinterpretation (Khanti as psychological patience) represents an attempted escape from the snare toward a tangled rope: they acknowledge the coordination benefit (building contemplative capacity) while rejecting the extraction (absolute nonresistance to harm). However, institutional pressure to accept traditional interpretations means that even reinterpreted Khanti retains extractive force.
 *
 * DIRECTIONALITY LOGIC:
 *   Khantivadin (primary victim): Victim + trapped + powerless → d≈0.98, f(d)≈1.45. Maximum extraction. This agent has no exit, no advocacy, and bears the full cost of the constraint. The king's torture instruments define the structural reality. Buddhist practitioners (organized victims): Victim + trapped + organized → d≈0.85, f(d)≈1.25. High extraction with possibility of coalition power. The organization factor acknowledges that Buddhist communities can, in principle, collectively reject the constraint (as modern communities increasingly do), but institutional authority and doctrinal commitment raise the exit costs. Sangha leadership (institutional beneficiary): Beneficiary + arbitrage + institutional → d≈0.15, f(d)≈0.02. Minimal effective extraction from their perspective because they have full exit: they can reinterpret Khanti, enforce it selectively, or abandon it entirely. Their structural position is one of pure benefit (authority, control, spiritual prestige). Secular rulers (powerful beneficiary): Beneficiary + arbitrage + powerful → d≈0.10, f(d)≈-0.08. They benefit from Khanti doctrine through reduced rebellion and increased governability without paying any cost. Modern practitioners (moderate, reinterpreting): Victim + constrained + moderate → d≈0.60, f(d)≈0.80. Mixed directionality because they have partial exit (reinterpretation) and partial coordination benefit (genuine spiritual practice alongside extraction). Analytical observer: Analytical → d≈0.88, f(d)≈1.32. High directionality toward the victim side because the observer recognizes the asymmetric costs.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The Khantivadin constraint forces a direct confrontation with the mandatrophy question: 'Are we describing a coordination mechanism that enables spiritual liberation, or a pure extraction mechanism that uses spirituality as justification?' The extractiveness value (0.78) and suppression value (0.92) resolve the mandatrophy in favor of snare. A genuine scaffold or tangled rope would require either (1) a credible sunset mechanism (enlightenment is not credible because it is perpetually deferred) or (2) genuine coordination benefits that exceed the extraction (spiritual growth is not a valid coordination benefit if it is purchased through trauma internalization). The mandatrophy resolution requires recognizing that the enlightenment frame is not an external justification for the constraint but a component of the extraction mechanism itself: by teaching practitioners to reframe torture as spiritual advancement, the constraint achieves the rare feat of making victims self-enforcing. The victim reinforces the snare through their own understanding of virtue. This is extraction at maximum potency. Modern reinterpretations (Khanti as psychological patience) represent an escape attempt from the mandatrophy: by severing the connection between absolute nonresistance and enlightenment, modern Buddhism seeks to reduce the constraint from snare to tangled rope. However, institutional authority still enforces the traditional interpretation, so the modern version is best classified as an attempted transition rather than a resolved mandatrophy. The original Khantivadin doctrine remains a pure snare: ε=0.78, suppression=0.92, χ≥0.66, mandatrophy resolved as snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enlightenment_empirical_status,
    'Is the karmic/enlightenment framework that justifies radical patience empirically coherent, or is it a post-hoc narrative that naturalizes extraction?',
    'Comparative analysis of enlightenment claims across traditions and centuries; correlation between reported enlightenment and measurable psychological/spiritual outcomes; examination of whether enlightenment narratives appear after victimization as rationalization or precede it as genuine motivation',
    'If enlightenment is real and accessible: the constraint transitions from pure snare to a tragic coordination problem (practitioners genuinely exchange bodily autonomy for spiritual liberation). If enlightenment claims are post-hoc: the constraint is pure extraction dressed in metaphysics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enlightenment_empirical_status, conceptual, 'Empirical status of enlightenment narratives underlying the constraint').

omega_variable(
    doctrine_enforcement_mechanism,
    'Is the enforcement of absolute nonresistance primarily coercive (institutional punishment for resistance) or consensual (practitioners internalize the virtue as self-enforcing)?',
    'Historical analysis of how Buddhist institutions enforced or discouraged resistance; examination of textual and pedagogical emphasis on Khanti across periods; study of exit rates and defection costs for practitioners',
    'If primarily coercive: snare classification is robust. If primarily consensual self-enforcement: the constraint may be a scaffold or tangled rope (practitioners opt in, though without full awareness of costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_enforcement_mechanism, empirical, 'Degree of institutional coercion vs. consensual internalization of nonresistance doctrine').

omega_variable(
    historical_counterfactual_extraction,
    'What would have been extracted from practitioners and populations if Khanti had not been taught — and by whom? Does Khanti prevent extraction or merely channelize it?',
    'Comparative historical study of Buddhist vs. non-Buddhist societies under conquest or oppression; examination of whether Khanti doctrine reduced violence or ensured its effectiveness; analysis of whether alternative forms of resistance or accommodation emerged in contexts where Khanti was not normative',
    'If Khanti prevented worse extraction: the constraint is a tragic scaffold or tangled rope (lesser evil, with sunset at enlightenment). If Khanti enabled or amplified extraction: the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_counterfactual_extraction, empirical, 'Whether Khanti prevented, channeled, or enabled extraction relative to counterfactual scenarios').

omega_variable(
    modern_reinterpretation_validity,
    'Do modern interpretations of Khanti as psychological patience (inner equanimity without absolute nonresistance) actually preserve the constraint''s core function, or do they dissolve it entirely?',
    'Textual analysis comparing historical and modern interpretations; institutional study of how modern Buddhist communities enforce (or fail to enforce) Khanti in contexts of actual harm; measurement of whether modern practitioners accept violence against themselves or others when reinterpreted Khanti is available',
    'If modern interpretations preserve core function: the constraint is transitioning from snare to piton or tangled rope (functionally degraded but institutionally persistent). If interpretations dissolve it: modern Buddhism has effectively rejected the snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modern_reinterpretation_validity, empirical, 'Whether modern psychological interpretations preserve or dissolve the constraint''s structural function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(khantivadin_radical_patience, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(khanti_theater_t0, khantivadin_radical_patience, theater_ratio, 0, 0.35).
narrative_ontology:measurement(khanti_theater_t5, khantivadin_radical_patience, theater_ratio, 5, 0.48).
narrative_ontology:measurement(khanti_theater_t10, khantivadin_radical_patience, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(khanti_extract_t0, khantivadin_radical_patience, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(khanti_extract_t5, khantivadin_radical_patience, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(khanti_extract_t10, khantivadin_radical_patience, base_extractiveness, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(khantivadin_radical_patience, enforcement_mechanism).
narrative_ontology:affects_constraint(khantivadin_radical_patience, nonresistance_doctrine_institutional_stability).
narrative_ontology:affects_constraint(khantivadin_radical_patience, karmic_justification_theodicy_framework).

% DUAL FORMULATION NOTE:
% The Khantivadin constraint decomposes into two structurally distinct mechanisms: (1) the doctrinal teaching that absolute patience is virtue (this story, ε=0.78, snare) and (2) the metaphysical framework (karma/enlightenment) that justifies the doctrine (downstream, higher ε, also snare). The doctrinal mechanism operates through institutional authority and behavioral enforcement; the metaphysical mechanism operates through narrative authority and post-hoc rationalization. Both are necessary for the complete extraction; neither alone achieves the full snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
