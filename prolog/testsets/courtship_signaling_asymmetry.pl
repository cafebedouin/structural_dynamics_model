% ============================================================================
% CONSTRAINT STORY: courtship_signaling_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_courtship_signaling_asymmetry, []).

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
 *   constraint_id: courtship_signaling_asymmetry
 *   human_readable: Courtship Signaling Asymmetry
 *   domain: interpersonal/social_coordination
 *
 * SUMMARY:
 *   Courtship signaling asymmetry describes the structural constraint where
 *   one party (typically the initiator) benefits from controlling the pace,
 *   intensity, and interpretation of romantic signals while the other party
 *   (typically the target) bears asymmetric costs of response management,
 *   performance demand, and identity-based suppression of authentic
 *   preference. This constraint exhibits the full range of DR classification
 *   depending on the observer's position: pure coordination (rope) from the
 *   confident initiator's perspective, mixed coordination with asymmetric
 *   extraction (tangled rope) from ambivalent participants, identity-trapping
 *   extraction (snare) from targets experiencing identity fusion with
 *   romantic receptiveness, a dissolving temporary problem (scaffold) from
 *   consent culture advocates, a degraded institutional script (piton) from
 *   cultural analysts, and an immutable evolutionary law (false mountain)
 *   from those naturalizing cultural arrangements as biological destiny. The
 *   constraint's theater_ratio (0.65) reflects that traditional courtship
 *   involves substantial performative enactment of gender scripts, emotional
 *   availability signals, and romantic interest displays that diverge from
 *   authentic preference and communication.
 *
 * KEY AGENTS:
 *   - Signal Initiators: Primary beneficiary (institutional/arbitrage) — control agenda and timing of romantic engagement; high structural mobility and exit optionality; experience the constraint as legitimate coordination
 *   - Receptive Targets: Primary victim (powerless/identity-locked) — identity constituted through romantic availability and responsiveness; structurally mobile but psychologically trapped by identity fusion; suppression operates through internalized standards and fear of identity breach
 *   - Ambivalent Participants: Secondary actor (moderate/constrained) — experience both genuine social coordination and asymmetric extraction of emotional labor; exit available but costly; moderate agency within constraint
 *   - Consent Culture Movement: Organized institutional actor (organized/constrained) — activist and educational networks building alternative courtship protocols with explicit consent and mutual initiation; perceive generational sunset
 *   - Traditional Courtship Scripts: Institutional system (institutional/arbitrage) — self-perpetuating narrative framework that maintains gender-asymmetric initiation norms; persists through inertia rather than functional necessity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent cultural arrangements as immutable biological/evolutionary facts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(courtship_signaling_asymmetry, 0.58).
domain_priors:suppression_score(courtship_signaling_asymmetry, 0.52).
domain_priors:theater_ratio(courtship_signaling_asymmetry, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(courtship_signaling_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(courtship_signaling_asymmetry, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(courtship_signaling_asymmetry, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(courtship_signaling_asymmetry, tangled_rope).
narrative_ontology:human_readable(courtship_signaling_asymmetry, "Courtship Signaling Asymmetry").
narrative_ontology:topic_domain(courtship_signaling_asymmetry, "interpersonal/social_coordination").

domain_priors:requires_active_enforcement(courtship_signaling_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(courtship_signaling_asymmetry, signal_initiators).
narrative_ontology:constraint_victim(courtship_signaling_asymmetry, signal_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RECEPTIVE TARGET (SNARE) — Identity-locked. The target's self-concept is constituted through romantic availability and responsiveness to signals. Structurally mobile (could reject advances, set boundaries, end interest) but psychologically trapped by identity fusion with the role of 'desirable partner.' Suppression operates through internalized standards of attractiveness, responsiveness norms, and the fear of being labeled uninterested, cold, or difficult. Exit from this constraint would require abandoning a core identity frame — the target cannot perceive refusal as legitimate without experiencing profound identity breach.
constraint_indexing:constraint_classification(courtship_signaling_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: AMBIVALENT PARTICIPANT (TANGLED ROPE) — Constrained by genuine social coordination benefits (courtship process does serve the legitimate function of mutual evaluation and relationship building) alongside asymmetric extraction of emotional labor, performance demands, and attention. The agent experiences both genuine coordination and cost imposition. Exit is possible but carries social penalties (reputation damage, loneliness signals, loss of access to social venues). Moderate power because some agency exists but is exercised against real costs.
constraint_indexing:constraint_classification(courtship_signaling_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: CONFIDENT INITIATOR (ROPE) — Experiences the signaling asymmetry as coordination mechanism: they are communicating interest, testing receptiveness, building sexual/romantic connection. Net beneficiary. High exit optionality (can move to alternative partners, alternative venues, alternative courtship frames). Institutional power because they occupy the structural position of agenda-setter and signal-producer. Low experienced extraction — the constraint operates in their favor.
constraint_indexing:constraint_classification(courtship_signaling_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CONSENT CULTURE MOVEMENT (SCAFFOLD) — Organized agents (educators, activists, peer networks) see courtship signaling asymmetry as a temporary coordination failure with a generational sunset. Explicit consent norms, mutual signal initiation frameworks, and communication-based courtship protocols are building alternative pathways that reduce suppression and asymmetric extraction. Theater_ratio is high now (traditional courtship has substantial performative content — playing hard to get, indirect signals, maintaining romantic script) but is declining as consent culture matures. Low effective extraction because the movement has organizational agency and perceives an exit path.
constraint_indexing:constraint_classification(courtship_signaling_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL COURTSHIP SCRIPT (PITON) — Institutional courtship norms (gender scripts, initiation asymmetry, female receptiveness, male pursuit) persist through cultural inertia despite generational challenges to their functional necessity. Theater_ratio is high (0.65) because much of traditional courtship behavior is performative — enacting romantic scripts rather than communicating authentic intent. The institutional framework maintains itself through narrative inertia ('this is just how courtship works') rather than functional necessity. Younger generations experience the script as degraded rather than adaptive.
constraint_indexing:constraint_classification(courtship_signaling_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EVOLUTIONARY INEVITABILITY VIEW (MOUNTAIN) — From a civilizational analytical perspective, sexual selection and differential mating effort are immutable features of biological reproduction: asymmetric signaling between sexes emerges from asymmetric reproductive investment and cannot be designed away without consequences for human pair-bonding. However, this perspective risks false summit classification — the structural data (high suppression, high theater ratio, identity-locked exit) reveals that much of what appears 'evolutionary' is actually cultural layering and institutional enforcement. The mountain classification is perspectival naturalization of contingent social arrangements.
constraint_indexing:constraint_classification(courtship_signaling_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(courtship_signaling_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(courtship_signaling_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(courtship_signaling_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(courtship_signaling_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(courtship_signaling_asymmetry, TR),
    TR >= 0.70.

:- end_tests(courtship_signaling_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The signaling asymmetry produces tangible extraction of targets' attention, emotional labor, response management, and identity performance. Unlike simple mating call competition, human courtship involves elaborate performance and interpretation work that concentrates on targets. The asymmetry is not total (genuine mutual benefit from courtship coordination exists) but is skewed. Measurement trajectory shows increase from 0.45 to 0.58 over the interval, suggesting extractive layering onto coordination as courtship scripts become more elaborated and performance demands increase with cultural complexity. Suppression (0.52): Moderate. Significant barriers to refusing signals include fear of social judgment, internalized norms that romantic availability is virtuous, identity fusion with desirable partner role, and material consequences (social isolation, reputation damage, access loss to paired-identity social spaces). However, suppression is not absolute — refusal is physically possible and legally protected; the barriers are primarily social and psychological. Theater ratio (0.65): Moderate-high. Traditional courtship involves substantial performative content: playing hard to get, maintaining romantic scripts, performing emotional availability, enacting gender roles that may diverge from authentic preference. Theater has increased over the measurement interval as courtship scripts have become more elaborated. Consent culture norms are beginning to reduce theater by making explicit communication acceptable, but traditional forms still dominate.
 *
 * PERSPECTIVAL GAP:
 *   The sharpest gap lies between the receptive target (identity-locked Snare) and the confident initiator (Rope). From the target's position, the constraint is extractive and identity-binding — refusal feels like identity suicide. From the initiator's position, the constraint is a coordination mechanism for mutual romantic evaluation — perfectly legitimate. This gap is not random perspectival disagreement; it reflects real structural asymmetry in signal control, exit options, and identity-stake. The ambivalent participant perspective (Tangled Rope) inhabits the middle ground — experiencing both coordination benefits and asymmetric cost imposition. The consent culture perspective (Scaffold) suggests the gap can be narrowed through institutional redesign (explicit mutual consent replacing implicit signal interpretation), but the omega variable on consent culture effectiveness questions whether the gap can be truly closed or whether it merely relocates.
 *
 * DIRECTIONALITY LOGIC:
 *   Confident initiators with institutional power and arbitrage options derive d ≈ 0.15 (beneficiary + high exit mobility). Receptive targets with powerless position, identity-locked exit, and victim status derive d ≈ 0.92 (trapped victim + identity fusion + no exit perception). This produces f(d) ≈ 1.38 vs f(d) ≈ -0.08, a stark asymmetry in experienced extractiveness. Ambivalent participants with moderate power and constrained exit derive d ≈ 0.58, producing f(d) ≈ 0.65 — moderate experienced extraction reflecting mixed coordination and cost imposition. The consent culture advocates with organized power, constrained exit, and beneficiary status (advocating change that benefits wider society) derive d ≈ 0.35, producing f(d) ≈ 0.38 — moderate negative chi reflecting that their advocacy benefits collective epistemic clarity while their own exit optionality is somewhat constrained by institutional inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY COMPOSITION: The courtship signaling asymmetry decomposes into multiple structurally distinct sub-constraints with different ε values. This story captures the high-level signaling dynamics (ε=0.58, Tangled Rope). Related constraints include: (1) gender_role_identity_fusion (ε=0.68, Snare) — the specific mechanism binding targets through feminine identity construction; (2) romantic_script_performance (ε=0.52, Tangled Rope) — the performative layer of enacting gender scripts; (3) attention_asymmetry_labor (ε=0.45, Rope) — the coordination of mutual romantic interest detection; (4) consent_protocol_adoption (ε=0.25, Scaffold) — the sunset mechanism via explicit consent norms. Each sub-constraint has its own ε because they have different measurement bases: identity fusion (ε=0.68 because identity cannot exit), script performance (ε=0.52 because performance is voluntary but socially enforced), attention coordination (ε=0.45 because it's partly genuine mutual benefit), consent protocols (ε=0.25 because they're low-cost information standards). The mandatrophy is resolved by decomposing and showing that the perspectival gap (Snare vs Rope) reflects real structural differences in identity-stake, not merely perspectival disagreement about the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signaling_authenticity_threshold,
    'Where is the boundary between legitimate courtship signal variance (normal heterogeneity in romantic expression) and extractive performance (enacting scripts that suppress authentic preference)?',
    'Longitudinal analysis of post-commitment authenticity restoration — do partners relax their courtship performance after commitment is secured? Comparison of pre-commitment and post-commitment behavior variance; correlation between performance intensity and relationship satisfaction',
    'If post-commitment behavior shifts significantly: high portion of courtship performance is extractive suppression of authenticity. If minimal shift: performance is expression of personality rather than role-playing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signaling_authenticity_threshold, empirical, 'Boundary between authentic courtship expression and extractive performance').

omega_variable(
    consent_culture_sunset_timeline,
    'Does the consent culture movement''s shift toward mutual/explicit initiation reduce extractiveness and suppression, or does it merely relocate them to different constraint structures (e.g., performance anxiety about initiating, imposter syndrome in ''egalitarian'' roles)?',
    'Comparative analysis of suppression metrics pre- and post-consent culture adoption; tracking whether agent reports of autonomy increase or migrate to new forms of constraint; generational cohort analysis of relationship satisfaction and asymmetry persistence',
    'If suppression genuinely decreases: scaffold sunset is real and the constraint dissolves. If suppression relocates: the constraint is deeper than institutional scripts and sunset is optimistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_culture_sunset_timeline, empirical, 'Whether consent culture reduces or relocates constraint suppression').

omega_variable(
    identity_lock_cognitive_capture_mechanism,
    'Is the identity-locked exit characteristic of targets primarily driven by internalized cultural scripts (girls should be desirable, receptiveness is feminine virtue) or by deeper cognitive fusion with relational identity that persists even when targets consciously reject the scripts?',
    'Analysis of targets who intellectually reject romantic availability norms but continue to enact them; examination of whether conscious script-rejection reduces behavioral suppression; tracking of whether explicit cultural critique of courtship asymmetry changes reported identity fusion with receptiveness role',
    'If driven by scripts alone: deprogramming and narrative shift can reduce identity lock. If driven by relational identity fusion: cognitive critique is insufficient — deeper relational restructuring required.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_cognitive_capture_mechanism, conceptual, 'Mechanism binding identity-locked targets to receptiveness role').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'What portion of suppression (0.52) is structural (inability to refuse without material/social consequence) versus internalized (target believes rejection is morally wrong or identity-threatening)?',
    'Post-exit suppression trajectory: if targets who exit courtship retain suppression patterns in other contexts, internalization is high. If suppression drops post-exit, it was primarily structural. Longitudinal tracking of autonomy reports before and after exit from identity-locked role.',
    'If highly structural: external barrier removal (clearer refusal norms, institutional support) can reduce suppression rapidly. If highly internalized: suppression will persist after structural barriers dissolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(courtship_signaling_asymmetry, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csa_tr_t0, courtship_signaling_asymmetry, theater_ratio, 0, 0.5).
narrative_ontology:measurement(csa_tr_t5, courtship_signaling_asymmetry, theater_ratio, 5, 0.58).
narrative_ontology:measurement(csa_tr_t10, courtship_signaling_asymmetry, theater_ratio, 10, 0.65).
narrative_ontology:measurement(csa_tr_t15, courtship_signaling_asymmetry, theater_ratio, 15, 0.63).

% Extraction over time
narrative_ontology:measurement(csa_be_t0, courtship_signaling_asymmetry, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(csa_be_t5, courtship_signaling_asymmetry, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(csa_be_t10, courtship_signaling_asymmetry, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(csa_be_t15, courtship_signaling_asymmetry, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(courtship_signaling_asymmetry, attachment_coordination).
narrative_ontology:boltzmann_floor_override(courtship_signaling_asymmetry, 0.12).
narrative_ontology:affects_constraint(courtship_signaling_asymmetry, gender_role_identity_fusion).
narrative_ontology:affects_constraint(courtship_signaling_asymmetry, romantic_script_performance).
narrative_ontology:affects_constraint(courtship_signaling_asymmetry, attention_asymmetry_labor).
narrative_ontology:affects_constraint(courtship_signaling_asymmetry, consent_protocol_adoption).

% DUAL FORMULATION NOTE:
% Courtship signaling asymmetry is the umbrella constraint governing high-level romantic dyadic dynamics. It decomposes into identity-fusion (higher ε, snare-type), performance labor (moderate ε, tangled rope), attention coordination (lower ε, rope-type), and sunset via consent protocols (lowest ε, scaffold). Each sub-constraint has distinct measurements and perspectives. The family is linked by the fact that addressing signaling asymmetry requires addressing all sub-components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(courtship_signaling_asymmetry, powerless, 0.92).
constraint_indexing:directionality_override(courtship_signaling_asymmetry, moderate, 0.58).
constraint_indexing:directionality_override(courtship_signaling_asymmetry, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
