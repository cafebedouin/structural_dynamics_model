% ============================================================================
% CONSTRAINT STORY: victorian_sexual_morality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_victorian_sexual_morality, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: victorian_sexual_morality
 *   human_readable: Victorian Sexual Morality Constraint
 *   domain: social/cultural/gender
 *
 * SUMMARY:
 *   Victorian sexual morality operated as a comprehensive institutional
 *   constraint coordinating patriarchal property inheritance, labor control,
 *   and religious authority while extracting reproductive autonomy from women
 *   and sexual expression from working-class populations. The constraint
 *   exhibited genuine coordination functions (ensuring paternity certainty
 *   for property transfer, controlling female labor supply, legitimizing
 *   medical and religious authority) alongside severe asymmetric extraction
 *   (denying women property rights, reproductive autonomy, and sexual
 *   agency). The constraint's suppression mechanisms were extraordinarily
 *   comprehensive: legal disability (coverture), economic dependency through
 *   wage discrimination, medical gatekeeping of reproductive knowledge,
 *   religious doctrine positioning sexual restraint as moral virtue, and
 *   social surveillance making transgression catastrophic. The extractiveness
 *   trajectory (0.72→0.48 over 100 years) reflects progressive institutional
 *   weakening as legal reform, education access, and organized feminist
 *   resistance built alternative coordination systems (legal property rights,
 *   contraception access, divorce reform, economic independence). The theater
 *   ratio increased (0.52→0.68) as the constraint's functional necessity
 *   declined and enforcement became increasingly performative rather than
 *   structurally necessary.
 *
 * KEY AGENTS:
 *   - Women (all classes): Primary victim (powerless/trapped or identity-locked) — bears full cost of reproductive restriction and sexual autonomy denial
 *   - Patriarchal property system: Primary beneficiary (institutional/arbitrage) — gains paternity certainty and female labor control
 *   - Male heads of household: Secondary beneficiary (powerful/arbitrage) — gains sexual double standard and property consolidation
 *   - Religious institutions: Secondary beneficiary (institutional/arbitrage) — gains moral authority and institutional legitimacy through sexual doctrine
 *   - Medical profession: Secondary beneficiary (institutional/arbitrage) — gains gatekeeping authority over reproductive knowledge
 *   - Working-class women: Moderate victim (moderate/constrained) — faces wage discrimination plus reproductive constraint
 *   - Early feminist movement: Organized resistance (organized/constrained) — building alternative systems through legal reform and norm-shifting
 *   - Vestigial moral authorities: Institutional degradation (institutional/arbitrage) — maintaining performances of Victorian morality after functional decline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(victorian_sexual_morality, 0.58).
domain_priors:suppression_score(victorian_sexual_morality, 0.72).
domain_priors:theater_ratio(victorian_sexual_morality, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(victorian_sexual_morality, extractiveness, 0.58).
narrative_ontology:constraint_metric(victorian_sexual_morality, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(victorian_sexual_morality, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(victorian_sexual_morality, tangled_rope).
narrative_ontology:human_readable(victorian_sexual_morality, "Victorian Sexual Morality Constraint").
narrative_ontology:topic_domain(victorian_sexual_morality, "social/cultural/gender").

domain_priors:requires_active_enforcement(victorian_sexual_morality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(victorian_sexual_morality, patriarchal_property_system).
narrative_ontology:constraint_beneficiary(victorian_sexual_morality, male_sexual_autonomy).
narrative_ontology:constraint_beneficiary(victorian_sexual_morality, religious_institutional_authority).
narrative_ontology:constraint_victim(victorian_sexual_morality, women_reproductive_autonomy).
narrative_ontology:constraint_victim(victorian_sexual_morality, working_class_sexual_autonomy).
narrative_ontology:constraint_victim(victorian_sexual_morality, unmarried_sexual_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED WOMAN (SNARE) — Structurally trapped by legal marriage dependency, property restrictions, social ostracism for sexual transgression, and absence of reproductive autonomy. The constraint's suppression mechanisms are total: economic dependency, legal disability, social isolation, and internalized shame. No exit options exist within the frame. Extraction is maximum — the constraint transfers reproductive labor, sexual compliance, and property rights to the male beneficiary while denying the woman autonomy over her own body.
constraint_indexing:constraint_classification(victorian_sexual_morality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESPECTABLE WOMAN—IDENTITY-LOCKED (SNARE) — Structurally mobile (unmarried women of means have some economic autonomy) but identity-fused with respectability and marriageability. The constraint's suppression is primarily internalized: the agent's self-concept depends on sexual purity, modesty, and the path to marriage. Exit would require abandoning the identity of 'respectable woman' entirely. The agent experiences the constraint as unchangeable at biographical timescale because changing it would require becoming a different person. Theater is high — the respectable woman performs propriety through dress, speech, deportment, and social presence, with non-compliance risking total social death.
constraint_indexing:constraint_classification(victorian_sexual_morality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: WORKING-CLASS WOMAN (TANGLED ROPE) — Faces severe but somewhat surmountable constraints: economic dependency through wage discrimination, social penalty for sexual transgression, but also some agency through reproductive work, mill labor, domestic service roles. The constraint coordinates labor (ensures women accept lower wages, longer hours, occupational segregation) while extracting reproductive compliance and denying access to birth control knowledge. The coordination function is genuine — the factory system needs a docile, controllable female workforce with disrupted child-bearing patterns. The extraction is also severe — women bear disproportionate labor burden and have no control over family planning.
constraint_indexing:constraint_classification(victorian_sexual_morality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PATRIARCHAL PROPERTY SYSTEM (ROPE) — Benefits from the constraint as pure coordination mechanism. Victorian sexual morality ensures paternity certainty (critical for property inheritance), controls female property access, and consolidates male lineage through restricted female sexual autonomy. The constraint coordinates property transfer across generations while the property system experiences minimal costs. This perspective sees the constraint as natural and necessary social order.
constraint_indexing:constraint_classification(victorian_sexual_morality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RELIGIOUS AND MEDICAL AUTHORITY (ROPE) — Institutional beneficiary (churches, medical profession) that coordinates sexual morality through theological doctrine and pseudo-scientific claims about women's nature, hysteria, and reproductive destiny. Benefits from authority over sexual knowledge, medical gatekeeping of reproductive care, and institutional legitimacy derived from enforcing moral norms. The constraint coordinates community values and medical practice while authority gains institutional prestige and power.
constraint_indexing:constraint_classification(victorian_sexual_morality, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EARLY FEMINIST MOVEMENT (SCAFFOLD) — Organized actors (first-wave feminism, suffrage movements, birth control advocates) see the constraint as a temporary coordination failure with an explicit sunset. Legal reform (property rights, suffrage, divorce access), education access, and birth control availability are building parallel systems that bypass patriarchal sexual morality. The movement has agency and a clear exit path: legislative change, institutional reform, and cultural norm-shifting. Theater increases as the movement grows because the moral objections become more performative while the underlying structural transformation accelerates.
constraint_indexing:constraint_classification(victorian_sexual_morality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: VESTIGIAL MORAL AUTHORITY (PITON) — In the long view (post-1970s), Victorian sexual morality persists as largely theatrical institutional practice despite functional atrophy. Legal marriage no longer requires sexual compliance (divorce for non-consummation was abolished), reproductive autonomy is established (contraception access), and women's property and earning rights are secured. The constraint's original functions (property coordination, paternity certainty, labor control) are carried by distinct institutions (property law, DNA testing, wage law). Victorian morality persists through institutional inertia, cultural residue, and performative nostalgia rather than structural necessity.
constraint_indexing:constraint_classification(victorian_sexual_morality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER—NATURAL LAW RISK (MOUNTAIN) — Risks naturalizing Victorian morality as an immutable law reflecting biological or evolutionary facts about human sexuality. The frame: 'Sexual restraint is natural to women and necessary for social order' presents the constraint as emerging from human nature rather than institutional design. However, the structural data contradicts this mountain classification: the constraint requires active enforcement (high suppression, legal mechanisms, social surveillance, religious doctrine). If it emerged naturally from human nature, enforcement would be minimal. The mountain perspective is a false summit — a naturalizing frame that the engine's falsity detector should flag.
constraint_indexing:constraint_classification(victorian_sexual_morality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(victorian_sexual_morality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(victorian_sexual_morality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(victorian_sexual_morality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(victorian_sexual_morality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(victorian_sexual_morality, TR),
    TR >= 0.70.

:- end_tests(victorian_sexual_morality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at T=100): Moderate-to-high. The constraint extracts reproductive autonomy, sexual expression, and economic independence from its primary targets (women). However, the extractiveness at the historical endpoint (T=100, representing ~1920) is lower than at T=0 (0.72) because legal and institutional reforms have begun transferring some autonomy back. The metric reflects the snapshot at the END of the Victorian period, when the constraint was degrading. Suppression (0.72): Severe. Legal coverture law, wage discrimination, absence of contraception access, medical gatekeeping of reproductive knowledge, social ostracism, religious doctrine positioning sexuality as sinful, and institutional enforcement through courts and churches create multiple independent suppression mechanisms that prevent exit even for agents with nominal mobility. Theater ratio (0.68): Moderate-high. A significant portion of Victorian moral enforcement is performative (display of propriety through dress, speech, deportment, social presence) rather than functionally necessary. The theater increases over time as the underlying structural functions are replaced by dedicated institutions (property law, wage law, contraception), leaving moral enforcement increasingly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The trapped woman sees a snare with no exit (mountain from her constrained perspective). The respectable woman sees the same snare but framed through identity lock (rope classification from her biographical perspective because she perceives the constraint as theoretically changeable in principle — she just cannot change it from within her identity frame). The working-class woman sees tangled rope — genuine labor coordination alongside extraction. The property system sees rope — pure coordination without cost. The feminist movement sees a scaffold with a sunset — legal reform building exit paths. The vestigial moral authority sees piton — theatrical persistence of once-functional constraint. The analytical observer risks mountain (naturalizing false summit). These gaps are not errors or disagreements but structural features of how different agents experience the same constraint differently based on their power, exit options, and temporal horizon. The constraint is simultaneously a natural law (mountain), coordination mechanism (rope), hybrid (tangled rope), pure extraction (snare), temporary problem being solved (scaffold), and degraded ritual (piton) depending on your position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is derived from their structural position relative to the constraint's extraction flow and their exit capacity. Women with no economic independence (coverture) are trapped (d≈0.95) and experience maximum extraction. Respectable women with economic independence but identity-fused with respectability are identity-locked (d≈0.88) — they can theoretically leave but their identity frame makes exit unthinkable. Working-class women with some wage income but severe constraints are constrained (d≈0.72). The patriarchal property system experiences negative effective extraction (d≈0.05) — the constraint benefits it directly without cost. Religious and medical institutions as beneficiaries experience low d values (d≈0.15) through their arbitrage positions. The early feminist movement, as organized resistance, experiences moderate d (d≈0.55) — they are partly targeted by the constraint but have organizational capacity to resist. The analytical observer at civilizational scope risks d≈0.72 (false summit) if they naturalize the constraint as an immutable law rather than seeing it as contingent institutional design.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how mandatrophy resolution works across interpersonal constraints. The question is not 'which classification is correct?' but 'which perspective reveals the true structural relationship?' The property system's rope perspective is genuine — the constraint does coordinate property transfer and inheritance. The trapped woman's snare perspective is also genuine — she does experience maximum extraction with no exit. These are not contradictory; they are complementary descriptions of the same constraint from different structural positions. The analytical observer's mountain risk reveals the mandatrophy's core problem: naturalizing language ('women's nature', 'social order', 'natural restraint') threatens to collapse the perspectival gap into false universality. The engine's false summit detector should flag the mountain classification as a naturalizing frame obscuring institutional design. The true mandatrophy resolution is the presheaf over all eight perspectives: the constraint is a hybrid-to-extractive structure whose perceived type depends entirely on your structural position. No single classification is 'the' answer. The identity-locked perspective is particularly diagnostic: it reveals that suppression is not merely structural (legal and economic barriers) but also cognitive (identity fusion making exit unthinkable). Agents whose material barriers are removed but whose identity frames remain intact continue experiencing the constraint as mountain, while agents whose identity frames shift experience the same structure as rope or scaffold. This is exactly what happened post-1960s: legal and economic barriers fell (material exit became possible) but identity-frame shifts (feminism, consciousness-raising) were required for agents to perceive and exercise their exit options.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalized_vs_structural_suppression,
    'How much of the measured suppression (0.72) is structural (legal, economic, social barriers) versus internalized (shame, identity fusion, cognitive capture)?',
    'Post-liberation trajectory analysis: measure suppression decline after legal and institutional barriers are removed (post-1960s); track whether internalized suppression persists in agents no longer materially trapped',
    'If primarily structural: suppression falls rapidly after legal reform (1970s confirms). If partially internalized: some agents continue suppressing their own autonomy even after barriers are removed, requiring additional cognitive/therapeutic intervention. This affects the identity_locked exit option''s explanatory power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    double_standard_extraction_asymmetry,
    'Does the double standard (male sexual autonomy tolerated, female sexuality punished) extract primarily to males individually or to the patriarchal property system institutionally?',
    'Analyze beneficiary benefit distribution: do individual males gain increased reproductive access and property security, or is the benefit primarily institutional (property system gains paternity certainty and female labor)? Cross-cultural comparison with societies using different paternity mechanisms.',
    'If individual extraction: the constraint is interpersonal (males as beneficiaries). If institutional extraction: the constraint is structural (property system as beneficiary). This determines whether perspectives should emphasize male-female asymmetry or property-system logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(double_standard_extraction_asymmetry, conceptual, 'Whether extraction benefits individuals or institutions').

omega_variable(
    replacement_constraint_structure,
    'What constraint structure replaced Victorian sexual morality after the 1960s? Is modern sexual autonomy genuinely unconstrained or does it impose different extraction mechanisms (consumption, attention, emotional labor)?',
    'Comparative constraint analysis: measure extractiveness of modern sexual-romantic constraint structures (marketplace dating, algorithmic matching, attention economy); compare beneficiary/victim structure to Victorian predecessor',
    'If modern constraints are lower-extraction: the sunset was successful (scaffold logic confirmed). If extraction merely shifted form: the constraint family continues with different labels and enforcement mechanisms (Goodhart drift).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(replacement_constraint_structure, empirical, 'Post-Victorian constraint replacement and structural continuity').

omega_variable(
    male_sexual_autonomy_cost,
    'What costs do males bear under Victorian sexual morality? Is male sexual autonomy genuinely unrestrained or is it constrained by honor codes, marital fidelity norms, and respectability requirements?',
    'Detailed analysis of male suppression mechanisms (honor violence, marital surveillance, social penalty for certain sexual expressions); comparison with female suppression costs; measurement of whether male power derives from full autonomy or from selective autonomy plus asymmetric enforcement',
    'If males face equal suppression: the constraint is symmetric (Rope from both positions). If males face lower suppression: the asymmetry confirms tangled rope / snare structure. If males face different kinds of suppression: the constraint might decompose into multiple family members (marital sexual obligation, male sexual expressiveness control, etc.).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(male_sexual_autonomy_cost, empirical, 'Symmetry of male and female suppression under Victorian morality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(victorian_sexual_morality, 1820, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vict_tr_t0, victorian_sexual_morality, theater_ratio, 0, 0.52).
narrative_ontology:measurement(vict_tr_t50, victorian_sexual_morality, theater_ratio, 50, 0.61).
narrative_ontology:measurement(vict_tr_t100, victorian_sexual_morality, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(vict_be_t0, victorian_sexual_morality, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(vict_be_t50, victorian_sexual_morality, base_extractiveness, 50, 0.64).
narrative_ontology:measurement(vict_be_t100, victorian_sexual_morality, base_extractiveness, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(victorian_sexual_morality, identity_coordination).
narrative_ontology:affects_constraint(victorian_sexual_morality, female_reproductive_autonomy).
narrative_ontology:affects_constraint(victorian_sexual_morality, coverture_law).
narrative_ontology:affects_constraint(victorian_sexual_morality, double_standard_enforcement).
narrative_ontology:affects_constraint(victorian_sexual_morality, medical_gatekeeping_reproduction).

% DUAL FORMULATION NOTE:
% Victorian sexual morality decomposes into multiple structurally distinct constraints with different ε values: coverture law (ε≈0.85, pure extraction via legal disability), female reproductive autonomy restriction (ε≈0.78, extraction via medical gatekeeping), male sexual double standard (ε≈0.62, extraction via asymmetric enforcement), and respectability performance (ε≈0.45, theater-heavy identity coordination). Each story gets its own perspectives and measurement trajectories. The parent story (victorian_sexual_morality) operates at ε≈0.58 as a synthetic overview of the entire moral framework. Downstream constraints have higher extractiveness because they represent specific mechanisms rather than the coordinating system as a whole.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(victorian_sexual_morality, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
