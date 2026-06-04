% ============================================================================
% CONSTRAINT STORY: foundational_charters__magna_carta_1215
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_foundational_charters__magna_carta_1215, []).

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
 *   constraint_id: foundational_charters__magna_carta_1215
 *   human_readable: Magna Carta 1215: Royal Submission to Law and Baronial Liberty
 *   domain: political/historical
 *
 * SUMMARY:
 *   Magna Carta (1215) emerges from the baronial revolt against King John's
 *   fiscal and judicial extraction. The charter is simultaneously a feudal
 *   peace treaty (regulating baronial reliefs, wardships, and homages) and a
 *   written constraint on arbitrary royal prerogative. The constraint
 *   exhibits the full tension of the tangled-rope classification: genuine
 *   coordination of feudal customs alongside real suppression of the Crown's
 *   traditional revenue sources. The beneficiaries (initially the baronage,
 *   later generalized to 'the commons') experience the constraint as limiting
 *   the Angevin fiscal machine — arbitrary scutage, arbitrary amercements,
 *   arbitrary wardships. The victims are the royal prerogative and the
 *   revenue streams it depends on. Over 400 years, the charter's
 *   extractiveness declines (feudal extraction becomes less central) while
 *   its theater increases (the invocation of Magna Carta as a symbol of
 *   liberty diverges from the actual enforcement mechanism, which derives
 *   from Parliament and statute, not from the charter text itself). This
 *   constraint is one reading of the foundational_charters kernel, alongside
 *   the Petition of Right (1628) and the Habeas Corpus Act (1679). This
 *   reading emphasizes the charter as a written royal submission to law
 *   through baronial concession — the reading that grounds later claims that
 *   English liberty descends from this act of written constraint.
 *
 * KEY AGENTS:
 *   - The Baronage (Primary Beneficiary / Institutional): Extracts relief from fiscal extraction; benefits from regularized feudal incidents. Institutional/arbitrage — can enforce through revolt threat.
 *   - The Angevin Fiscal Machine (Primary Victim / Institutional): The administrative extraction apparatus (scutage, reliefs, wardships, amercements). Loses revenue capacity through the charter's constraints.
 *   - The Powerless Peasant and Townsman (Secondary Victim / Powerless): Subject to arbitrary exaction but lacking enforcement mechanisms. Trapped/powerless — sees the charter as constraint on extraction but cannot compel enforcement.
 *   - The Urban Merchant (Secondary Beneficiary-Victim / Moderate): Benefits from merchant protections and property standardization; constrained by feudal subordination. Moderate/constrained — mixed experience.
 *   - The Crown as Institutional Actor (Moderate Beneficiary-Victim / Institutional): Experiences the charter as constraint on revenue but also as regularization that enables better governance and reduces revolt risk. Organized/constrained at the royal council level.
 *   - The Later Liberal Interpreter (Piton / Institutional): Projects universal rule-of-law principles backward into a feudal bargain. Maintains the charter as symbol while actual enforcement derives from Parliament. Arbitrage — can choose when to invoke Magna Carta for legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(foundational_charters__magna_carta_1215, 0.38).
domain_priors:suppression_score(foundational_charters__magna_carta_1215, 0.65).
domain_priors:theater_ratio(foundational_charters__magna_carta_1215, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(foundational_charters__magna_carta_1215, extractiveness, 0.38).
narrative_ontology:constraint_metric(foundational_charters__magna_carta_1215, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(foundational_charters__magna_carta_1215, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(foundational_charters__magna_carta_1215, tangled_rope).
narrative_ontology:human_readable(foundational_charters__magna_carta_1215, "Magna Carta 1215: Royal Submission to Law and Baronial Liberty").
narrative_ontology:topic_domain(foundational_charters__magna_carta_1215, "political/historical").

domain_priors:requires_active_enforcement(foundational_charters__magna_carta_1215).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(foundational_charters__magna_carta_1215, 'c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e').
narrative_ontology:cs_kernel_codification('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', fixed_text).
narrative_ontology:cs_authority_grounding('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', lineage).
narrative_ontology:cs_interpretation_layer_present('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e').
narrative_ontology:cs_reading_relation('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', foundational_charters__petition_of_right_1628, influences).
narrative_ontology:cs_reading_relation('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', foundational_charters__habeas_corpus_act_1679, influences).
narrative_ontology:cs_axiom('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', foundational, royal_written_submission_is_foundational_authority).
narrative_ontology:cs_axiom_status(royal_written_submission_is_foundational_authority, holdable).
narrative_ontology:cs_axiom_grounding('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', royal_written_submission_is_foundational_authority, conventional).
narrative_ontology:cs_axiom('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', foundational, later_liberties_descend_from_charter_concession).
narrative_ontology:cs_axiom_status(later_liberties_descend_from_charter_concession, overridden).
narrative_ontology:cs_axiom_grounding('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', later_liberties_descend_from_charter_concession, empirically_contingent).
narrative_ontology:cs_reference_frame('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', written_royal_submission_to_law).
narrative_ontology:cs_drift_state('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', contemporary, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c1fd5fc7-0c88-4af4-afb0-dc5129c01d6e', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(foundational_charters__magna_carta_1215, foundational_charters).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(foundational_charters__magna_carta_1215, baronage).
narrative_ontology:constraint_beneficiary(foundational_charters__magna_carta_1215, later_commons).
narrative_ontology:constraint_victim(foundational_charters__magna_carta_1215, angevin_fiscal_machine).
narrative_ontology:constraint_victim(foundational_charters__magna_carta_1215, royal_arbitrary_prerogative).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEASANT AND TOWNSMAN (SNARE) — Trapped by the Angevin fiscal machine (arbitrary scutage, arbitrary amercements, arbitrary reliefs, wardships sold to the highest bidder). Magna Carta forbids 'unjust' exactions and requires 'lawful judgment of peers' but provides no enforcement mechanism for the powerless. The constraint is experienced as extraction with minimal coordination benefit — the charter's protections apply nominally but the Crown retains all enforcement power. Maximum experienced suppression; no exit.
constraint_indexing:constraint_classification(foundational_charters__magna_carta_1215, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BARONAGE / MAGNA CARTA BENEFICIARY (ROPE) — Experiences the charter as coordination mechanism: a written statement of customs that secures baronial property rights (wardships, reliefs, homages) against arbitrary royal exploitation and guarantees feudal rights against the ransoming Crown. The charter benefits the baronage directly — it arrests the fiscal machinery that had been extracting feudal incidents. Arbitrage option available: baronage can enforce through threat of revolt. Net beneficiary — constraint runs toward this agent.
constraint_indexing:constraint_classification(foundational_charters__magna_carta_1215, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN MERCHANT AND GUILDSMAN (TANGLED ROPE) — Benefits from the charter's coordination of property rights (protection of merchant guilds, standardization of weights and measures, free movement of merchant ships). But also subject to the charter's maintenance of feudal order, which constrains their rise and subordinates urban law to baronial and royal courts. The constraint is genuinely mixed: real coordination function alongside real extraction. Cannot arbitrage easily (constrained by guild subordination and urban dependency on baronial protection).
constraint_indexing:constraint_classification(foundational_charters__magna_carta_1215, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CROWN (TANGLED ROPE) — The Crown reissues the charter (1217, 1225, plus continued reissues) because the charter solves a real coordination problem: it converts the Crown's arbitrary fiscal extraction into a regularized system of customs and aids. Regular taxation is more predictable and reliable than arbitrary levies and ransom extraction; predictability enables better planning and reduces baronial revolt risk. But the Crown also experiences extraction: the charter constrains royal revenue and requires consensus for new taxation. Mixed coordination and constraint — the Crown both benefits and loses.
constraint_indexing:constraint_classification(foundational_charters__magna_carta_1215, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LATER LIBERAL INTERPRETATION / PITON (PROCEDURAL MYTH) — Later centuries interpret Magna Carta as a foundational document of universal liberty and the rule of law, reading backward into the charter a commitment to human rights and constitutional limits on power that the 1215 document does not contain. The charter becomes performative — invoked as a symbol of liberty whose actual 1215 text is subordinated to ideological reinterpretation. Theater ratio high (0.65+): the invocation of Magna Carta does meaningful symbolic work (legitimates Parliament, constrains kings ideologically) but the actual enforcement derives from subsequent institutional development (statutes, precedent, parliamentary power), not from the charter text itself. The performative work sustains institutional legitimacy.
constraint_indexing:constraint_classification(foundational_charters__magna_carta_1215, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — From a civilizational perspective, the constraint appears as a natural law of political development: any state extracting beyond a threshold of fairness must eventually commit itself to written law as a legitimacy mechanism. Kings who break written covenants face revolt; this is an iron law of governance. However, the structural data contradicts the mountain classification: the charter is an institutional bargain (ransom kingship converted into feudal custom), beneficiaries are identifiable (baronage), and the extraction mechanism is concrete (fiscal machinery). The engine's false-summit detector identifies this as naturalization of a contingent feudal arrangement.
constraint_indexing:constraint_classification(foundational_charters__magna_carta_1215, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(foundational_charters__magna_carta_1215_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(foundational_charters__magna_carta_1215, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(foundational_charters__magna_carta_1215, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(foundational_charters__magna_carta_1215, TR),
    TR >= 0.70.

:- end_tests(foundational_charters__magna_carta_1215_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The chart measures the baseline extraction suppressed by the charter in its 1215 form. The Angevin fiscal machine (arbitrary reliefs, scutage, wardships) constitutes genuine extraction of feudal incidents — the charter bargains this down into regularized custom. The extraction is not as severe as a snare (no total suppression, some coordination benefit) but significant (the Crown loses real revenue). The measurement captures the 1215 snapshot before institutional developments (Parliament, statute, common law) transform the constraint's mechanism. Suppression (0.65): High. The charter requires 'lawful judgment of peers' for major extractions (aids, reliefs, wardships) and bans 'unjust' exactions — but provides no enforcement mechanism for the powerless. Baronial enforcement (threat of revolt) is available; common enforcement is not. The suppression is structural (the Crown retains enforcement authority) and internalized (later reinterpretations project the charter's text backward as if it contained universal principles it does not). Theater ratio (0.52): Moderate-high. In 1215, the charter is substantive but performative — a written ritual of royal submission that the Crown can (and does) ignore or violate. By 400 years later, the theater ratio rises to 0.72 as the charter becomes a legendary authority invoked for legitimacy but enforced through Parliament and statute, not through the charter text itself.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps reveal the constraint's tangled nature. The baronage sees Rope (coordination of feudal customs, protection of property rights, arbitrage option via revolt). The powerless see Snare (nominally protected but lacking enforcement mechanisms). The merchant sees mixed Tangled Rope (coordination benefit with extraction constraint). The Crown sees Tangled Rope (constraint on revenue but regularization of taxation). The later liberal interpreter sees Piton (legendary authority maintained through invocation, not function). The analytical observer risks seeing Mountain (natural law of kingship submission) but the false-summit detector identifies naturalization — the charter is an institutional bargain, not a law of nature. The gaps show that the same base properties produce different experienced classifications depending on the agent's structural position (power, exit options, beneficiary/victim status).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural relationship to the extraction flow. Beneficiaries (baronage, urban merchants, later commons) have low d (0.15-0.40): the charter suppresses extraction that ran toward them, so they experience the constraint as coordinating their interests. The powerless peasant has high d (0.85-0.95): no enforcement mechanism exists for them; the charter's constraints apply nominally but not in practice. The Crown, as both beneficiary (regularized taxation is more reliable) and victim (loses arbitrary revenue), has intermediate d (0.50-0.60). The directionality values feed the sigmoid f(d), producing the effective extractiveness chi each agent experiences. The constraint appears extractive from the powerless perspective (high chi) and coordinating from the baronial perspective (low chi), even though the base extractiveness (0.38) is the same. This shows why single-point classification fails and why indexical classification is necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates that the mandatrophy is resolved not by finding a single 'correct' type but by recognizing that all six types are legitimate perspectival readings. The charter is simultaneously Snare (for the powerless), Rope (for the baronage), Tangled Rope (for the Crown and merchants), Piton (for the later liberal interpreter), and Mountain candidate (for the analytical observer — though the false-summit detector rejects this). The extractiveness (0.38) and suppression (0.65) produce tangled-rope metrics at the base level, but the indexical perspectives show how the same constraint appears across all six types depending on the observer's position. The mandatrophy is resolved by showing that the presheaf over the observation sites (the six perspectives) is the complete answer — there is no single type that describes this constraint from all positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_ambiguity_feudal_vs_liberal,
    'Is Magna Carta fundamentally a feudal custom bargain between king and baronage, or a foundational statement of universal rule-of-law principles?',
    'Textual analysis of the 1215 charter clauses: count feudal clauses (property rights, reliefs, wardships) vs. universal-principle clauses (no arbitrary exactions, lawful judgment). Historical analysis of contemporary baronial understanding vs. later reinterpretation. Compare reissues (1217, 1225, 1265) for continuity of intent.',
    'If feudal bargain: the charter is a Tangled Rope (mixed coordination of feudal customs with extraction-suppression). If universal principle: the charter is aspirational Rope (coordination mechanism toward rule of law). This distinction determines whether the piton perspective is legitimate (later reinterpretation) or false (projecting principles backward).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_ambiguity_feudal_vs_liberal, empirical, 'Feudal bargain vs. universal rule-of-law reading ambiguity').

omega_variable(
    enforcement_mechanism_efficacy,
    'Does the 1215 charter contain ANY enforcement mechanism by which the powerless can compel the Crown to observe its terms, or is enforcement entirely dependent on baronial threat of revolt?',
    'Clause-by-clause analysis of the charter text and reissues. Identify enforcement provisions: do any allow common subjects to petition courts, or is all enforcement baronial? Historical tracing of cases where common subjects invoked charter clauses vs. cases where only baronage could enforce.',
    'If no common enforcement: the powerless perspective (Snare) is definitionally accurate — the charter provides no exit option. If enforcement mechanisms exist: the constraint becomes Tangled Rope even for powerless agents (mixed coordination and constraint). This affects the entire classificatory landscape.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_efficacy, empirical, 'Whether the 1215 charter contains mechanisms for non-baronial enforcement').

omega_variable(
    successor_causation_chain,
    'Do later English liberties (Habeas Corpus, Bill of Rights, Parliamentary sovereignty) causally descend from Magna Carta''s writing of royal submission, or do they emerge from distinct institutional developments that the charter merely prefigures symbolically?',
    'Institutional history: trace parliamentary claims and habeas corpus arguments back to cited sources. Count instances of explicit invocation of Magna Carta in statutes, legal arguments, and political rhetoric across centuries. Distinguish causal derivation from symbolic appropriation.',
    'If causal lineage: Magna Carta is a structural anchor (Rope/Tangled Rope) whose institutional weight compounds through reissue and citation. If symbolic appropriation: Magna Carta becomes a Piton (legendary authority maintained through invocation, not function). This determines whether the charter is a living constraint or a performative memorial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_causation_chain, empirical, 'Causal vs. symbolic relationship between Magna Carta and successor liberties').

omega_variable(
    reading_contest_structure,
    'Within the commitment-system frame, do these three readings (Magna Carta, Petition of Right, Habeas Corpus) represent three distinct theories of rule-of-law authority, or do they represent progressive refinement of a single theory?',
    'Analysis of the three readings'' axioms and reference frames. Do they foreclose each other (mutually exclusive theories), coexist (different parties'' legitimate readings), or influence (upstream → downstream development)? Examine contemporary and historical arguments to see whether advocates of Petition of Right treated Magna Carta as foundational (influences) or as superseded (forecloses).',
    'If forecloses: readings are logically incompatible; the kernel cannot hold all three. If influences: readings form a developmental lineage with Magna Carta as base authority. If coexists: readings represent different institutions'' (Crown vs. Parliament, executive vs. judiciary) legitimate interpretations of rule of law. This determines the reading_relations values in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_structure, conceptual, 'Structural relationship among three readings of the rule-of-law kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(foundational_charters__magna_carta_1215, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magcarta_theater_t0, foundational_charters__magna_carta_1215, theater_ratio, 0, 0.35).
narrative_ontology:measurement(magcarta_theater_t200, foundational_charters__magna_carta_1215, theater_ratio, 200, 0.52).
narrative_ontology:measurement(magcarta_theater_t400, foundational_charters__magna_carta_1215, theater_ratio, 400, 0.72).

% Extraction over time
narrative_ontology:measurement(magcarta_extract_t0, foundational_charters__magna_carta_1215, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(magcarta_extract_t200, foundational_charters__magna_carta_1215, base_extractiveness, 200, 0.32).
narrative_ontology:measurement(magcarta_extract_t400, foundational_charters__magna_carta_1215, base_extractiveness, 400, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(magcarta_suppress_t0, foundational_charters__magna_carta_1215, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(magcarta_suppress_t200, foundational_charters__magna_carta_1215, suppression_requirement, 200, 0.58).
narrative_ontology:measurement(magcarta_suppress_t400, foundational_charters__magna_carta_1215, suppression_requirement, 400, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(foundational_charters__magna_carta_1215, enforcement_mechanism).
narrative_ontology:affects_constraint(foundational_charters__magna_carta_1215, foundational_charters__petition_of_right_1628).
narrative_ontology:affects_constraint(foundational_charters__magna_carta_1215, foundational_charters__habeas_corpus_act_1679).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the foundational_charters kernel. The kernel contest involves three structural interpretations of rule-of-law grounding: (1) written royal submission (this reading), (2) parliamentary reassertion of medieval rights (Petition of Right), (3) judicial procedure enforcing detention justification (Habeas Corpus). Each reading has its own constraint story with its own extractiveness and suppression values because each reading foregrounds different structural mechanisms. The readings coexist as competing institutional theories held by different parties in ongoing constitutional discourse. Magna Carta influences the Petition of Right (later barons cite the charter as foundational) and both influence Habeas Corpus (the judicial procedure grounds itself in the tradition of written constraint). The reading_relations in cs_structure capture these upstream/downstream dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(foundational_charters__magna_carta_1215, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
