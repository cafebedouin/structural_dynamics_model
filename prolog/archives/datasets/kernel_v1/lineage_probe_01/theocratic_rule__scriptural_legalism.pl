% ============================================================================
% CONSTRAINT STORY: theocratic_rule__scriptural_legalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_theocratic_rule__scriptural_legalism, []).

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
 *   constraint_id: theocratic_rule__scriptural_legalism
 *   human_readable: Theocratic Rule via Scriptural Legalism
 *   domain: political/comparative
 *
 * SUMMARY:
 *   Scriptural legalism is a reading of the theocratic kernel that frames
 *   governance as the authoritative application of revealed code by trained
 *   interpreters. The state is subordinate to a law it did not make and
 *   cannot amend; legislative sovereignty is suppressed in favor of
 *   hermeneutical authority. This reading presents a distinct structural
 *   claim from its sibling readings: unlike divine kingship (where the ruler
 *   embodies divine presence), and unlike clerical guardianship (where the
 *   jurist rules pending an awaited perfection), scriptural legalism
 *   positions the code itself as supreme, with interpreters as its servants
 *   rather than its authors or proxies. The beneficiary is the interpreting
 *   class, whose exclusive authority to declare the code's meaning becomes
 *   the institutional rent. The victim is legislative innovation itself — the
 *   capacity to originate statutory law is permanently foreclosed. The
 *   constraint exhibits a perspectival range: the interpreting class
 *   experiences it as enabling coordination (Rope), the legislative
 *   bureaucracy as mixed coordination and constraint (Tangled Rope), the
 *   foreclosed legislative capacity as pure extraction (Snare), reform
 *   movements as a temporary problem with reinterpretation pathways
 *   (Scaffold), and the textualist ideal as increasingly theatrical (Piton).
 *   The analytical observer risks naturalizing the suppression of legislative
 *   sovereignty as an immutable theological law (Mountain candidate), but the
 *   structural data reveals institutional extraction warranted by a
 *   theological claim.
 *
 * KEY AGENTS:
 *   - Interpreting Class (ulema, qadi, trained jurists): Primary beneficiary (institutional/arbitrage) — holds exclusive authority to declare code meaning; captures institutional rent while providing coordination function
 *   - Legislative Apparatus: Secondary victim (moderate/constrained) — benefits from code's stability but cannot originate statutory reform; trapped between coordination benefit and suppressed innovation capacity
 *   - Statutory Innovation Capacity: Primary victim (powerless/trapped) — legislative authority is permanently subordinated; no alternative exit exists
 *   - Modernization Coalition: Organized secondary actor (organized/mobile) — judges and administrators seeking reinterpretation doctrine as alternative pathway; sees Scaffold sunset
 *   - Theological Establishment: Secondary beneficiary (institutional/arbitrage) — benefits from preservation of clerical authority structure; maintains performance of textual completeness
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks treating institutional suppression as theological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(theocratic_rule__scriptural_legalism, 0.38).
domain_priors:suppression_score(theocratic_rule__scriptural_legalism, 0.62).
domain_priors:theater_ratio(theocratic_rule__scriptural_legalism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(theocratic_rule__scriptural_legalism, extractiveness, 0.38).
narrative_ontology:constraint_metric(theocratic_rule__scriptural_legalism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(theocratic_rule__scriptural_legalism, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(theocratic_rule__scriptural_legalism, tangled_rope).
narrative_ontology:human_readable(theocratic_rule__scriptural_legalism, "Theocratic Rule via Scriptural Legalism").
narrative_ontology:topic_domain(theocratic_rule__scriptural_legalism, "political/comparative").

domain_priors:requires_active_enforcement(theocratic_rule__scriptural_legalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(theocratic_rule__scriptural_legalism, 'a6ba5ee2-80da-470a-b2b3-e05e86646c05').
narrative_ontology:cs_kernel_codification('a6ba5ee2-80da-470a-b2b3-e05e86646c05', fixed_text).
narrative_ontology:cs_authority_grounding('a6ba5ee2-80da-470a-b2b3-e05e86646c05', lineage).
narrative_ontology:cs_interpretation_layer_present('a6ba5ee2-80da-470a-b2b3-e05e86646c05').
narrative_ontology:cs_reading_relation('a6ba5ee2-80da-470a-b2b3-e05e86646c05', theocratic_rule__clerical_guardianship, coexists_with).
narrative_ontology:cs_reading_relation('a6ba5ee2-80da-470a-b2b3-e05e86646c05', theocratic_rule__divine_kingship, coexists_with).
narrative_ontology:cs_axiom('a6ba5ee2-80da-470a-b2b3-e05e86646c05', foundational, scriptural_supremacy_over_legislation).
narrative_ontology:cs_axiom_status(scriptural_supremacy_over_legislation, holdable).
narrative_ontology:cs_axiom_grounding('a6ba5ee2-80da-470a-b2b3-e05e86646c05', scriptural_supremacy_over_legislation, deontological).
narrative_ontology:cs_axiom('a6ba5ee2-80da-470a-b2b3-e05e86646c05', foundational, interpretation_not_legislation).
narrative_ontology:cs_axiom_status(interpretation_not_legislation, holdable).
narrative_ontology:cs_axiom_grounding('a6ba5ee2-80da-470a-b2b3-e05e86646c05', interpretation_not_legislation, deontological).
narrative_ontology:cs_reference_frame('a6ba5ee2-80da-470a-b2b3-e05e86646c05', text_as_sovereign_law).
narrative_ontology:cs_drift_state('a6ba5ee2-80da-470a-b2b3-e05e86646c05', contemporary_statutory_complexity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6ba5ee2-80da-470a-b2b3-e05e86646c05', '').
narrative_ontology:cs_kernel_id(theocratic_rule__scriptural_legalism, theocratic_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(theocratic_rule__scriptural_legalism, interpreting_class).
narrative_ontology:constraint_beneficiary(theocratic_rule__scriptural_legalism, theological_establishment).
narrative_ontology:constraint_victim(theocratic_rule__scriptural_legalism, legislative_innovation).
narrative_ontology:constraint_victim(theocratic_rule__scriptural_legalism, statutory_discretion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORECLOSED STATUTORY CAPACITY (SNARE) — Legislative bodies are prohibited from originating law; they may only apply or minor-interpret the revealed code. This victim set (statutory innovation itself) experiences the constraint as pure extraction: the capacity to legislate is removed, and no alternative exit exists. The state apparatus that might otherwise exercise legislative sovereignty is permanently subordinated.
constraint_indexing:constraint_classification(theocratic_rule__scriptural_legalism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE BUREAUCRACY (TANGLED ROPE) — State administrators and legislators benefit from the stability and legitimacy the scriptural code provides (coordination function: law predates and outlasts any faction) but are constrained by their inability to originate statutory reform. They experience both the coordination benefit and the extraction cost. Exit requires either abandoning the theocratic frame or revolution — both high-cost, so exit_options: constrained.
constraint_indexing:constraint_classification(theocratic_rule__scriptural_legalism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERPRETING CLASS (ROPE) — The clergy and trained jurists (ulema, qadi) benefit from exclusive authority to declare the code's meaning. They experience the constraint as coordination: the code's authority is only meaningful if someone authorizes its application. The interpreting class captures the institutional rent (control of jurisprudence, judicial authority, legislative veto) but also provides the genuine coordination function (stable, transcendent law enables long-term governance planning). Net beneficiary with arbitrage exit: they could theoretically exit by refusing interpretation, but the institutional structure makes that exit costless — they can simply hand authority to a successor class, maintaining arbitrage value.
constraint_indexing:constraint_classification(theocratic_rule__scriptural_legalism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MODERNIZATION COALITION (SCAFFOLD) — Reform-minded factions (judges seeking precedent flexibility, administrators seeking statutory innovation, economic actors seeking regulatory response) experience the scriptural legalism constraint as a temporary obstacle with a known sunset: reinterpretation doctrine (tafsir, ijtihad, maslaha) promises to make the code flexible without formally amending it. The scaffold gate (low effective extraction, theater ≤ 0.70) holds if the reinterpretation pathway is genuinely available and advancing. If reinterpretation doctrine is foreclosed by orthodox interpretation, this perspective collapses to Tangled Rope or Snare.
constraint_indexing:constraint_classification(theocratic_rule__scriptural_legalism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TEXTUALIST IDEAL (PITON) — The foundational claim that scriptural law is complete, determinate, and capable of governing all contingencies is increasingly theatrical. The performed application of text proceeds as if the code answers contemporary questions (constitutional law, financial regulation, digital governance) for which it was not designed. The interpreting class maintains this theater through hermeneutical expansion and creative analogy (qiyas). Piton classification reflects that the performance ('the code contains all answers') persists despite acknowledged institutional inertia — the interpreters know the text underdetermines many questions but maintain the performance to preserve clerical authority.
constraint_indexing:constraint_classification(theocratic_rule__scriptural_legalism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THEOLOGICAL IMMUTABILITY (MOUNTAIN CANDIDATE) — From a civilizational/universal perspective grounded in theological commitments to scriptural perfection, the subordination of legislative sovereignty to revealed law appears as an unchangeable feature: a revealed code cannot be amended by human will without ceasing to be revealed law. The constraint appears as an immutable consequence of the theological kernel. However, the structural data reveals this as a false summit: the suppression (0.62) and extractiveness (0.38) are institutional properties, not theological necessities. The theological claim does not prevent alternative readings (clerical guardianship, divine kingship) or reinterpretation doctrines that preserve the code while enabling legislative flexibility.
constraint_indexing:constraint_classification(theocratic_rule__scriptural_legalism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theocratic_rule__scriptural_legalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(theocratic_rule__scriptural_legalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theocratic_rule__scriptural_legalism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(theocratic_rule__scriptural_legalism, TR),
    TR >= 0.70.

:- end_tests(theocratic_rule__scriptural_legalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The interpreting class captures institutional rent from exclusive interpretive authority, but the extraction is bounded and warranted by the text itself. Unlike arbitrary despotism, the extraction cannot exceed what the text can be credibly interpreted to justify. The 0.38 value reflects that the extraction is real (clerical authority concentrates power) but not maximal — the text imposes limits on its own interpretation. The measurement trajectory (0.28 → 0.38 over 500 time units, typically centuries) shows gradual increases in extractiveness as reinterpretation becomes more expansive and sophisticated — the hermeneutical tools grow more powerful, enabling the interpreting class to expand their authority while maintaining the text-bounded fiction. Suppression (0.62): Moderate-high. Legislative sovereignty is structurally suppressed: statutory amendment requires the text to be amended, which is forbidden; statutory innovation must be disguised as reinterpretation; new law cannot contradict the code. This suppression is not maintained by overt force alone but by institutional architecture (only interpreters can authorize law) and ideological commitment (the code's completeness is affirmed). The measurement trajectory (0.55 → 0.62) shows gradual enforcement intensification as modernization pressures increase, requiring stricter orthodox gatekeeping to prevent reinterpretation doctrine from breaking its moorings. Theater ratio (0.58): Moderate. The claim that the scriptural code governs all contingencies — from 7th-century Bedouin life to digital governance — is substantially performative. The interpreting class maintains this theater through hermeneutical expansion and creative analogy. The trajectory (0.42 → 0.58) shows increasing theater as the gap between what the text actually addresses and what it is claimed to address grows. Contemporary interpreters must work harder to maintain the fiction of textual completeness.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates deep perspectival incommensurability. The interpreting class sees a Rope (coordination mechanism enabling stable transcendent governance) while the foreclosed legislative capacity sees a Snare (permanent suppression with no exit). The legislative apparatus sees a Tangled Rope (genuine coordination value but with extraction cost). The modernization coalition sees a Scaffold with a reinterpretation sunset. The textualist ideal sees an increasingly theatrical Piton (the performance of completeness persists despite hermeneutical strain). The theological observer risks seeing a Mountain (scriptural supremacy as immutable law). The analytical observer's mountain classification is the crucial diagnostic: it reveals that theological claims are being used to naturalize institutional extraction. The true gap is between readings that accept the scriptural legalism frame (all except the Mountain perspective) and readings that recognize the frame itself as contingent and contestable (the Mountain perspective when corrected by the engine's false summit detector).
 *
 * DIRECTIONALITY LOGIC:
 *   The interpreting class (institutional/arbitrage) has low directionality d because they are the beneficiary of the constraint and have exit options (they can cease interpretation, though institutional structure makes this costless). The legislative apparatus (moderate/constrained) has moderate d because they bear costs (suppressed innovation) but also benefits (code stability) and have some exit capacity at high cost (revolution, constitutional reform). The foreclosed legislative capacity (powerless/trapped) has high d because it is a pure victim with no exit. The modernization coalition (organized/mobile) has moderate d because they face barriers but see an exit path (reinterpretation doctrine). The analytical observer (analytical/analytical) derives d from the observation position: an analyst sees the structure clearly, so d ≈ 0.72 (canonical for analytical), producing a mountain classification that the engine will flag as false summit due to the beneficiary declarations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reinterpretation_doctrine_operationality,
    'Do reinterpretation doctrines (ijtihad, tafsir, qiyas, maslaha) genuinely enable statutory innovation within the scriptural legalism framework, or are they performative proxies for what amounts to legislative amendment disguised as interpretation?',
    'Historical analysis of judicial decisions employing reinterpretation doctrine vs. formal statutory changes; measurement of the scope of novel law generated through reinterpretation vs. traditional amendment in comparable legal systems; examination of whether reinterpretation doctrine gates have been formally or informally closed by orthodox interpretation authorities',
    'If genuinely operational: the constraint is better classified as Rope (reinterpretation = low-friction coordination mechanism) and the Scaffold perspective is confirmed. If performative: the constraint is Tangled Rope or Snare (apparent flexibility masks actual suppression of legislative sovereignty), and the Piton perspective dominates. This determines whether the constraint''s future is sunset (Scaffold) or inertial (Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reinterpretation_doctrine_operationality, empirical, 'Whether reinterpretation doctrines enable genuine statutory innovation or are performative covers for legislative suppression').

omega_variable(
    clerical_authority_boundary,
    'What distinguishes this reading (scriptural legalism: law is found, not made, by trained interpreters) from the sibling reading of clerical guardianship (the jurist rules pending perfection)? Where is the boundary between interpretation and governance?',
    'Institutional analysis of the division between judicial interpretation authority and executive/legislative authority in theocratic systems claiming scriptural legalism vs. clerical guardianship; examination of whether the interpreting class is formally subordinate to the state or whether it exercises direct governance; analysis of whether legislation-by-reinterpretation is presented as interpretation or as governance decision',
    'If the boundary is maintained (interpreters advise/limit but do not govern): scriptural legalism holds. If the interpreting class exercises direct governance (as in clerical guardianship): the constraint should be reclassified. This ω documents the reading''s dependence on institutional role separation that may not hold in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clerical_authority_boundary, conceptual, 'Whether the interpreting class remains formally subordinate to the state (scriptural legalism) or exercises direct governance (clerical guardianship)').

omega_variable(
    scriptural_completeness_assumption,
    'This reading''s fundamental axiom is that revealed scriptural law is complete and determinative for governance. Is this claim empirically falsifiable, or is it a theological commitment immune to evidence?',
    'Analysis of how the interpreting class responds when the code fails to address a novel contingency (digital rights, financial derivatives, pandemic response). If they reinterpret to accommodate, the completeness claim is preserved theatrically. If they acknowledge gaps, the claim breaks. Examination of whether theological tradition treats scriptural completeness as a testable claim or as a foundational commitment beyond evidence.',
    'If falsifiable: the mountain perspective''s ''theological immutability'' is a false summit, and the engine should flag it for clerical authority benefit. If non-falsifiable (theological commitment): the mountain perspective is correct from the theological frame, but this reveals a committer-axis conflict (theological commitments vs. institutional reality). This ω documents the incommensurability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_completeness_assumption, conceptual, 'Whether scriptural completeness is an empirical claim or a theological commitment immune to evidence').

omega_variable(
    institutional_benefit_apportionment,
    'Does the extractiveness (0.38) accurately represent how much the interpreting class captures relative to the state apparatus and the population? Is the institutional rent concentrated in interpretation authority or distributed across the state?',
    'Comparative institutional analysis: allocation of budgets, authority, and decision-making power between clerical, judicial, and legislative branches in theocracies practicing scriptural legalism vs. comparable secular and other theocratic states; measurement of the career mobility and compensation premium for trained interpreters vs. secular administrators',
    'If extractiveness understates the clerical capture: reclassify to higher ε (0.50+), shifting the constraint toward Snare. If it overstates: reclassify lower (0.25-0.30), shifting toward Rope. This determines whether the interpreting class is the sole beneficiary or whether benefit is shared with executive/legislative apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_apportionment, empirical, 'Accurate measurement of how much institutional rent is captured by the interpreting class vs. distributed across the state').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(theocratic_rule__scriptural_legalism, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theolegal_tr_t0, theocratic_rule__scriptural_legalism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(theolegal_tr_t250, theocratic_rule__scriptural_legalism, theater_ratio, 250, 0.52).
narrative_ontology:measurement(theolegal_tr_t500, theocratic_rule__scriptural_legalism, theater_ratio, 500, 0.58).

% Extraction over time
narrative_ontology:measurement(theolegal_be_t0, theocratic_rule__scriptural_legalism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(theolegal_be_t250, theocratic_rule__scriptural_legalism, base_extractiveness, 250, 0.35).
narrative_ontology:measurement(theolegal_be_t500, theocratic_rule__scriptural_legalism, base_extractiveness, 500, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(theolegal_su_t0, theocratic_rule__scriptural_legalism, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(theolegal_su_t250, theocratic_rule__scriptural_legalism, suppression_requirement, 250, 0.59).
narrative_ontology:measurement(theolegal_su_t500, theocratic_rule__scriptural_legalism, suppression_requirement, 500, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(theocratic_rule__scriptural_legalism, enforcement_mechanism).
narrative_ontology:affects_constraint(theocratic_rule__scriptural_legalism, theocratic_rule__clerical_guardianship).
narrative_ontology:affects_constraint(theocratic_rule__scriptural_legalism, theocratic_rule__divine_kingship).

% DUAL FORMULATION NOTE:
% The theocratic_rule kernel decomposes into three constraint stories, one for each reading. Each story has its own ε, its own beneficiary/victim structure, and its own type classification. Scriptural_legalism (this file, ε=0.38, Tangled Rope) emphasizes the text as supreme and the interpreter as servant. Clerical_guardianship (sibling, ε≈0.50+, likely Snare or Tangled Rope) emphasizes the guardian's direct governance role. Divine_kingship (sibling, ε≈0.45+, likely Tangled Rope or Piton) emphasizes the ruler's cosmic status. The three stories are linked via network.affects_constraints because they are alternative readings of the same kernel, and institutional choices to emphasize one reading over another reshape the constraint landscape for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
