% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: US Constitutional Authority Under Living Constitutionalism
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint captures the living constitutionalist reading of US
 *   constitutional authority — the framework in which constitutional
 *   principles endure but their application evolves with social attitudes and
 *   circumstances. This is ONE READING of a contested kernel (the US
 *   Constitution itself). The kernel is a fixed text ratified in 1787, but
 *   how that text constrains contemporary judicial action depends on which
 *   reading of the Constitution's authority structure is adopted. Living
 *   constitutionalism interprets the Constitution as a set of enduring
 *   principles (liberty, equal protection, due process) whose meaning adapts
 *   as society evolves. This reading empowers judges to develop doctrine
 *   responsive to contemporary moral consensus while remaining bound by
 *   foundational constitutional principles. The constraint exhibits classic
 *   tangled-rope structure: it genuinely coordinates the constitutional
 *   project (preventing both rigid ossification and unfettered majoritarian
 *   override) while also extracting power from legislatures and constraining
 *   originalist competitors. The extractiveness value (0.52) reflects the
 *   moderate power shift from elected branches to judiciary; the suppression
 *   trajectory shows declining suppression of rights expansion over the
 *   interval (from 0.58 to 0.38), consistent with the living
 *   constitutionalist narrative that contemporary rights recognition becomes
 *   easier as moral consensus expands. However, the analytical observer
 *   perspective risks naturalizing this institutional arrangement (empowering
 *   judges to follow moral evolution) as an immutable feature of how language
 *   and law work — triggering false summit detection because the structural
 *   data shows clear beneficiaries and victims.
 *
 * KEY AGENTS:
 *   - Rights Claimants in Evolving Contexts: Primary beneficiaries (powerless/trapped) — individuals asserting constitutional protections not explicitly secured at ratification benefit from judicial willingness to adapt application to changing social conditions
 *   - Federal Judiciary: Primary institutional beneficiary (institutional/constrained) — receives power to develop doctrine and adapt constitutional meaning; constrained by need to maintain appearance of fidelity to enduring principles
 *   - Counter-Majoritarian Constraint (Democracy Advocates): Primary victim (moderate/constrained) — face erosion of direct democratic input and legislative primacy; benefit marginally from constraint that judges cannot completely ignore constitutional limits
 *   - Legislative Branch (Congress, State Legislatures): Secondary victim (powerless/trapped) — statutory enactments and policy choices vulnerable to judicial reinterpretation under evolving constitutional doctrine; have no effective recourse short of constitutional amendment
 *   - Originalist Authority Structure: Institutional competitor (institutional/arbitrage) — persists despite declining doctrinal dominance; maintains institutional legitimacy and ideological constituency but increasingly operates performatively
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks confusing contingent institutional arrangement (judicial power to adapt doctrine) with necessity (that meaning must evolve with language and society)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.52).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.38).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "US Constitutional Authority Under Living Constitutionalism").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, '839a6440-a282-4aeb-b6bc-efc0c600a970').
narrative_ontology:cs_kernel_codification('839a6440-a282-4aeb-b6bc-efc0c600a970', fixed_text).
narrative_ontology:cs_authority_grounding('839a6440-a282-4aeb-b6bc-efc0c600a970', lineage).
narrative_ontology:cs_interpretation_layer_present('839a6440-a282-4aeb-b6bc-efc0c600a970').
narrative_ontology:cs_reading_relation('839a6440-a282-4aeb-b6bc-efc0c600a970', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('839a6440-a282-4aeb-b6bc-efc0c600a970', us_constitution_meaning__positivist_reading, influences).
narrative_ontology:cs_axiom('839a6440-a282-4aeb-b6bc-efc0c600a970', foundational, constitutional_meaning_evolves_with_society).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_society, holdable).
narrative_ontology:cs_axiom_grounding('839a6440-a282-4aeb-b6bc-efc0c600a970', constitutional_meaning_evolves_with_society, empirically_contingent).
narrative_ontology:cs_axiom('839a6440-a282-4aeb-b6bc-efc0c600a970', foundational, judicial_discretion_bounded_by_enduring_principles).
narrative_ontology:cs_axiom_status(judicial_discretion_bounded_by_enduring_principles, holdable).
narrative_ontology:cs_axiom_grounding('839a6440-a282-4aeb-b6bc-efc0c600a970', judicial_discretion_bounded_by_enduring_principles, deontological).
narrative_ontology:cs_reference_frame('839a6440-a282-4aeb-b6bc-efc0c600a970', adaptive_fidelity_framework).
narrative_ontology:cs_drift_state('839a6440-a282-4aeb-b6bc-efc0c600a970', contemporary_moral_consensus_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('839a6440-a282-4aeb-b6bc-efc0c600a970', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, judiciary_in_adaptation_role).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, counter_majoritarian_constraint).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, historical_meaning_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIGHTS CLAIMANT (ROPE) — Individual seeking to assert constitutional rights not explicitly protected at the Constitution's ratification. Living constitutionalism makes exit from the constraint impossible (trapped) but classifies the mechanism as coordination rather than pure extraction — the judicial adaptation process is genuinely constrained by enduring principles, not purely extractive. The powerless agent benefits from judicial discretion to evolve application with contemporary moral consensus.
constraint_indexing:constraint_classification(us_constitution_meaning__living_constitutionalist_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COUNTER-MAJORITARIAN CONSTRAINT (TANGLED ROPE) — Critics of judicial adaptation who fear erosion of democratic process. Constrained by the fact that constitutional amendment (the alternative) is nearly impossible, yet benefit from the constraint's coordination function — it does prevent pure majoritarian override of constitutional principles. Moderate extraction (the expansion of judicial power) combined with genuine coordination (preventing democratic tyranny). Generational time horizon reflects that counter-majoritarian capacity accumulates over multiple election cycles.
constraint_indexing:constraint_classification(us_constitution_meaning__living_constitutionalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL JUDICIARY (TANGLED ROPE) — Institutional actor constrained by enduring constitutional principles yet empowered to develop doctrine in light of contemporary social attitudes. Receives extraction benefit (enhanced power and discretion to shape constitutional meaning) while also performing genuine coordination function (preventing both rigid ossification of text and unfettered majoritarian override). Constrained exit reflects Article III limits on jurisdiction and appointment politics; judiciary cannot simply abandon the role but exercises significant adaptive discretion within it.
constraint_indexing:constraint_classification(us_constitution_meaning__living_constitutionalist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ORIGINALIST AUTHORITY STRUCTURE (PITON) — The competing institutional reading (originalism) persists despite living constitutionalism's theoretical dominance in contemporary judicial practice. Originalism retains institutional legitimacy and command of powerful constituencies, yet the core originalist function (retrieving historical public meaning) is increasingly performative — judges claiming to pursue historical meaning often anchor on contemporary originalist scholarship rather than genuine archival research. Theater ratio high: the originalist ritual persists through institutional and ideological inertia even as its epistemic claims degrade. Arbitrage exit reflects that originalist judges face no material barriers to adopting living constitutionalist reasoning — the choice to maintain originalist doctrine is strategic, not structural.
constraint_indexing:constraint_classification(us_constitution_meaning__living_constitutionalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE BRANCH (SNARE) — Congress and state legislatures face pure extraction from judicial adaptation that circumvents the amendment process. Trapped by the fact that the Constitution is the supreme law and only an amendment can override judicial interpretation. No exit: legislatures cannot escape the constraint without mobilizing the nearly-impossible amendment supermajority. High suppression of legislative alternatives. Living constitutionalism distributes lawmaking power toward judges and away from elected branches, yet provides no legislative recourse short of constitutional amendment (which is structurally suppressed by design). Powerless/trapped/biographical: the constraint operates at every legislative session, every statutory enactment potentially vulnerable to judicial reinterpretation.
constraint_indexing:constraint_classification(us_constitution_meaning__living_constitutionalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational universal perspective, constitutional meaning MUST evolve with social conditions — meaning is a linguistic phenomenon, language is inherently tied to contemporary usage, and society changes inexorably. No document can be frozen in time. This perspective sees living constitutionalism not as a policy choice but as an immutable feature of how language and law work. However, the structural data contradicts pure mountain classification: living constitutionalism benefits identified agents (rights claimants, judiciary), harms others (legislative branch, originalist authority), and requires active enforcement (judicial discretion must be exercised deliberately). The engine will detect this as a false summit naturalization — confusing an institutional arrangement (empowering judges to adapt doctrine) with a law of nature (that meaning must change).
constraint_indexing:constraint_classification(us_constitution_meaning__living_constitutionalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_meaning__living_constitutionalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_meaning__living_constitutionalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Living constitutionalism redistributes constitutional lawmaking power from elected branches (bound by specific statutory language and amendment process) to judiciary (able to develop doctrine responsive to evolving conditions). The extraction is real — federal judges gain discretion to reinterpret constitutional requirements in light of contemporary moral consensus — but not maximal because judges remain constrained by the requirement to ground reasoning in enduring principles and the written text. The trajectory (0.28 → 0.38 → 0.52) reflects increasing confidence in judicial adaptation over the interval as living constitutionalism has become ascendant in the Court (post-1960s expansion of rights). Suppression (0.38 baseline, declining from 0.58): The constraint actively suppresses originalist alternatives (judges who follow historical public meaning face reputational costs in elite legal discourse) and suppresses legislative alternatives (Congress cannot override judicial constitutional interpretation without amendment, which requires 2/3 + 3/4 supermajorities). The declining trajectory reflects that as living constitutionalism consolidated, the need for active enforcement of judicial adaptation declined — the constraint became self-reinforcing through institutional culture. Theater ratio (0.55 baseline, rising from 0.42): Moderate and rising. Judicial reasoning in living constitutionalist cases involves genuine legal analysis (constraint derivation from text and principle), but also involves substantial performative elements — judges invoking 'contemporary moral consensus' without clear epistemology, claiming continuity with enduring principles while permitting substantial doctrinal evolution, performing fidelity to the Constitution while exercising significant discretion. The rising trajectory reflects increasing sophistication in the rituals of living constitutionalism (see, e.g., the elaborate principled reasoning in Obergefell) — more theater required to maintain the appearance of constraint as actual discretion expands.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates large perspectival variation from the same structural data. Rights claimants perceive rope because judicial adaptation empowers them to claim protections not available through the legislative process. The judiciary perceives tangled rope because the constraint both empowers (discretion to adapt) and binds them (requirement to maintain fidelity to enduring principles and text). The counter-majoritarian critic perceives tangled rope because democratic process is constrained (extraction) while constitutional limits on majority power are maintained (coordination function). The legislative branch perceives snare because they have no exit from judicial reinterpretation and no ability to override constitutional interpretation. The originalist institutional structure perceives piton because the competing interpretive ritual (originalism) persists through institutional loyalty and ideological commitment despite living constitutionalism's doctrinal dominance. The analytical observer risks mountain perception (treating constitutional evolution as a law of nature rather than institutional choice), which the engine detects as false summit. The gap between snare (legislature), rope (rights claimants), and mountain (analyst) reveals the constraint's true structure: a contingent institutional arrangement benefiting some agents at the expense of others, not an immutable feature of how language or law must work.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position — the agent's power level, exit options, and relationship to the extraction flow. Rights claimants are powerless and trapped (high d, maximum extraction from their perspective, but beneficiary status reduces experienced chi toward rope). Federal judiciary is institutional with constrained exit (moderate d, beneficiary status makes d low, experienced as coordination). Counter-majoritarian advocates are moderate with constrained exit (moderate-high d, split between beneficiary and victim reduces d to moderate range). Legislative branch is powerless and trapped (maximum d, victim status, maximum experienced extraction). Originalist structure is institutional with arbitrage (low d, competing institutional actor with choices, beneficiary-facing relative to its own institutional interests, but victim-facing relative to the broader constitutional system). Analytical observer is analytical with analytical exit (d ≈ 0.72, canonical value for observer position). The perspectival gaps are substantial: beneficiaries perceive rope (coordination), institutional victims perceive snare (extraction), moderate conflicted agents perceive tangled rope (mixed), institutional competitors perceive piton (degraded competing ritual). The analytical view risks mountain (naturalizing the institutional arrangement), triggering false summit detection.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CLASSIFICATION RESOLVES MANDATROPHY: Living constitutionalism is neither pure coordination (rope) nor pure extraction (snare) because it performs BOTH functions simultaneously and they cannot be decomposed. Genuine coordination function: the constraint prevents two pathologies — rigid ossification of the Constitution (which would abandon it as guidance) and unfettered majoritarian override of constitutional limits (which would eliminate counter-majoritarian protection). Genuine extraction function: judicial adaptation redistributes lawmaking power from elected branches and suppresses originalist competitors, benefiting rights claimants and judges at the expense of democratic process and constitutional stability. The tangled rope classification holds these in tension rather than resolving them. The mandatrophy emerges in the kernel reading context: originalism claims fidelity to fixed meaning (rejects extraction), living constitutionalism claims adaptive application of enduring principles (permits extraction justified by coordination function), positivism claims validity derives from procedure alone (orthogonal to both). These readings coexist because no single authoritative framework (the Constitution itself) determines which reading is correct — the Constitution is the contested kernel, and the readings generate different extractiveness profiles depending on which institutional interpretation prevails. Mandatrophy is not resolved by choosing a type; it is resolved by recognizing that living constitutionalism is indeed tangled rope: coordination + extraction + active enforcement + beneficiaries + victims, all structurally inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enduring_principle_determinacy,
    'What makes a constitutional principle ''enduring'' rather than merely superseded? Is there a principled boundary, or does the distinction depend on contemporary preference?',
    'Historical analysis of which principles courts have actually treated as enduring; examination of judicial reasoning for maintaining vs. reinterpreting foundational principles across different eras; conceptual analysis of what counts as ''the same principle'' under semantic change.',
    'If determinacy is principled: living constitutionalism provides meaningful constraint on judicial adaptation (rope-ward classification). If determinacy is preference-dependent: the constraint collapses toward snare (pure judicial discretion masked by appeals to enduring principles).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enduring_principle_determinacy, conceptual, 'Determinacy of ''enduring principles'' under living constitutionalism').

omega_variable(
    contemporary_moral_consensus_epistemology,
    'How do judges reliably identify ''contemporary moral consensus'' without simply projecting elite legal culture onto the broader public? What prevents moral consensus from being a cover story for judicial preference?',
    'Empirical analysis of whether judicial identifications of moral consensus track actual public opinion or elite legal academic opinion; examination of disconnect cases where courts claimed consensus but polling showed plurality disagreement; normative analysis of what sources (polls, legislation, democratic deliberation) would constitute legitimate consensus detection.',
    'If consensus reliably tracked public opinion: living constitutionalism provides counter-majoritarian constraint without foreclosing democratic input (genuine tangled_rope). If consensus is elite construct: judicial adaptation is largely unconstrained (snare-ward). If impossible to determine: the constraint requires permanent interpretive labor (scaffolding rather than stabilized rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_moral_consensus_epistemology, empirical, 'Epistemology of judicial identification of contemporary moral consensus').

omega_variable(
    amendment_suppression_mechanism,
    'Is the extreme difficulty of constitutional amendment (2/3 House + 2/3 Senate + 3/4 states) a feature that living constitutionalism ameliorates, or does living constitutionalism depend on that suppression to justify judicial adaptation?',
    'Counterfactual analysis: if amendment were easier (simple majority or supermajority of both chambers), would judges still adopt living constitutionalism? Historical analysis of whether living constitutionalism expanded as amendment became effectively impossible after Reconstruction.',
    'If amendment suppression drives living constitutionalism: the constraint is a second-order response to democratic failure, and judicial extraction is justified by democratic deficiency (deep tangled_rope with high suppression). If living constitutionalism would persist even with easier amendment: it rests on genuine substantive claims about meaning and principle rather than procedural necessity (tangled_rope with lower suppression, or possible snare if adaptation becomes unconstrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_suppression_mechanism, conceptual, 'Whether amendment suppression justifies or drives living constitutionalism').

omega_variable(
    foundational_axiom_stability,
    'What happens when contemporary moral consensus contradicts foundational constitutional principles? Can living constitutionalism accommodate both constraints, or does consensus override principle?',
    'Case analysis: examination of instances where moral consensus (e.g., majoritarian preferences for exclusion of groups) conflicted with foundational principles (equal protection, due process); how courts resolved the conflict; whether principle or consensus prevailed.',
    'If principle overrides consensus: living constitutionalism maintains real constraint and counter-majoritarian function (tangled_rope confirmed). If consensus overrides principle: the constraint collapses toward judicial discretion (snare-ward). If courts affirm both without resolution: the constraint is performative (piton-ward or scaffold-ward).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_axiom_stability, empirical, 'Priority between enduring principles and contemporary moral consensus').

omega_variable(
    sibling_reading_kernel_identity,
    'Is this reading of the US Constitution-as-kernel functionally distinct from originalism and legal positivism, or does it share foundational commitments that prevent true foreclosure?',
    'Doctrinal analysis of whether living constitutionalism and originalism can coexist within the same judicial framework (Scalia''s public meaning originalism vs. living constitutionalism in Obergefell and Lawrence); examination of how positivism handles constitutional evolution; assessment of whether the readings truly partition the space or merely emphasize different aspects of a single authority structure.',
    'If truly distinct: reading relations are coexists_with (different judges hold different readings) or influences (living constitutionalism shapes originalist judges'' doctrine). If overlapping: kernel may be insufficiently precise to warrant separate readings, or the constraint family should be reorganized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_identity, conceptual, 'Structural distinctness of living constitutionalism from sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uscon_living_tr_t0, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(uscon_living_tr_t5, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(uscon_living_tr_t10, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(uscon_living_be_t0, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(uscon_living_be_t5, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(uscon_living_be_t10, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(uscon_living_su_t0, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(uscon_living_su_t5, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(uscon_living_su_t10, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% The US Constitution as kernel generates three separate constraint stories corresponding to three major reading traditions. Each story represents how constitutional authority operates under that reading's framework. Living constitutionalism (THIS STORY) distributes power toward adaptive judges and rights claimants; originalism shifts power toward historical meaning and legislative deference; positivism focuses on institutional procedure. The three readings coexist in contemporary constitutional practice because the kernel (the Constitution itself) does not determine which reading is correct. Decomposition is required per ε-invariance: measuring extractiveness under living constitutionalism vs. originalism yields different values (0.52 vs. estimated 0.25) because the institutional structure the measurement describes is fundamentally different. All three constraints affect each other via reading_relations in cs_structure (coexist_with, influences) and via network.affects_constraints (bidirectional: each reading's adoption constrains the interpretive space available to competitors).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__living_constitutionalist_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
