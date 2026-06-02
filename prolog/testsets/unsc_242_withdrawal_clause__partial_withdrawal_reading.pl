% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC 242 Withdrawal Clause — Partial Withdrawal Reading
 *   domain: international_law/treaty_interpretation/diplomatic_settlement
 *
 * SUMMARY:
 *   UN Security Council Resolution 242 (November 1967) codified the
 *   termination of the 1967 Six-Day War through a framework permitting
 *   'withdrawal of Israeli armed forces from territories occupied in the
 *   conflict.' The indefinite English article 'from territories' — not 'from
 *   the territories' — created structural ambiguity: the drafters neither
 *   specified which territories nor fixed a temporal endpoint for withdrawal.
 *   This constraint models ONE reading of that contested kernel: the partial
 *   withdrawal interpretation, wherein the indefiniteness functions as an
 *   enabling device for the occupying power and mediating authorities to
 *   retain strategic territories while remaining nominally compliant with the
 *   withdrawal commitment. The constraint instantiates what the framework
 *   calls the Ledger: a written commitment that converts its own
 *   indefiniteness into persistent negotiating leverage. Under the partial
 *   withdrawal reading, the occupying power experiences the constraint as
 *   pure coordination (maintaining negotiating position through phased
 *   agreements and buffer-zone justification). The claimant state experiences
 *   it as pure extraction (trapped in indefinite negotiation with no fixed
 *   boundary). Mediating powers experience it as coordination (ongoing
 *   mediation creates institutional value). The constraint exhibits tangled
 *   rope characteristics: genuine coordination functions exist (periodic
 *   agreements, humanitarian mechanisms, confidence-building measures)
 *   alongside asymmetric extraction (claimant state's indefinite
 *   displacement, occupying power's retained strategic territories). The
 *   theater ratio (rising from 0.35 to 0.68 across 40 years) reflects the
 *   accumulation of compliance-assessment rituals — UN reports, International
 *   Court of Justice advisory opinions, Human Rights Council inquiries — that
 *   address the withdrawal question while producing no structural change.
 *   These rituals maintain the appearance of the withdrawal regime while the
 *   underlying territorial configuration remains substantially fixed. The
 *   suppression requirement has increased over time (0.35→0.48) as the
 *   international legal community developed interpretive frameworks to manage
 *   the indefiniteness, requiring more active enforcement effort to sustain
 *   the partial withdrawal reading against growing explicit argument for
 *   maximal withdrawal.
 *
 * KEY AGENTS:
 *   - Occupying Power (Institutional/Arbitrage): Primary beneficiary — captures strategic territorial retention, phased negotiation advantage, and indefinite deferral of withdrawal commitment. Experiences constraint as pure coordination.
 *   - Claimant State (Powerless/Trapped): Primary victim — bound by treaty but cannot enforce fixed boundary; trapped in indefinite negotiation with no exit. Experiences constraint as pure extraction.
 *   - Mediating Powers (Institutional/Arbitrage): Secondary beneficiary — benefit from ongoing mediation infrastructure, institutional value creation through periodic agreements, diplomatic leverage maintenance. Experience constraint as coordination.
 *   - Refugee Populations (Moderate/Constrained): Secondary victims — displaced indefinitely, cannot return to named territories, constrained by statelessness. Experience constraint as tangled rope (some humanitarian benefit, persistent extraction).
 *   - International Legal Community (Organized/Constrained): Trapped in interpretive dilemma — standard treaty interpretation canons cannot resolve indefiniteness; must generate ad hoc frameworks. Experience constraint as tangled rope (some institutional coordination, persistent extraction through impossible interpretive task).
 *   - Analytical Observer (Analytical/Analytical): Sees the constraint as instantiation of the Ledger — indefiniteness converted to leverage. Identifies the civilizational coordination function (maintaining negotiating framework) alongside the regional extraction mechanism (territorial retention).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.52).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.48).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC 242 Withdrawal Clause — Partial Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/treaty_interpretation/diplomatic_settlement").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, '49fe6537-6100-4ac2-babe-7ea30aedf6c1').
narrative_ontology:cs_kernel_codification('49fe6537-6100-4ac2-babe-7ea30aedf6c1', formalized).
narrative_ontology:cs_authority_grounding('49fe6537-6100-4ac2-babe-7ea30aedf6c1', extraction).
narrative_ontology:cs_interpretation_layer_present('49fe6537-6100-4ac2-babe-7ea30aedf6c1').
narrative_ontology:cs_reading_relation('49fe6537-6100-4ac2-babe-7ea30aedf6c1', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('49fe6537-6100-4ac2-babe-7ea30aedf6c1', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('49fe6537-6100-4ac2-babe-7ea30aedf6c1', foundational, indefiniteness_preserves_phased_flexibility).
narrative_ontology:cs_axiom_status(indefiniteness_preserves_phased_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('49fe6537-6100-4ac2-babe-7ea30aedf6c1', indefiniteness_preserves_phased_flexibility, instrumental).
narrative_ontology:cs_axiom('49fe6537-6100-4ac2-babe-7ea30aedf6c1', foundational, secure_boundaries_permits_strategic_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_permits_strategic_retention, holdable).
narrative_ontology:cs_axiom_grounding('49fe6537-6100-4ac2-babe-7ea30aedf6c1', secure_boundaries_permits_strategic_retention, empirically_contingent).
narrative_ontology:cs_reference_frame('49fe6537-6100-4ac2-babe-7ea30aedf6c1', phased_compliance_via_negotiated_scope_definition).
narrative_ontology:cs_drift_state('49fe6537-6100-4ac2-babe-7ea30aedf6c1', contemporary_post_2000_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('49fe6537-6100-4ac2-babe-7ea30aedf6c1', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_powers).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, claimant_states_without_fixed_lines).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, refugee_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLAIMANT STATE WITHOUT FIXED WITHDRAWAL LINE (SNARE) — The indefinite 'withdrawal of Israeli armed forces from territories occupied in the 1967 conflict' creates structural entrapment. No fixed boundary demarcates what 'from territories' means; the occupying power retains unilateral discretion over which territories constitute 'occupied,' which are 'disputed,' which are 'security zones.' The claimant state cannot exit this arrangement — it is bound by the treaty but cannot enforce a definition. Maximum experienced extraction: forced indefinite negotiation, phased withdrawal that never reaches completion, strategic territories retained under reinterpretation of the text.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__partial_withdrawal_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REFUGEE POPULATIONS DISPLACED BY OCCUPATION (TANGLED ROPE) — Constrained by the indefinite withdrawal timeline and de facto territorial retention. Also benefit from phased negotiations that occasionally yield incremental territorial concessions and humanitarian agreements. The constraint coordinates some resource flows (aid, humanitarian access) alongside asymmetric extraction (indefinite displacement, inability to return to named territories). Exit is constrained by statelessness and economic dependence; some agency exists through diaspora organization and international appeals.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: OCCUPYING POWER (ROPE) — The indefiniteness is a coordination mechanism from this perspective: it permits phased withdrawal negotiation without surrendering strategic territories. The occupying power experiences the constraint as pure coordination — maintaining negotiating position while managing international pressure. The constraint enables 'security zone' retention, buffer-territory classification, and de facto annexation of strategic areas while remaining technically compliant with the withdrawal clause. Arbitrage exit (can walk away or reinterpret the text with diplomatic cover) makes this perspective experience low extraction.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__partial_withdrawal_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEDIATING POWERS / INTERNATIONAL COMMUNITY (ROPE) — The indefinite language creates coordination opportunities: mediation processes, phased agreements, and confidence-building measures can all be framed as 'implementing' the withdrawal clause without triggering binary compliance/violation metrics. Mediating powers benefit from the ambiguity — it permits ongoing negotiation and leverage maintenance. Arbitrage exit (can shift interpretations, pause negotiations, broker deals) means experienced extraction is low or negative. The constraint coordinates between the occupying power's territorial interests and the claimant state's representation.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__partial_withdrawal_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL AUTHORITIES (TANGLED ROPE) — Legal scholars, treaty bodies, and human rights organizations experience the constraint as both coordination and extraction. The constraint coordinates the international legal system's mechanisms for treaty interpretation; it also extracts from these bodies by creating irresolvable interpretive dilemmas (indefiniteness cannot be resolved by standard canons of construction). The legal community has some agency (issuing advisory opinions, scholarly reinterpretation) but is constrained by the structural indefiniteness. Moderate extraction.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — LEDGER INTERPRETATION (TANGLED ROPE) — From a civilizational perspective, the partial withdrawal reading instantiates the Ledger: a tool that converts indefiniteness into persistent negotiating leverage. The occupying power's ability to retain 'strategic territories' while remaining nominally compliant with a withdrawal clause reveals that the constraint coordinates institutional survival (ongoing negotiation, mediation infrastructure, periodic agreements) alongside asymmetric territorial extraction. The indefiniteness is functional — it preserves both the legitimacy of the withdrawal commitment and the practical benefit of partial retention. This is tangled rope: genuine coordination function (managing diplomatic relations, enabling periodic agreements) paired with asymmetric extraction (claimant state trapped in indefinite negotiation). Theater ratio (0.64) reflects that compliance assessment rituals (UN reports, International Court of Justice opinions) persist despite the structural impossibility of establishing binary compliance. These rituals maintain the appearance of the withdrawal regime while permitting de facto retention.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__partial_withdrawal_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__partial_withdrawal_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value for the occupying power and mediators but is not maximal because genuine coordination mechanisms exist (periodic agreements, humanitarian transfers, confidence-building measures actually occur). The extraction is conditional on continued negotiation; if negotiation ceased, the extraction mechanism would collapse. The value reflects that the occupying power cannot simply unilaterally retain all territories without negotiation — the commitment to withdrawal is binding in form, which constrains the occupying power's options. Suppression (0.48): Moderate. The claimant state faces significant barriers to enforcing withdrawal (no military capacity to reclaim territories, diplomatic isolation, economic dependence, refugee populations create humanitarian hostages to negotiation), but suppression is not total — some international legal mechanisms exist (ICJ advisory opinions, Human Rights Council mechanisms, third-party diplomatic pressure). The indefiniteness itself suppresses the claimant state's ability to claim clear violation — every territorial retention can be reframed as temporary, conditional, or justified under 'secure boundaries.' Theater ratio (0.64): High and rising. The compliance assessment rituals (UN reports, ICJ opinions, periodic reviews) address the withdrawal question but produce no structural change. These rituals perform legitimacy for the withdrawal commitment while the underlying configuration remains fixed. The rising trajectory reflects increasing rhetorical effort required to maintain the partial withdrawal reading against mounting explicit legal argument for maximal withdrawal. The performance intensifies as the actual coordination function (periodic agreements, humanitarian transfers) provides less and less value — the theater must substitute.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint. The occupying power sees pure coordination (Rope) — the indefiniteness permits necessary phased negotiation without surrendering strategic position. The claimant state sees pure extraction (Snare) — the indefiniteness is a trap, a legal mechanism to lock them into permanent displacement. The mediating powers see coordination (Rope) — the indefiniteness enables their institutional role and periodic achievements. The analytical observer sees tangled rope — both coordination and extraction are structurally real, and the constraint persists because the occupying power's coordination interest (maintaining negotiating position) and the claimant state's extraction (indefinite displacement) feed each other. The gap between Rope and Snare is not a perspective error — it is the structural signature of the Ledger. The indefiniteness genuinely permits phased coordination AND enables indefinite extraction. The two are not contradictory; they are functionally coupled. The occupying power can claim it is coordinating (true) while extracting (also true). The claimant state can claim it is being extracted from (true) while occasional agreements demonstrate coordination rhetoric (also true).
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying power's directionality (d ≈ 0.15) derives from beneficiary status (retains strategic territories) plus arbitrage exit (can negotiate new terms, can walk away, can reinterpret the text). The claimant state's directionality (d ≈ 0.95) derives from victim status (displaced indefinitely, cannot enforce boundary) plus trapped exit (no military recourse, diplomatically isolated, refugee populations create dependence). The derivation chain produces the asymmetric chi values: beneficiary with arbitrage experiences low/negative chi; victim with trapped exit experiences high chi. Mediating powers' directionality (d ≈ 0.20) derives from beneficiary status (institutional value from ongoing mediation) plus arbitrage exit. The moderate power (refugees, organized legal community) derives moderate d values from mixed cost-benefit positions and constrained exit options. The analytical observer's directionality (d ≈ 0.72) reflects the observer's position outside the dispute but theoretically neutral — captures the structural asymmetry without partisan interest.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by identifying how the same legal text produces different classifications from different positions. The mandatrophy is not 'which type is the constraint really?' but 'which functional role does the constraint play from each agent's structural position?' The partial withdrawal reading does NOT collapse the mandatrophy — it clarifies it. The reading specifies that the indefiniteness is functionally ambiguous by design: it permits the occupying power to experience coordination while the claimant state experiences extraction. The Ledger is precisely a tool that converts indefiniteness into this kind of perspectival divergence. The constraint is coherent across all six types because each perspective captures a real structural feature: the occupying power's genuine coordination interest, the claimant state's genuine extraction, the mediators' institutional value, the refugees' mixed position, the legal community's interpretive trap, and the analyst's civilizational view of how indefiniteness creates persistent leverage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indefiniteness_intentionality,
    'Was the indefinite English article (''from territories'') deliberately encoded by the drafters to preserve negotiating flexibility, or does it reflect genuine drafting ambiguity?',
    'Historical analysis of draft negotiations, drafting records, preparatory work (travaux préparatoires), contemporaneous statements by permanent members. Comparison with parallel texts in other UN resolutions to identify whether similar constructions were deliberate or accidental.',
    'If intentional: the partial withdrawal reading is confirmed as the drafters'' design, instantiating the Ledger. If accidental: the constraint might be reframed as a genuine natural law of treaty drafting (Mountains cannot avoid ambiguity). If deliberately ambiguous by some drafters but not others: the constraint instantiates cognitive capture (some parties locked into their reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefiniteness_intentionality, empirical, 'Intentionality of indefinite English article in UNSC 242').

omega_variable(
    security_zone_boundary_specification,
    'Can the phrase ''secure and recognized boundaries'' be operationalized as a fixed spatial line, or does ''secure boundaries'' necessarily permit retention of strategic buffer zones under security-based reinterpretation?',
    'Comparative analysis of security terminology in other treaties and international agreements. Evaluation of whether any occupying power has voluntarily accepted a fixed boundary while claiming it satisfies ''secure boundaries'' principle. Historical precedent analysis: have any territorial disputes been resolved by fixing a boundary and then requiring the occupying power to withdraw to it?',
    'If boundaries can be fixed: the partial withdrawal reading becomes unjustified and the constraint shifts toward maximal withdrawal. If security logic necessarily permits buffer zones: the constraint is locked into the partial withdrawal reading via the ''secure boundaries'' clause itself, and both readings coexist because they interpret different clauses. The coupling between ''withdrawal'' (indefinite) and ''secure boundaries'' (permits retention) becomes the structural lock.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_zone_boundary_specification, empirical, 'Whether ''secure boundaries'' principle permits indefinite retention of strategic territories').

omega_variable(
    renegotiation_cost_dynamics,
    'Does the indefinite withdrawal regime reduce the total cost of renegotiation by permitting phased agreements without triggering binary compliance judgments, or does it increase total cost by prolonging the negotiation indefinitely?',
    'Time-series analysis of negotiation rounds, agreement periods, and territorial transfer rates. Counterfactual: if the withdrawal clause had been maximally specific (e.g., ''to the 1967 lines within 24 months''), would the total number of agreements, humanitarian transfers, and confidence-building measures have been higher or lower? Cost accounting: sum of resources devoted to ongoing mediation, legal interpretation, and compliance monitoring under the indefinite regime vs. projected costs under a fixed regime.',
    'If indefiniteness reduces renegotiation cost: the tangled rope classification is confirmed — genuine coordination function exists. If indefiniteness increases total cost through prolonged negotiation: the constraint is pure extraction (Snare) masked by coordination rhetoric. The answer determines whether the mediating power and occupying power are genuinely benefiting from coordination or merely extracting through indefinite leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renegotiation_cost_dynamics, empirical, 'Net renegotiation cost dynamics of indefinite withdrawal regime').

omega_variable(
    maximal_withdrawal_foreclosure,
    'Does the partial withdrawal reading structurally foreclose the maximal withdrawal reading, or can both coexist within the same treaty framework as competing interpretations?',
    'Logical and textual analysis: if a drafting intent deliberately encoded indefiniteness, does that intent rule out good-faith maximal withdrawal interpretation? Or does indefiniteness itself permit multiple sincere readings? Test: can a subsequent government of the occupying power adopt the maximal withdrawal reading and claim it is equally valid interpretation of the same text?',
    'If foreclosed: the partial withdrawal reading is the binding committer reading — it is what the treaty IS. If not foreclosed: the partial and maximal readings coexist and the constraint is genuinely contested (the committer frame tracks a live dispute, not a settled interpretation). This determines whether omega foreclosure_reasoning_type is ''logical'' (one reading rules the other out) or ''institutional'' (one reading has captured the interpretation authority).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maximal_withdrawal_foreclosure, conceptual, 'Whether partial withdrawal reading forecloses maximal withdrawal reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2007).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc242pw_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(unsc242pw_tr_t1987, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1987, 0.52).
narrative_ontology:measurement(unsc242pw_tr_t2007, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2007, 0.68).

% Extraction over time
narrative_ontology:measurement(unsc242pw_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.38).
narrative_ontology:measurement(unsc242pw_be_t1987, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1987, 0.48).
narrative_ontology:measurement(unsc242pw_be_t2007, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2007, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(unsc242pw_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.35).
narrative_ontology:measurement(unsc242pw_su_t1987, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1987, 0.42).
narrative_ontology:measurement(unsc242pw_su_t2007, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2007, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, post_1973_sinai_disengagement_territorial_phasing).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, oslo_accords_indefinite_final_status).

% DUAL FORMULATION NOTE:
% The UNSC 242 constraint family decomposes into three structurally distinct readings: (1) partial withdrawal (this story) — indefiniteness as negotiating leverage, ε=0.52, tangled rope; (2) maximal withdrawal — indefiniteness as drafting error, ε=0.38, scaffold (temporary occupation pending withdrawal); (3) interpretive authority — focus on who decides interpretation, ε=0.45, tangled rope. Each reading has different beneficiaries, different victim structures, and different functional mechanisms. They are NOT the same constraint viewed from different angles; they instantiate different legal and political logics. The partial reading influences downstream constraints (Sinai phasing, Oslo indefinite final status) by establishing the precedent that indefiniteness permits strategic retention under coordination rhetoric.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, institutional, 0.18).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
