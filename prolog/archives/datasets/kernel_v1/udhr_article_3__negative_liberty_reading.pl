% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: Article 3 Negative Liberty Reading: State Deprivation Prohibition via Procedural Justice
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   Article 3 of the Universal Declaration of Human Rights states: 'Everyone
 *   has the right to life, liberty and security of person.' The negative
 *   liberty reading interprets this as a prohibition on state deprivation of
 *   life and liberty except through narrow procedural justice — due process,
 *   fair trial, evidence standards, proportionality. This reading positions
 *   'security' not as a state provision but as freedom FROM state violence.
 *   The constraint exhibits tangled rope structure: genuine coordination
 *   function (due process standards enable cross-jurisdictional human rights
 *   adjudication) combined with asymmetric extraction (the state retains
 *   control over the procedure while appearing bound by it). The reading's
 *   theater-ratio trajectory reflects the growing distance between procedural
 *   form and substantive outcome: early interpretations treated procedure as
 *   substantive; contemporary application treats procedure as form that
 *   security apparatus can control. The extractiveness trajectory reflects
 *   cumulative capacity of state security systems to absorb and
 *   instrumentalize due process norms. From the condemned individual's
 *   perspective, the constraint is a snare: procedural legitimation of state
 *   violence. From the international institution's perspective, it is rope: a
 *   stable coordination mechanism. From the abolitionist perspective, it is
 *   scaffold: a temporary stage toward categorical prohibition. From the
 *   analytical observer, it risks appearing as mountain: a natural law about
 *   inviolable boundaries. Yet the structural data reveals this as false
 *   summit — the constraint is a contingent institutional reading, not a
 *   natural law.
 *
 * KEY AGENTS:
 *   - Condemned Individual: Primary victim (powerless/trapped) — trapped in the procedural gate; maximum suppression; experienced extraction is snare
 *   - Detained Population: Structural victim (powerless/trapped) — Article 3 grants no affirmative rights to humane conditions, only prohibition of arbitrary deprivation; high suppression through silence
 *   - State Security Apparatus: Primary beneficiary (powerful/constrained) — retains effective control over procedure; constrained only by obligation to maintain procedural form; experienced extraction is tangled rope (benefits from procedure theater, constrained by procedure formality)
 *   - Individual Liberty Holders: Beneficiary class (moderate/constrained) — benefit from due process standards but constrained by state's interpretive authority over adequacy of procedure
 *   - International Legal Institutions: Institutional beneficiary (institutional/arbitrage) — due process standards enable harmonized treaty interpretation and dispute resolution; low extraction
 *   - Human Rights Advocacy Organizations: Organized victim (organized/constrained) — benefit from having stable legal standard; constrained by the reading's narrow frame (no affirmative rights claims allowed)
 *   - Abolitionist Coalition: Organized agent (organized/mobile) — views negative liberty reading as scaffold toward categorical prohibition; mobile exit path via norm evolution
 *   - Analytical Observer: External perspective (analytical/analytical) — risks naturalizing reading choice as universal natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.58).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.62).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "Article 3 Negative Liberty Reading: State Deprivation Prohibition via Procedural Justice").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, '1407e344-8af9-4f40-93c3-47076ac35e87').
narrative_ontology:cs_kernel_codification('1407e344-8af9-4f40-93c3-47076ac35e87', fixed_text).
narrative_ontology:cs_authority_grounding('1407e344-8af9-4f40-93c3-47076ac35e87', lineage).
narrative_ontology:cs_interpretation_layer_present('1407e344-8af9-4f40-93c3-47076ac35e87').
narrative_ontology:cs_reading_relation('1407e344-8af9-4f40-93c3-47076ac35e87', udhr_article_3__positive_entitlement_reading, forecloses).
narrative_ontology:cs_reading_relation('1407e344-8af9-4f40-93c3-47076ac35e87', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1407e344-8af9-4f40-93c3-47076ac35e87', foundational, security_equals_freedom_from_state_violence).
narrative_ontology:cs_axiom_status(security_equals_freedom_from_state_violence, holdable).
narrative_ontology:cs_axiom_grounding('1407e344-8af9-4f40-93c3-47076ac35e87', security_equals_freedom_from_state_violence, deontological).
narrative_ontology:cs_axiom('1407e344-8af9-4f40-93c3-47076ac35e87', foundational, no_affirmative_state_duty_to_provide_life_conditions).
narrative_ontology:cs_axiom_status(no_affirmative_state_duty_to_provide_life_conditions, holdable).
narrative_ontology:cs_axiom_grounding('1407e344-8af9-4f40-93c3-47076ac35e87', no_affirmative_state_duty_to_provide_life_conditions, deontological).
narrative_ontology:cs_reference_frame('1407e344-8af9-4f40-93c3-47076ac35e87', negative_liberty_primacy).
narrative_ontology:cs_drift_state('1407e344-8af9-4f40-93c3-47076ac35e87', contemporary_security_apparatus_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1407e344-8af9-4f40-93c3-47076ac35e87', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individual_liberty_holders).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, due_process_legal_tradition).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, collective_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_emergency_powers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED INDIVIDUAL (SNARE) — Faces state deprivation of life under this reading only via narrow procedural gates (trial, evidence, due process). Yet the procedural form itself becomes the extraction mechanism: the state retains absolute authority over the determination of whether procedures were 'adequate.' No exit from the constraint; maximum experienced suppression. Theater is high — the procedure performs legitimacy while the outcome is predetermined by state power.
constraint_indexing:constraint_classification(udhr_article_3__negative_liberty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DETAINED POPULATION (SNARE) — Article 3's negative liberty framing grants no affirmative state duty to prevent torture, provide humane conditions, or secure mental/physical health. Protection is only against arbitrary deprivation, not against degradation meeting the harm threshold. Trapped; experiences high suppression through the constraint's silence on conditions, nutrition, isolation.
constraint_indexing:constraint_classification(udhr_article_3__negative_liberty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HUMAN RIGHTS ADVOCACY ORGANIZATIONS (TANGLED ROPE) — The negative liberty reading provides a coordination mechanism: due process standards create a shared reference point for challenging state action across jurisdictions. Advocates benefit from the procedural framework's universal applicability. But the same reading suppresses affirmative rights claims — advocates must fight within the narrow procedural gate, cannot demand affirmative social/economic supports. Constrained by the framing's own limits; benefit from having a stable legal standard.
constraint_indexing:constraint_classification(udhr_article_3__negative_liberty_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL INSTITUTIONS (ROPE) — The negative liberty reading is coordinating for their purposes: establishes a universal non-negotiable floor (no arbitrary deprivation) that all signatories must accept, enabling harmonized treaty interpretation and dispute resolution. Institutional beneficiary; experiences the constraint as a coordination mechanism enabling their authority.
constraint_indexing:constraint_classification(udhr_article_3__negative_liberty_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE SECURITY APPARATUS (TANGLED ROPE) — Constrained by mandatory due process but retains effective control over the process (evidence rules, classified information, interrogation protocols). The negative liberty reading benefits security by avoiding affirmative duties (no right to food, shelter, medical care during detention); harms security by requiring procedural legitimation. Coordination function: procedure provides legitimacy theater; extraction function: the state controls the procedure while appearing bound by it.
constraint_indexing:constraint_classification(udhr_article_3__negative_liberty_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ABOLITIONIST COALITION (SCAFFOLD) — Views this reading as transitional: the negative liberty framing (no arbitrary deprivation) is stage one toward a permanent sunset (abolition of capital punishment entirely). The procedural gates are scaffolding for moving societies toward elimination. Mobile; sees an exit path via norm evolution from procedural restriction to categorical prohibition.
constraint_indexing:constraint_classification(udhr_article_3__negative_liberty_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL RIGHTS VIEW (MOUNTAIN) — From a pre-institutional philosophical perspective, the negative liberty claim (security = freedom from state violence) appears as a natural law: it is logically prior to government itself, describing an inviolable boundary that any legitimate state must respect. This perspective risks naturalizing what is actually a contested reading choice — a human rights commitment, not a discovered fact.
constraint_indexing:constraint_classification(udhr_article_3__negative_liberty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(udhr_article_3__negative_liberty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(udhr_article_3__negative_liberty_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The negative liberty reading concentrates power over the deprivation decision in the state while requiring only procedural legitimation. Base extractiveness is elevated by the state's de facto control over procedure (evidence rules, classified information exceptions, interrogation doctrine). However, the constraint is not maximal snare because genuine international oversight exists (ICC, regional courts), and genuine procedural requirements do constrain state action in some contexts. The extractiveness trajectory reflects accumulation: early UDHR adoption (1948) faced loose enforcement; contemporary application sees tighter procedural requirements but also greater state capacity to absorb and instrumentalize those requirements. Suppression (0.62): Moderate-high. Significant suppression exists through: (1) state control over evidence and classified information; (2) state control over determination of procedure adequacy; (3) practical barriers to challenging procedure (cost, access to legal counsel, political retaliation); (4) unequal information (state knows interrogation conditions, detainees do not); (5) the constraint's silence on affirmative rights to humane conditions. Theater ratio (0.68): High and rising. Early procedural due process was somewhat substantive (independent judges, evidence presentation); contemporary application increasingly treats procedure as form (abbreviated trials, evidence exclusion for security, rapid executions). The performance of legitimacy increases as procedural substance decreases. The negative liberty reading's framing (freedom FROM state violence, not affirmative state duty) enables this drift: the state can progressively pare down procedure without violating the reading's core axiom (don't deprive arbitrarily) because the procedure itself is the sufficient legitimation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from identical base properties. The condemned individual sees snare (procedure is theater for predetermined outcome). The international institution sees rope (procedure is coordination mechanism). The security apparatus sees tangled rope (constrained by procedure requirement, benefiting from procedural control). The abolitionist sees scaffold (temporary stage). The analytical observer risks seeing mountain (natural law about inviolable individual boundaries). The perspectival gap reflects that the negative liberty reading is itself a choice point: it privileges individual liberty over collective security, and that priority is not logically necessitated by human dignity — it is a contested interpretation. The gap reveals that 'natural rights' framing naturalizes a contingent reading choice.
 *
 * DIRECTIONALITY LOGIC:
 *   State security apparatus derives d ≈ 0.40 (partial beneficiary with constrained exit): retains effective control over procedure, benefits from procedural theater, but obligated to maintain procedural form. Individual victim derives d ≈ 0.92 (full target with trapped exit): no meaningful participation in procedure design or application; no exit option. International institution derives d ≈ 0.05 (full beneficiary with arbitrage): benefits from coordination standardization; can exit via withdrawing from regime. Human rights organizations derive d ≈ 0.65 (mixed: benefit from legal standard but constrained by reading's narrow frame): moderate d reflecting partial agency and partial constraint. The security apparatus's constrained exit (not arbitrage) reflects that while it could theoretically exit (refuse to sign treaty), the exit cost is enormous (international isolation, sanctions, delegitimation). Thus d is higher than pure beneficiary would be.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through reading pluralism. The negative liberty reading is not 'the correct interpretation' but one pole of a genuine contest with positive entitlement and procedural hybrid readings. The classification appears to move from rope (international institution coordination view) to snare (condemned individual view) to tangled rope (security apparatus actual practice) — but the movement is perspectival, not temporal. All perspectives are simultaneously valid because they are observations from genuinely different structural positions. The constraint is not degrading; rather, it is exhibiting the internal tension of a reading that privileges individual negative liberty while leaving state security apparatus in control of the procedure defining that liberty. Mandatrophy is resolved by accepting that tangled rope IS the correct classification: genuine coordination (due process standards) combined with asymmetric extraction (state controls the process).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedure_as_form_vs_substance,
    'Does ''deprivation except via narrow procedural justice'' mean the procedure itself must be fair (due process) or merely that a procedure must exist (legal form)?',
    'Comparative constitutional analysis: jurisdictions that narrowly read ''procedure'' (form only) vs those that read ''procedure'' as substantive fairness; outcome divergence in capital cases, detention conditions',
    'If form only: effective extraction increases (state controls the procedure) — reclassify toward Snare. If substantive: extraction decreases (independent review becomes obligatory) — reclassify toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedure_as_form_vs_substance, conceptual, 'Whether procedural justice means formal process or substantive fairness').

omega_variable(
    negative_liberty_vs_positive_rights_frontier,
    'Does the negative liberty reading (freedom FROM state violence) logically foreclose a positive entitlement reading (state duty to provide conditions necessary for life), or can both coexist in a single interpretive framework?',
    'Jurisprudential analysis of jurisdictions that have adopted both readings (e.g., EU social rights + procedural due process); identification of whether the readings compete or compose into a coherent doctrine',
    'If foreclose: this reading''s axiom is foundational and excludes positive_entitlement_reading. If coexist: both readings are live in the kernel, and mandatrophy reflects genuine pluralism. If compose: reading_relations should shift from coexists_with to influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negative_liberty_vs_positive_rights_frontier, conceptual, 'Logical relationship between negative and positive rights interpretations').

omega_variable(
    security_apparatus_capture_of_procedural_gates,
    'In practice, do state security apparatuses effectively capture the procedural gates (evidence rules, classified information exceptions, interrogation doctrine) such that the procedure becomes a form rather than substance?',
    'Longitudinal analysis of appeal outcomes in capital/detention cases; correlation between classified-information invocation and case dismissal rates; temporal evolution of evidence-exclusion doctrines in security contexts',
    'High capture (>70% of procedural challenges fail in security contexts): constraint reclassifies toward Snare for security apparatus (constrained→arbitrage via capture). Low capture (<30%): constraint remains Tangled Rope as genuinely balanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_apparatus_capture_of_procedural_gates, empirical, 'Degree to which security apparatus captures procedural justice mechanisms').

omega_variable(
    kernel_reading_contest_in_udhr_article_3,
    'Which reading of the Article 3 kernel (negative liberty vs positive entitlement vs procedural hybrid) is institutionally dominant in the global human rights regime?',
    'Analysis of International Court of Justice and European Court of Human Rights case law; treaty interpretation doctrines across signatories; primary authority claims in competing jurisdictions',
    'If negative_liberty dominates: this story''s classification reflects actual institutional commitments. If positive_entitlement dominates: this story models a minority reading and should be marked as aspirational or rejected. If hybrid dominates: this story represents one pole of an active interpretive contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_in_udhr_article_3, empirical, 'Institutional dominance of negative liberty reading in global human rights regime').

omega_variable(
    capital_punishment_abolition_as_reading_terminus,
    'Is the abolitionist perspective''s endpoint (categorical prohibition of capital punishment) a logical endpoint of the negative liberty reading, or does it require an additional interpretive move beyond this reading''s axioms?',
    'Jurisprudential analysis of abolition doctrines: can they be grounded solely in procedural impossibility (no procedure is adequate) or do they require a positive claim (state has no right to take life)? The latter is outside the negative liberty frame.',
    'If within frame: scaffold perspective is correctly positioned as reading terminus. If outside frame: abolitionist movement is actually instantiating a different reading (positive entitlement or categorical dignity), and this story''s scaffold perspective is mislabeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_punishment_abolition_as_reading_terminus, conceptual, 'Whether abolition follows logically from negative liberty reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_art3_nl_theater_1948, udhr_article_3__negative_liberty_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(udhr_art3_nl_theater_1998, udhr_article_3__negative_liberty_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement(udhr_art3_nl_theater_2023, udhr_article_3__negative_liberty_reading, theater_ratio, 75, 0.68).

% Extraction over time
narrative_ontology:measurement(udhr_art3_nl_extract_1948, udhr_article_3__negative_liberty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(udhr_art3_nl_extract_1998, udhr_article_3__negative_liberty_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(udhr_art3_nl_extract_2023, udhr_article_3__negative_liberty_reading, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(udhr_art3_nl_suppress_1948, udhr_article_3__negative_liberty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(udhr_art3_nl_suppress_1998, udhr_article_3__negative_liberty_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(udhr_art3_nl_suppress_2023, udhr_article_3__negative_liberty_reading, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, capital_punishment_abolition_movement).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, due_process_doctrine_evolution).

% DUAL FORMULATION NOTE:
% Article 3 of UDHR instantiates multiple structurally distinct constraints depending on reading choice. The negative_liberty_reading models the narrower interpretation (freedom FROM state violence, minimal affirmative duties). The positive_entitlement_reading models state duty to affirmatively provide. The procedural_hybrid_reading models both. Each has different beneficiary/victim structures, different epsilon values, different theater trajectories. All three are linked via the same kernel text but are separate constraints with separate stories. The negative liberty reading has the highest extractiveness (0.58) because it concentrates power in state procedure-setting. Positive entitlement reading would have lower extractiveness (hypothetically 0.32) because it imposes affirmative duties. Procedural hybrid would be intermediate (0.45). These are not observables of one constraint — they are different constraints derived from different reading choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__negative_liberty_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
