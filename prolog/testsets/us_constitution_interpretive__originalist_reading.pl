% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: US Constitution Interpretive Authority: Originalist Reading
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   The originalist reading of the US Constitution posits that constitutional
 *   meaning is fixed at the moment of ratification, and judicial
 *   interpretation must recover either the Framers' intent or the original
 *   public meaning of the text. This is ONE READING of the contested kernel —
 *   the Constitution itself. The reading competes with living-constitution
 *   and popular-constitutionalism interpretations, each grounded in different
 *   authority claims and producing different beneficiary/victim structures.
 *   Originalism constrains federal judicial authority to apply the
 *   Constitution only as its text would have been understood in 1787 (or at
 *   ratification for later amendments), which benefits federalism advocates,
 *   property rights claimants, and religious liberty claimants whose
 *   preferred outcomes align with historical narrow scope, while it excludes
 *   claims based on unenumerated or evolving rights. The constraint exhibits
 *   Tangled Rope structure: genuine coordination function (fixed meaning
 *   enables predictable law) combined with asymmetric extraction
 *   (unenumerated rights claimants are structurally foreclosed). The temporal
 *   measurements show rising theater ratio and rising extractiveness over the
 *   measurement interval, consistent with originalism's institutional
 *   ascendance in judicial appointments without corresponding epistemological
 *   settlement of the intent-recovery problem.
 *
 * KEY AGENTS:
 *   - Originalist Judicial Coalition: Institutional beneficiary (institutional/arbitrage) — judges, justices, and jurisprudential networks committed to originalist method; experience constraint as coordination enabling predictable, rule-bound decision-making
 *   - Unenumerated Rights Claimants: Primary victims (powerless/identity_locked) — agents seeking constitutional protection for rights not explicitly enumerated (bodily autonomy, privacy, consensual adult relationships); face structural foreclosure; exit is identity-locked because departing originalism abandons the constitutional claim
 *   - Federal Regulatory Agencies: Secondary victims (moderate/constrained) — interpret delegated authority under originalist constraints on implied powers; experience both coordination benefit (constrained delegation reduces scope-creep) and extraction (narrowed interpretations limit regulatory capacity)
 *   - Living Constitution Coalition: Organized secondary victims (organized/constrained) — legal academics, civil rights organizations, social movement advocates; perceive coordination benefit from adaptive constitutional interpretation; bear extraction from originalist dominance in appointments
 *   - Federalism Advocates: Institutional beneficiaries (institutional/arbitrage) — states' rights advocates, Tenth Amendment interpreters; benefit from originalism's narrow construction of federal enumerated powers
 *   - Constitutional Amendment Advocates: Organized agents with exit path (powerful/mobile) — actors pursuing formal amendment as override mechanism; see originalism as temporary coordination problem with legal sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.38).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.48).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "US Constitution Interpretive Authority: Originalist Reading").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '9953918e-2e70-4de8-953c-284c7c0aceae').
narrative_ontology:cs_kernel_codification('9953918e-2e70-4de8-953c-284c7c0aceae', fixed_text).
narrative_ontology:cs_authority_grounding('9953918e-2e70-4de8-953c-284c7c0aceae', lineage).
narrative_ontology:cs_interpretation_layer_present('9953918e-2e70-4de8-953c-284c7c0aceae').
narrative_ontology:cs_reading_relation('9953918e-2e70-4de8-953c-284c7c0aceae', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('9953918e-2e70-4de8-953c-284c7c0aceae', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('9953918e-2e70-4de8-953c-284c7c0aceae', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('9953918e-2e70-4de8-953c-284c7c0aceae', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('9953918e-2e70-4de8-953c-284c7c0aceae', foundational, judicial_interpretation_constrained_by_historical_understanding).
narrative_ontology:cs_axiom_status(judicial_interpretation_constrained_by_historical_understanding, holdable).
narrative_ontology:cs_axiom_grounding('9953918e-2e70-4de8-953c-284c7c0aceae', judicial_interpretation_constrained_by_historical_understanding, deontological).
narrative_ontology:cs_reference_frame('9953918e-2e70-4de8-953c-284c7c0aceae', framers_intent_fixed_meaning).
narrative_ontology:cs_drift_state('9953918e-2e70-4de8-953c-284c7c0aceae', contemporary_judicial_composition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9953918e-2e70-4de8-953c-284c7c0aceae', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, originalist_judicial_coalition).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_historical_scope).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, living_constitution_jurisprudence_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNENUMERATED RIGHTS CLAIMANTS (SNARE) — Agents seeking constitutional protection for rights not explicitly enumerated (bodily autonomy, privacy, consensual adult relationships) face structural elimination under originalist framing. Their exit is identity-locked: departing originalism would require abandoning the constitutional claim itself. The constraint extracts by foreclosing entire categories of rights from judicial recognition. High suppression — the interpretive framework institutionally denies alternatives.
constraint_indexing:constraint_classification(us_constitution_interpretive__originalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: LIVING CONSTITUTION COALITION (TANGLED ROPE) — Organized actors (progressive legal academics, civil rights organizations, social movement advocates) perceive genuine coordination benefit from constitutional interpretation as an adaptive mechanism — the constitution must address novel technological and social questions. But they also bear extraction: originalism's ascendance in judicial appointments constrains the coalition's ability to advance adaptive interpretations. Constrained exit: they remain within the constitutional system but at increased cost.
constraint_indexing:constraint_classification(us_constitution_interpretive__originalist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINALIST JUDICIAL COALITION (ROPE) — Judges, justices, and jurisprudential networks committed to originalist method experience the constraint as coordination: shared methodology enables predictable, rule-bound judicial decision-making. The coalition benefits from interpretive authority concentration and from reputational arbitrage (originalism carries prestige as 'law-like,' constraining vs. creative). Extraction runs toward this agent; they have arbitrage options (can apply originalist methodology portably across doctrine).
constraint_indexing:constraint_classification(us_constitution_interpretive__originalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL AMENDMENT MOVEMENT (SCAFFOLD) — Actors pursuing formal constitutional amendment (e.g., Fourteenth Amendment Due Process reinterpretation, explicit unenumerated-rights amendments) see originalism as a temporary coordination problem with a legal exit path: amend the Constitution to explicitly codify desired rights. This perspective has sunset logic — if amendment succeeds, the constraint loses force. Mobile exit: amendments remain structurally possible despite high procedural cost.
constraint_indexing:constraint_classification(us_constitution_interpretive__originalist_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL REGULATORY AGENCIES (TANGLED_ROPE) — Agencies interpreting their delegated authority face originalist constraints on the scope of implied powers. Originalism provides coordination benefit (constrained delegation reduces agency scope-creep). But agencies also bear extraction: narrowed originalist interpretations of the Commerce Clause, Necessary and Proper Clause, and delegated authority limit regulatory capacity. Constrained exit: agencies operate within the constitutional system but at reduced effective authority.
constraint_indexing:constraint_classification(us_constitution_interpretive__originalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL JUDICIAL INSTITUTIONALISM (PITON) — The long-established practice of judicial review divorced from systematic methodological commitment (pre-originalism jurisprudence) has largely atrophied. The constraint persists through inertia: living constitutionalism remains the default teaching and practice in law schools even as originalism controls appointment politics. Theater is high — the continued invocation of 'constitutional interpretation' as a discipline obscures the underlying methodological contestation. Function has degraded; theater maintains the institutional form.
constraint_indexing:constraint_classification(us_constitution_interpretive__originalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational and universal perspective, all written constitutions face an inescapable problem: their meaning at ratification must somehow fix meaning for perpetuity, or meaning must evolve with application. Some gap between text-at-ratification and text-as-applied is inherent to constitutional governance. This perspective sees originalism as naming a structural necessity of written law itself. However, the structural data contradicts this — the constraint's beneficiary/victim structure is asymmetric, and originalism is a contingent methodological choice, not an immutable law.
constraint_indexing:constraint_classification(us_constitution_interpretive__originalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_interpretive__originalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_interpretive__originalist_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Originalism benefits a clear beneficiary coalition (originalist judges, federalism advocates, narrow-scope property and religious liberty claimants) while systematically foreclosing unenumerated rights claims. The extraction is real but not extreme because: (1) living-constitution jurisprudence remains embedded in legal education and lower-court practice despite originalist dominance, (2) amendment remains a formal exit path (though expensive), and (3) originalism's own methodological commitments constrain arbitrary outcome-construction, even if intent recovery is epistemically underdetermined. Suppression (0.48): Moderate-high. Institutional barriers include appointment politics (originalist judges are deliberately selected), professional prestige concentration (originalism carries 'law-like' legitimacy), and doctrinal path-dependence (once originalist precedents accumulate, moving away is costly). But suppression is not total — bar associations, law schools, and lower courts continue producing alternative methodologies. Theater ratio (0.62): High and rising. The constraint's theater has increased over the measurement interval because the methodological question (how to recover Framers' intent? what counts as 'original public meaning'?) remains epistemically unsettled, yet originalists invoke 'fidelity to the text' as if the answer were mechanically determinable. The gap between methodological rhetoric and epistemological reality drives theater growth.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification across institutional and marginalized perspectives. The originalist judicial coalition experiences coordination (Rope) — they experience the constraint as enabling shared methodology and predictable law. Federal regulatory agencies experience mixed costs and benefits (Tangled Rope) — coordination benefit from constrained delegation; extraction from narrowed power scope. Unenumerated rights claimants experience pure extraction (Snare) — systematically foreclosed with no structural exit. The living-constitution coalition experiences organized resistance to extraction (Tangled Rope) — organized enough to produce alternative doctrine but constrained by appointment politics. The amendment movement experiences a temporary constraint with a legal sunset (Scaffold) — though the amendment process is expensive and slow. The traditional pre-originalist judicial practice has atrophied but persists through institutional inertia (Piton) — performative invocation of 'constitutional interpretation' as a discipline obscures methodological contestation. The civilizational analytical observer risks naturalizing originalism as inherent to written law (Mountain) — seeing the Framer's-intent gap as inescapable feature of all constitutions — but the structural data reveals asymmetric beneficiaries and foreclosed victim groups, contradicting the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint operates through asymmetric interpretive authority: originalist judges hold institutional power to adjudicate constitutional meaning, and their methodology systematically benefits federalism advocates and narrow-scope property/religious liberty claimants while foreclosing unenumerated rights. The beneficiary coalition (originalist judges, federalism advocates) derives d ≈ 0.15-0.25 (institutional + arbitrage exit = beneficiary position with low experienced extraction). The victim coalition (unenumerated rights claimants, regulatory expansion advocates) derives d ≈ 0.85-0.95 (powerless/organized + identity_locked/constrained exit = victim position with high experienced extraction). The living-constitution coalition occupies an intermediate position (organized power, constrained exit) derived d ≈ 0.55-0.65. The originalist coalition's institutional power level and arbitrage options mean the constraint's effective extraction relative to them is low or negative (they capture disproportionate benefit from the interpretive authority concentration), while powerless agents experience the constraint's full suppressive force.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading resolves the mandatrophy by grounding its claims in a specific epistemological commitment: historical intent (or original public meaning) fixes meaning. This is contestable but coherent. The constraint's Tangled Rope classification reflects genuine coordination function (shared methodology, predictable law) alongside asymmetric extraction (unenumerated rights foreclosed). The false-summit risk is that originalism presents itself as mere 'fidelity to law' when it actually instantiates a methodological choice with identifiable beneficiaries and victims. The omega variables route the unresolved questions (intent-recovery feasibility, scope of axiom logical necessity, amendment viability, reading vs. power structure decomposition) through the apparatus for ongoing analysis. The mandatrophy is not 'which reading is correct?' but 'what structural commitments does each reading instantiate, and what are their beneficiary/victim asymmetries?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framers_intent_recovery_feasibility,
    'Is the historical intent of the Framers (or the original public meaning of the text) epistemically recoverable to sufficient precision to ground judicial decisions on contested constitutional questions?',
    'Comparative analysis of originalist decisions using different intent-recovery methodologies (New Originalism vs. Old Originalism vs. Public Meaning Originalism); correlation between methodological choice and outcome; identification of underspecified intent questions where multiple ''original meanings'' are defensible.',
    'If high-precision recovery is impossible: originalism collapses into interpretive discretion masked by fidelity rhetoric (Tangled Rope). If recovery is possible: originalism is a genuine constraint on judicial creativity (Rope). The distinction determines whether the constraint''s suppression is structural or performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framers_intent_recovery_feasibility, empirical, 'Epistemological status of intent recovery and original public meaning').

omega_variable(
    unenumerated_rights_logical_scope,
    'Does the originalist prohibition on unenumerated rights logically follow from the constraint''s axioms, or is it a contingent interpretive choice?',
    'Logical analysis of Ninth Amendment interpretation; examination of whether originalism committed to Ninth Amendment silence necessarily forecloses unenumerated rights, or whether alternative originalist readings (Ninth Amendment as affirmative constraint on federal power) are coherent within originalist premises.',
    'If prohibition logically follows: the constraint forecloses living-constitution reading on unenumerated rights (relation: forecloses). If contingent: the readings coexist, and originalism''s scope is narrower than commonly assumed (relation: coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unenumerated_rights_logical_scope, conceptual, 'Logical necessity of unenumerated rights rejection within originalism').

omega_variable(
    amendment_process_sufficiency,
    'Is the constitutional amendment process (Article V) a functionally viable exit path for overriding originalist constraints, or has constitutional lock-in made amendment effectively impossible?',
    'Historical data on amendment success rates and timescales; network analysis of veto-player coalitions preventing amendment; comparison of amendment feasibility pre- and post-originalist ascendance in judiciary.',
    'If amendment is viable: scaffold perspective is structurally real, and the constraint has genuine sunset logic. If amendment is blocked: scaffold becomes aspirational, and unenumerated-rights claimants face permanent structural exclusion (Snare magnitude increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_sufficiency, empirical, 'Viability of Article V as exit mechanism for constraint override').

omega_variable(
    originalism_reading_identity,
    'Is this constraint ''the originalist reading of the Constitution'' or ''the institutional dominance of originalist courts''? Are they the same constraint or two?',
    'Decomposition check: does the ε value change if we measure originalism-as-interpretive-methodology vs. originalism-as-institutional-power-structure? If yes, we have two constraints.',
    'If one constraint: the story captures both the logical content of originalism and the institutional power dynamics. If two: separate stories required — one for the jurisprudential constraint (ε lower, focuses on interpretive coherence), one for the institutional constraint (ε higher, focuses on appointment politics and enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_reading_identity, conceptual, 'Whether originalism-as-reading and originalism-as-power are a single or decomposed constraint').

omega_variable(
    false_summit_natural_law_candidate,
    'Is originalism presented as fidelity to an immutable constitutional fact (natural law framing), when it is actually a methodological choice with identifiable beneficiaries and victims?',
    'Comparative study of originalist rhetoric (appeals to ''what the Constitution says,'' ''rule of law,'' ''constraining judicial discretion'') vs. originalist outcomes (asymmetric beneficiaries: federalism advocates, property rights claimants, religious liberty under narrow scope; asymmetric victims: unenumerated rights claimants, regulatory expansion advocates). High asymmetry + beneficiary/victim structure = false summit candidate.',
    'If false summit confirmed: the mountain classification at analytical perspective is a naturalization of a contingent political arrangement. The engine''s FSM detector will flag this and potentially reclassify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, conceptual, 'Whether originalism naturalizes a contingent institutional arrangement as immutable law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usco_tr_t0, us_constitution_interpretive__originalist_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(usco_tr_t10, us_constitution_interpretive__originalist_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(usco_tr_t20, us_constitution_interpretive__originalist_reading, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(usco_be_t0, us_constitution_interpretive__originalist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(usco_be_t10, us_constitution_interpretive__originalist_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(usco_be_t20, us_constitution_interpretive__originalist_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(usco_su_t0, us_constitution_interpretive__originalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usco_su_t10, us_constitution_interpretive__originalist_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(usco_su_t20, us_constitution_interpretive__originalist_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive_living_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive_popular_constitutionalism_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, fourteenth_amendment_due_process_scope).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, enumerated_federal_powers_commerce_clause).

% DUAL FORMULATION NOTE:
% The US Constitution interpretive authority constraint family comprises three constraint stories: originalist_reading (ε≈0.38), living_reading (ε≈0.42), and popular_constitutionalism_reading (ε≈0.35). Each story instantiates one reading of the contested kernel (the Constitution itself). The three stories are NOT the same constraint viewed from different angles — they have distinct ε values reflecting different epistemological and structural commitments. Originalism: fixed meaning at ratification, beneficiaries include federalism advocates and narrow-scope rights claimants. Living reading: adaptive meaning over time, beneficiaries include federal regulatory expansion and unenumerated-rights claimants. Popular reading: popular sovereign binds meaning, beneficiaries include democratic participation advocates. Network edges link these stories to downstream constraints they affect (fourteenth amendment scope, commerce clause interpretation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__originalist_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
