% ============================================================================
% CONSTRAINT STORY: living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_constitutionalist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: living_constitutionalist_reading
 *   human_readable: Living Constitutionalism: Adaptive Interpretation of Constitutional Meaning
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   Living constitutionalism holds that the meaning of the U.S. Constitution
 *   evolves with society, and that judicial interpretation must adapt
 *   constitutional principles to contemporary circumstances rather than
 *   treating the text as fixed by historical authorial intent. This reading
 *   instantiates ONE interpretation of the contested kernel of constitutional
 *   authority: the Constitution itself. Other readings (originalism,
 *   positivism) interpret the same founding document differently, producing
 *   different constraints with different ε values and different
 *   beneficiary/victim structures. Living constitutionalism presents as a
 *   Tangled Rope: it provides genuine coordination benefit (enabling
 *   constitutional principles to address contexts the authors could not have
 *   specified, preventing constitutional calcification) while simultaneously
 *   extracting through judicial displacement of democratic and
 *   amendment-based constraint-setting (judges set constitutional boundaries
 *   through adaptive interpretation rather than through the difficult
 *   democratic process of formal amendment). The reading's authority grounds
 *   itself in lineage (continuity with a tradition of evolutionary
 *   jurisprudence) and practice (actual practice of U.S. courts adapting
 *   interpretation over time). Its reference frame is
 *   flexible_constitutional_authority — the view that constitutional meaning
 *   must accommodate social change. The constraint shows rising theater ratio
 *   over time (0.38 → 0.52) as adaptive interpretation becomes increasingly
 *   decoupled from textual constraint and more dependent on judicial
 *   discretion about what 'contemporary circumstances' require.
 *
 * KEY AGENTS:
 *   - Rights Claimants in Changed Contexts: Primary beneficiary (powerless/trapped) — structurally benefit from adaptive interpretation enabling recognition of rights in contexts (same-sex marriage, privacy, digital expression) the historical Constitution did not contemplate.
 *   - Judicial Branch: Secondary beneficiary (institutional/arbitrage) — expands interpretive authority and insulates courts from being locked into historical meanings through claims to adaptive necessity.
 *   - Democratic Legislatures: Primary victim (moderate/constrained) — experience extraction through judicial preemption of constitutional boundary-setting; constrained exit via formal amendment (very high cost) or statutory reinterpretation.
 *   - Originalist / Fixed-Meaning Tradition: Identity-locked victim (moderate/identity_locked) — institutional identity constituted through claim to recoverable historical meaning; living constitutionalism delegitimizes the core professional project.
 *   - Progressive Rights Movement: Organized beneficiary (organized/mobile) — uses living constitutionalism as temporary scaffold for rights recognition; sees sunset as movement builds consensus for durable change.
 *   - Constitutional Amendment Process: Institutional victim (institutional/arbitrage) — formal mechanism becomes theatrically marginal (piton status); amendment persists as vestigial alternative pathway.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent interpretive choice as immutable constitutional structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_constitutionalist_reading, 0.48).
domain_priors:suppression_score(living_constitutionalist_reading, 0.35).
domain_priors:theater_ratio(living_constitutionalist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_constitutionalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(living_constitutionalist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(living_constitutionalist_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(living_constitutionalist_reading, "Living Constitutionalism: Adaptive Interpretation of Constitutional Meaning").
narrative_ontology:topic_domain(living_constitutionalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_constitutionalist_reading, '9b8c2214-9cc0-44af-b40a-878065849efa').
narrative_ontology:cs_created_at('9b8c2214-9cc0-44af-b40a-878065849efa', '').
narrative_ontology:cs_kernel_codification('9b8c2214-9cc0-44af-b40a-878065849efa', fixed_text).
narrative_ontology:cs_authority_grounding('9b8c2214-9cc0-44af-b40a-878065849efa', lineage).
narrative_ontology:cs_interpretation_layer_present('9b8c2214-9cc0-44af-b40a-878065849efa').
narrative_ontology:cs_kernel_id(living_constitutionalist_reading, us_constitution_text).
narrative_ontology:cs_reading_relation('9b8c2214-9cc0-44af-b40a-878065849efa', originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b8c2214-9cc0-44af-b40a-878065849efa', positivist_reading, coexists_with).
narrative_ontology:cs_axiom('9b8c2214-9cc0-44af-b40a-878065849efa', foundational, constitutional_meaning_evolves_with_society).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_society, holdable).
narrative_ontology:cs_axiom_grounding('9b8c2214-9cc0-44af-b40a-878065849efa', constitutional_meaning_evolves_with_society, empirically_contingent).
narrative_ontology:cs_axiom('9b8c2214-9cc0-44af-b40a-878065849efa', foundational, contemporary_social_values_authoritative_interpretation).
narrative_ontology:cs_axiom_status(contemporary_social_values_authoritative_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('9b8c2214-9cc0-44af-b40a-878065849efa', contemporary_social_values_authoritative_interpretation, deontological).
narrative_ontology:cs_reference_frame('9b8c2214-9cc0-44af-b40a-878065849efa', flexible_constitutional_authority).
narrative_ontology:cs_drift_state('9b8c2214-9cc0-44af-b40a-878065849efa', contemporary_judicial_practice, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_constitutionalist_reading, rights_claimants_in_changed_contexts).
narrative_ontology:constraint_beneficiary(living_constitutionalist_reading, judicial_branch_interpretive_authority).
narrative_ontology:constraint_victim(living_constitutionalist_reading, fixed_meaning_claims).
narrative_ontology:constraint_victim(living_constitutionalist_reading, democratic_constraint_on_judges).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIGHTS CLAIMANTS (ROPE) — Structurally trapped by historical social conditions that the original Constitution did not contemplate. Living constitutionalism provides genuine coordination benefit: it enables adaptation of constitutional principles to address contemporary social realities (privacy rights, equal protection under changed gender/sexuality norms, freedom of association in digital contexts). These agents benefit from the constraint's flexibility; extraction is minimal because the coordination function is real — principles must adapt or become functionally dead.
constraint_indexing:constraint_classification(living_constitutionalist_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEMOCRATIC LEGISLATURES & POPULACES (TANGLED ROPE) — Experience mixed coordination and extraction. Living constitutionalism coordinates around the need to adapt law without formal amendment (genuine coordination benefit). But it also extracts through judicial displacement of legislative authority: judges interpret 'what the Constitution requires' in changed contexts, preempting democratic processes for constitutional change. Constrained exit: legislatures can attempt constitutional amendment (very high cost) or pass statutes that reinterpret constitutional boundaries through practice, but courts retain final authority.
constraint_indexing:constraint_classification(living_constitutionalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINALIST / FIXED-MEANING TRADITION (SNARE) — The institutional identity of originalism is constituted through the claim to fixed, recoverable original meaning. Living constitutionalism forecloses the originalist position's core claim: if the Constitution's meaning evolves with society, then the originalist project (recovering a stable historical meaning) becomes incoherent or relegated to historical interest. The identity-lock is institutional: the originalist judiciary and academic tradition cannot exit without abandoning their foundational professional commitment. The constraint appears as a pure extraction mechanism from this perspective — it delegitimizes the very epistemic framework originalism depends on.
constraint_indexing:constraint_classification(living_constitutionalist_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIAL BRANCH (TANGLED ROPE) — Benefits substantially from living constitutionalism: it expands judicial discretion, elevates judicial authority to set constitutional boundaries through adaptation narratives, and insulates courts from being locked into historical meanings that become socially incoherent. This is genuine beneficiary extraction — the judiciary captures increased authority over constitutional meaning-making. But the constraint also provides coordination benefit: it enables courts to maintain constitutional legitimacy as social values shift, preventing constitutional systems from calcifying into irrelevance. The arbitrage exit option reflects that judges can move between originalist and living constitutionalist reasoning; they are not trapped.
constraint_indexing:constraint_classification(living_constitutionalist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE RIGHTS MOVEMENT (SCAFFOLD) — Organized agents using living constitutionalism as a temporary scaffold for rights recognition before formal constitutional amendment or statutory entrenchment. Courts adapt principles to recognize marriage equality, abortion access, and other rights; the movement sees this as a transition mechanism toward durable legal change. The sunset is embedded: once society achieves enough consensus to formally amend (as with voting rights) or entrench through statute, the need for adaptive judicial interpretation declines. Extraction is present but declining as consensus builds.
constraint_indexing:constraint_classification(living_constitutionalist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL AMENDMENT PROCESS (PITON) — Living constitutionalism partly inactivates the formal amendment mechanism (Article V). Amendment becomes theatrically marginal: major constitutional evolution happens through judicial interpretation, not through the difficult but democratically transparent amendment process. The formal amendment process persists (theater ≥ 0.7) as a vestigial alternative pathway that remains available but is functionally degraded — courts reinterpret faster than legislatures amend, so amendment becomes institutional inertia rather than primary constitutional evolution mechanism.
constraint_indexing:constraint_classification(living_constitutionalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some constitutional adaptation is inherent to long-lived legal systems: principles written at moment T0 encounter social realities at T1, T2, T3 that the authors could not have specified. The gap between historical specificity and universal principles is built into constitutional structure itself. This perspective treats adaptive interpretation as an immutable property of how constitutional systems work, not as a choice. However, the structural data contradicts this: identifiable beneficiaries (judges, rights claimants) and victims (fixed-meaning claims, democratic constraint) show this is a constructed interpretive regime, not a law of nature.
constraint_indexing:constraint_classification(living_constitutionalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_constitutionalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(living_constitutionalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(living_constitutionalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(living_constitutionalist_reading, TR),
    TR >= 0.70.

:- end_tests(living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Living constitutionalism extracts through judicial authority expansion: judges claim to declare what 'the Constitution requires' in changed contexts, preempting democratic processes for constitutional boundary-setting. The original Constitution vested amendment authority in a difficult supermajority process; living constitutionalism moves major constitutional evolution to courts. But extraction is moderated by genuine coordination benefit: principles written in 1787 must be adapted to address contexts (digital privacy, modern association, expanded equality claims) the authors could not specify. Pure extraction would be snare; the presence of real coordination function (enabling long-lived constitutions to remain functional) classifies as tangled rope. Suppression (0.35): Moderate. Barriers to resisting adaptive interpretation exist (judicial authority, institutional legitimacy of courts as final constitutional arbiters) but are not total. Originalist and positivist framings remain live academic and judicial positions; democratic resistance can occur through legislative reinterpretation, constitutional amendment attempts, and resistance to judicial authority. Theater ratio (0.52): Moderate. Adaptive interpretation has real functional content (courts do address changed circumstances) but also performative content (interpretive narratives often obscure the degree of judicial discretion being exercised; 'discovering' contemporary meaning provides legitimacy cover for making policy choices). Rising theater over time reflects increasing gap between the constraint's functional justification (adapting principles to new contexts) and the discretionary breadth claimed (judges set boundaries based on amorphous 'contemporary values').
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the core perspectival gap between judicial branch and democratic constraint institutions. The judicial branch (institutional/arbitrage) sees rope: adaptive interpretation is a coordination function enabling constitutional principles to remain relevant. Democratic legislatures (moderate/constrained) see tangled rope: the constraint provides some coordination benefit but extracts through displacement of amendment-based authority. The originalist tradition (moderate/identity_locked) sees snare: the constraint delegitimizes their entire interpretive project and they have no exit without abandoning institutional identity. Rights claimants (powerless/trapped) see rope: they benefit from adaptive interpretation that recognizes their claims. The piton perspective reveals that formal amendment (the intended mechanism for constitutional evolution) becomes theatrically marginal. The analytical observer risks the mountain perspective — treating necessary constitutional adaptation as immutable law rather than as a constructed interpretive regime with identifiable beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Living constitutionalism's directionality derives from the distribution of benefits and costs across institutional actors. Judges (beneficiary/arbitrage) have low d — the constraint subsidy flows toward them. Rights claimants (beneficiary/trapped) have higher d because they benefit but are trapped within the constraint's scope. Democratic institutions (victim/constrained) have high d — they bear extraction costs (loss of amendment authority) but have constrained exit (can attempt amendment at extreme cost). The originalist tradition (victim/identity_locked) has very high d — they are trapped not by external barriers but by institutional identity constituted through fixed-meaning claims. The piton perspective shows that amendment authority experiences d approaching 1.0 — it is almost entirely victim of the constraint (conversion to theatrical vestigial status) with no real coordination benefit from living constitutionalism's rise.
 *
 * MANDATROPHY ANALYSIS:
 *   Living constitutionalism resolves mandatrophy by acknowledging that it performs both coordination and extraction simultaneously. The coordination function is real: long-lived constitutions must adapt or become functionally dead. The extraction is real: adaptive interpretation displaces democratic amendment authority and concentrates boundary-setting in courts. The constraint is legitimately tangled rope, not snare masquerading as rope. The false summit (analytical/mountain perspective) must be rejected: adaptive interpretation is not an immutable law of constitutional systems but a constructed institutional arrangement with measurable beneficiaries and victims. The perspectival variance (rope from judicial view, snare from originalist view, piton from amendment view) is explained by the agents' different structural positions relative to the extraction flow, not by disagreement about constitutional nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_vs_adaptation_foreclosure,
    'Does living constitutionalism logically foreclose originalism as a coherent interpretive methodology, or do they coexist as competing frameworks held by different institutional actors?',
    'Analysis of whether a single judge or interpretive community can coherently hold both living constitutionalist and originalist commitments. If foreclosure: the axioms directly contradict. If coexistence: they are live positions within different jurisprudential traditions.',
    'If forecloses: originalism must be classified as a victim of this constraint (the snare perspective is correct). If coexists: they are alternative readings of the same kernel, both structurally viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_vs_adaptation_foreclosure, conceptual, 'Whether living constitutionalism logically forecloses originalism').

omega_variable(
    judicial_authority_legitimacy_grounding,
    'What grounds the legitimacy of judicial authority to declare contemporary meaning? Is it constitutional interpretation, lawmaking by other means, or representational authority derived from democratic institutions?',
    'Examination of the explicit doctrinal justifications courts provide for adaptive interpretation. If justified as ''reading the Constitution'': interpretation frame. If justified as ''discovering evolving standards'': empirical claim about constitutional meaning. If justified as ''responding to contemporary needs'': acknowledgment of lawmaking function.',
    'If justified as interpretation: living constitutionalism''s legitimacy depends on defending the interpretive frame. If as lawmaking: the constraint becomes snare from democratic perspective (judicial override of legislature). If as representation: must demonstrate democratic mandate for judicial authority to set constitutional boundaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_authority_legitimacy_grounding, conceptual, 'What grounds judicial authority for adaptive constitutional interpretation').

omega_variable(
    amendment_process_viability,
    'Is the Article V amendment process genuinely available as an alternative to judicial adaptation, or has it become functionally impossible such that living constitutionalism is the only pathway for constitutional evolution?',
    'Historical analysis of amendment success rates relative to major constitutional questions. Comparison of time-to-resolution via judicial interpretation vs amendment. Assessment of whether contemporary political polarization makes amendment viable for contested constitutional questions.',
    'If amendment is viable: courts have a real alternative; living constitutionalism is discretionary. If amendment is impossible: courts face a genuine coordination problem (constitutional meaning must evolve or system calcifies); living constitutionalism becomes functionally necessary rather than extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_viability, empirical, 'Whether formal constitutional amendment remains a viable alternative pathway').

omega_variable(
    reading_identity_kernel_boundary,
    'Is the distinction between living constitutionalism and originalism a reading of a single kernel (the Constitution), or do they represent fundamentally different kernels (written text vs evolving principles)?',
    'Examination of whether both readings claim authority from the same founding document. If yes: single kernel, different readings. If no: different kernels with different authority sources.',
    'If single kernel: the reading_relations structure correctly models them as competing interpretations. If different kernels: they should be separate constraints, not readings of the same constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_kernel_boundary, conceptual, 'Whether originalism and living constitutionalism are readings of the same kernel or distinct kernels').

omega_variable(
    social_change_authority_source,
    'What makes contemporary social values authoritative for constitutional interpretation? Is it majority preference (democratic), reasoned consensus (epistemic), practical necessity (functional), or natural law evolution (metaphysical)?',
    'Analysis of the axioms supporting living constitutionalism. Examination of which authority sources the tradition actually relies on. Comparison with originalist claims about authority sources (historical intent, textual meaning, original public meaning).',
    'If majority preference: democratic legitimacy depends on majoritarian processes; minority protections become weaker. If reasoned consensus: legitimacy depends on the quality of the interpretive dialogue. If practical necessity: constraint becomes functionally justified but divorced from democratic authority. If natural law: living constitutionalism inherits mountain-like properties (immutable evolutionary force).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_change_authority_source, conceptual, 'What authorizes contemporary social values to shape constitutional meaning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_constitutionalist_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_constitutionalist_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(livi_tr_t3, living_constitutionalist_reading, theater_ratio, 3, 0.45).
narrative_ontology:measurement(livi_tr_t6, living_constitutionalist_reading, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_constitutionalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(livi_be_t3, living_constitutionalist_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(livi_be_t6, living_constitutionalist_reading, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(living_constitutionalist_reading, originalist_reading).
narrative_ontology:affects_constraint(living_constitutionalist_reading, positivist_reading).
narrative_ontology:affects_constraint(living_constitutionalist_reading, constitutional_amendment_viability).

% DUAL FORMULATION NOTE:
% Living constitutionalism and originalism are competing readings of the same constitutional kernel. Each reading produces a different constraint (different ε, different beneficiary/victim structure) because they disagree on what constitutes authoritative meaning. These are not the same constraint viewed from different angles — they are structurally distinct constraints derived from different authority claims. The network links them to show interdependence: living constitutionalism's rise correlates with originalism's institutional decline, and both are affected by the viability of formal constitutional amendment (which would reduce the functional pressure for judicial adaptation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_constitutionalist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
