% ============================================================================
% CONSTRAINT STORY: equality_code__classification_test_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_code__classification_test_reading, []).

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
 *   constraint_id: equality_code__classification_test_reading
 *   human_readable: Article 14 Classification Test: Intelligible Differentia and Rational Nexus
 *   domain: constitutional_law/equality_doctrine
 *
 * SUMMARY:
 *   Article 14 of the Indian Constitution guarantees equality before law and
 *   equal protection of the laws. The classification test reading
 *   operationalizes this guarantee as a reasonableness review of legislative
 *   line-drawing: a classification satisfies Article 14 if it is based on an
 *   intelligible differentia and the differentia has a rational nexus to the
 *   objective of the legislation. This reading instantiates one specific
 *   doctrinal approach to equality — equality as rational administration of
 *   categories. The test suppresses arbitrary classification but does not
 *   examine substantive outcomes or the legitimacy of the stated purpose
 *   itself. It is a procedural constraint on how states sort citizens, not a
 *   substantive constraint on what sortings are permissible. This reading
 *   coexists with two siblings: the arbitrariness doctrine reading (which
 *   holds that capricious state action violates Article 14 regardless of
 *   whether a classification exists) and the horizontal reach reading (which
 *   contests whether Article 14 constrains private power — clubs, employers,
 *   platforms — that perform the same categorical sorting). The
 *   classification test reading assumes the state-action boundary is
 *   appropriate and that the relevant equality question is 'Is the
 *   classification rational?' rather than 'Is unexplained action itself
 *   impermissible?' or 'How far does equality extend into private
 *   associations?' The test has become the dominant doctrinal framework, but
 *   its operation reveals a structural tension: those with articulate policy
 *   purposes (legislatures, organized interests) benefit from the test's
 *   requirement that classification be rationalized; those sorted into
 *   disfavored categories without explanation bear the burden of articulating
 *   the state's unstated logic.
 *
 * KEY AGENTS:
 *   - Unexplained Excluded Persons: Primary victims (powerless/trapped) — sorted into disfavored categories without intelligible differentia; bear burden of proving exclusion irrational
 *   - Legislative Authorities with Coherent Purposes: Primary beneficiaries (institutional/arbitrage) — benefit from test that protects rational classifications and places burden on challengers
 *   - Advocacy Coalitions: Secondary actor (organized/constrained) — fight arbitrary classifications using the test; benefit from coordination mechanism but bear asymmetric burden of proof
 *   - Judiciary: Institutional actor (institutional/arbitrage) — administers the test through reasonableness review; maintains doctrinal tradition while rarely invalidating defended legislation (piton perspective)
 *   - Constitutional Reform Movements: Organized agents (organized/constrained) — advocate beyond classification test toward substantive equality; see the test as temporary scaffold
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the classification framework as inherent to law rather than as a contingent doctrinal choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_code__classification_test_reading, 0.38).
domain_priors:suppression_score(equality_code__classification_test_reading, 0.52).
domain_priors:theater_ratio(equality_code__classification_test_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_code__classification_test_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(equality_code__classification_test_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(equality_code__classification_test_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_code__classification_test_reading, tangled_rope).
narrative_ontology:human_readable(equality_code__classification_test_reading, "Article 14 Classification Test: Intelligible Differentia and Rational Nexus").
narrative_ontology:topic_domain(equality_code__classification_test_reading, "constitutional_law/equality_doctrine").

domain_priors:requires_active_enforcement(equality_code__classification_test_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_code__classification_test_reading, 'ab6cb04c-652e-479e-86c7-563c28d04459').
narrative_ontology:cs_kernel_codification('ab6cb04c-652e-479e-86c7-563c28d04459', formalized).
narrative_ontology:cs_authority_grounding('ab6cb04c-652e-479e-86c7-563c28d04459', lineage).
narrative_ontology:cs_interpretation_layer_present('ab6cb04c-652e-479e-86c7-563c28d04459').
narrative_ontology:cs_reading_relation('ab6cb04c-652e-479e-86c7-563c28d04459', equality_code__arbitrariness_doctrine_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab6cb04c-652e-479e-86c7-563c28d04459', equality_code__horizontal_reach_question_reading, influences).
narrative_ontology:cs_axiom('ab6cb04c-652e-479e-86c7-563c28d04459', foundational, classification_rationality_is_equality_test).
narrative_ontology:cs_axiom_status(classification_rationality_is_equality_test, holdable).
narrative_ontology:cs_axiom_grounding('ab6cb04c-652e-479e-86c7-563c28d04459', classification_rationality_is_equality_test, deontological).
narrative_ontology:cs_axiom('ab6cb04c-652e-479e-86c7-563c28d04459', foundational, burden_on_challenger_to_prove_irrationality).
narrative_ontology:cs_axiom_status(burden_on_challenger_to_prove_irrationality, holdable).
narrative_ontology:cs_axiom_grounding('ab6cb04c-652e-479e-86c7-563c28d04459', burden_on_challenger_to_prove_irrationality, conventional).
narrative_ontology:cs_reference_frame('ab6cb04c-652e-479e-86c7-563c28d04459', rational_administration_of_categories).
narrative_ontology:cs_drift_state('ab6cb04c-652e-479e-86c7-563c28d04459', contemporary_substantive_equality_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ab6cb04c-652e-479e-86c7-563c28d04459', '').
narrative_ontology:cs_kernel_id(equality_code__classification_test_reading, equality_code).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_code__classification_test_reading, parties_within_rational_classifications).
narrative_ontology:constraint_beneficiary(equality_code__classification_test_reading, legislative_actors_with_coherent_sorting_logic).
narrative_ontology:constraint_victim(equality_code__classification_test_reading, persons_in_unexplained_legislative_categories).
narrative_ontology:constraint_victim(equality_code__classification_test_reading, excluded_groups_lacking_intelligible_differentia).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNEXPLAINED EXCLUDED PERSON (SNARE) — Persons sorted into disfavored categories without intelligible differentia or rational nexus experience maximal extraction. The classification test requires them to articulate the state's own unstated logic to challenge their exclusion. Trapped: cannot exit the category or the jurisdiction. Suppression is high — burden of proof inverted, requiring the excluded to rationalize the legislator's silence. No coordination benefit; pure extraction disguised as reasonableness review.
constraint_indexing:constraint_classification(equality_code__classification_test_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADVOCACY COALITION (TANGLED ROPE) — Organizations fighting arbitrary classifications (civil rights groups, constitutional scholars) experience both genuine coordination (the test does enable legal challenge to unexplained sorting) and asymmetric extraction (the burden of articulating rationality rests on the challenger, not the legislator). Constrained exit: they can withdraw from litigation but cannot exit the constitutional framework. Suppression is moderate — the classification test provides a doctrinal tool, but the rational nexus standard is indeterminate and systematically disfavors challengers.
constraint_indexing:constraint_classification(equality_code__classification_test_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATIVE AUTHORITY (ROPE) — Legislatures that adopt intelligible differentia and articulate rational nexus experience the classification test as pure coordination. The test rewards and protects rational line-drawing; a legislature with clear policy logic sees Article 14 as enabling rather than constraining. Arbitrage: the legislature can adopt compliant classifications that serve its purposes. The constraint is experienced as cooperative — a framework for legitimate regulatory sorting.
constraint_indexing:constraint_classification(equality_code__classification_test_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIARY (PITON) — Courts applying the classification test engage in performative reasonableness review. The test provides a ritual structure ('intelligible differentia,' 'rational nexus') but the outcome is substantially predetermined by the power asymmetry: courts rarely strike down legislation that legislators defend with any coherent story. The judiciary sees its own doctrine as theatrically applied — proceeding through the categories while rarely invalidating. The test persists through institutional inertia and doctrinal tradition rather than functional rigor.
constraint_indexing:constraint_classification(equality_code__classification_test_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL NECESSITY (MOUNTAIN) — From a civilizational perspective, some form of classification is inherent to legislation: laws must sort agents and contexts into categories (eligibility, benefit, obligation). Classification is structurally necessary; the classification test emerges as a natural gatekeeping mechanism — a sorting mechanism for legitimate vs illegitimate sorting. However, structural data reveals this as a false summit: the classification test is a contingent institutional choice (India's Article 14 implements it; other regimes use different standards). The 'natural necessity' framing naturalizes a specific doctrinal construction.
constraint_indexing:constraint_classification(equality_code__classification_test_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Actors (scholars, movements, emerging jurisprudence) advocating beyond the classification test toward substantive equality standards see the test as a temporary placeholder. The classification test coordinates equality review at low institutional cost (courts don't need to investigate legislative purpose or effects), but it fails to catch discriminatory outcomes from facially neutral categories. Reform movements see the test's sunset: advanced equal protection analysis (disparate impact doctrine, intersectionality, substantive equal opportunity) is building alternative frameworks. The classification test persists as a scaffold while deeper equality principles are still being theorized.
constraint_indexing:constraint_classification(equality_code__classification_test_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_code__classification_test_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equality_code__classification_test_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equality_code__classification_test_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(equality_code__classification_test_reading, TR),
    TR >= 0.70.

:- end_tests(equality_code__classification_test_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The classification test does enable legal challenge to arbitrary categorization — it provides a doctrinal handle that did not exist before. But the test requires the excluded party to articulate why their exclusion is irrational, placing the burden of reasoning-backward from the legislator's silence. The extractiveness reflects this asymmetry: genuine coordination benefit (rational classifications are protected, encouraging legislators to articulate purposes) but asymmetric burden (excluded parties must prove irrationality, not legislatures must prove rationality). Suppression (0.52): Moderate. The burden of articulation suppresses challenges to categories that have some plausible rationale, no matter how post-hoc. Courts rarely demand that legislatures prove a nexus exists; they assess whether a rationale can be articulated. The indeterminacy of 'rational nexus' suppresses aggressive challenges. But suppression is not total — the test does prevent flagrant classifications with no coherent story, and advocacy groups can organize around it. Theater ratio (0.48): Moderate. The test produces ritual application: courts proceed through 'intelligible differentia' and 'rational nexus' categories, performing reasonableness review. But the outcome is substantially predetermined by power asymmetry — courts rarely strike down legislation that has any coherent defense. The theater is rising as the test becomes more formalized and less substantive, but it has not yet reached piton-level (0.70+) because the test still invalidates some classifications that lack any defensible rationale.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates marked perspectival divergence. The legislative actor with a coherent purpose sees coordination (Rope) — the test protects rational line-drawing. The unexplained excluded person sees pure extraction (Snare) — they bear the burden of rationalizing their own exclusion. The advocacy coalition sees mixed dynamics (Tangled Rope) — the test provides a tool but with asymmetric burden. The judiciary sees a degraded ritual (Piton) — the test's mechanical application masks predetermined outcomes. The reform movement sees a temporary framework (Scaffold) — substantive equality standards are building alternatives. The civilizational analyst risks naturalizing the classification framework itself (Mountain) — but the structural data reveals contingency: India's Article 14 implements this test; other jurisdictions use different standards, proving the test is not inherent to equality doctrine. The perspectival gap reveals the test's core tension: it appears neutral (rational nexus is reasonable!) but operationally favors those with articulate purposes and burdens those without voice in the legislative process.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint is driven by the structural relationship to the burden of proof. Legislative actors with coherent purposes face low d (beneficiary + arbitrage exit → d ≈ 0.15). Unexplained excluded persons face high d (victim + trapped exit → d ≈ 0.95). Advocacy coalitions face moderate d (victim + constrained exit, but also beneficiary of coordination mechanism → d ≈ 0.55). The judiciary faces low d (institutional + arbitrage, maintaining the doctrine → d ≈ 0.05). The reform movement faces moderate d (organized + constrained, pushing against the test → d ≈ 0.50). No overrides are needed; the structural derivation captures the asymmetry: those who must articulate suffer higher d; those whose silence is protected experience low d.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the classification test is a specific doctrinal reading, not the only reading of Article 14. The test's moderate extractiveness (0.38) reflects that it genuinely enables challenges to arbitrary sorting (coordination benefit) but burdens challengers with proving irrationality (asymmetric extraction). The piton perspective (judiciary) reveals that the test's mechanical application masks predetermined outcomes — reasonableness review rarely invalidates defended legislation. The scaffold perspective (reform movement) reveals that the test is being supplemented by substantive equality standards that directly examine outcomes rather than rationality of categories. The mountain perspective (analytical) is a false summit: the test appears to be a natural necessity for law (law must categorize; the test is the natural way to review categorization) but decomposition reveals it as a contingent institutional choice. Other jurisdictions, other eras of Indian jurisprudence, and emerging interpretations use different frameworks. The mandatrophy resolves by showing that all six types are structurally justified: the test genuinely is a coordination mechanism (rope) when legislatures have coherent purposes, genuinely is a snare when applied to unexplained exclusions, genuinely is a scaffold when courts are building toward more substantive equality standards, and genuinely is a piton when mechanically applied without substantive inquiry. The presheaf over the interpretation site (the multiple readings) is the answer — not a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_nexus_indeterminacy,
    'What counts as a rational nexus? Is any coherent story sufficient, or must the nexus be necessary or proportional to the stated purpose?',
    'Comparative jurisprudence: analysis of which rationales courts accept/reject; empirical study of whether rational-nexus test correlates with legislative intent or post-hoc rationalization patterns',
    'If ''any coherent story'': test becomes permissive rubber stamp (extraction masked as review). If ''necessary/proportional'': test becomes demanding equal protection standard (suppression of arbitrary classification becomes substantive). Classification changes from Rope (beneficiary view) to Snare (victim view) as the standard tightens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rational_nexus_indeterminacy, conceptual, 'Ambiguity in what qualifies as rational nexus to purpose').

omega_variable(
    burden_allocation_asymmetry,
    'Should the burden of articulating rational nexus rest on the challenger (current doctrine) or the legislator (affirmative defense model)?',
    'Doctrinal analysis of precedent; comparative study of burden-shifting in equal protection regimes; empirical outcome analysis comparing success rates under different burden allocations',
    'Current burden on challenger maintains suppression at 0.52; if shifted to legislator, suppression drops and the constraint becomes less extractive (Rope or Scaffold). If burden remains on challenger and rational nexus bar remains indeterminate, suppression rises to 0.65+ and classification shifts to pure Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_allocation_asymmetry, preference, 'Which party bears burden of articulating rational nexus').

omega_variable(
    intelligible_differentia_definition,
    'Does intelligible differentia require the differentiating factor itself to be relevant to the legislative objective, or only that the differentiating factor consistently sorts agents (relevance immaterial)?',
    'Doctrinal genealogy of the test (tracing to the European or Indian origins); textual analysis of the defining cases; assessment of whether courts demand relevance or accept arbitrary-but-consistent sorting',
    'If relevance required: test screens out many facially neutral but designed-discriminatory classifications (suppression drops, becomes Tangled Rope or Rope). If consistency sufficient: test fails to catch purposeful discrimination hidden behind neutral criteria (suppression rises, becomes Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligible_differentia_definition, conceptual, 'Whether differentiating factor must be relevant to legislative objective').

omega_variable(
    reading_vs_arbitrariness_doctrine_foreclosure,
    'Does the classification test''s focus on rational nexus foreclose the arbitrariness doctrine reading, which holds that caprice itself (regardless of classification) violates Article 14?',
    'Jurisprudential analysis: can a case be decided under the classification test without deciding whether unexplained action is arbitrary as a standalone violation? Are courts applying both standards simultaneously or choosing between them?',
    'If foreclosed: the two readings cannot coexist in a single coherent framework; the kernel''s interpretation vector bifurcates. If coexistent: courts can apply both tests in sequence without contradiction, holding that classification requires rational nexus AND unexplained categorization is independently arbitrary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_arbitrariness_doctrine_foreclosure, conceptual, 'Foreclosure relationship between classification test and arbitrariness doctrine').

omega_variable(
    state_vs_private_power_boundary,
    'Does the classification test apply to private power (clubs, employers, platforms)? If not, does this represent a structural limitation of Article 14 or a contingent jurisdictional boundary?',
    'Doctrinal analysis: tracing the state-action requirement in equality jurisprudence; examination of cases addressing private discrimination and whether the classification test''s rational-nexus language appears; comparative study of whether other jurisdictions extend equal protection to private categorization',
    'If classification test applies to private power: the horizontal reach reading''s boundary question is answered (Article 14 applies widely). If it does not: the horizontal reading identifies a genuine structural lacuna (private classification unconstrained). Current doctrine suggests non-applicability to pure private action, supporting the horizontal reading''s claim that private power is the frontier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_vs_private_power_boundary, empirical, 'Whether classification test applies to private power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_code__classification_test_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial_royappa_era, equality_code__classification_test_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_post_mandate_expansion, equality_code__classification_test_reading, theater_ratio, 5, 0.43).
narrative_ontology:measurement(theater_contemporary, equality_code__classification_test_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(extract_initial_narrow_test, equality_code__classification_test_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(extract_mid_judicial_application, equality_code__classification_test_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(extract_contemporary_burden_shift, equality_code__classification_test_reading, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_code__classification_test_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_code__classification_test_reading, equality_code__arbitrariness_doctrine_reading).
narrative_ontology:affects_constraint(equality_code__classification_test_reading, equality_code__horizontal_reach_question_reading).

% DUAL FORMULATION NOTE:
% The equality_code kernel admits three structurally distinct readings, each instantiating a different constraint with different ε values and victim sets. This story (classification_test_reading) focuses on rational line-drawing as the operative equality guarantee (ε=0.38). The arbitrariness_doctrine_reading would focus on unexplained state action itself (lower ε, around 0.18-0.25). The horizontal_reach_reading would focus on the state-vs-private boundary (different ε, around 0.42-0.52, and different victim set: those excluded by private power). All three are readings of the same statutory text; they are not observable-dependent variants of one constraint. The ε-invariance principle applies: when a single text admits readings that entail different extraction mechanisms, victim sets, and core structural tensions, those readings are different constraints. They are linked through network.affects_constraints to signal that they are siblings in the same interpretive family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
