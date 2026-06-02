% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Equality Clause Scope: Expansive Universalist Reading
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   The expansive universalist reading of the Equality Clause treats the
 *   principle 'all humans are equal' as a self-evident universal truth that
 *   binds all interpreters regardless of historical exclusions. This reading
 *   instantiates one contested kernel — the meaning and scope of 'equality'
 *   in constitutional text — against two sibling readings: the restrictive
 *   originalist reading (scope limited by framers' intent and original
 *   understanding) and the progressive textualist reading (scope determined
 *   by text-level interpretation of 'all' without appeal to original intent
 *   or self-evidence claims). The expansive universalist distinguishes itself
 *   by grounding equality in a claimed self-evident universal principle that
 *   transcends historical context and makes historical exclusions into
 *   hypocrisy to be corrected rather than binding precedent. This reading has
 *   driven major jurisprudential developments: Women's suffrage and voting
 *   rights (19th Amendment interpretation), civil rights era equality
 *   doctrine (14th Amendment expansion), LGBTQ+ rights recognition
 *   (Obergefell), and ongoing debates over affirmative action, disability
 *   rights, and economic justice. The constraint reveals itself through a
 *   perspectival gap: excluded groups experience the universal claim as a
 *   snare (false promise binding without protecting), while progressive
 *   interpreters experience it as rope (enabling coordination for rights
 *   protection), originalists experience it as tangled rope (providing
 *   coordination benefit for narrow readings while eroding their interpretive
 *   authority), and status quo gatekeepers experience it as piton
 *   (performative compliance). The analytical observer risks naturalizing
 *   this contested institutional arrangement as a self-evident truth rather
 *   than analyzing it as a structural claim with identifiable beneficiaries
 *   and costs.
 *
 * KEY AGENTS:
 *   - Historically Excluded Groups (powerless/trapped) — racial minorities, women, LGBTQ+ persons, religious minorities, persons with disabilities. Experience the universal equality claim as snare when institutional practice contradicts it. Primary victims of restrictive readings; primary beneficiaries of expansive interpretations.
 *   - Progressive Judiciary and Constitutional Interpreters (institutional/arbitrage) — judges, legal scholars, civil rights advocates committed to expansive equality reading. Primary beneficiaries: the reading provides interpretive authority and enables rights adjudication. Experience constraint as rope (pure coordination).
 *   - Originalist Constitutional Authority (powerful/mobile) — justices, scholars, originalist legal tradition committed to bounded interpretation. Experience expansive reading as tangled rope: coordination benefit (stable meaning from fixed text) alongside authority erosion (each expansion constrains their interpretive scope).
 *   - Status Quo Institutional Gatekeepers (institutional/constrained) — state legislatures, corporations, professional guilds, hierarchical institutions benefiting from restrictive equality scope. Experience expansive reading as piton: performative compliance (diversity initiatives, anti-discrimination policies) without structural change.
 *   - Civil Rights Movements and Advocacy Organizations (moderate/constrained) — NAACP, women's suffrage movements, LGBTQ+ rights groups, disability rights organizations. Experience expansive reading as tangled rope: genuine coordination benefit (legal claims enabled) alongside suppression (political opposition, institutional resistance).
 *   - Analytical Observer (analytical/analytical) — constitutional theorist viewing from civilizational scope. Risks treating the expansive reading as self-evident natural law (mountain) rather than as one contested interpretation among three.
 *   - General Public / Democratic Majority (moderate/mobile) — voters, citizens whose democratic will either supports or opposes equality expansion. Their relationship to the constraint depends on time horizon and which groups they identify with.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.38).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.42).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Equality Clause Scope: Expansive Universalist Reading").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '54a53b9b-4d39-4a89-aa8f-791e8fd843ba').
narrative_ontology:cs_kernel_codification('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', fixed_text).
narrative_ontology:cs_authority_grounding('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', lineage).
narrative_ontology:cs_interpretation_layer_present('54a53b9b-4d39-4a89-aa8f-791e8fd843ba').
narrative_ontology:cs_reading_relation('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', foundational, universal_moral_equality_foundational).
narrative_ontology:cs_axiom_status(universal_moral_equality_foundational, holdable).
narrative_ontology:cs_axiom_grounding('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', universal_moral_equality_foundational, deontological).
narrative_ontology:cs_axiom('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', foundational, historical_exclusions_contingent_hypocrisy).
narrative_ontology:cs_axiom_status(historical_exclusions_contingent_hypocrisy, holdable).
narrative_ontology:cs_axiom_grounding('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', historical_exclusions_contingent_hypocrisy, deontological).
narrative_ontology:cs_reference_frame('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', universal_moral_equality_framework).
narrative_ontology:cs_drift_state('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', contemporary_post_civil_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('54a53b9b-4d39-4a89-aa8f-791e8fd843ba', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, progressive_interpreters).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, institutional_status_quo).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, originalist_jurisprudence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICALLY EXCLUDED GROUPS (SNARE) — Those denied equal protection under a narrow, originalist reading of 'equality' (women, racial minorities, LGBTQ+ persons in most historical periods) face maximum extraction: the constraint purports to be universal ('all men are created equal') while institutional practice excludes them systematically. They cannot exit this classification without access to courts, political power, or cultural change. The snare is the gap between universalist aspiration and particularist application — the false promise of equality that binds without protecting.
constraint_indexing:constraint_classification(equality_clause_scope__expansive_universalist, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS MOVEMENTS (TANGLED ROPE) — Moderate-power organized actors (NAACP, women's suffrage movements, LGBTQ+ rights groups) experience genuine coordination benefit (the expansive reading enables their legal claims) alongside real suppression (political opposition, institutional resistance, violence). They benefit from the universal framing while bearing costs of enforcing it. Moderate exit options — they can form coalitions and pressure the system, but cannot escape the constraint's jurisdiction.
constraint_indexing:constraint_classification(equality_clause_scope__expansive_universalist, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROGRESSIVE JUDICIARY (ROPE) — Institutional actors committed to expansive interpretation experience the constraint as pure coordination: the universalist reading solves the coordination problem of how to adjudicate rights claims beyond the original enumeration. They have arbitrage options (they can cite precedent, legislative guidance, or international norms) and benefit from the interpretive latitude the expansive reading provides. Minimal extraction experienced — the constraint enables their institutional function.
constraint_indexing:constraint_classification(equality_clause_scope__expansive_universalist, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORIGINALIST CONSTITUTIONAL AUTHORITY (TANGLED ROPE) — The originalist reading (restrictive scope) provides genuine coordination benefit: it anchors constitutional interpretation to a fixed text, reducing judicial discretion and political capture. But it also imposes extraction costs on the originalist framework itself — each expansion of equality constrains the originalist's interpretive authority, eroding their claim to 'faithful' reading. They experience this as a loss of institutional authority masquerading as legal interpretation. Mobile exit options at the civilizational horizon — the originalist framework can evolve or be superseded — but biographical-level constraints bind them to defend a narrowing reading.
constraint_indexing:constraint_classification(equality_clause_scope__expansive_universalist, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATUS QUO GATEKEEPERS (PITON) — Institutions that benefited from restrictive equality readings (state legislatures, corporate gatekeepers, hierarchical professions) experience the expansive reading as corrosive to their authority. They maintain performative compliance with equality norms (hiring diversity officers, issuing anti-discrimination policies) while resisting structural change. Theater ratio is high because much institutional 'diversity work' is theatrical maintenance rather than functional equality coordination. The constraint persists through inertia and superficial performance, not because the restrictive reading is still operationally stable.
constraint_indexing:constraint_classification(equality_clause_scope__expansive_universalist, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing analytical perspective, the claim that 'equality is self-evident and applies to all humans' can appear as a mountain — an immutable truth independent of historical exclusions or institutional practice. This view treats the universal principle as a natural law that institutional arrangements either reflect or violate. However, this classification is suspect: the structural data shows beneficiaries (progressive interpreters, historically excluded groups seeking rights), victims (institutional gatekeepers facing authority erosion), and active enforcement (courts imposing expanded readings). The mountain here naturalizes what is actually a contested institutional arrangement about the scope of an authoritative text.
constraint_indexing:constraint_classification(equality_clause_scope__expansive_universalist, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equality_clause_scope__expansive_universalist, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equality_clause_scope__expansive_universalist, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, TR),
    TR >= 0.70.

:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The expansive universalist reading extracts genuine benefits for progressive interpreters and historically excluded groups seeking rights adjudication, but the extraction is not as severe as the status quo extraction it displaces. The reading enables new claimants while constraining originalist authority — the net extraction flow depends on perspective. Measured at the moderate/constrained civil rights advocate level. Suppression (0.42): Moderate. Significant barriers to equality enforcement include: institutional resistance (state legislatures, corporations defending status quo), constitutional litigation costs, political opposition, cultural inertia, and doctrinal contestation (originalists and textualists offer competing readings). But suppression is declining (measured at time_point 100): civil rights infrastructure has matured, precedent has accumulated, and social norms have shifted. Theater ratio (0.55): Moderate-high, rising. At time_point 0 (founding era), theater was low because equality principle had not yet entered practical application. At time_point 50 (mid-20th century), institutional theater rose as courts developed equality doctrine while actual exclusions persisted. At time_point 100 (contemporary), theater has stabilized as institutions perform diversity compliance while resisting deeper structural change. The measurement trajectory shows theater rising as enforcement mechanisms become routinized and performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits extreme perspectival divergence. Excluded groups see a snare (false promise); excluded groups with advocacy power see tangled rope (mixed coordination and suppression); progressive interpreters see rope (pure coordination); originalists see tangled rope from the opposite direction (coordination in bounded reading, extraction as authority erosion); status quo institutions see piton (degraded ritual); and the analytical observer risks seeing mountain (self-evident truth). The gap is not merely different weightings of the same facts — it represents genuinely different structural experiences of the same constraint. The universal equality claim functions simultaneously as a binding force (for those excluded from its protection), a liberation mechanism (for those seeking expanded rights), an authority constraint (for originalist interpreters), a performative cover (for institutional gatekeepers), and a coordination anchor (for rights advocates). The falsity of the mountain perspective is revealed by the existence of these other perspectives: if equality were truly self-evident and universal, the constraint would not need institutional enforcement, legal interpretation, or political struggle. Its mundane contestation proves it is not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The expansive universalist reading generates directionality flows that differ dramatically by agent power and position. Beneficiaries (historically excluded groups, progressive interpreters) derive low d values — they experience the reading as enabling, not extractive. But their low d reflects their structural position as reading-beneficiaries, not their individual power levels (powerless agents and institutional agents both benefit). The victims (originalist authority, status quo gatekeepers) experience higher d values — the reading constrains their authority or requires institutional change. The originalist case is instructive: a powerful agent (institutional jurist) experiences the constraint as extractive because it erodes their interpretive authority, even though they have significant power to resist. Directionality is not determined by power alone but by the structural flow of benefit and cost through the constraint. The engine computes d from beneficiary/victim declarations and exit options, producing the perspectival gap visible in the six perspectives: powerless excluded group d ≈ 0.92 (full target), institutional progressive interpreter d ≈ 0.12 (full beneficiary), powerful originalist d ≈ 0.65 (mixed burden and benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive universalist reading resolves mandatrophy by treating the constraint as genuinely tangled rope from institutional-moderate perspectives (those with power to enforce or resist): it provides real coordination benefit (enabling rights adjudication) alongside real extraction (constraining alternative readings, empowering progressive interpreters, displacing originalist authority). From the powerless perspective (excluded groups), it appears as snare because the coordination benefit is theoretical (the reading must be enforced through institutional struggle) while the suppression is structural (barriers to enforcement remain high). From the status quo perspective, it appears as piton because the constraint has become institutional ritual (performed without structural change). The mandatrophy is not resolved by showing 'the real type' but by showing that all six types are structurally legitimate from their respective positions. The classification is not undetermined — each perspective has a determinate classification. The underdetermination is at the meta-level: which perspective's classification should dominate? The expansive universalist reading claims the answer is the universalizing perspective (hence 'mountain' from the analytical observer), but this claim is precisely what the tangled rope classification challenges: the universalizing perspective naturalizes what is actually a contested institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_evidence_vs_construction,
    'Is the universal equality principle genuinely self-evident and historically binding, or is it constructed through successive legal and political struggle?',
    'Historical analysis: compare the principle''s actual historical scope (who was included/excluded at each period) against the claim of self-evidence. If self-evident, inclusion should have been immediate; construction is demonstrated by incremental struggle and resistance.',
    'If self-evident: mountain classification holds and the constraint is a natural law. If constructed: false summit detection fires, revealing the expansive reading as a contingent institutional interpretation, not a discovered universal truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_evidence_vs_construction, conceptual, 'Whether equality is self-evident natural truth or constructed through political struggle').

omega_variable(
    original_intent_empirical_scope,
    'What was the actual empirical scope of ''all men are created equal'' in 1776, and does historical evidence show the framers believed they were stating a universal principle or a limited one?',
    'Scholarly analysis of founding-era documents, framers'' correspondence, and state-level equality provisions. Cross-reference with actual implementation: women, enslaved persons, indigenous peoples, non-property-owners were explicitly excluded from legal protections.',
    'If universal intent: expansive reading is fidelity to original meaning, not judicial activism. If limited intent: expansive reading is genuine interpretation beyond original scope, validating the ''contestation'' framing. This determines whether ''original intent'' favors expansive or restrictive reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_empirical_scope, empirical, 'Historical scope of equality principle at founding').

omega_variable(
    legitimacy_ground_for_expansion,
    'What legitimacy standard justifies expanding equality scope beyond the original text and intent? Is it majoritarian democracy, moral philosophy, pragmatic institutional stability, or something else?',
    'Legal philosophy analysis: review justifications courts have offered for expansive readings (substantive due process, evolving standards of decency, moral progress, lived experience of excluded groups). Assess internal coherence and whether the standard applies to other constitutional provisions.',
    'If legitimacy standard is internal to constitutional tradition (reliance on precedent, institutional evolution): expansive reading is structurally justified within the framework. If legitimacy depends on external moral standards: the expansion is contestable as an importation of non-constitutional values into interpretation, shifting classification toward snare (extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_ground_for_expansion, conceptual, 'Legitimacy ground for expanding equality scope beyond original formulation').

omega_variable(
    institutional_capture_risk,
    'Does the expansive universalist reading enable genuine rights protection, or does it risk becoming a vector for institutional capture — courts imposing equality that serves elite interests while marginalizing community norms or alternative values?',
    'Case analysis: compare outcomes of expansive equality readings in rights-protective cases (civil rights, LGBTQ+ equality) vs. cases where equality language serves elite gatekeeping (property rights, corporate personhood, privatization). Examine whose interests are advanced when ''universal'' principles are applied.',
    'If predominantly protective: expansive reading is net coordination benefit, snare/tangled rope classification is stable. If mixed outcomes: the constraint itself is a site of contestation where universal language serves multiple masters, shifting classification toward piton (degraded institutional machinery performing multiple functions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_risk, empirical, 'Risk of institutional capture via expansive equality reading').

omega_variable(
    horizon_specific_beneficiary_set,
    'Does the expansive universalist reading produce the same beneficiary set across time horizons? Are historically excluded groups the long-term beneficiaries, or does expansion dynamics shift benefits over generations?',
    'Longitudinal analysis: track which groups benefit from expansive readings in period 1 (civil rights era), period 2 (post-civil rights institutionalization), period 3 (contemporary rights expansion). Assess whether benefits persist or redistribute.',
    'If consistent beneficiary set: expansive reading is stable coordination mechanism, tangled rope classification holds. If shifting beneficiaries: the reading itself becomes a stage in a longer process where initial expansion (snare→rope) becomes institutionalized gatekeeping (rope→piton), suggesting the constraint''s type is horizon-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizon_specific_beneficiary_set, empirical, 'Stability of beneficiary set across time horizons for expansive equality reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equ_exp_tr_t0, equality_clause_scope__expansive_universalist, theater_ratio, 0, 0.35).
narrative_ontology:measurement(equ_exp_tr_t50, equality_clause_scope__expansive_universalist, theater_ratio, 50, 0.48).
narrative_ontology:measurement(equ_exp_tr_t100, equality_clause_scope__expansive_universalist, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(equ_exp_be_t0, equality_clause_scope__expansive_universalist, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(equ_exp_be_t50, equality_clause_scope__expansive_universalist, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(equ_exp_be_t100, equality_clause_scope__expansive_universalist, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(equ_exp_su_t0, equality_clause_scope__expansive_universalist, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(equ_exp_su_t50, equality_clause_scope__expansive_universalist, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(equ_exp_su_t100, equality_clause_scope__expansive_universalist, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, judicial_authority_scope__constitutional_interpretation).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, civil_rights_enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The equality clause constraint family decomposes into three structurally distinct constraints corresponding to three readings of the same constitutional text (the Fourteenth Amendment's 'equal protection of the laws'). Each reading has different ε values: expansive_universalist (0.38, tangled rope at moderate/constrained civil rights advocate perspective), restrictive_originalist (0.25, rope at originalist interpreter perspective), progressive_textualist (0.32, tangled rope at textualist perspective). The readings do not coexist in a single reading of the constraint — they represent genuinely different claims about what the constraint is. They are linked through network.affects_constraints as siblings in a kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, powerless, 0.92).
constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, institutional, 0.12).
constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
