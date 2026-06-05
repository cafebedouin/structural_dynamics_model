% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel_flat_control, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_kernel_flat_control
 *   human_readable: Equal Protection Clause: Constitutional Text Governing State Racial Classification
 *   domain: constitutional_law/civil_rights/education_policy
 *
 * SUMMARY:
 *   The Equal Protection Clause of the Fourteenth Amendment, ratified in
 *   1868, exists as a stabilized constitutional text whose legitimacy is
 *   grounded in lineage (Reconstruction-era commitment to prevent state
 *   discrimination) and formal constitutional authority. Since its
 *   stabilization, the clause has functioned as both a coordination mechanism
 *   (states need a framework for managing racial distinctions in education
 *   and public life) and an extraction mechanism (the text's ambiguity
 *   permits institutional actors to benefit from preserving status quo
 *   hierarchies while claiming constitutional protection). The constraint
 *   exhibits tangled rope properties: a genuine coordination problem (how
 *   should states handle racial composition of public schools?) exists
 *   alongside asymmetric extraction (minorities bear the cost of delayed
 *   remediation while states and elite institutions benefit from preserving
 *   discretion). The theater ratio (0.68) reflects that doctrinal
 *   pronouncements (Brown v. Board's declaration that separate is inherently
 *   unequal, the shift toward colorblindness in recent decades) often mask
 *   persistent operational extraction through procedural mechanisms (intent
 *   requirements, remedial limitations, deference to institutional actors).
 *   Suppression remains substantial (0.62) because alternatives to the Equal
 *   Protection framework—direct legislative remediation, constitutional
 *   amendment, institutional reform without judicial oversight—face high
 *   barriers. The constraint has experienced mandate drift: the original
 *   mandate (equal protection for freed slaves and their descendants) has
 *   been obscured by doctrinal developments that operationally protect state
 *   discretion and institutional access concentrations.
 *
 * KEY AGENTS:
 *   - Racial Minorities and Historically Excluded Groups: Primary victims (powerless/trapped) — depend on the constraint's enforcement but trapped within its doctrinal limitations; bear maximum extractiveness
 *   - State Institutional Actors (School Boards, State Legislatures, State Executives): Primary beneficiaries (powerful/constrained) — face coordination requirements but benefit from ambiguous doctrine allowing preservation of status quo distributions
 *   - Elite Educational Institutions (Universities, Selective Schools): Secondary beneficiaries (organized/arbitrage) — use institutional deference and doctrinal ambiguity to manage selective enrollment while maintaining legal claims to race-neutrality
 *   - Federal Judiciary (Supreme Court, Circuit Courts): Enforcement machinery (institutional/arbitrage) — maintains the constraint through doctrinal interpretation; theater ratio suggests enforcement is increasingly performative rather than remedial
 *   - Civil Rights Organizations and Advocates: Moderate actors (moderate/constrained) — use the constraint's text to advance equality but constrained by restrictive doctrines and procedural barriers
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the constraint as universal constitutional principle rather than examining its function in operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel_flat_control, 0.48).
domain_priors:suppression_score(equal_protection_kernel_flat_control, 0.62).
domain_priors:theater_ratio(equal_protection_kernel_flat_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel_flat_control, extractiveness, 0.48).
narrative_ontology:constraint_metric(equal_protection_kernel_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equal_protection_kernel_flat_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel_flat_control, "Equal Protection Clause: Constitutional Text Governing State Racial Classification").
narrative_ontology:topic_domain(equal_protection_kernel_flat_control, "constitutional_law/civil_rights/education_policy").

domain_priors:requires_active_enforcement(equal_protection_kernel_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel_flat_control, 'd738cef1-f11a-44c3-8c55-d60a415b6a04').
narrative_ontology:cs_kernel_codification('d738cef1-f11a-44c3-8c55-d60a415b6a04', fixed_text).
narrative_ontology:cs_authority_grounding('d738cef1-f11a-44c3-8c55-d60a415b6a04', lineage).
narrative_ontology:cs_interpretation_layer_present('d738cef1-f11a-44c3-8c55-d60a415b6a04').
narrative_ontology:cs_created_at('d738cef1-f11a-44c3-8c55-d60a415b6a04', '2026-02-26T00:00:00Z').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(equal_protection_kernel_flat_control, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel_flat_control, institutional_state_actors).
narrative_ontology:constraint_beneficiary(equal_protection_kernel_flat_control, elite_educational_institutions).
narrative_ontology:constraint_victim(equal_protection_kernel_flat_control, racial_minorities).
narrative_ontology:constraint_victim(equal_protection_kernel_flat_control, historically_excluded_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RACIAL MINORITIES (SNARE) — Trapped within the text that claims to protect them but whose enforcement depends on institutional actors who benefit from preserving access restrictions. The Equal Protection Clause itself becomes extractive when the doctrine it authorizes (strict scrutiny for affirmative action, colorblindness framing) is weaponized to prevent remedial measures. Victims bear maximum extractiveness without exit option.
constraint_indexing:constraint_classification(equal_protection_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE ACTORS (TANGLED ROPE) — Face genuine coordination problem: must allocate educational resources and manage racial composition of schools while respecting constitutional constraints. But also benefit from the ambiguity in the text — 'strict scrutiny' and 'colorblindness' doctrines allow states to restrict remedial action (desegregation busing, affirmative admission) while claiming constitutional virtue. Mixed coordination (must manage diversity) and extraction (constrained targets while claiming protection).
constraint_indexing:constraint_classification(equal_protection_kernel_flat_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE INSTITUTIONS (ROPE) — Experience the clause as coordination: the text provides a framework for managing admissions that appears race-neutral while allowing selective enforcement. Institutions benefit through access to the text's authority without bearing the cost of remediation. The constraint coordinates institutional practice (admissions process) while the text's ambiguity permits extraction of competitive advantage through enrollment management.
constraint_indexing:constraint_classification(equal_protection_kernel_flat_control, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL JUDICIARY (PITON) — The enforcement machinery has become increasingly performative. Landmark doctrines (Plessy's 'separate but equal,' Brown's rejection thereof, the pivotal role of intentional discrimination standards) ostensibly changed the constraint's operation, but the theatre of doctrinal evolution masks underlying institutional inertia. Courts maintain the constraint through ritual adjudication and interpretation rather than through effective remediation. Theater ratio reflects the gap between doctrinal pronouncement and actual equalization of educational access.
constraint_indexing:constraint_classification(equal_protection_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL RIGHTS ADVOCATES (TANGLED ROPE) — Face coordination problem: must use the same text (Equal Protection Clause) that constrains remedial action to advance equality. Also extractive through procedural barriers — litigation is resource-intensive, standing requirements restrict who can sue, and adverse precedent accumulates. Mixed position: advocates benefit from the text's existence and authoritative status, while constrained by doctrinal developments that weaponize it against remediation.
constraint_indexing:constraint_classification(equal_protection_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — Risk perspective: from the universalizing analytical view, the Equal Protection Clause might appear as a natural law of constitutional democracy itself — an immutable principle that law-governed states must recognize. This naturalizes what is actually a contestable reading of a specific historical text produced through specific political struggles. The mountain classification is a false summit candidate: the 'universal principle' framing obscures how the text's authority is constitutively dependent on continuous institutional acceptance and enforcement choices.
constraint_indexing:constraint_classification(equal_protection_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_kernel_flat_control, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(equal_protection_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(equal_protection_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, reflecting the asymmetry between the clause's stated mandate (equal protection) and its actual operation (preservation of institutional discretion and status quo hierarchies). The value is not maximal (0.72+) because some remedial machinery exists and some states have pursued integration efforts; but it is substantially elevated above pure coordination (0.15) because the constraint permits institutional actors to benefit from delay and doctrinal ambiguity. Suppression (0.62): Moderate-high. Substantial barriers exist to alternatives: constitutional amendment is practically impossible, direct legislative remediation faces judicial review under the same clause, and institutional reform outside the legal system faces capacity constraints. However, suppression is not total — some remedial paths remain (targeted litigation, legislative action within doctrinal bounds), and alternative enforcement mechanisms (community organizing, institutional pressure) exist at high cost. Theater ratio (0.68): High and rising. Brown v. Board (1954) was a powerful doctrinal pronouncement that appeared to establish clear equal protection protection, yet operational desegregation proceeded slowly and incompletely. Subsequent doctrinal shifts (intent requirement in Washington v. Davis, colorblindness in Parents Involved v. Seattle, voting rights restrictions in Shelby County v. Holder) used the language of equal protection to restrict remediation. The theater consists of the gap between doctrinal pronouncements and actual equalization; the ratio has risen as doctrinal restrictiveness has increased while segregation persists. At t=70 (contemporary), theater ratio remains high (0.68) despite the appearance of settled doctrine, indicating the constraint maintains ritual pronouncements of equal protection while operationally preserving institutional access concentrations.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival disagreement. Racial minorities trapped within the system experience snare classification (pure extraction masked by constitutional language). State actors face tangled rope (genuine coordination problems with extraction opportunities). Elite institutions experience rope (coordination that preserves their position). The judiciary experiences piton (enforcement has become performative). Civil rights advocates face tangled rope (using the text constrains their options). The analytical observer risks mountain (naturalizing the constraint as universal constitutional principle). This perspectival gap reveals that the disagreement is not about facts but about structural position: the same text simultaneously protects (from an institutional perspective) and extracts (from a powerless perspective). The gap is not resolvable through doctrinal argument alone because it is structural in the text's design.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent's structural relationship to the constraint. Racial minorities classified as victims with trapped exit → d approaches 1.0 (full target). State actors classified as beneficiaries with constrained exit → d moderate to high (constrained targets who also benefit). Elite institutions classified as beneficiaries with arbitrage exit → d approaches 0.0 (full beneficiary). Judiciary classified as beneficiary (maintains institutional authority through constraint enforcement) with arbitrage exit → d low. Civil rights advocates classified as victims with constrained exit (can use the text but constrained by doctrine) → d elevated but not maximal. The analytical observer → d = 0.5 (no structural stake). Suppression is a raw structural property (unscaled): the constraint's suppression of alternatives remains stable across all indices because all agents face the same doctrinal barriers and amendment difficulties. Only directionality (and thus effective extraction chi) varies by position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE DIVERGENCE DETECTED. The Fourteenth Amendment's original mandate was equal protection for freed slaves and their descendants against state discrimination. Contemporary operation of the Equal Protection Clause has evolved such that institutional actors (states, elite institutions, the judiciary itself) can claim fidelity to the clause while operationally preserving the hierarchies the clause was designed to dismantle. This is not a case where the mandate has been fulfilled and the constraint has become unnecessary — on the contrary, racial inequality persists in education, wealth, and institutional access. Rather, the constraint's function has shifted from remediation to legitimation: it provides constitutional authority for the status quo while constraining the remedies available to address inequality. This is mandatrophy. The constraint maintains its formal commitment (equal protection) while its operative function (preservation of institutional discretion and access concentrations) diverges from the stated purpose. The theater ratio's rise over time reflects this divergence: each doctrinal development (from expansive interpretation post-Brown to restrictive interpretation in recent decades) claimed fidelity to the Equal Protection Clause while operationally narrowing remediation options. Mandatrophy resolution would require either (a) radical reinterpretation of the clause to align its operation with its stated mandate, (b) constitutional amendment to replace or clarify the clause, or (c) institutional reform that breaks the cycle of doctrinal restriction and performative enforcement. The constraint's current status is unresolved mandatrophy with high institutional entrenchment preventing reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_discrimination_threshold,
    'Does the ''intentional discrimination'' standard for identifying equal protection violations capture actual mechanisms of racial inequality, or does it naturalize structural discrimination that persists without explicit intent?',
    'Comparative analysis: measuring outcomes under intentional discrimination vs structural discrimination framings; empirical study of whether acknowledged intent is actually recoverable from institutional records; cross-jurisdictional variation in how intent is inferred',
    'If intentional discrimination standard captures actual mechanisms: snare classification for minorities is overstated; constraint approximates rope. If structural discrimination operates outside the standard: snare classification confirmed; constraint enables extraction by hiding behind intent requirement. Terminal classification depends on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentional_discrimination_threshold, empirical, 'Whether intentional discrimination standard captures actual mechanisms of racial inequality').

omega_variable(
    colorblind_doctrine_extractiveness,
    'Does the colorblind interpretation of the clause (race must never be considered by state actors) protect racial minorities by preventing racial discrimination, or does it extract from minorities by preventing remedy measures and institutionalizing status quo distributions?',
    'Longitudinal comparison of racial achievement gaps, educational access, and wealth distribution under colorblind vs race-conscious enforcement periods; meta-analysis of remediation effectiveness; historical trajectory of segregation and integration under different doctrinal regimes',
    'If colorblindness protects: extractiveness drops, constraint appears more rope-like to powerless agents. If colorblindness institutionalizes inequality: extractiveness confirmed as high, snare classification sustained. This is the central contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_doctrine_extractiveness, empirical, 'Whether colorblind doctrine protects or extracts from racial minorities').

omega_variable(
    affirmative_action_compensation_sufficiency,
    'Do race-conscious remedies (affirmative action in admissions, desegregation remediation, targeted funding) actually compensate for historical and ongoing structural discrimination, or do they prove insufficient relative to the scale of accumulated disadvantage?',
    'Comparison of remedial measures enacted vs documented structural inequalities; longitudinal study of whether race-conscious policies narrow or maintain achievement gaps; cost-benefit analysis of remediation vs status quo approaches',
    'If remedies are sufficient: the constraint''s extractiveness is offset by effective remediation machinery; coordination function dominates. If remedies are structurally insufficient: extraction dominates; the constraint becomes snare-classifying for minorities. Current evidence suggests insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(affirmative_action_compensation_sufficiency, empirical, 'Whether affirmative action and remediation measures sufficiently compensate for structural inequality').

omega_variable(
    doctrinal_stability_vs_institutional_capture,
    'Is the shift in equal protection doctrine (from Warren Court expansive view toward Rehnquist and Roberts Court restrictive view) driven by evolving constitutional interpretation, or by institutional capture of the judiciary by state interests and elite institutions?',
    'Analysis of judicial composition changes and funding sources; comparison of doctrinal developments against empirical conditions (did conditions improve, justifying narrower doctrine?); study of amicus brief patterns and institutional pressure on the Court',
    'If doctrinal shift is constitutionally justified: the constraint''s classification reflects genuinely evolved understanding, piton classification is overstated. If institutional capture: doctrinal restrictiveness is performative (theater), piton classification confirmed, underlying extraction mechanism persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_stability_vs_institutional_capture, empirical, 'Whether doctrinal shifts reflect constitutional interpretation or institutional capture').

omega_variable(
    mandate_vs_implementation_gap,
    'The Fourteenth Amendment''s mandate is equal protection; has the constraint''s actual function in operation evolved to something else — maintenance of institutional stability, protection of elite institutional access, preservation of state discretion — such that mandatrophy has occurred?',
    'Historical comparison of stated equal protection mandate vs actual distributional outcomes; analysis of how courts weigh equal protection against other constitutional values (federalism, institutional deference, state sovereignty); measurement of whether remedial machinery actually produces equalization',
    'If mandate and implementation align: constraint is functioning as designed (coordination with extraction gaps). If mandate has been hollowed: mandatrophy is occurring; the constraint maintains the appearance of equal protection while operationally preserving inequality. Current evidence suggests mandate has substantially diverged from implementation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_vs_implementation_gap, empirical, 'Whether equal protection mandate has diverged from actual implementation function').

omega_variable(
    institutional_entrenchment_of_text,
    'The Equal Protection Clause exists as a stabilized textual commitment grounded in lineage (Reconstruction-era origin, formal constitutional status). To what extent does this institutional entrenchment prevent revision of the text''s interpretation, even when interpretation produces outcomes contrary to its stated purpose?',
    'Study of amendment difficulty and institutional barriers to reinterpreting the clause; comparison with other constitutional provisions that have undergone radical reinterpretation; analysis of whether institutional resistance to change is grounded in genuine jurisprudential conviction or in distribution of benefits from status quo interpretation',
    'If entrenchment is justified: the constraint''s stability reflects appropriate constitutional weight, and interpretive change must proceed through legitimate channels. If entrenchment enables institutional capture: the textual form becomes a cage preventing remedy, extractiveness increases, and mandatrophy is structurally inevitable unless the text''s binding authority is challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_entrenchment_of_text, conceptual, 'Whether institutional entrenchment of the text prevents beneficial reinterpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel_flat_control, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eqprot_theater_post_brown_1954, equal_protection_kernel_flat_control, theater_ratio, 0, 0.52).
narrative_ontology:measurement(eqprot_theater_post_milliken_1974, equal_protection_kernel_flat_control, theater_ratio, 25, 0.61).
narrative_ontology:measurement(eqprot_theater_post_shelby_2013, equal_protection_kernel_flat_control, theater_ratio, 45, 0.71).
narrative_ontology:measurement(eqprot_theater_contemporary_2024, equal_protection_kernel_flat_control, theater_ratio, 70, 0.68).

% Extraction over time
narrative_ontology:measurement(eqprot_extract_post_brown_1954, equal_protection_kernel_flat_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eqprot_extract_post_milliken_1974, equal_protection_kernel_flat_control, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(eqprot_extract_post_shelby_2013, equal_protection_kernel_flat_control, base_extractiveness, 45, 0.54).
narrative_ontology:measurement(eqprot_extract_contemporary_2024, equal_protection_kernel_flat_control, base_extractiveness, 70, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(eqprot_suppress_post_brown_1954, equal_protection_kernel_flat_control, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(eqprot_suppress_post_milliken_1974, equal_protection_kernel_flat_control, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(eqprot_suppress_post_shelby_2013, equal_protection_kernel_flat_control, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(eqprot_suppress_contemporary_2024, equal_protection_kernel_flat_control, suppression_requirement, 70, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel_flat_control, affirmative_action_admissions_doctrine).
narrative_ontology:affects_constraint(equal_protection_kernel_flat_control, voter_identification_suppression_mechanics).
narrative_ontology:affects_constraint(equal_protection_kernel_flat_control, school_funding_inequality_persistence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel_flat_control, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
