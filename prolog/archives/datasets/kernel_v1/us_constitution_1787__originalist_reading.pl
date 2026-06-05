% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: US Constitution (1787): Originalist Reading
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The originalist reading of the US Constitution interprets the 1787 text
 *   as having fixed meaning determined by the framers' intent and the public
 *   understanding of the constitutional language at ratification. This
 *   reading constrains constitutional meaning to what the text would have
 *   meant to educated readers in 1787, excluding modern extensions of
 *   constitutional protection to claims not explicitly enumerated or
 *   historically practiced at ratification. The originalist constraint
 *   exhibits the full structural signature of a tangled rope: it provides
 *   genuine coordination (clear historical boundaries enable predictable
 *   judicial decision-making) while simultaneously extracting from modern
 *   rights claimants (by foreclosing their claims from constitutional
 *   protection). The constraint's mechanism operates through institutional
 *   enforcement: originalist judges, law schools teaching originalism as
 *   neutral methodology, and political coalitions prioritizing originalist
 *   appointments. The extractiveness has accumulated over time as the
 *   originalist coalition gained institutional dominance (Scalia's
 *   originalism in the 1980s-1990s, the federalist Society expansion, recent
 *   originalist Supreme Court majorities). The theater ratio reflects that
 *   originalism claims scientific objectivity (historical evidence is
 *   empirical fact) while embedding contestable interpretive choices (which
 *   sources count? whose understanding of 'public meaning'? what methodology
 *   resolves conflicts between sources?).
 *
 * KEY AGENTS:
 *   - Modern Rights Claimants (powerless/trapped): Groups asserting rights not enumerated in 1787 — women's rights, LGBTQ+ rights, reproductive autonomy, privacy rights — find claims systematically foreclosed by originalism's narrow constraint boundary. No exit option.
 *   - Progressive Reform Coalitions (organized/constrained): Civil rights movements, feminist organizations, LGBTQ+ advocacy experience originalism as constraining but also organizing target. Retain agency through democratic amendment campaigns.
 *   - Originalist Judiciary (institutional/arbitrage): Federal judges adopting originalist methodology benefit from clear historical boundaries and reduced appearance of judicial lawmaking. Net beneficiary of the constraint.
 *   - Conservative Political Coalition (institutional/constrained): Republican parties, religious conservatives, originalist advocacy networks benefit from constraint's narrowing effect but must invest heavily in enforcement (judicial appointments, litigation strategy, doctrinal maintenance).
 *   - Constitutional Amendment Movement (organized/mobile): Agents pursuing direct amendment see originalism as temporary constraint with built-in sunset through supermajority democratic process.
 *   - Academic Originalist Establishment (institutional/arbitrage): Law schools and constitutional scholarship institutionalize originalism as neutral, scientific methodology. Maintains the constraint through institutional inertia and appearance of objectivity.
 *   - Analytical Observer (analytical/analytical): Civilizational perspective risks naturalizing originalism as linguistic necessity rather than institutional construction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.48).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.62).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "US Constitution (1787): Originalist Reading").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '39670aa3-6b71-4420-9b53-d49a25fb2aee').
narrative_ontology:cs_kernel_codification('39670aa3-6b71-4420-9b53-d49a25fb2aee', formalized).
narrative_ontology:cs_authority_grounding('39670aa3-6b71-4420-9b53-d49a25fb2aee', lineage).
narrative_ontology:cs_interpretation_layer_present('39670aa3-6b71-4420-9b53-d49a25fb2aee').
narrative_ontology:cs_reading_relation('39670aa3-6b71-4420-9b53-d49a25fb2aee', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('39670aa3-6b71-4420-9b53-d49a25fb2aee', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('39670aa3-6b71-4420-9b53-d49a25fb2aee', foundational, semantic_fixation_at_ratification).
narrative_ontology:cs_axiom_status(semantic_fixation_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('39670aa3-6b71-4420-9b53-d49a25fb2aee', semantic_fixation_at_ratification, empirically_contingent).
narrative_ontology:cs_axiom('39670aa3-6b71-4420-9b53-d49a25fb2aee', foundational, framers_intent_is_binding).
narrative_ontology:cs_axiom_status(framers_intent_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('39670aa3-6b71-4420-9b53-d49a25fb2aee', framers_intent_is_binding, deontological).
narrative_ontology:cs_reference_frame('39670aa3-6b71-4420-9b53-d49a25fb2aee', semantic_fixation_at_ratification_1787).
narrative_ontology:cs_drift_state('39670aa3-6b71-4420-9b53-d49a25fb2aee', contemporary_post_living_constitutionalism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('39670aa3-6b71-4420-9b53-d49a25fb2aee', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, constitutional_conservatives).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, historically_dominant_groups).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, modern_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, disenfranchised_groups).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, progressive_constitutional_agenda).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MODERN RIGHTS CLAIMANTS (SNARE) — Groups asserting rights not explicitly enumerated in 1787 text (voting rights expansions, privacy rights, marriage equality, reproductive autonomy) find their claims systematically foreclosed by originalist constraint. Cannot exit the constitutional framework; must accept defeat or engage in protracted amendment battle. Maximum extraction: originalism narrows the constraint boundary, excluding modern claims from constitutional protection regardless of contemporary democratic consensus. High suppression through epistemic gatekeeping — originalist methodology requires historical evidence standards that newer rights cannot meet.
constraint_indexing:constraint_classification(us_constitution_1787__originalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROGRESSIVE REFORM COALITIONS (TANGLED ROPE) — Organized agents (civil rights movements, feminist organizations, LGBTQ+ advocacy) experience originalism as both constraining their constitutional claims AND providing a stable target for organizing. The constraint has a coordination function: it defines which battles must be fought through amendment (constitutional clarity) even though it restricts the available weapons. Significant extraction (excluded from constitutional protection) but not maximal — the coalitions retain agency through democratic mobilization and amendment pathways. Constrained exit: could abandon constitutional legitimacy but choose to contest it.
constraint_indexing:constraint_classification(us_constitution_1787__originalist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINALIST JUDICIARY (ROPE) — Federal judges adopting originalist methodology experience the constraint as coordination: clear historical boundaries enable consistent decision-making and reduce judicial discretion costs. Low extraction from this perspective because the methodology serves judicial interests (institutional legitimacy, predictability, reduced appearance of lawmaking). Arbitrage exit: judges could adopt living constitution methodology but choose originalism for structural advantages. Net beneficiary.
constraint_indexing:constraint_classification(us_constitution_1787__originalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSERVATIVE POLITICAL COALITION (TANGLED ROPE) — Republican parties, religious conservatives, and originalist advocacy networks benefit from the constraint's narrowing effect on constitutional rights claims. But the coalition also invests heavily in enforcement: originalist jurisprudence requires constant litigation, appointment strategies, and doctrinal maintenance to prevent living constitution penetration. Extraction toward this group but significant enforcement costs. Constrained exit: could abandon originalism but would lose the coalition's primary constitutional weapon.
constraint_indexing:constraint_classification(us_constitution_1787__originalist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL AMENDMENT MOVEMENT (SCAFFOLD) — Organized agents pursuing direct amendment (19th Amendment suffrage, 26th Amendment voting age, potential ERA) see originalism as a temporary constraint with built-in sunset: the amendment process itself is the exit mechanism. If sufficient democratic consensus materializes, amendment replaces judicial interpretation, converting originalist constraint into constitutional fact. Low extraction from this perspective because the movement has agency and sees the exit path (supermajority amendment). Theater ratio moderate: amendment requires public mobilization and ratification drama, but the mechanism is constitutive, not performative.
constraint_indexing:constraint_classification(us_constitution_1787__originalist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ACADEMIC ORIGINALIST ESTABLISHMENT (PITON) — Law schools and constitutional law scholarship institutionalize originalism as the neutral, scientistic approach to constitutional meaning. Originalist methodology is taught as neutral methodology rather than as contested political reading. Theater ratio high (0.70+): originalism claims scientific objectivity (historical evidence is empirical, framers' intent is discoverable fact) but the methodology itself embeds contestable choices (which historical sources count? whose understanding of 'original public meaning'? what weight to various framers?). The establishment maintains the methodology through institutional inertia and the appearance of neutrality rather than because it produces better outcomes. Academic piton: the ritual persists despite known internal problems (historians dispute framers' intent, evidence is sparse for many claims).
constraint_indexing:constraint_classification(us_constitution_1787__originalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From civilizational perspective, originalism appears as an immutable constraint: language, once uttered, has a fixed semantic content; the meaning of 1787 text is discoverable through historical methods; no authority can change the past meaning of words. This perspective naturalizes originalism as emerging from linguistic necessity rather than from political choice. However, this view is a FALSE SUMMIT: the constraint is actually constructed through institutional enforcement (originalist judiciary, originalist scholarship, originalist political coalition). Semantic immutability masks a choice about interpretation methodology.
constraint_indexing:constraint_classification(us_constitution_1787__originalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_1787__originalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_1787__originalist_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_1787__originalist_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from modern rights claimants by systematically excluding their claims from constitutional protection. However, the extraction is not maximal (snare-level 0.66+) because: (1) the constraint operates through interpretive methodology rather than through overt coercion, (2) organized agents retain agency through democratic amendment processes, and (3) the extraction benefits specific groups (conservative coalition, originalist judiciary) rather than operating as pure rent-seeking. The constraint is sustained through institutional enforcement rather than through suppression alone. Suppression (0.62): High. Originalism suppresses alternative readings through multiple mechanisms: (a) gatekeeping epistemic standards — originalist methodology requires historical evidence standards that modern rights claims cannot meet; (b) institutional dominance — law schools, judiciaries, political coalitions prioritize originalist appointments and interpretations; (c) rhetorical naturalization — originalism claims scientific objectivity, positioning alternative readings as activist or legislative. Suppression is not absolute (0.95+) because living constitutionalism and positivist readings retain institutional footholds in academia, lower courts, and dissenting opinions. Theater ratio (0.58): Moderate-high. Originalism's claim to scientific objectivity (historical evidence is discoverable fact, framers' intent is recoverable) embeds interpretive choices that appear neutral but are not. The appearance of neutrality requires performative elements: scholarly debates over interpretive methodology framed as empirical disagreements; careful curation of historical sources; selective citation of framers; construction of 'public meaning' as a fact rather than an interpretive choice. Theater has increased over time as originalism has achieved institutional dominance — it now performs the role of neutral, scientific constitutional interpretation rather than defending itself as a contested reading.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence: the original research group (originalist judiciary) sees coordination (Rope); the victims (modern rights claimants) see pure extraction (Snare); organized agents with exit pathways see temporary constraint with sunset (Scaffold); the academic establishment sees neutral methodology (Piton, performative); the analytical observer risks naturalizing the constraint as immutable linguistic fact (Mountain, false summit). The gap between rope (beneficiary view) and snare (victim view) is maximal — the same constraint appears as coordination mechanism to those who benefit and as pure extraction to those who bear costs. This is the diagnostic signature of tangled rope: genuine coordination function (clear historical boundaries enable predictable interpretation) coexists with asymmetric extraction (beneficiaries control the interpretation methodology and can exclude disfavored claims).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness (chi) is determined by the agent's power level, exit options, and relationship to the constraint's extraction flow. Modern rights claimants (powerless/trapped) experience maximum chi: they face high base extractiveness (0.48) scaled by f(d) where d=0.95 (full target status), producing chi ≈ 0.73. Progressive coalitions (organized/constrained) experience moderate chi: base extractiveness scaled by f(d) where d=0.62 (target with some agency), producing chi ≈ 0.42. Originalist judiciary (institutional/arbitrage) experience negative or zero chi: beneficiary status with arbitrage exit produces low or negative d, making the constraint appear as coordination (rope) from their perspective. Conservative coalition (institutional/constrained) experience low to moderate chi: beneficiary status with enforcement costs. Amendment movement (organized/mobile) experience low chi: agent has genuine exit option (amendment pathway). The derivation follows the canonical directionality table: beneficiary + arbitrage → low d → negative/near-zero f(d) → negative chi; victim + trapped → high d → high f(d) → high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: This is the originalist reading of the contested US Constitution kernel. Mandatrophy is resolved by recognizing that originalism is a legitimate but particular reading, not the singular correct interpretation. The constraint exhibits tangled rope structure because it both coordinates (provides clear boundaries for interpretation) and extracts (benefits originalist coalition, excludes modern rights claims). The false-summit temptation (to see the constraint as an immutable law of language) is resisted through the structural data: originalism is enforced through institutional mechanisms (judicial appointments, law school curricula, political coalition pressure), not through linguistic necessity. If originalism were truly immutable linguistic fact, no institutional enforcement would be needed — the constraint would emerge naturally from language itself. The fact that enormous institutional energy is devoted to maintaining originalism indicates that the constraint is constructed and contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framers_intent_discovery_epistemology,
    'What epistemic standard determines whether a particular historical practice, document, or statement constitutes reliable evidence of ''framers'' intent''? Is intent a recoverable fact or a constructed narrative?',
    'Comparative analysis of originalist scholarship: identify cases where different originalist scholars reach opposite conclusions from the same evidence set. Examine meta-level disagreement about which sources (ratification debates, Federalist Papers, state conventions, prior state constitutions) carry most weight. Evaluate whether ''original public meaning'' is discoverable through methods or constructed through interpretive choice.',
    'If intent is reliably discoverable: originalism is epistemically sound, constraint is justified by linguistic facts, false-summit reading is incorrect. If intent is constructed through interpretive choice: originalism embeds political decisions while claiming historical objectivity, false-summit reclassification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framers_intent_discovery_epistemology, empirical, 'Epistemology of historical intent discovery and its reliability').

omega_variable(
    semantic_stability_across_contexts,
    'Does the semantic meaning of 1787 constitutional text remain stable across different interpretive contexts, or does application-context (modern social conditions, technological change, institutional evolution) necessarily shift semantic content?',
    'Philosophical linguistics analysis: can a text''s meaning be context-independent? Case analysis: examine originalist interpretations of constitutional terms (commerce, necessary and proper, cruel and unusual) across different historical applications; identify semantic shifts that originalists attribute to ''changed conditions'' (implicitly accepting context-dependence) vs. those they claim preserve original meaning.',
    'If meaning is context-independent: originalism''s core commitment (fixed meaning) is defensible. If application necessarily shifts meaning: the constraint''s immutability is illusory, originalism enforces a particular context-sensitive reading while claiming context-independence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_stability_across_contexts, conceptual, 'Whether constitutional semantic content is context-independent').

omega_variable(
    amendment_process_sufficiency,
    'Does the Article V amendment process provide adequate democratic correction for originalism''s narrow constraint boundaries, or do supermajority requirements create structural bias favoring status quo and originalist constraint preservation?',
    'Historical analysis of amendment success rates and failure patterns; game-theoretic modeling of amendment-blocking coalitions; comparative study of constitutional amendment in federalist vs. majoritarian systems.',
    'If amendment process is adequate: scaffold perspective is confirmed — originalism is temporary constraint with genuine sunset. If process is structurally biased: amendment pathway is illusory, progressive coalition truly faces snare-level extraction with no exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_sufficiency, empirical, 'Efficacy of constitutional amendment as exit mechanism from originalist constraint').

omega_variable(
    reading_coexistence_or_foreclosure,
    'Are originalism and living constitutionalism logically foreclosing readings of the same constitutional text, or can they coexist as simultaneous frameworks in a plural legal system?',
    'Formal analysis of core premises: does originalism''s claim that ''meaning is fixed at ratification'' logically entail that living constitutionalism is false? Or do they merely disagree on epistemic authority without foreclosing each other? Examine whether mixed courts (originalist and living justices) can coexist or whether the readings create decision instability.',
    'If foreclosing: one reading must eventually eliminate the other through institutional dominance; current coexistence is unstable. If coexisting: plural system can hold both readings indefinitely; the constraint''s type may be stable across time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_or_foreclosure, conceptual, 'Logical relationship between originalist and living constitutional readings').

omega_variable(
    historical_contingency_of_framers_coalition,
    'To what extent is the constraint''s binding force dependent on a particular empirical fact — that the 1787 framers actually shared a determinate intent — that could prove false upon closer historical examination?',
    'Deep historical scholarship: Did the framers hold a unified position on interpretive methodology? Did they expect future courts to be bound by their intent? How much disagreement existed among framers on key constitutional provisions? Examine framers'' own views on constitutional amendment and interpretation.',
    'If framers lacked unified intent: the constraint''s epistemic foundation collapses, originalism narrows the constraint set based on a false historical premise. If framers explicitly expected interpretation evolution: originalism misreads the constitution''s self-understood nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_of_framers_coalition, empirical, 'Historical facticity of unified framers'' intent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 1787, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orig_tr_t1787, us_constitution_1787__originalist_reading, theater_ratio, 1787, 0.25).
narrative_ontology:measurement(orig_tr_t1900, us_constitution_1787__originalist_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement(orig_tr_t1980, us_constitution_1787__originalist_reading, theater_ratio, 1980, 0.58).
narrative_ontology:measurement(orig_tr_t2020, us_constitution_1787__originalist_reading, theater_ratio, 2020, 0.58).

% Extraction over time
narrative_ontology:measurement(orig_be_t1787, us_constitution_1787__originalist_reading, base_extractiveness, 1787, 0.15).
narrative_ontology:measurement(orig_be_t1870, us_constitution_1787__originalist_reading, base_extractiveness, 1870, 0.32).
narrative_ontology:measurement(orig_be_t1970, us_constitution_1787__originalist_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement(orig_be_t2020, us_constitution_1787__originalist_reading, base_extractiveness, 2020, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(orig_su_t1787, us_constitution_1787__originalist_reading, suppression_requirement, 1787, 0.2).
narrative_ontology:measurement(orig_su_t1870, us_constitution_1787__originalist_reading, suppression_requirement, 1870, 0.45).
narrative_ontology:measurement(orig_su_t1970, us_constitution_1787__originalist_reading, suppression_requirement, 1970, 0.62).
narrative_ontology:measurement(orig_su_t2020, us_constitution_1787__originalist_reading, suppression_requirement, 2020, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, judicial_originalism_methodology).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, federalist_society_network).

% DUAL FORMULATION NOTE:
% The originalist reading is one of three sibling readings of the us_constitution_1787 kernel. All three readings interpret the same 1787 text but derive different constraint types, different ε values, and different beneficiary/victim structures. Network links indicate causal and institutional influence: originalist institutional growth (Federalist Society, originalist judiciaries) creates downstream pressure on living and positivist readings, making their institutional foothold more difficult (influences relation). The readings coexist in contemporary jurisprudence across different courts and jurisdictions, preventing any reading from achieving total dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
