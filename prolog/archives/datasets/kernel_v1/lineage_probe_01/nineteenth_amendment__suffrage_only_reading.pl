% ============================================================================
% CONSTRAINT STORY: nineteenth_amendment__suffrage_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nineteenth_amendment_suffrage_only_reading, []).

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
 *   constraint_id: nineteenth_amendment__suffrage_only_reading
 *   human_readable: Nineteenth Amendment: Suffrage-Only Reading
 *   domain: constitutional_law/gender_equality
 *
 * SUMMARY:
 *   The Nineteenth Amendment, ratified in 1920, enfranchised women by
 *   prohibiting denial or abridgment of the right to vote on account of sex.
 *   But it addressed only voting rights. Women remained subject to coverture
 *   (legal non-personhood in marriage), property restrictions, employment
 *   discrimination, reproductive control, and exclusion from juries and many
 *   professions. The suffrage-only reading holds that the amendment did
 *   exactly what its text says: it granted ballot access and left every other
 *   legal disability untouched. This interpretation constrains how courts and
 *   litigants can invoke the amendment to challenge sex discrimination in
 *   domains outside voting. The reading coexists with an alternative
 *   interpretation: the full-citizenship reading, which holds that the
 *   Nineteenth Amendment stands for women's full and equal citizenship
 *   status, a structural commitment that should inform sex-equality
 *   interpretation across the Constitution. These readings share the same
 *   text but diverge on what structural claim the amendment makes and what
 *   doctrinal consequences follow. The suffrage-only reading is a contested
 *   kernel reading — it is one lived position in constitutional law, held by
 *   some judges, originalist scholars, and institutional actors, while other
 *   constituencies hold the full-citizenship reading.
 *
 * KEY AGENTS:
 *   - Women gaining ballot access: Primary beneficiary (organized/constrained) — genuine benefit of enfranchisement, but constrained by the reading's foreclosure of broader constitutional recourse
 *   - Women under remaining legal disabilities: Primary victim (powerless/trapped) — coverture, employment discrimination, reproductive control; the suffrage-only reading denies constitutional ground to challenge these disabilities
 *   - Broader sex-equality claims grounded in the amendment: Victim category (conceptual/organizational) — claims of constitutional sex equality that might rest on the amendment are foreclosed or weakened by the suffrage-only interpretation
 *   - Courts applying the suffrage-only doctrine: Institutional beneficiary (institutional/arbitrage) — clear interpretive rule provides coordination function; avoids having to determine scope of sex-equality across entire constitutional order
 *   - Originalist constitutional doctrine: Institutional beneficiary (institutional/arbitrage) — suffrage-only reading maintains textual fidelity boundary; preserves methodological boundaries of originalism
 *   - Legislative reformers and statutory advocates: Organized agents (organized/constrained) — must pursue sex equality through separate legislative acts (EPA, Title VII, Title IX, state ERAs) rather than constitutional interpretation
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent doctrinal choice as immutable textual requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nineteenth_amendment__suffrage_only_reading, 0.52).
domain_priors:suppression_score(nineteenth_amendment__suffrage_only_reading, 0.48).
domain_priors:theater_ratio(nineteenth_amendment__suffrage_only_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nineteenth_amendment__suffrage_only_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(nineteenth_amendment__suffrage_only_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(nineteenth_amendment__suffrage_only_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nineteenth_amendment__suffrage_only_reading, tangled_rope).
narrative_ontology:human_readable(nineteenth_amendment__suffrage_only_reading, "Nineteenth Amendment: Suffrage-Only Reading").
narrative_ontology:topic_domain(nineteenth_amendment__suffrage_only_reading, "constitutional_law/gender_equality").

domain_priors:requires_active_enforcement(nineteenth_amendment__suffrage_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nineteenth_amendment__suffrage_only_reading, 'dacff548-02e5-4323-aa2c-2f372dc03a97').
narrative_ontology:cs_kernel_codification('dacff548-02e5-4323-aa2c-2f372dc03a97', formalized).
narrative_ontology:cs_authority_grounding('dacff548-02e5-4323-aa2c-2f372dc03a97', lineage).
narrative_ontology:cs_interpretation_layer_present('dacff548-02e5-4323-aa2c-2f372dc03a97').
narrative_ontology:cs_reading_relation('dacff548-02e5-4323-aa2c-2f372dc03a97', nineteenth_amendment__nineteenth_amendment_full_citizenship_reading, coexists_with).
narrative_ontology:cs_axiom('dacff548-02e5-4323-aa2c-2f372dc03a97', foundational, textual_minimalism_interpretive_fidelity).
narrative_ontology:cs_axiom_status(textual_minimalism_interpretive_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('dacff548-02e5-4323-aa2c-2f372dc03a97', textual_minimalism_interpretive_fidelity, conventional).
narrative_ontology:cs_axiom('dacff548-02e5-4323-aa2c-2f372dc03a97', foundational, separation_of_amendment_from_general_equality).
narrative_ontology:cs_axiom_status(separation_of_amendment_from_general_equality, holdable).
narrative_ontology:cs_axiom_grounding('dacff548-02e5-4323-aa2c-2f372dc03a97', separation_of_amendment_from_general_equality, conventional).
narrative_ontology:cs_reference_frame('dacff548-02e5-4323-aa2c-2f372dc03a97', textual_suffrage_boundary).
narrative_ontology:cs_drift_state('dacff548-02e5-4323-aa2c-2f372dc03a97', contemporary_expanded_sex_equality_doctrine, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dacff548-02e5-4323-aa2c-2f372dc03a97', '').
narrative_ontology:cs_kernel_id(nineteenth_amendment__suffrage_only_reading, nineteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nineteenth_amendment__suffrage_only_reading, women_as_voters).
narrative_ontology:constraint_victim(nineteenth_amendment__suffrage_only_reading, broader_sex_equality_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN UNDER REMAINING LEGAL DISABILITIES (SNARE) — Women who gained ballot access but remain subject to coverture, property restrictions, employment discrimination, and reproductive control. The suffrage-only reading offers no textual foothold to challenge these persistent disabilities. Trapped by the narrow interpretation: ballot access is genuine benefit, but the reading forecloses recourse to the amendment for every other disability. Maximum extraction burden on this agent.
constraint_indexing:constraint_classification(nineteenth_amendment__suffrage_only_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WOMEN'S RIGHTS ADVOCATES AND FEMINISTS (TANGLED ROPE) — Organized groups benefit from ballot access itself (coordination function: women can now participate in democratic process). But constrained by the suffrage-only reading's foreclosure of constitutional sex-equality arguments grounded in the amendment. Some agency (can advocate for separate legislation, state-level action) but textual mobility is blocked. Mixed: coordination gain (enfranchisement) + extraction (avenue to broader equality closed).
constraint_indexing:constraint_classification(nineteenth_amendment__suffrage_only_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COURTS APPLYING THE SUFFRAGE-ONLY DOCTRINE (ROPE) — Institutional actors experience the reading as a coordination mechanism: it provides a clear, textually defensible interpretive rule. Courts can adjudicate ballot-access claims cleanly while deferring all other sex-equality questions to separate doctrinal tracks (rational basis under 14th Amendment, equal protection, statutory construction). Low extraction from this perspective because the reading solves the interpretive coordination problem — 'the Nineteenth says what it says, nothing more.' Beneficiary: institutional stability and interpretive clarity.
constraint_indexing:constraint_classification(nineteenth_amendment__suffrage_only_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGISLATIVE REFORMERS AND STATUTORY ADVOCATES (SCAFFOLD) — Organized agents who accept the suffrage-only reading but pursue sex equality through separate legislative acts: Equal Pay Act (1963), Civil Rights Act Title VII (1964), Educational Amendments Title IX (1972). The suffrage-only reading creates a sunset logic — its constraints are temporary because other legal channels exist and are being activated. Theater low because the coordination mechanism (ballot access) is genuine and the legislative pathways are real. This perspective has agency and sees an exit path through the legislative process.
constraint_indexing:constraint_classification(nineteenth_amendment__suffrage_only_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORIGINALIST CONSTITUTIONAL DOCTRINE (PITON) — The suffrage-only reading is maintained as a doctrinal anchor for originalist jurisprudence: it stands as proof that constitutional meaning is fixed at ratification, independent of historical consequence. The reading persists largely through institutional and pedagogical inertia — originalism requires maintaining strict boundaries between what the text says and what it means for contemporary problems. Theater ratio high: the originalist commitment to the suffrage-only reading has become substantially performative, maintained to preserve the broader originalist framework rather than because the interpretive method would clearly favor this reading over alternatives. The mechanism has degraded: originalism has had to accommodate evolving sex-equality doctrine anyway, making the suffrage-only anchor more theatrical over time.
constraint_indexing:constraint_classification(nineteenth_amendment__suffrage_only_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TEXTUAL FIDELITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, the suffrage-only reading appears as an immutable constraint on constitutional interpretation: the text says 'the right of citizens of the United States to vote shall not be denied or abridged by the United States or by any State on account of sex.' It does not say 'women have full and equal citizenship' or 'sex classifications are suspect.' The mountain classification rests on textual immutability — the words are what they are. However, the structural data reveals a false summit: the choice to treat textual minimalism as natural law (rather than as one interpretive posture among others) benefits identifiable institutional actors (courts insulated from sex-equality challenges, originalist doctrine maintaining boundaries) and bears costs on identifiable victims (women denied constitutional recourse for non-ballot disabilities). The naturalness is contingent.
constraint_indexing:constraint_classification(nineteenth_amendment__suffrage_only_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nineteenth_amendment__suffrage_only_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nineteenth_amendment__suffrage_only_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nineteenth_amendment__suffrage_only_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nineteenth_amendment__suffrage_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nineteenth_amendment__suffrage_only_reading, TR),
    TR >= 0.70.

:- end_tests(nineteenth_amendment__suffrage_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The suffrage-only reading extracts genuine value from the institutional perspective (courts gain interpretive clarity and jurisdictional boundaries) while imposing significant costs on agents seeking broader sex-equality protections (women under coverture, employment discrimination, reproductive control have no constitutional foothold via the amendment). The extraction has grown over time as the gap between ballot access (achieved) and broader sex equality (still blocked) has widened — from 1920 to contemporary era, the relative value of being able to vote has decreased as other legal disabilities have become socially visible problems. The 0.52 value reflects that this is a genuine mixed constraint: ballot access is a real coordination mechanism and genuine benefit, but the narrowness of the reading forecloses alternative pathways. Suppression (0.48): Moderate. Suppression operates textually (the narrow reading suppresses appeals to the amendment's scope) and institutionally (doctrinal boundaries restrict how litigants can frame sex-equality arguments). But suppression is not total — alternative legislative and constitutional pathways exist, and the full-citizenship reading remains a live interpretive alternative. Theater ratio (0.35): Low-moderate and rising. At ratification, the suffrage-only reading had high functional content — it was a genuine, novel substantive achievement. Over the 50-year interval, as broader sex-equality movements emerged and the inadequacy of ballot access alone became evident, the suffrage-only reading's performative content increased. Courts and originalists continued invoking it to establish interpretive boundaries even as its practical centrality diminished. The rising trajectory reflects this degradation — the doctrine persists largely to maintain textual-fidelity boundaries rather than because suffrage alone solves the sex-equality problem.
 *
 * PERSPECTIVAL GAP:
 *   The suffrage-only reading exhibits perspectival fracture across all six types. Women gaining ballot access and organized feminists see coordination (Rope/Tangled Rope) — enfranchisement is a genuine political good. Courts and originalists see coordination (Rope) — the reading solves interpretive puzzles and maintains doctrinal boundaries. Legislative reformers see a temporary problem solvable through statutes (Scaffold) — the suffrage-only constraint is a real but not insurmountable barrier. Women under remaining legal disabilities see pure extraction (Snare) — they gained vote access but lost constitutional recourse for every other disability. Originalist doctrine sees its own interpretive method solidified (Piton) — the suffrage-only reading has become largely performative, maintained to preserve methodological boundaries. The civilizational analytical view risks naturalizing a doctrinal choice as textual immutability (Mountain / false summit). The gap is extreme: from snare to rope within the same structural facts, depending on who is classified.
 *
 * DIRECTIONALITY LOGIC:
 *   The suffrage-only reading's directionality structure creates a peculiar asymmetry. Courts and originalist doctrine benefit (low d values, negative chi) because the reading provides interpretive clarity and maintains doctrinal boundaries — they are institutional beneficiaries with arbitrage options (can apply the suffrage-only rule and defer sex-equality questions to other tracks). Women seeking broader sex-equality protection through the amendment are victims (high d values, high chi) with trapped or constrained exit options (cannot invoke the amendment, must pursue separate legislation or state constitutions). Organized feminists benefit from enfranchisement but are constrained by the reading's foreclosure effect — they have some agency and alternative pathways, making them moderate rather than powerless. The reading's extractiveness rises as the benefits (ballot access) become normalized and taken-for-granted, while the costs (inability to invoke the amendment for non-voting sex equality) remain acutely felt by subsequent generations. This is a classic pattern of initial coordination (suffrage) degrading into residual extraction (foreclosure of constitutional alternatives) as conditions change.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING INSTANTIATION: This constraint resolves the mandatrophy by making explicit that the suffrage-only reading is a contestable choice, not a natural law. The reading is classified as Tangled Rope (0.52 extractiveness) rather than as Mountain (textual immutability) because the structural data reveals beneficiaries (courts, originalist doctrine) and victims (women under remaining disabilities, broader sex-equality claims). The mountain classification at the analytical perspective is a false summit: the suffrage-only interpretation is natural-looking because it respects textual boundaries, but those boundaries are themselves a choice that benefits institutional incumbents. The mandatrophy is resolved by showing that the reading's apparent textual necessity is actually a doctrinal move that closes off alternative constitutional pathways. The full-citizenship reading (sibling constraint) would produce different classifications and metrics: higher extractiveness for the suffrage-only doctrine itself (as a constraint on constitutional mobility), different beneficiary/victim sets, different temporal trajectory. The two readings are not two ways of classifying the same constraint — they instantiate different constraints with different ε values and different structural implications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_scope_intent_ambiguity,
    'Did the Nineteenth Amendment''s drafters and ratifiers intend the amendment to address only ballot access, or did they intend it as a statement of women''s broader legal and constitutional equality, with ballot access as the primary enforceable mechanism?',
    'Historical analysis of ratification debates, suffragist speeches and writings, contemporaneous legal commentary, comparative state-level enfranchisement movements and their broader sex-equality implications',
    'If intent was suffrage-only: the suffrage-only reading is faithful to original meaning. If intent was broader: the reading is a doctrinal narrowing that forecloses the amendment''s full constitutional force. This directly maps to the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_scope_intent_ambiguity, empirical, 'Whether Nineteenth Amendment''s intent was suffrage-only or broader equality statement').

omega_variable(
    textual_minimalism_vs_structural_interpretation,
    'Is textual minimalism (''the amendment says what it says, nothing more'') a neutral interpretive principle, or is it a choice that systematically favors institutional incumbents by blocking constitutional mobility?',
    'Comparison of interpretive outcomes: which groups benefit when courts apply strict textual boundaries vs. structural constitutional principles? Historical trajectory of constitutional sex-equality doctrine — did suffrage-only reading enable or impede?',
    'If minimalism is neutral: the suffrage-only reading is natural law. If minimalism systematically disadvantages vulnerable groups: the reading is a disguised extraction mechanism. This determines whether the mountain classification is true or false-summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_minimalism_vs_structural_interpretation, conceptual, 'Whether textual minimalism is neutral or systematically extractive').

omega_variable(
    legislative_alternative_sufficiency,
    'Do legislative pathways (Equal Pay Act, Title VII, Title IX, state ERAs) provide equivalent constitutional protection to what a broader Nineteenth Amendment reading would provide?',
    'Doctrinal comparison: scope of protection, standards of review, enforceability mechanisms, resistance to rollback. Empirical trajectory: have legislative acts been more or less durable than constitutional protection? Do they reach all covered domains?',
    'If legislative alternatives are sufficient: the suffrage-only reading''s costs are minimized — the scaffold perspective is validated. If not: the reading forecloses important constitutional avenues with no equivalent substitute. This affects classification severity and the mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_alternative_sufficiency, empirical, 'Whether legislative alternatives provide equivalent sex-equality protection').

omega_variable(
    kernel_identity_and_reading_relationship,
    'This constraint is the suffrage-only reading of the Nineteenth Amendment kernel. How does this specific reading relate to the sibling full-citizenship reading? Are the readings logically foreclosed from one another, or do they coexist as live alternatives?',
    'Examine whether accepting the suffrage-only reading''s core premise (''the amendment enfranchised women and left every other legal disability untouched'') logically requires rejecting the full-citizenship reading''s core premise (''the amendment stands for women''s full and equal citizenship''). Assess whether courts and scholars currently hold both positions in different contexts, or whether one necessarily excludes the other.',
    'If foreclosed: the readings cannot coexist in a single coherent legal system. If coexists_with: both readings remain live in contemporary constitutional discourse. The answer determines the cs_structure.reading_relations entries. This is the omega documenting the kernel contest itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_and_reading_relationship, conceptual, 'Structural relationship between suffrage-only and full-citizenship readings of the Nineteenth Amendment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nineteenth_amendment__suffrage_only_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nineteenth_suff_tr_t0, nineteenth_amendment__suffrage_only_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(nineteenth_suff_tr_t25, nineteenth_amendment__suffrage_only_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(nineteenth_suff_tr_t50, nineteenth_amendment__suffrage_only_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(nineteenth_suff_be_t0, nineteenth_amendment__suffrage_only_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nineteenth_suff_be_t20, nineteenth_amendment__suffrage_only_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(nineteenth_suff_be_t50, nineteenth_amendment__suffrage_only_reading, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nineteenth_amendment__suffrage_only_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nineteenth_amendment__suffrage_only_reading, nineteenth_amendment_full_citizenship_reading).

% DUAL FORMULATION NOTE:
% The Nineteenth Amendment kernel admits two distinct constraint readings with different extractiveness, suppression, and victim profiles. The suffrage-only reading (this file) classifies as Tangled Rope (0.52 extractiveness) — genuine ballot-access coordination mixed with foreclosure of broader sex-equality claims. The full-citizenship reading (separate file) classifies with different metrics — higher extractiveness as a mechanism of doctrinal expansion, broader victim set (not just sex-equality claims but also women's property and reproductive autonomy claims rooted in citizenship), different institutional beneficiaries. These are not alternative measurements of one constraint. They are two structurally distinct constraints instantiated from the same kernel text. Both readings are live in contemporary constitutional law; the engine's task is to show how different interpretive choices produce different classifications and identify which agents benefit and bear costs under each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
