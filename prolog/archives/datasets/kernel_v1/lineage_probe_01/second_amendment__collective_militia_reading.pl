% ============================================================================
% CONSTRAINT STORY: second_amendment__collective_militia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment__collective_militia_reading, []).

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
 *   constraint_id: second_amendment__collective_militia_reading
 *   human_readable: Second Amendment as Collective Militia Right (Collective Reading)
 *   domain: constitutional_law/doctrinal_interpretation
 *
 * SUMMARY:
 *   The Second Amendment's collective militia reading interprets the
 *   constitutional text as protecting state militia capacity to arm
 *   themselves against federal disarmament, not as guaranteeing private
 *   citizens a right to personal arms for self-defense. Under this reading,
 *   the prefatory clause ('A well regulated Militia, being necessary to the
 *   security of a free State') is not merely hortatory but genuinely limiting
 *   — it constrains the operative clause ('the right of the people to keep
 *   and bear Arms, shall not be infringed') to apply only to organized
 *   militia service. This reading was the dominant constitutional
 *   interpretation from the 18th century through the 1960s and remains a live
 *   doctrinal position. However, it faces structural competition from the
 *   individual-right reading, which gained substantial doctrinal traction
 *   after D.C. v. Heller (2008) established the individual right as the
 *   prevailing Supreme Court doctrine. The collective reading now operates as
 *   a contested constitutional claim rather than settled law. From a
 *   structural perspective, the collective reading creates a tangled-rope
 *   constraint: it coordinates federalism boundaries (protecting state
 *   militia capacity against federal disarmament) while simultaneously
 *   suppressing individual-right claims that individuals cannot remedy
 *   through democratic process once the judicial interpretation is fixed. The
 *   theater ratio (0.55) reflects that the collective reading now appears in
 *   doctrinal discourse with reduced operative authority — scholars and some
 *   judges continue to invoke it, but it lacks the institutional power it
 *   once held.
 *
 * KEY AGENTS:
 *   - State Militia Institutions: Primary beneficiary (institutional/arbitrage) — the collective reading protects their capacity to arm themselves without federal disarmament; they experience the constraint as pure coordination
 *   - Individual-Right Claimants: Primary victim (powerless/trapped) — under the collective reading, private citizens claiming personal self-defense rights are structurally foreclosed; they cannot exit or remedy the constraint through the interpreted constitutional text
 *   - Federal Judiciary: Powerful interpreter (powerful/mobile) — courts enforce the collective reading, shaping its boundaries and application; they experience mixed coordination (protecting federalism) and extraction (constraining their own interpretive range)
 *   - Gun-Rights Advocacy Organizations: Organized opposition (organized/constrained) — these groups are committed to individual-right interpretation and bear high costs from doctrinal suppression of their framing; they have constrained exit (litigation, legislation, amendment efforts)
 *   - Constitutional Amendment Coalition: Reform pathway (powerful/mobile) — political actors who can exit the collective reading through constitutional amendment or legislative reinterpretation have genuine mobile exit options
 *   - Historical Doctrinal Custodians: Institutional continuity (institutional/arbitrage) — law schools, historical scholarship, and some judges preserve the collective reading in doctrinal memory even as its operative authority has degraded post-Heller
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contested reading as an immutable textual fact rather than recognizing it as one interpretation among competing alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment__collective_militia_reading, 0.38).
domain_priors:suppression_score(second_amendment__collective_militia_reading, 0.62).
domain_priors:theater_ratio(second_amendment__collective_militia_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment__collective_militia_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment__collective_militia_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment__collective_militia_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment__collective_militia_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment__collective_militia_reading, "Second Amendment as Collective Militia Right (Collective Reading)").
narrative_ontology:topic_domain(second_amendment__collective_militia_reading, "constitutional_law/doctrinal_interpretation").

domain_priors:requires_active_enforcement(second_amendment__collective_militia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment__collective_militia_reading, '7fca1440-ec9a-4e51-8b15-ef93a52a6cb8').
narrative_ontology:cs_kernel_codification('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8', fixed_text).
narrative_ontology:cs_authority_grounding('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8', lineage).
narrative_ontology:cs_interpretation_layer_present('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8').
narrative_ontology:cs_reading_relation('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8', second_amendment__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8', foundational, militia_clause_is_limiting).
narrative_ontology:cs_axiom_status(militia_clause_is_limiting, holdable).
narrative_ontology:cs_axiom_grounding('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8', militia_clause_is_limiting, empirically_contingent).
narrative_ontology:cs_axiom('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8', foundational, people_means_collective_militia).
narrative_ontology:cs_axiom_status(people_means_collective_militia, holdable).
narrative_ontology:cs_axiom_grounding('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8', people_means_collective_militia, empirically_contingent).
narrative_ontology:cs_reference_frame('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8', federalist_militia_protection).
narrative_ontology:cs_drift_state('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8', contemporary_post_heller, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('7fca1440-ec9a-4e51-8b15-ef93a52a6cb8', '').
narrative_ontology:cs_kernel_id(second_amendment__collective_militia_reading, second_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment__collective_militia_reading, state_militia_institutions).
narrative_ontology:constraint_victim(second_amendment__collective_militia_reading, individual_right_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL-RIGHT CLAIMANT (SNARE) — Under the collective reading, private citizens seeking personal self-defense arms face maximum suppression: the constitutional right is unavailable to them by definition. Their claim to a personal right is foreclosed by the reading's core premise. They cannot exit or arbitrage; they experience the constraint as a pure extraction mechanism that denies them a constitutional protection they might otherwise claim.
constraint_indexing:constraint_classification(second_amendment__collective_militia_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE MILITARY INSTITUTIONS (ROPE) — The collective reading protects state militias' capacity to arm themselves without federal disarmament. This is pure coordination: the Second Amendment is reinterpreted as a mechanism preserving state militia capacity against federal overreach. State military institutions experience this as a coordination benefit with no extraction cost — they are the intended beneficiary. The constraint solves their collective action problem (preventing federal disarmament) without imposing costs on them.
constraint_indexing:constraint_classification(second_amendment__collective_militia_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL JUDICIARY (TANGLED ROPE) — Courts interpreting the Second Amendment under the collective reading must enforce suppression of individual claims while protecting state militia capacity. This creates a mixed constraint: genuine coordination function (preserving federalism boundaries) alongside asymmetric enforcement burden (suppressing individual claims). Judges have mobility and institutional resources to shape doctrine, but the collective reading constrains their interpretive options. They experience moderate extraction — the doctrinal commitment constrains their power while they benefit from the interpretive authority the reading grants them.
constraint_indexing:constraint_classification(second_amendment__collective_militia_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: GUN-RIGHTS ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized advocacy groups committed to individual-right interpretation experience the collective reading as extraction: their framing is denied constitutional recognition, and they must operate against the doctrinal grain. They face high costs to exit (abandoning their core mission) but have some agency (litigation strategies, legislative alternatives, movement coordination). The constraint extracts from them by delegitimizing their interpretive claim while offering no coordination benefit — it is pure doctrinal opposition.
constraint_indexing:constraint_classification(second_amendment__collective_militia_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM COALITION (SCAFFOLD) — Powerful political actors (state legislatures, Congress, amendment proponents) can exit the collective reading by amending the Constitution or enacting legislation reinterpreting the Second Amendment. The collective reading is structurally contingent — it depends on sustained judicial endorsement and political acquiescence. Powerful actors have mobile exit options: they can change the constitutional text, elect judges who adopt alternative readings, or legislate around the constraint. This perspective sees the collective reading as a temporary doctrinal settlement with a sunset: once political will aligns, the amendment can be revisited or legislatively reframed.
constraint_indexing:constraint_classification(second_amendment__collective_militia_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL DOCTRINAL LINEAGE (PITON) — The collective militia reading has been part of constitutional doctrine for centuries, though its operative status has varied widely. In the modern era (post-1960s), it has become largely performative — ceremonially invoked by some judges but not functionally determinative of outcomes after D.C. v. Heller (2008) established the individual right as the prevailing doctrine. The collective reading persists in doctrinal discourse through institutional inertia: law schools teach both readings, some lower courts cite it, academic traditions preserve it. But its functional interpretive authority is degraded. Theater ratio is elevated because the reading's continued scholarly and judicial mention masks its reduced operative power.
constraint_indexing:constraint_classification(second_amendment__collective_militia_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective focused on the constitutional text itself, the Second Amendment's language ('A well regulated Militia, being necessary to the security of a free State, the right of the people to keep and bear Arms, shall not be infringed') contains an immutable grammatical and historical fact: the militia clause is present and constrains interpretation. Under this view, any reading must grapple with the text as written — the militia framing is not contingent but structural. However, this mountain classification risks naturalizing what is actually a contested interpretive choice. The structural data (suppression of individual claims, beneficiary is state institutions, enforced through doctrine) reveals this as potentially a false summit: the 'immutable textual constraint' framing naturalizes what is actually a doctrinal commitment that competes with alternative readings.
constraint_indexing:constraint_classification(second_amendment__collective_militia_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment__collective_militia_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment__collective_militia_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment__collective_militia_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment__collective_militia_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment__collective_militia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The collective reading extracts from individual-right claimants by denying them a constitutional framing they might otherwise claim. However, the extraction is limited by the reading's own structural dependence on sustained interpretive authority — the moment the Supreme Court, Congress, or states shift doctrinal commitment, the extraction mechanism collapses. The moderate value reflects that the suppression is real and imposed, but not backed by the kind of persistent institutional machinery that sustains a snare's extraction over long periods. The extractiveness has remained stable across the interval because the reading's extractive force depends on doctrinal commitment, not on external machinery that accumulates over time. Suppression (0.62): Moderate-high. Individuals who claim a personal self-defense right under the collective reading face maximal doctrinal suppression — their claim is ruled out by the reading's core premise. However, suppression is not complete in the practical sense: individuals can still advocate for constitutional amendment, can seek legislative change, can exit the constitutional framework by relocating to jurisdictions with more favorable doctrine, or can pursue political movements to shift judicial appointments. The suppression is doctrinal and interpretive rather than material — it is enforced through constitutional reasoning rather than physical barriers. Theater ratio (0.55): Moderate. The collective reading has some genuine interpretive content (the militia clause is actually present in the text), but its operative authority has degraded significantly since Heller. The reading now appears in doctrinal discourse more as a preserved historical position than as a functionally determining rule. The theater ratio has increased over the interval (from 0.30 at ratification, when the reading was fresh and operative, to 0.55 in the post-Heller era, when it persists as a cited but largely displaced doctrine). Claimed type (tangled_rope): The reading simultaneously coordinates federalism boundaries (genuine coordination function) and suppresses individual claims (asymmetric extraction). Both features are structurally essential to the reading — it cannot protect militia capacity without denying individual claims, and it cannot suppress individual claims without positing a coordination rationale. The tangled_rope classification captures this hybrid structure.
 *
 * PERSPECTIVAL GAP:
 *   The collective reading produces sharp perspectival divergence. State militia institutions see rope (pure coordination benefit). Individual-right claimants see snare (pure extraction with no exit). The federal judiciary sees tangled rope (coordination and extraction mixed). Gun-rights organizations see tangled rope from below (they bear the extraction cost but lack the judiciary's institutional power). The constitutional reform coalition sees scaffold (contingent doctrinal settlement with an exit path via amendment). The historical custodian sees piton (the reading persists through inertia despite reduced operative authority). The analytical observer risks mountain classification by naturalizing the reading as immutable textual fact, but the structural data (beneficiary/victim asymmetry, enforced suppression) reveals this as a potential false summit — the reading is a contingent interpretive choice, not an immutable property of the constitutional text.
 *
 * DIRECTIONALITY LOGIC:
 *   The collective reading's directionality derives from the agent's structural position relative to the suppression mechanism. State militia institutions (beneficiaries with arbitrage options) experience low/negative effective extraction — the constraint protects them without cost. Individual-right claimants (victims with trapped exit) experience maximum effective extraction — they are foreclosed by the reading's core premise and cannot remedy through interpretation. The federal judiciary (powerful institution with mobile options) experiences moderate extraction — they have institutional authority to interpret but are constrained by the reading's doctrinal commitments. Gun-rights organizations (organized but constrained by career/ideological investment) experience high extraction — their framing is delegitimized and they must operate against the doctrinal grain. The constitutional reform coalition (powerful with mobile exit) experiences minimal extraction because they can exit via amendment. The historical custodian perspective (institutional/arbitrage) experiences the reading as degraded but sustained through inertia — theater dominates over extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the collective reading instantiates genuine coordination (protecting state militia capacity) alongside genuine extraction (suppressing individual-right claims). These are not contradictory features but structurally coupled: the reading cannot protect militia capacity without constraining individual scope, and that constraint operates as suppression on individual claimants. The tangled_rope classification is appropriate because both features are essential and neither can be removed without collapsing the reading's internal logic. The reading is not a pure snare masquerading as coordination, nor is it a pure coordination mechanism with extractive side effects — it is genuinely hybrid. The perspectival gaps (different agents see different types) reflect not logical contradiction but structural fact: the same constraint provides coordination benefit to some agents (state militias) and extraction cost to others (individual claimants). The mandatrophy dissolves when we recognize that 'which type is correct?' is the wrong question — the presheaf over the observation site is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_interpretive_scope,
    'Does the prefatory militia clause (''A well regulated Militia, being necessary to the security of a free State'') grammatically and historically limit the operative clause (''the right of the people to keep and bear Arms, shall not be infringed''), or does it merely announce a purpose while leaving operative scope unconstrained?',
    'Eighteenth-century grammar and rhetorical analysis; comparative study of other constitutional prefatory clauses and their interpretive function (e.g., Preamble to the Constitution); historical framing-era gun regulations showing whether private arms were distinguished from militia arms',
    'If prefatory clause is limiting: collective reading is structurally sound. If prefatory clause is merely hortatory: individual-right reading is supported by the text itself, and the collective reading becomes a doctrinal imposition rather than textual discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_interpretive_scope, empirical, 'Whether the militia clause grammatically limits the operative clause').

omega_variable(
    original_public_meaning_versus_founding_intent,
    'When the Framers wrote ''the people,'' did they mean private citizens as a whole, or did they mean ''the people as organized in militia'' — is ''the people'' a natural-language collective reference that depends on context for its scope?',
    'Lexical analysis of ''the people'' usage in founding-era documents (state constitutions, Federalist Papers, state militia statutes); examination of whether ''the people'' appears in other constitutional provisions (First Amendment, Fourth Amendment, Tenth Amendment) and how it has been interpreted in those contexts; founding-era gun regulations and who was permitted to carry arms',
    'If ''the people'' is context-dependent and militia-qualified: collective reading is supported. If ''the people'' is a universal reference transcending organizational context: individual-right reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_versus_founding_intent, empirical, 'Whether ''the people'' in the Second Amendment is militia-qualified or universal').

omega_variable(
    extraction_mechanism_stability,
    'Is the suppression of individual-right claims a necessary feature of protecting state militia capacity, or is suppression an imposed doctrinal choice that could be relaxed without losing the militia-protection function?',
    'Comparative constitutional law: how other democracies protect militia or reserve capacity while also protecting individual arms rights; historical analysis of periods when both collective militia protection and private arms rights coexisted (18th-19th century America); modeling of whether federal militia protection requires denial of individual rights or merely limits their scope',
    'If suppression is necessary: the collective reading minimizes extraction (it is pure coordination for state protection). If suppression is avoidable: the collective reading is extractive — it denies individual claims without functional necessity, shifting to snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_stability, empirical, 'Whether suppression of individual claims is necessary for militia protection').

omega_variable(
    reading_sibling_kernel_contest,
    'Which reading of the Second Amendment kernel — collective militia or individual right — reflects the Framers'' actual intent, and does the historical record foreclose one reading in the other''s framework?',
    'Comprehensive historical scholarship examining framing-era state constitutions, militia statutes, private gun ownership patterns, and relevant founding-era political theory; assessment of whether the Framers'' intent was unitary or contested; determination of whether the individual-right reading was even available to founding-era readers or whether it emerges as a post-Heller innovation',
    'If Framers'' intent clearly supports collective reading: individual-right reading becomes a doctrinal innovation (post-1960s), and the collective reading is the ''true'' constitutional meaning. If Framers'' intent is genuinely ambiguous or supported both readings: both readings coexist as legitimate interpretive possibilities (both persist as ''coexist_with'' siblings). If historical record supports individual-right reading: collective reading is an imposition, and the constraint''s classification shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_sibling_kernel_contest, empirical, 'Framing-era intent and whether historical record supports one reading over the other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment__collective_militia_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1791, second_amendment__collective_militia_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(theater_1891, second_amendment__collective_militia_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(theater_2006, second_amendment__collective_militia_reading, theater_ratio, 215, 0.55).

% Extraction over time
narrative_ontology:measurement(extract_1791, second_amendment__collective_militia_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extract_1891, second_amendment__collective_militia_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(extract_2006, second_amendment__collective_militia_reading, base_extractiveness, 215, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment__collective_militia_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment__collective_militia_reading, second_amendment__individual_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel decomposes into two structurally distinct constraint stories with different ε values, beneficiary/victim structures, and operational dynamics. The collective militia reading (this story) has ε ≈ 0.38 and protects state militia capacity while suppressing individual claims. The individual right reading has a different ε and beneficiary/victim structure, with individual claimants as beneficiaries and gun-restriction advocates as victims. The two readings coexist as competing doctrinal claims within constitutional law. Both are live interpretive positions held by different judicial coalitions and scholarly communities. They affect one another through doctrinal competition — when one reading gains ground in case law, it constrains the other's operative authority. Link them via network.affects_constraints to model the doctrinal coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
