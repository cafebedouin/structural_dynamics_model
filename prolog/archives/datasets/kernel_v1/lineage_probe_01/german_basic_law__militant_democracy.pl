% ============================================================================
% CONSTRAINT STORY: german_basic_law__militant_democracy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_german_basic_law__militant_democracy, []).

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
 *   constraint_id: german_basic_law__militant_democracy
 *   human_readable: Militant Democracy Doctrine in the German Basic Law
 *   domain: constitutional_law/political_order
 *
 * SUMMARY:
 *   The militant democracy doctrine embedded in the German Basic Law
 *   represents a foundational constitutional choice to arm democratic
 *   institutions against actors who would use democracy's protections to
 *   destroy democracy itself. Article 18 (forfeiture of certain
 *   constitutional rights), Article 21(2) (party bans for
 *   unconstitutionality), and the eternity clause of Article 79(3) together
 *   create a self-preserving constitutional order that explicitly rejects the
 *   neutrality toward anti-constitutional movements that characterized the
 *   Weimar Republic. This constraint is ONE READING of a contested
 *   constitutional kernel — the Basic Law itself. Alternative readings
 *   emphasize the Basic Law's amendment history (amendments responding to
 *   crises), its foundational rights catalog, the centrality of human
 *   dignity, or its federal structure. The militant democracy reading
 *   foregrounds the suppressive and self-protective logic embedded in the
 *   text, treating the doctrine as a core constitutional principle rather
 *   than an instrumental safeguard. This reading instantiates a tangled rope
 *   constraint: genuine coordination function (protecting the democratic
 *   order from existential threats) combined with asymmetric extraction
 *   (suppression directed at movements the Constitutional Court judges
 *   hostile, with no guaranteed objective threshold). The constraint exhibits
 *   perspectival collapse across multiple institutional actors, each
 *   experiencing different classification depending on their structural
 *   relationship to the suppression apparatus.
 *
 * KEY AGENTS:
 *   - Free Democratic Basic Order (Freiheitlich-demokratische Grundordnung): Primary beneficiary (institutional/arbitrage) — the constitutional framework that militant democracy protects; defines the boundary between protected and proscribable political activity
 *   - Proscribed Political Movements: Primary victim (powerless/trapped) — subject to party bans, rights forfeiture, legal nonexistence; cannot appeal for recognition within constitutional frame that denies them standing
 *   - Constitutional Court (Bundesverfassungsgericht): Active enforcer (institutional/constrained) — adjudicates the threshold between legitimate opposition and anti-constitutional threat; coordinates doctrine and bears the political cost of acting as final arbiter of constitutional legitimacy
 *   - Established Political Parties: Secondary actors (organized/constrained) — benefit from clearing the field of existential threats; constrained by precedent that any party can be proscribed if deemed hostile to free democratic basic order
 *   - Democratic Renewal / Civil Society: Organized advocates (organized/mobile) — may view militant democracy as temporary scaffolding for democratic consolidation, declining as institutional norms harden
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contested constitutional choice as a law of democratic self-preservation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_basic_law__militant_democracy, 0.58).
domain_priors:suppression_score(german_basic_law__militant_democracy, 0.72).
domain_priors:theater_ratio(german_basic_law__militant_democracy, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_basic_law__militant_democracy, extractiveness, 0.58).
narrative_ontology:constraint_metric(german_basic_law__militant_democracy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(german_basic_law__militant_democracy, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(german_basic_law__militant_democracy, tangled_rope).
narrative_ontology:human_readable(german_basic_law__militant_democracy, "Militant Democracy Doctrine in the German Basic Law").
narrative_ontology:topic_domain(german_basic_law__militant_democracy, "constitutional_law/political_order").

domain_priors:requires_active_enforcement(german_basic_law__militant_democracy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(german_basic_law__militant_democracy, '7c70f06d-3fae-491b-a4b5-c90ee54f2932').
narrative_ontology:cs_kernel_codification('7c70f06d-3fae-491b-a4b5-c90ee54f2932', formalized).
narrative_ontology:cs_authority_grounding('7c70f06d-3fae-491b-a4b5-c90ee54f2932', lineage).
narrative_ontology:cs_interpretation_layer_present('7c70f06d-3fae-491b-a4b5-c90ee54f2932').
narrative_ontology:cs_reading_relation('7c70f06d-3fae-491b-a4b5-c90ee54f2932', german_basic_law__amendment_history, influences).
narrative_ontology:cs_reading_relation('7c70f06d-3fae-491b-a4b5-c90ee54f2932', german_basic_law__basic_rights_catalog, influences).
narrative_ontology:cs_reading_relation('7c70f06d-3fae-491b-a4b5-c90ee54f2932', german_basic_law__dignity_and_eternity, coexists_with).
narrative_ontology:cs_reading_relation('7c70f06d-3fae-491b-a4b5-c90ee54f2932', german_basic_law__federal_construction, influences).
narrative_ontology:cs_axiom('7c70f06d-3fae-491b-a4b5-c90ee54f2932', foundational, democracy_requires_self_defense_against_destroyers).
narrative_ontology:cs_axiom_status(democracy_requires_self_defense_against_destroyers, holdable).
narrative_ontology:cs_axiom_grounding('7c70f06d-3fae-491b-a4b5-c90ee54f2932', democracy_requires_self_defense_against_destroyers, deontological).
narrative_ontology:cs_axiom('7c70f06d-3fae-491b-a4b5-c90ee54f2932', foundational, weimar_neutrality_failed).
narrative_ontology:cs_axiom_status(weimar_neutrality_failed, holdable).
narrative_ontology:cs_axiom_grounding('7c70f06d-3fae-491b-a4b5-c90ee54f2932', weimar_neutrality_failed, empirically_contingent).
narrative_ontology:cs_reference_frame('7c70f06d-3fae-491b-a4b5-c90ee54f2932', democratic_self_defense_framework).
narrative_ontology:cs_drift_state('7c70f06d-3fae-491b-a4b5-c90ee54f2932', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c70f06d-3fae-491b-a4b5-c90ee54f2932', '').
narrative_ontology:cs_kernel_id(german_basic_law__militant_democracy, german_basic_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(german_basic_law__militant_democracy, free_democratic_basic_order).
narrative_ontology:constraint_victim(german_basic_law__militant_democracy, proscribed_political_movements).
narrative_ontology:constraint_victim(german_basic_law__militant_democracy, legal_equality_of_political_parties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROSCRIBED POLITICAL MOVEMENT (SNARE) — Subject to party ban, forfeiture of constitutional protections, and legal nonexistence. Cannot organize, appeal to courts as constitutional persons, or claim rights. Maximum suppression with minimal exit pathways. The constraint extracts total legality denial.
constraint_indexing:constraint_classification(german_basic_law__militant_democracy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL COURT (TANGLED ROPE) — Coordinating the institutional threshold for proscription (must demonstrate threat to free democratic basic order) while enforcing suppression against movements that cross that threshold. Benefits from doctrine clarity and authority to decide constitutional matters; bears costs of acting as political actor and managing the boundary between legitimate opposition and proscribable extremism.
constraint_indexing:constraint_classification(german_basic_law__militant_democracy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FREE DEMOCRATIC BASIC ORDER (ROPE) — Primary beneficiary. The constitutional order uses militant democracy doctrine as a coordination mechanism for self-preservation. The suppression apparatus protects the institutional framework that creates beneficiaries. No exit option — the order cannot walk away from its own defense. Experiences this constraint as legitimate self-coordination against existential threats.
constraint_indexing:constraint_classification(german_basic_law__militant_democracy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED POLITICAL PARTIES (TANGLED ROPE) — Benefit from the militant democracy doctrine insofar as it protects their competitive field from anti-constitutional challengers; constrained by the precedent that any party can be proscribed if the Constitutional Court judges it hostile to the free democratic basic order. Experiences both coordination (clearing the field of existential threats) and extraction (the threat of potential proscription constrains their discourse and strategy).
constraint_indexing:constraint_classification(german_basic_law__militant_democracy, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEMOCRATIC RENEWAL MOVEMENT (SCAFFOLD) — Sees militant democracy as a temporary response to Weimar's failures, declining in necessity as democratic culture strengthens. As institutional norms harden and far-right extremism recedes (hypothetical), the proscriptive apparatus becomes unnecessary — the sunset is normalization of democratic resilience. This perspective reads the doctrine as scaffolding for democratic consolidation, not permanent constitutional structure.
constraint_indexing:constraint_classification(german_basic_law__militant_democracy, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / UNIVERSALIST NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, some constitutional orders must defend themselves against actors who would use democratic freedoms to destroy democracy itself — a paradox of self-referential systems. This perspective naturalizes militant democracy as an inescapable structural feature of any self-preserving constitutional order. However, structural data (identifiable beneficiaries, specific suppression targets, legal doctrine) reveals this as a false summit — what appears natural is a contested reading of the Basic Law, not a law of nature.
constraint_indexing:constraint_classification(german_basic_law__militant_democracy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(german_basic_law__militant_democracy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(german_basic_law__militant_democracy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(german_basic_law__militant_democracy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(german_basic_law__militant_democracy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(german_basic_law__militant_democracy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts legal standing and political voice from movements adjudged hostile. However, extractiveness is not maximal (snare-level ≥0.66) because: (a) the doctrine requires Constitutional Court authorization, not administrative discretion; (b) the threshold is defined in doctrine as threat to free democratic basic order, not mere political disfavor; (c) alternative movements can operate legally within the constraints. The constraint benefits the constitutional order and established parties, so extraction is not pure. Over time (t0=0.42 to t75=0.58), extractiveness has risen as the doctrine's application has broadened from explicit Nazi-revival or communist movements to more marginal far-right and far-left parties. This trajectory suggests doctrine mission-creep or increasing sensitivity to threats. Suppression (0.72): High. Severe barriers to exit for proscribed movements: complete legal nonexistence, no appeal mechanism within constitutional order (the Constitutional Court's decision is final and unreviewable), no meaningful path to rehabilitation or return to legality. Suppression is structural, not merely internalized — the legal order itself bars all formal participation. Theater ratio (0.35): Low. The suppression apparatus is genuine and functional — party bans are legally binding, Constitutional Court decisions end political existence. Theater is not the primary mechanism; legal force is. The modest theater ratio reflects that militant democracy relies on substantive doctrine and legal enforcement, not performative ritual. Some theater exists in the constitutional court's public reason-giving and the formality of the proscription process, but the mechanism is not primarily theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is radical. The free democratic basic order sees self-preservation and legitimate self-defense (rope coordination). Proscribed movements see legal death and permanent suppression (snare extraction). The Constitutional Court sees balanced doctrine and threshold-setting (tangled rope coordination with enforcer burden). Established parties see both benefit (removing existential threats) and constraint (precedent that any party can be proscribed). The democratic renewal perspective sees temporary scaffolding that will decline as norms harden (scaffold with sunset). The analytical observer risks naturalizing a contingent constitutional choice as an inescapable law of democracy (false summit mountain). This perspectival collapse reveals that the same structural mechanism — suppression apparatus + legal proscription — is experienced as beneficial coordination by some observers and extractive harm by others. The gap is not empirical disagreement; it is structural position determining what the same legal doctrine *means*.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural relationship to the suppression apparatus. The free democratic basic order is the core beneficiary — it is the framework the doctrine protects. Proscribed movements are pure targets: high d (→0.95), producing high f(d) (→1.42), maximizing experienced extraction. The Constitutional Court enforcer has mixed position: benefits from authority and doctrine clarity (moderate beneficiary), bears enforcement burden (partial victim), constrained by precedent. Established parties benefit from threat removal (partial beneficiary) but constrained by precedent of potential proscription (partial victim). The scope modifier σ(S) = 1.0 (national) — the doctrine operates within German constitutional order, though with jurisprudential influence beyond. The χ formula produces: χ = 0.58 × f(d) × 1.0. For proscribed movements (d≈0.95, f(d)≈1.42): χ ≈ 0.82 (snare territory). For established parties (d≈0.50, f(d)≈0.65): χ ≈ 0.38 (tangled rope territory). For the beneficiary order (d≈0.05, f(d)≈-0.12): χ ≈ -0.07 (rope territory). The directionality derivation captures the structural asymmetry: the same doctrine produces different extractiveness values depending on position relative to it.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves the mandatrophy by making visible that militant democracy is BOTH a genuine coordination mechanism (protecting the democratic order from movements that would use democracy to destroy it) AND an extraction mechanism (suppressing voice and legal status of movements deemed hostile). The false summit perspective (natural law mountain) would assume that some constitutional orders naturally ban dangerous movements and thus militant democracy is inevitable. The facts contradict this: democracies without explicit proscriptive provisions survive (UK, Scandinavia, Canada). The tangled rope classification acknowledges both the coordination function and the extractive mechanism without collapsing them. The beneficiary (free democratic basic order) genuinely benefits from self-protection. The victim (proscribed movements) genuinely bears extraction costs. The mechanism is not merely performative (low theater ratio confirms this). The doctrine is actively enforced. The mandatrophy is resolved by accepting that the constraint does BOTH things: it coordinates democratic self-defense AND it extracts legal existence from those it targets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_threat_threshold,
    'What structural features of a political movement constitute a genuine threat to the free democratic basic order, versus political opposition the majority disfavors?',
    'Constitutional court doctrine analysis over time; comparison of proscription criteria with actual threat materialization; comparative study of movements proscribed vs. those that remained legal despite similar ideologies',
    'If threshold is objective and stable: militant democracy is defensible coordination mechanism (higher rope/lower snare). If threshold is contested/drifting: suppression is extractive overlay on discretionary power (higher snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_threat_threshold, conceptual, 'Definition and stability of threat threshold for party proscription').

omega_variable(
    reading_contest_kernel_ambiguity,
    'Does the Basic Law instantiate militant democracy as a foundational principle, or is it ONE AMONG FIVE co-equal readings of a contested constitutional kernel?',
    'Committer-frame analysis: which reading(s) does the Basic Law''s text actually authorize? Do Articles 18, 21(2), and 79(3) foreclose alternative readings or merely constrain them? Judicial doctrine history: have German courts ever embraced a non-militant reading of the same constitutional text?',
    'If militant democracy is the authoritative reading: the constraint''s legitimacy rests on Article 18/21(2) and the suppression is constitutional coordination. If multiple readings coexist: the constraint is a choice among alternatives (increases stakes of reading contests), and beneficiaries of militant reading have incentive to suppress awareness of alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_ambiguity, conceptual, 'Whether militant democracy is foundational or one contested reading of the Basic Law kernel').

omega_variable(
    weimar_lesson_generalization,
    'Is the Weimar lesson — that neutrality toward anti-democratic actors led to democracy''s destruction — a law of political order, or a specific historical contingency dependent on 1920s German conditions?',
    'Comparative constitutional history: survey democracies that survived without militant democracy provisions (UK, Scandinavia, Canada, Australia). Analyze conditions under which militant democracy succeeds vs. fails. Examine whether democracies without explicit proscriptive authority survived comparable far-right or far-left movements.',
    'If Weimar lesson is universal law: militant democracy is natural (mountain perspective is correct). If contingent: the doctrine is a policy choice that benefits certain actors by naturalizing it (false summit, extractive overlay).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weimar_lesson_generalization, empirical, 'Universality vs. contingency of the Weimar lesson for militant democracy necessity').

omega_variable(
    proscription_reversibility,
    'Can a proscribed movement, or its ideological successors, ever re-enter the legal political space, or does militant democracy doctrine create permanent legal death for movements once adjudged hostile?',
    'Historical analysis of proscribed movements in German law; examination of whether successor organizations succeed in legal status; assessment of whether ideological evolution allows movement rehabilitation or whether the proscription extends to all descendants',
    'If reversible: proscription is punishment + deterrent, not permanent exclusion. If irreversible: the constraint creates a permanent victim class with no exit pathway, raising extractiveness ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proscription_reversibility, empirical, 'Reversibility of party proscription under militant democracy doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(german_basic_law__militant_democracy, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gblaw_militant_theater_t0, german_basic_law__militant_democracy, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gblaw_militant_theater_t25, german_basic_law__militant_democracy, theater_ratio, 25, 0.32).
narrative_ontology:measurement(gblaw_militant_theater_t75, german_basic_law__militant_democracy, theater_ratio, 75, 0.35).

% Extraction over time
narrative_ontology:measurement(gblaw_militant_extract_t0, german_basic_law__militant_democracy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gblaw_militant_extract_t25, german_basic_law__militant_democracy, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(gblaw_militant_extract_t75, german_basic_law__militant_democracy, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gblaw_militant_suppression_t0, german_basic_law__militant_democracy, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gblaw_militant_suppression_t25, german_basic_law__militant_democracy, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(gblaw_militant_suppression_t75, german_basic_law__militant_democracy, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(german_basic_law__militant_democracy, enforcement_mechanism).
narrative_ontology:affects_constraint(german_basic_law__militant_democracy, german_basic_law__amendment_history).
narrative_ontology:affects_constraint(german_basic_law__militant_democracy, german_basic_law__basic_rights_catalog).
narrative_ontology:affects_constraint(german_basic_law__militant_democracy, german_basic_law__dignity_and_eternity).
narrative_ontology:affects_constraint(german_basic_law__militant_democracy, german_basic_law__federal_construction).

% DUAL FORMULATION NOTE:
% The militant democracy reading is one of five structurally linked readings of the German Basic Law kernel. All five readings are mutually constraining: the amendment history constrains how the text is revised; the rights catalog constrains which agents can claim constitutional protection; the dignity-and-eternity clause constrains what can be amended; the federal construction constrains where suppression authority resides; the militant democracy reading constrains which agents can legally participate. Each reading has its own extractiveness and suppression profile. The militant_democracy reading has higher suppression (0.72) than the basic_rights reading would suggest (rights without militant exceptions would have lower suppression). The network reflects the constraint family structure — these are not independent constraints but co-interpretations of a single foundational text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(german_basic_law__militant_democracy, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
