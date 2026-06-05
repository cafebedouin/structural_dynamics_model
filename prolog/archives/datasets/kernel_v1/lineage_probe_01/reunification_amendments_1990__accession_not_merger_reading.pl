% ============================================================================
% CONSTRAINT STORY: reunification_amendments_1990__accession_not_merger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reunification_amendments_1990__accession_not_merger_reading, []).

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
 *   constraint_id: reunification_amendments_1990__accession_not_merger_reading
 *   human_readable: 1990 Reunification: Accession Not Merger (Constitutional Reading)
 *   domain: constitutional_law/german_reunification
 *
 * SUMMARY:
 *   In 1990, German reunification proceeded via accession: the five new
 *   Länder joined the Federal Republic under its existing Basic Law, with
 *   targeted amendments (Articles 23, 51, 135a) rather than constitutional
 *   renegotiation. This constraint story instantiates ONE READING of the
 *   contested kernel 'reunification_amendments_1990': the
 *   accession-not-merger reading. This reading asserts that 1990 was legally
 *   and constitutionally a straightforward accession—a joining of existing
 *   state to existing constitutional order—rather than either a merger (two
 *   states creating a new shared constitution) or a treaty-based
 *   constitutional act (the Unification Treaty as the instrument of
 *   constitutional change). The reading suppresses two alternatives: Article
 *   146, which contemplates the Basic Law being replaced by a constitution
 *   adopted by the whole people (and was deliberately not invoked in 1990);
 *   and treaty constitutionalism, which treats the Unification Treaty's
 *   hundreds of pages as doing the real constitutional work. The structural
 *   delta is precisely as expected: suppression of constitutional
 *   renegotiation (immediate constraint: rapid reunification was incompatible
 *   with extended debate), beneficiary is Western legal continuity (the
 *   Federal Republic's constitutional order retained supremacy and was not
 *   fundamentally altered), and victim set includes Eastern constitutional
 *   agency and the possibility of all-German co-creation. The extractiveness
 *   (0.52) reflects that the transition concentrated costs and institutional
 *   changes eastward: Eastern actors bore the burden of adapting to Western
 *   law; Western actors benefited from institutional continuity and authority
 *   preservation. This is a genuine tangled rope: the constraint coordinated
 *   the urgent need for unified legal order while asymmetrically distributing
 *   the costs of that coordination.
 *
 * KEY AGENTS:
 *   - Western Legal Continuity (Federal Republic institutions): Beneficiary (institutional/arbitrage) — retained constitutional authority, avoided renegotiation, expanded jurisdiction eastward
 *   - Eastern Constitutional Agency (East German political leadership): Victim (organized/constrained) — faced impossible choice between accepting accession or negotiating (with cost of delay and political isolation)
 *   - Rapid Integration Architects (Kohl government, constitutional lawyers): Strategic agent (powerful/mobile) — designed accession framework to solve the urgency problem; experienced constraint as temporary scaffold, not permanent extraction
 *   - Article 146 Possibility (Alternative constitutional path): Victim (powerless/trapped) — was explicitly not invoked; cannot organize or exit; represents a foreclosed constitutional option
 *   - All-German Sovereign Claim (Hypothetical two-state co-creation): Victim (powerless/trapped) — was suppressed by framing accession as the necessary path; no constituency could advocate for genuine co-authorship without opposing reunification itself
 *   - Analytical Observer (Constitutional theorist): Sees structure that masks itself as natural law; demonstrates false summit signature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reunification_amendments_1990__accession_not_merger_reading, 0.52).
domain_priors:suppression_score(reunification_amendments_1990__accession_not_merger_reading, 0.68).
domain_priors:theater_ratio(reunification_amendments_1990__accession_not_merger_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reunification_amendments_1990__accession_not_merger_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reunification_amendments_1990__accession_not_merger_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(reunification_amendments_1990__accession_not_merger_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reunification_amendments_1990__accession_not_merger_reading, tangled_rope).
narrative_ontology:human_readable(reunification_amendments_1990__accession_not_merger_reading, "1990 Reunification: Accession Not Merger (Constitutional Reading)").
narrative_ontology:topic_domain(reunification_amendments_1990__accession_not_merger_reading, "constitutional_law/german_reunification").

domain_priors:requires_active_enforcement(reunification_amendments_1990__accession_not_merger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reunification_amendments_1990__accession_not_merger_reading, '3a0dc179-59e8-4a61-bb3d-90ef8057df63').
narrative_ontology:cs_kernel_codification('3a0dc179-59e8-4a61-bb3d-90ef8057df63', formalized).
narrative_ontology:cs_authority_grounding('3a0dc179-59e8-4a61-bb3d-90ef8057df63', lineage).
narrative_ontology:cs_interpretation_layer_present('3a0dc179-59e8-4a61-bb3d-90ef8057df63').
narrative_ontology:cs_reading_relation('3a0dc179-59e8-4a61-bb3d-90ef8057df63', reunification_amendments_1990__article_146_question_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a0dc179-59e8-4a61-bb3d-90ef8057df63', reunification_amendments_1990__treaty_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('3a0dc179-59e8-4a61-bb3d-90ef8057df63', foundational, constitutional_accession_necessity).
narrative_ontology:cs_axiom_status(constitutional_accession_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3a0dc179-59e8-4a61-bb3d-90ef8057df63', constitutional_accession_necessity, deontological).
narrative_ontology:cs_axiom('3a0dc179-59e8-4a61-bb3d-90ef8057df63', secondary, western_continuity_preservation).
narrative_ontology:cs_axiom_status(western_continuity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('3a0dc179-59e8-4a61-bb3d-90ef8057df63', western_continuity_preservation, instrumental).
narrative_ontology:cs_reference_frame('3a0dc179-59e8-4a61-bb3d-90ef8057df63', western_constitutional_supremacy).
narrative_ontology:cs_drift_state('3a0dc179-59e8-4a61-bb3d-90ef8057df63', contemporary_revisionist_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3a0dc179-59e8-4a61-bb3d-90ef8057df63', '').
narrative_ontology:cs_kernel_id(reunification_amendments_1990__accession_not_merger_reading, reunification_amendments_1990).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reunification_amendments_1990__accession_not_merger_reading, western_legal_continuity).
narrative_ontology:constraint_beneficiary(reunification_amendments_1990__accession_not_merger_reading, federal_republic_institutions).
narrative_ontology:constraint_beneficiary(reunification_amendments_1990__accession_not_merger_reading, rapid_integration_advocates).
narrative_ontology:constraint_victim(reunification_amendments_1990__accession_not_merger_reading, eastern_constitutional_agency).
narrative_ontology:constraint_victim(reunification_amendments_1990__accession_not_merger_reading, all_german_sovereign_claim).
narrative_ontology:constraint_victim(reunification_amendments_1990__accession_not_merger_reading, article_146_possibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EASTERN CONSTITUTIONAL AGENCY (SNARE) — East German actors faced an impossible choice framed as voluntary: accept accession under Western law or negotiate separately and delay unification. The framing eliminated the possibility of co-creating a genuinely new constitutional order. High suppression (exit cost of delay was political/economic isolation), high extraction (constitutional authority concentrated westward), minimal coordination function for the suppressed party.
constraint_indexing:constraint_classification(reunification_amendments_1990__accession_not_merger_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EAST GERMAN POLITICAL LEADERSHIP (TANGLED ROPE) — Genuinely benefited from rapid integration (economic support, democratic legitimacy, end to division) but operated under severe structural constraint: negotiate too hard and face accusations of delay; accept terms and lose agency in shaping the merged state. Beneficiary and victim simultaneously — significant extraction but not total. Coordination function existed (both sides needed a unified legal framework) but was asymmetrically enforced.
constraint_indexing:constraint_classification(reunification_amendments_1990__accession_not_merger_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WESTERN LEGAL CONTINUITY (ROPE) — The Federal Republic's Basic Law and institutions experienced this constraint as pure coordination: accession solved the problem of how to incorporate a new state without constitutional renegotiation. The Western legal order was the beneficiary — it retained its supremacy, expanded its jurisdiction, and bypassed the risk of constitutional challenge via Article 146. Low effective extraction toward this agent; benefits flow inward.
constraint_indexing:constraint_classification(reunification_amendments_1990__accession_not_merger_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RAPID INTEGRATION ARCHITECTS (SCAFFOLD) — The Kohl government and its legal architects experienced this constraint as a temporary coordination mechanism with a clear sunset: accession provided a rapid path to unification while preserving Western institutional continuity. Once integration proceeded, the constraint (urgency of immediate reunification) naturally dissolved. Sunset was implicit: once the five new Länder were established and Basic Law amendments completed, the accession logic was no longer operationally necessary. High agency, strategic clarity, low extraction toward this actor.
constraint_indexing:constraint_classification(reunification_amendments_1990__accession_not_merger_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL LEGITIMATION (PITON) — The formal narrative that 1990 was accession—not merger, not renegotiation, not treaty constitutionalism—persists institutionally despite its gradual atrophy. Contemporary German constitutional scholarship recognizes that the Unification Treaty did the real constitutional work, that Article 146 was left unexercised, and that the 'accession' framing was partly functional (enabling speed) and partly rhetorical (preserving Western authority claims). The doctrine persists through inertia in law school pedagogy and official commemoration; the theater ratio reflects that maintaining the accession narrative requires continued institutional assertion against alternative readings that explain more of what actually happened.
constraint_indexing:constraint_classification(reunification_amendments_1990__accession_not_merger_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective on state sovereignty and constitutional authority, accession appears as an immutable property: once a state joins another's legal order, the joining state necessarily inherits the existing constitution whole — there is no logical alternative to accession being accession. However, this perspective naturalizes what is actually a political and legal choice (the decision that unification would proceed via accession rather than negotiation). The engine will classify this as a false summit, revealing that 'logic of state merger' framing masks the contingent institutional arrangements that made accession the chosen path.
constraint_indexing:constraint_classification(reunification_amendments_1990__accession_not_merger_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reunification_amendments_1990__accession_not_merger_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reunification_amendments_1990__accession_not_merger_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reunification_amendments_1990__accession_not_merger_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reunification_amendments_1990__accession_not_merger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reunification_amendments_1990__accession_not_merger_reading, TR),
    TR >= 0.70.

:- end_tests(reunification_amendments_1990__accession_not_merger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The measurement trajectory (0.38 → 0.48 → 0.52) captures the increasing realization, over the initial years after 1990, that the accession framework involved real constitutional extraction. Initial phases saw the constraint as merely pragmatic (rapid path to unification); later analysis recognized that the choice to proceed via accession rather than negotiation concentrated institutional change eastward and foreclosed alternative constitutional paths. The 0.52 final value reflects that extraction is real but not total — Eastern actors did secure significant benefits (economic integration, democratic governance, international standing), and the constraint genuinely coordinated the legitimate need for unified legal order. Suppression (0.68): High. The barrier to constitutional renegotiation was severe: political cost of delay (Cold War momentum, international pressure, risk of Soviet intervention), economic cost (integration could not proceed while constitutional questions remained open), and institutional cost (two competing legal orders could not coexist long-term). The suppression remained stable across the interval (0.72 → 0.70 → 0.68) because while initial urgency declined after 1990-1991, the institutional path-dependency of accession meant that constitutional alternatives became progressively less accessible — the five new Länder were already integrated into Western structures, Article 146 was never formally revisited, and the Unification Treaty became the de facto constitutional instrument. Theater ratio (0.55): Moderate. The constraint involves real functional elements (the need for unified legal order is genuine; accession is a legitimate structural form) alongside performative elements (the 'accession not merger' narrative was partly rhetorical — it emphasized Western constitutional continuity in ways that overstated the formal/substantive distinction). The theater ratio trajectory (0.42 → 0.48 → 0.55) reflects increasing recognition that the accession framing obscures treaty constitutionalism's actual role. Contemporary scholarship notes that the Unification Treaty did far more constitutional work than the pure accession reading acknowledges.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark and reveals the reading's extractive structure. The Western legal order sees coordination (Rope): accession solved a genuine problem. The rapid-integration architects see a temporary measure (Scaffold): once integration proceeded, the urgency dissolved. The Eastern leadership sees mixed coordination and extraction (Tangled Rope): genuine benefits in rapid unification but asymmetric burden-bearing. The Eastern constitutional voice sees pure extraction (Snare): no agency in the constitutional framework; trapped by the choice between accession and delay. The constitutional legitimation ritual sees its own degradation (Piton): the accession narrative persists institutionally despite recognition that treaty and amendment did the real work. The analytical observer risks seeing a natural law (Mountain): the logic of state merger as such—but the false summit detector identifies this as naturalization. The reading's validity depends on suppressing the perspectival gap: treating the Western beneficiary's experience as canonical and the Eastern voice as consenting. The moment the gap is centered, the classification shifts from rope/scaffold toward tangled_rope/snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by agent. The Federal Republic's constitutional order is the primary beneficiary (d ≈ 0.10) — it is the fixed point that other agents must adapt to; the constraint runs toward it. Western legal continuity advocates (institutional/arbitrage) have d ≈ 0.15, low effective extraction because they can arbitrage to other legal frameworks if needed and because the constraint benefits them. Eastern political leadership (organized/constrained) occupies d ≈ 0.60 — they are nominally a beneficiary (rapid unification enabled economic integration and democratic legitimacy) but cannot exit without opposing reunification itself, making their consent coerced. The analytical observer (analytical/analytical) occupies d ≈ 0.72, high extractiveness from the perspective of epistemic integrity — the constraint suppresses alternative constitutional readings, and the observer sees the suppression as a cost to the field's self-understanding.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading instantiates a clear mandatrophy resolution. The baseline empirical fact is that German reunification happened via accession under the Federal Republic's existing Basic Law (with amendments), not via renegotiation of a shared constitution or via treaty-constituted new order. The accession-not-merger reading CLAIMS this fact as the constitutional meaning: 1990 was accession, and that suffices to explain the legal structure. But the structure contains tension: if accession is the constitutional truth, why was Article 146 (which explicitly contemplates constitutional replacement) not invoked? Why did the Unification Treaty require hundreds of pages of modification to the Basic Law? The accession reading resolves these tensions by accepting them as secondary (amendments are part of accession; Article 146 is a dead letter; the treaty merely facilitates accession). The sibling readings resolve them differently: article_146_question_reading says Article 146 was deliberately bypassed, revealing a choice, not a necessity; treaty_constitutionalism_reading says the treaty's constitutional work makes 'accession' a misnomer. All three readings explain the same facts. The mandatrophy is resolved by anchoring to the accession reading's axiom: constitutional_accession_necessity (the claim that accession is the legally mandatory form that any two-state unification must take). If this axiom holds, the reading is structurally complete. If it fails (if renegotiation or treaty-constitutionalism are live alternatives), the reading is revealed as one contingent choice among several, and the extraction it involves becomes visible. The constraint's classification (tangled_rope, not rope) reflects this recognition: the accession reading does coordinate (it solved the urgency problem) but asymmetrically extracts (it foreclosed alternatives and concentrated change eastward).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_146_interpretive_status,
    'Does Article 146—the Basic Law''s provision for its own replacement by a constitution adopted by the whole people—remain a live constitutional option after 1990, or was it effectively foreclosed by treating accession as the conclusive path?',
    'Constitutional court doctrine and scholarly consensus on Article 146''s ongoing applicability. Historical counterfactual: what institutional choices would have changed if Article 146 had been explicitly invoked as the framework instead of accession?',
    'If Article 146 remains live: the accession reading is incomplete (it describes one path, not the only path). If Article 146 is foreclosed by accession: the reading stands as the decisive constitutional move. This determines whether the sibling reading (article_146_question_reading) coexists or is logically preempted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_146_interpretive_status, conceptual, 'Whether Article 146 remains a live constitutional option post-1990').

omega_variable(
    accession_vs_treaty_constitutional_work,
    'Did the Unification Treaty and its hundreds of pages of modifications constitute the real constitutional work of unification, or does the accession framework adequately capture the legal substance?',
    'Comparison of Basic Law provisions post-1990 with pre-1990 baseline, mapped to their source (direct application of accession vs. specific treaty modifications). Analysis of what constitutional problems the treaty solved that accession alone could not have solved.',
    'If treaty modifications were non-trivial: treaty constitutionalism reading gains structural plausibility (reading_relation: influences or coexists_with). If modifications were marginal: accession reading''s claim to comprehensiveness is stronger (reading_relation: influences downstream).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accession_vs_treaty_constitutional_work, empirical, 'Whether Unification Treaty modifications constitute real constitutional work').

omega_variable(
    eastern_constitutional_participation_counterfactual,
    'What would a genuinely co-creative constitutional process have looked like? Would such a process have produced materially different constitutional outcomes, or would the basic structure of the Federal Republic have remained unchanged?',
    'Comparative analysis: constitutional outcomes if renegotiation had occurred (e.g., via Article 146 or treaty-based co-authorship) vs. what actually happened. Expert assessment of contested areas where eastern input might have moved the final text.',
    'If co-creation would have changed outcomes: suppression value is justified (genuine agency was foreclosed). If co-creation would have reached the same result: suppression is partly performative (the eastward constraint was perceived but structurally shallow). Affects the mandatrophy reading of whether this is extraction or coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eastern_constitutional_participation_counterfactual, preference, 'Counterfactual outcomes under genuinely co-creative constitutional process').

omega_variable(
    two_states_to_one_state_logical_necessity,
    'Is accession the logically necessary form that any two-state unification must take, or is it one contingent choice among several structurally defensible alternatives?',
    'Comparative historical/legal analysis: how other unifications have been structured (Italian unification, German unification 1870, EU member state admission, Indian partition). Identification of the minimal formal requirements any unification must meet and how many structurally different paths satisfy those requirements.',
    'If accession is logically necessary: the mountain perspective gains force (unification law naturalizes the accession path). If accession is one contingent choice: the mountain is false (the naturalization masks a political decision). This directly bears on the false summit detection and the axiom status of constitutional_accession_necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_states_to_one_state_logical_necessity, conceptual, 'Whether accession is logically necessary or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reunification_amendments_1990__accession_not_merger_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reun_acc_tr_t0, reunification_amendments_1990__accession_not_merger_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(reun_acc_tr_t2, reunification_amendments_1990__accession_not_merger_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(reun_acc_tr_t5, reunification_amendments_1990__accession_not_merger_reading, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(reun_acc_be_t0, reunification_amendments_1990__accession_not_merger_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(reun_acc_be_t2, reunification_amendments_1990__accession_not_merger_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(reun_acc_be_t5, reunification_amendments_1990__accession_not_merger_reading, base_extractiveness, 5, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(reun_acc_su_t0, reunification_amendments_1990__accession_not_merger_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(reun_acc_su_t2, reunification_amendments_1990__accession_not_merger_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(reun_acc_su_t5, reunification_amendments_1990__accession_not_merger_reading, suppression_requirement, 5, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reunification_amendments_1990__accession_not_merger_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reunification_amendments_1990__accession_not_merger_reading, article_146_question_reading).
narrative_ontology:affects_constraint(reunification_amendments_1990__accession_not_merger_reading, treaty_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This story is part of a constraint family modeling the three contested readings of the reunification constitutional act. All three readings are members of the same kernel. This story (accession-not-merger) treats accession as legally determinative and necessary; it affects the other readings by asserting that the alternatives (renegotiation, treaty constitutionalism) are structurally foreclosed or secondary. The other readings represent different framings of the same historical facts. They are not separate stories of different constraints but different readings of the same kernel's constitutional authority. See the cs_structure.reading_relations block for the formal typed relationships between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reunification_amendments_1990__accession_not_merger_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
