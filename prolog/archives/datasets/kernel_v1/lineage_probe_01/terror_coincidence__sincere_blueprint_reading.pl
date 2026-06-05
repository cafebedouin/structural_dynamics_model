% ============================================================================
% CONSTRAINT STORY: terror_coincidence__sincere_blueprint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_terror_coincidence_sincere_blueprint_reading, []).

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
 *   constraint_id: terror_coincidence__sincere_blueprint_reading
 *   human_readable: The 1936 Soviet Constitution as Sincere Blueprint — Bukharin's Deferred Promise
 *   domain: legal/doctrinal/revolutionary_commitment
 *
 * SUMMARY:
 *   The 1936 Soviet Constitution presents one of history's starkest
 *   constraint paradoxes. The document guaranteed collective bargaining
 *   rights, secret ballot elections, equality before law, and social welfare
 *   — commitments authored partly by genuine reform communists including
 *   Nikolai Bukharin, who believed in the promise of 'growing into
 *   socialism.' Yet within two years, the regime was executing the
 *   constitution's drafters by quota (the Great Purge, 1937–1938), using the
 *   document's claim to legality as cover for mass extrajudicial terror. The
 *   sincere blueprint reading holds that this was not pure cynicism from the
 *   start: some drafters meant the promises as real commitments (a deferred
 *   schedule, not a lie), and later reform communists — Khrushchev's thaw,
 *   Gorbachev's glasnost — would cite the text as the regime's unfulfilled
 *   commitment to socialism. The constraint is the binding of authentic hope
 *   to its own negation: the promise suppressed in schedule while preserved
 *   in statement, the document's guarantees conscripted into the Terror's
 *   alibi. This reading coexists with two others — the
 *   legitimation-during-purge reading (constitution as Terror cover), the
 *   plebiscitary-theater reading (consultation as mobilized assent) — and the
 *   three readings illuminate different aspects of the same kernel without
 *   necessarily foreclosing one another. The sincere blueprint reading is
 *   distinguished by its commitment to the reality of some drafters' sincere
 *   intention and the tragic structure of promise suppressed but not
 *   retracted.
 *
 * KEY AGENTS:
 *   - Nikolai Bukharin and reform communist drafters: Primary victims (powerless/trapped) — authored sincere guarantees, executed by 1938, their hopes conscripted into Terror legitimation
 *   - Stalinist security apparatus and purge commissions: Primary beneficiary (institutional/arbitrage) — uses constitution's stated legality as cover for quota-based terror; experiences the constraint as pure coordination
 *   - Reform communists post-1956 (Khrushchev cohort, Gorbachev era): Secondary beneficiary/victim (moderate/constrained) — constrained agents who cite the constitution's unfulfilled promise as the regime's own commitment to reform; benefit from having the sincere blueprint in writing, but extract no structural concessions
 *   - Soviet legal establishment: Institutional maintainer (institutional/arbitrage) — preserves the constitution through educational and ceremonial theater; experiences the document as a performative authority that legitimates the regime without constraining it
 *   - Analytical observer: Civilizational reader (analytical/analytical) — sees the tragic structure of the constraint: authentic commitment bound to its own negation, extractiveness measured by hope conscripted into the Terror's alibi
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(terror_coincidence__sincere_blueprint_reading, 0.58).
domain_priors:suppression_score(terror_coincidence__sincere_blueprint_reading, 0.72).
domain_priors:theater_ratio(terror_coincidence__sincere_blueprint_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(terror_coincidence__sincere_blueprint_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(terror_coincidence__sincere_blueprint_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(terror_coincidence__sincere_blueprint_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(terror_coincidence__sincere_blueprint_reading, tangled_rope).
narrative_ontology:human_readable(terror_coincidence__sincere_blueprint_reading, "The 1936 Soviet Constitution as Sincere Blueprint — Bukharin's Deferred Promise").
narrative_ontology:topic_domain(terror_coincidence__sincere_blueprint_reading, "legal/doctrinal/revolutionary_commitment").

domain_priors:requires_active_enforcement(terror_coincidence__sincere_blueprint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(terror_coincidence__sincere_blueprint_reading, '1b259780-1fbd-4a1e-9d51-fa7f8060f7a4').
narrative_ontology:cs_kernel_codification('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', fixed_text).
narrative_ontology:cs_authority_grounding('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', lineage).
narrative_ontology:cs_interpretation_layer_present('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4').
narrative_ontology:cs_reading_relation('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', terror_coincidence__legitimation_during_purge_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', terror_coincidence__plebiscitary_theater_reading, influences).
narrative_ontology:cs_axiom('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', foundational, reform_communist_authentic_authorship).
narrative_ontology:cs_axiom_status(reform_communist_authentic_authorship, holdable).
narrative_ontology:cs_axiom_grounding('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', reform_communist_authentic_authorship, empirically_contingent).
narrative_ontology:cs_axiom('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', foundational, schedule_suppression_not_negation).
narrative_ontology:cs_axiom_status(schedule_suppression_not_negation, holdable).
narrative_ontology:cs_axiom_grounding('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', schedule_suppression_not_negation, empirically_contingent).
narrative_ontology:cs_reference_frame('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', reformist_communist_framework).
narrative_ontology:cs_drift_state('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', contemporary_post_soviet, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1b259780-1fbd-4a1e-9d51-fa7f8060f7a4', '').
narrative_ontology:cs_kernel_id(terror_coincidence__sincere_blueprint_reading, terror_coincidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(terror_coincidence__sincere_blueprint_reading, reform_communists_post_1956).
narrative_ontology:constraint_victim(terror_coincidence__sincere_blueprint_reading, constitutional_drafters_bukharin_cohort).
narrative_ontology:constraint_victim(terror_coincidence__sincere_blueprint_reading, deferred_socialist_promise).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BUKHARIN AND SINCERE DRAFTERS (SNARE) — Trapped agents who authored the text believing in its promise, not knowing they had inscribed their own death warrant. The constitution's guarantees (collective bargaining, secret ballot, equality before law) were genuine commitments from some drafters, yet the regime used the document's claim to legality as cover for the Terror. The drafters cannot exit — they are shot by 1938. Maximum extraction: their hope and intellectual investment are conscripted into the Terror's alibi.
constraint_indexing:constraint_classification(terror_coincidence__sincere_blueprint_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STALINIST SECURITY APPARATUS (ROPE) — Benefits from the coordination of the constitutional promise: the text legitimates the regime as 'the most democratic in the world,' providing cover for quota-based extrajudicial terror. The apparatus uses the constitution's stated guarantees to make the purge appear lawful. No meaningful extraction experienced because the apparatus controls the text's interpretation — the constraint is a tool of power, not a burden.
constraint_indexing:constraint_classification(terror_coincidence__sincere_blueprint_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM COMMUNISTS POST-1956 (TANGLED ROPE) — Constrained agents who cite the 1936 constitution as the regime's unfulfilled promise. They experience both coordination and extraction: the text genuinely enables their reform arguments (it says what they want communism to be), yet the regime's failure to implement it extracts their hope into futility. They benefit from having the sincere blueprint in writing, but the constraint of the unfulfilled promise drains legitimacy from their reform program. Significant extraction tied to the schedule's suppression, not the statement's negation.
constraint_indexing:constraint_classification(terror_coincidence__sincere_blueprint_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SOVIET LEGAL ESTABLISHMENT (PITON) — The constitution persists as theatrical authority: cited to demonstrate socialist legality, printed in constitutional ceremonies, taught in schools, yet functionally degraded by the gap between its guarantees and state practice. The legal system maintains the document through institutional inertia — it provides legitimacy theater while the actual operation of law remains untethered from the text. Theater ratio rises as generations pass and the promissory gap widens.
constraint_indexing:constraint_classification(terror_coincidence__sincere_blueprint_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational perspective, the sincere blueprint reading reveals a genuine structural hybrid: the constitution contains authentic drafting from reform communists (Bukharin et al.) who meant the promises as real commitments, yet the regime weaponizes the text as Terror cover. The constraint is neither pure coordination (the promise is suppressed in schedule) nor pure extraction (the text is sincere, the promise is real) — it is the tragic binding of authentic commitment to its own negation. Extractiveness reflects the conscription of hope into the Terror's legitimation.
constraint_indexing:constraint_classification(terror_coincidence__sincere_blueprint_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(terror_coincidence__sincere_blueprint_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(terror_coincidence__sincere_blueprint_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(terror_coincidence__sincere_blueprint_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(terror_coincidence__sincere_blueprint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(terror_coincidence__sincere_blueprint_reading, TR),
    TR >= 0.70.

:- end_tests(terror_coincidence__sincere_blueprint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the conscription of the drafters' and reform communists' authentic commitment into the regime's legitimation apparatus. The extractiveness is not maximal (0.72+) because the constraint contains genuine coordination elements — the constitutional text really does represent some sincere policy commitments, and the promise is not entirely retracted, only scheduled indefinitely. The extraction is in the use of the document to cover the Terror while suppressing the schedule, not in the negation of the promise itself. Suppression (0.72): High. The regime actively suppresses demands to implement the constitutional guarantees, criminalizes citation of specific rights (e.g., invoking collective bargaining or secret ballot), and executes those who press the schedule. Yet the statement (the text itself) is preserved and celebrated — this asymmetry between statement preservation and schedule suppression is the mechanism of the constraint. Theater ratio (0.55): Moderate. The constraint is genuinely hybrid — not pure theater (some drafters meant it sincerely), not pure function (the promises are not implemented). The theater increases over time as the gap between text and practice widens. By the 1950s, the constitution is cited in ceremonies and schools while remaining functionally degraded — the theater ratio rises. The measurement trajectory (0.35 → 0.55 from 1936 to 1940) reflects the rapid transition from sincere drafting and public discussion (1936) to the Terror's use of the text as cover and the subsequent institutional embedding of the gap between promise and practice.
 *
 * PERSPECTIVAL GAP:
 *   The sincere blueprint reading produces maximal perspectival gaps. The drafters see a Snare (trapped, their commitment weaponized). The regime sees a Rope (pure coordination, the text legitimates power). The reform communists see a Tangled Rope (they have agency and benefit, yet the unfulfilled promise constrains them). The Soviet legal establishment sees a Piton (the text persists through institutional theater, not functional force). The analytical observer sees a Tangled Rope with tragic structure (genuine hybrid of commitment and negation). The perspectival gap reveals the constraint's extractive mechanism: the same text that represents authentic policy commitment to some agents (the sincere drafters, the reform communists) is a tool of power to others (the regime, the security apparatus). The gap is not a misunderstanding — it is the structural reality of the constraint. The sincere blueprint reading is distinguished from its siblings precisely by recognizing this gap as tragic rather than cynical.
 *
 * DIRECTIONALITY LOGIC:
 *   The drafters experience maximum directionality toward victimhood (d ≈ 0.95) — trapped agents whose authentic commitment is weaponized against them, executed by the regime. The regime experiences maximum directionality toward beneficiary status (d ≈ 0.05) — uses the promise for legitimation while executing its authors. Reform communists experience moderate directionality (d ≈ 0.60) — constrained agents who benefit from having the sincere blueprint as a legitimating reference, yet extract no structural capacity from citing it. The constraint's extractiveness (χ) depends critically on the observer's position. For trapped drafters, χ approaches the snare maximum. For the regime, χ is negative (the text is a tool of power). For reform communists, χ is moderate (they have agency and some benefit, but the schedule's suppression extracts their reform agenda into futility). The analytical observer sees the tragic structure: the constraint's extractiveness is proportional to how much authentic hope it represents — the more sincerely the drafters believed, the greater the extraction of that hope into the Terror's cover.
 *
 * MANDATROPHY ANALYSIS:
 *   The sincere blueprint reading resolves the mandatrophy by showing that the 1936 constitution is neither pure extraction (pure Snare — the text is sincere, not a lie) nor pure coordination (pure Rope — the promises are suppressed in schedule, not in statement). It is a Tangled Rope precisely because it contains both genuine policy coordination (the sincere drafting of guarantees) and asymmetric extraction (the regime's suppression of the schedule and execution of the drafters). The mandatrophy dissolves when we recognize that the constraint's type depends on the observer: trapped drafters experience Snare; the regime experiences Rope; reform communists experience Tangled Rope; the analytical observer sees the hybrid. No single type is 'correct' — the presheaf of classifications IS the analytical content. The sincere blueprint reading's unique contribution is to insist on the reality of the drafters' sincerity and the tragedy of that sincerity's conscription into the Terror.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bukharin_authorship_definiteness,
    'Did Bukharin and the reform communist drafters genuinely author the constitutional guarantees as sincere policy commitments, or were they performing commitment while knowing the regime had no intent to implement them?',
    'Textual analysis of draft manuscripts and revision history; interviews with surviving drafters (Maiskii, Mekhlis); comparison of guarantees in earlier draft versions with final text; historical context of Bukharin''s platform in 1934–1936 (theory of ''growing into socialism'')',
    'If sincere authorship: the constraint is a genuine hybrid (Tangled Rope) with tragic structure — authentic hope conscripted into Terror cover. If performative commitment: the constraint becomes plebiscitary theater (closer to pure Snare from drafters'' perspective, pure Rope for regime).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bukharin_authorship_definiteness, empirical, 'Whether constitutional guarantees were sincerely authored or performatively inscribed').

omega_variable(
    promise_schedule_suppression_mechanism,
    'By what structural mechanism does the regime suppress the constitution''s schedule (the timeline of promised reforms) while preserving the statement (the text)?',
    'Analysis of regime discourse: propaganda emphasizing achievement of the ''most democratic constitution,'' simultaneous suppression of demands to implement specific guarantees; examination of purge trials where constitution''s guarantees were invoked by defendants and how judges handled those invocations; track regime''s response to reform attempts citing constitutional text (e.g., 1956–1968 reform cycles)',
    'If suppression is active (regime explicitly says ''not yet'' or ''conditions not ripe''): constraint is Tangled Rope with clear beneficiary-victim asymmetry. If suppression is passive (regime ignores the schedule entirely, treats the promise as already fulfilled): constraint approaches pure Snare from drafters'' perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(promise_schedule_suppression_mechanism, empirical, 'Structural mechanism of schedule suppression vs statement preservation').

omega_variable(
    reading_kernel_foreclosure,
    'Does the sincere blueprint reading logically foreclose the other two readings (legitimation-during-purge, plebiscitary-theater) in a single coherent framework, or do all three readings coexist as live positions?',
    'Logical reconstruction of each reading''s core premises; check for direct contradiction: Can one accept that the text is a sincere blueprint AND simultaneously accept that it legitimated the purge? Can one accept sincere authorship AND that the consultation was pure theater? Or do the readings occupy genuinely incompatible frameworks?',
    'If foreclosure: the sincere blueprint reading is incompatible with the legitimation-during-purge reading within the Soviet regime''s self-presentation (a framework contradiction). If coexistence: all three readings remain live positions held by different parties (scholars, survivors, regime apologists). Resolution determines the reading_relations structure in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Logical relationships between sincere blueprint reading and sibling readings').

omega_variable(
    reform_communist_retroactive_benefit,
    'Do later reform communists (Khrushchev cohort, Gorbachev era) actually benefit from having Bukharin''s sincere constitutional guarantees in writing, or is citing the text a performative legitimation move that does not expand their actual political capacity?',
    'Historical analysis of how reform communists invoked the 1936 constitution (1956 thaw, 1968 Prague Spring, 1985–1991 glasnost period); did citing constitutional guarantees produce structural changes or only rhetorical cover? Did regimes grant concessions when citing the text?',
    'If genuine benefit (text expands reform capacity): the constraint is Tangled Rope from reform communists'' perspective — they are beneficiaries of having the sincere blueprint, even if unfulfilled. If rhetorical only (text has no structural force): the constraint is closer to Snare — the promise extracts their hope without yielding capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_communist_retroactive_benefit, empirical, 'Whether reform communists received tangible benefit from constitutional citations').

omega_variable(
    committer_reading_identity,
    'Which reading instantiates the Soviet regime''s ACTUAL self-presentation during the 1936–1938 period: sincere blueprint (we meant it, conditions not yet ripe), legitimation-during-purge (the constitution justifies the Terror), or plebiscitary-theater (consultation was genuine)?',
    'Analysis of official regime rhetoric 1936–1938: speeches, press, constitutional commentary; examination of how the regime reconciled constitution-as-promise with purge-by-quota; reconstruction of the regime''s actual committed framework (not retrospective apologetics)',
    'If sincere blueprint was the regime''s actual self-presentation: the regime was genuinely committed to the deferred promise, making the purge a tragic internal contradiction (not deliberate cynicism). If legitimation-during-purge was the regime''s actual frame: the Terror and constitution were always coordinated from the start (pure extraction). The reading that matches the regime''s actual committed self-presentation is the most diagnostically important for understanding the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_identity, conceptual, 'Which reading matches the regime''s actual committed framework vs retrospective interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(terror_coincidence__sincere_blueprint_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terror_sincere_tr_t0, terror_coincidence__sincere_blueprint_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(terror_sincere_tr_t2, terror_coincidence__sincere_blueprint_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(terror_sincere_tr_t4, terror_coincidence__sincere_blueprint_reading, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(terror_sincere_be_t0, terror_coincidence__sincere_blueprint_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(terror_sincere_be_t2, terror_coincidence__sincere_blueprint_reading, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(terror_sincere_be_t4, terror_coincidence__sincere_blueprint_reading, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(terror_coincidence__sincere_blueprint_reading, attachment_coordination).
narrative_ontology:affects_constraint(terror_coincidence__sincere_blueprint_reading, terror_coincidence__legitimation_during_purge_reading).
narrative_ontology:affects_constraint(terror_coincidence__sincere_blueprint_reading, terror_coincidence__plebiscitary_theater_reading).

% DUAL FORMULATION NOTE:
% The 1936 Soviet Constitution (terror_coincidence kernel) decomposes into three constraint stories representing three readings of the same contested kernel. The sincere_blueprint_reading instantiates the framework of reformist communist commitment — the constitution as deferred promise. The legitimation_during_purge_reading instantiates the framework of the Terror apparatus — the constitution as operational cover. The plebiscitary_theater_reading instantiates the framework of mobilization theory — the constitution as rehearsed assent. Each reading has its own ε value, its own beneficiary/victim structure, its own type. They coexist as live positions in different parties' frameworks; none forecloses the others in historical fact. The sincere_blueprint_reading is distinguished by insisting on the authentic authorship of some drafters and the tragic (not cynical) structure of the constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
