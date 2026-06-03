% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Honor Violence Legitimacy (Drop Reading): External Cost Suppression of Dueling Practice
 *   domain: legal_anthropology/historical_sociology/commitment_systems
 *
 * SUMMARY:
 *   The drop reading frames dueling's historical decline as a structural
 *   suppression of practice frequency while the conceptual-legal legitimacy
 *   of honor violence remains intact. In this reading, dueling remains
 *   thinkable as an honor mechanism throughout the suppression period —
 *   individuals can articulate the claim 'I have the right to defend my honor
 *   through challenge' while the state enforces the prohibition 'but you will
 *   be executed if you do.' The constraint is a tangled rope: state monopoly
 *   on violence creates genuine coordination benefits (centralized authority
 *   prevents honor-based fragmentation), while simultaneously extracting from
 *   honor claimants who cannot practice the honor mechanism their cultural
 *   framework still makes available. The extractiveness rises over the
 *   measurement interval (0.28 → 0.42) because external enforcement costs
 *   accumulate — execution, exile, social banishment spread to duelists'
 *   families — making the practice increasingly expensive without changing
 *   its legitimacy within the honor framework. Theater ratio rises (0.42 →
 *   0.55) as actual dueling becomes rare but honor-talk persists in military
 *   culture, literature, and aristocratic identity, creating performative
 *   continuity without functional enforcement. This reading explicitly
 *   disagrees with the contraction reading (which posits that honor itself
 *   was redefined to exclude violence) and distinguishes itself from the
 *   composite reading (which treats drop and contraction as simultaneous).
 *   The drop reading asserts temporal and logical priority: external
 *   suppression occurs first, creating a decoupling between legitimacy and
 *   practice; only later (or never, in some communities) does the honor
 *   concept itself shift to exclude violence.
 *
 * KEY AGENTS:
 *   - Honor Claimants (Military Officers / Aristocrats): Victims (powerless/identity_locked) — identity fused with honor-based status; state suppression creates impossible bind: cannot claim honor without violating law.
 *   - Military Officer Class: Organized beneficiaries and victims (organized/constrained) — benefit from honor-enforcement of military discipline and hierarchy; bear extraction from legal jeopardy and institutional precarity.
 *   - State Monopoly on Violence: Primary beneficiary (institutional/arbitrage) — centralizes legitimate violence authority; experiences dueling prohibition as coordination mechanism, not extraction.
 *   - Emergent Bourgeoisie: Partial beneficiary (powerful/mobile) — exit the honor system by choosing wealth-based status; constrained by inability to claim honor through traditional mechanisms but benefit from coordination away from honor-required violence.
 *   - Honor Code Ideology: Institutional residuum (institutional/arbitrage) — persists as cultural narrative and military tradition; degrades into piton (performative maintenance through inertia).
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent state monopoly as universal law; must attend to the drop reading's assertion that legitimacy persists despite practice suppression.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.38).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.62).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Honor Violence Legitimacy (Drop Reading): External Cost Suppression of Dueling Practice").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "legal_anthropology/historical_sociology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '70b1bbc2-23bc-4926-88b6-23f01ee73ddc').
narrative_ontology:cs_kernel_codification('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', distributed).
narrative_ontology:cs_authority_grounding('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', extraction).
narrative_ontology:cs_reading_relation('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', foundational, legitimacy_practice_decoupling_possible).
narrative_ontology:cs_axiom_status(legitimacy_practice_decoupling_possible, holdable).
narrative_ontology:cs_axiom_grounding('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', legitimacy_practice_decoupling_possible, deontological).
narrative_ontology:cs_axiom('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', foundational, external_suppression_sufficient_explanation).
narrative_ontology:cs_axiom_status(external_suppression_sufficient_explanation, holdable).
narrative_ontology:cs_axiom_grounding('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', external_suppression_sufficient_explanation, empirically_contingent).
narrative_ontology:cs_reference_frame('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', honor_legitimacy_intact_practice_accessible).
narrative_ontology:cs_drift_state('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', historical_transition_period_1750_1900, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('70b1bbc2-23bc-4926-88b6-23f01ee73ddc', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, state_monopoly_on_violence).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, emergent_bourgeoisie).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, civil_administration).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, honor_claimants).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, military_officer_class).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, kinship_lineages).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONOR CLAIMANT (SNARE) — Structurally mobile (could theoretically refuse dueling) but identity-fused with honor claim. The duelist cannot claim honor through non-violent mechanisms within the available cultural frame. Exit would require abandoning identity as a honorable person, not just paying a material cost. External state suppression (criminalization, execution) is the visible extraction mechanism; internal identity lock is the binding mechanism. Dueling remains thinkable but practically impossible — maximum tension between legitimacy and suppressibility.
constraint_indexing:constraint_classification(honor_violence_legitimacy__drop_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITARY OFFICER CLASS (TANGLED ROPE) — Organized enough to resist enforcement collectively but constrained by career consequences. Genuine coordination function: dueling within officer corps enforces hierarchy, loyalty, and courage norms that the institution values. Simultaneous extraction: the state extracts monopoly violence rights by criminalizing duels while using officers' honor-enforcement for military discipline. Officers benefit from the coordination (dueling deterred cowardice) while bearing the extraction (legal jeopardy, institutional precarity). Mixed experience — neither pure coordination nor pure extraction.
constraint_indexing:constraint_classification(honor_violence_legitimacy__drop_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE MONOPOLY ON VIOLENCE (ROPE) — Experiences dueling prohibition as pure coordination: centralizing legitimate violence claims under state authority solves the collective action problem of competing honor claims fragmenting allegiance. No experienced extraction — the state is the beneficiary. From this perspective, dueling suppression is coordination that benefits all actors who value centralized order. Dueling is structurally legitimate (the honor framework remains intact) but becomes practically unthinkable-to-enact because the state offers coordination benefits for renouncing challenge-based honor.
constraint_indexing:constraint_classification(honor_violence_legitimacy__drop_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMERGENT BOURGEOISIE (TANGLED ROPE) — Powerful enough to exit the honor-based status system and do exit, choosing reputation through wealth instead. But the constraint affects them through forced association: bourgeois cannot claim status through dueling without adopting the aristocratic frame (which they reject) or can claim status through wealth-based avenues (which escape the constraint entirely). For bourgeois, the constraint is partial: it coordinates bourgeois away from dueling (enabling capital accumulation without honor-defense distraction) while extracting legitimacy cost (they cannot claim honor through traditional mechanisms and must invent new status claims). Mixed benefit and cost — tangled rope.
constraint_indexing:constraint_classification(honor_violence_legitimacy__drop_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HONOR CODE IDEOLOGY (PITON) — The honor framework persists as a cultural artifact through narrative, literature, military honor courts, and institutional identity claims, despite being functionally superseded. Military academies still invoke honor; dueling appears in romantic literature; honor remains a cultural reference point. But the actual mechanism that made honor-claims forceful (the readiness to fight and die) has atrophied through state suppression. The ideology persists through institutional inertia and nostalgia — maintained not because it works but because it hasn't been fully replaced by an alternative status mechanism. Theater ratio (0.55) reflects that honor-talk continues substantially (speeches, codes, literary representation) while honor-enforcement (actual dueling) is rare or theatrical when it occurs.
constraint_indexing:constraint_classification(honor_violence_legitimacy__drop_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN ATTEMPT) — From a civilizational universalizing view, honor systems based on violent challenge appear as a natural law of human status competition: whenever hierarchy is unclear, agents resort to honor-based violence to establish position. This perspective reads dueling as emerging naturally from the logic of uncentralized authority and sees its decline as the inevitable result of centralization. However, this risks false summit: the analytical observer naturalizes what is a contingent institutional rearrangement (state monopoly on violence) and conflates structural illegitimacy (the state has made dueling illegal) with conceptual illegitimacy (honor itself is no longer available as a claim). The drop reading disagrees — honor remains conceptually available; only practice becomes rare.
constraint_indexing:constraint_classification(honor_violence_legitimacy__drop_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(honor_violence_legitimacy__drop_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honor_violence_legitimacy__drop_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, TR),
    TR >= 0.70.

:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The state extraction from honor claimants is real — execution, exile, and social banishment are severe costs — but extraction is partial rather than total because the honor framework remains conceptually available. An honor claimant who refuses to duel loses status but retains legal life; an honor claimant who duels loses life. The extractiveness is primarily the cost of conformity to state authority (refusing the honor claim) rather than coercive extraction per se. The measurement trajectory (0.28 → 0.42) reflects accumulating enforcement costs as the state's suppression mechanisms mature and spread deterrence. Suppression (0.62): High. State monopoly on violence actively prevents dueling through legal prohibition, execution, and institutional tracking (military courts, dueling registries, family banishment). Suppression is not total — dueling persists in some communities and subcultures — but is severe enough to make practice rare. Suppression remains high throughout the measurement interval because enforcement requires continuous institutional maintenance. Theater ratio (0.55): Moderate-high. Honor-talk persists substantially in military culture, romantic literature, and aristocratic identity (speeches, codes, ceremonies, memoirs) while honor-enforcement through dueling becomes rare or theatrical. The theater is neither pure performance (honor still carries real social consequence) nor pure function (actual duels are increasingly rare). The upward trajectory reflects that as practice becomes rarer, the cultural narrative increasingly carries the symbolic weight — the theater expands to fill the functional vacuum.
 *
 * PERSPECTIVAL GAP:
 *   The drop reading generates a characteristic perspectival gap between the honor claimant (snare: trapped by identity lock to the now-suppressed practice) and the state (rope: experiences coordination benefits from centralized violence authority). The honor claimant sees a constraint structured around identity impossibility — 'I am bound to defend my honor, but defending it is now illegal and fatal.' The state sees a coordination mechanism — 'Centralizing legitimate violence claims solves the fragmentation problem.' Neither sees the other's experience. The analytical observer risks a false summit by naturalizing the state's coordination as a universal law of centralized authority, missing the drop reading's insight: the state's success is not because honor-based violence is inherently unstable but because the state actively suppresses it through enforcement, leaving the honor claimant in a binding psychological trap. The military officer class inhabits the mixed position (tangled rope): they benefit from the coordination (hierarchy and loyalty norms) while bearing the extraction (legal jeopardy). The bourgeoisie escape partially by choosing an alternative status system (wealth-based honor), but do not fully escape the constraint's frame because they must navigate a social world where traditional honor claims remain conceptually available even if they reject them. This partial escape is key to the drop reading: the constraint works through suppression, not through redefinition. If honor had been truly redefined, bourgeois actors could claim honor through their new status frame without any reference to dueling. Instead, they must actively reject dueling while honor-claimants remain psychologically bound to it. The gap is structural, not perspectival.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to the constraint — whether they benefit from suppression (low d) or bear cost from suppression (high d). The state (beneficiary with arbitrage exit) derives d ≈ 0.05–0.15: the state benefits from dueling suppression and can arbitrage into alternative authority mechanisms, producing low effective extraction toward the state. Honor claimants (victims with identity_locked exit) derive d ≈ 0.90: they bear the full cost of suppression and cannot exit through redefining honor within their available identity frame, producing high experienced extraction. Military officers (organized beneficiaries bearing extraction) derive d ≈ 0.55–0.65: they benefit from honor-enforcement of discipline but bear the extraction of legal jeopardy, producing moderate-high extraction. Bourgeoisie (powerful beneficiaries with mobile exit) derive d ≈ 0.35–0.45: they bear the secondary constraint of not being able to claim traditional honor without adopting aristocratic identity, but they can exit entirely by choosing alternative status mechanisms, producing moderate extraction. The piton perspective (institutional/arbitrage/generational) derives d from the honor ideology's institutional position: the ideology benefits from continued narrative circulation but extracts nothing tangible, producing near-zero extraction (maintained through inertia, not benefit). The analytical observer uses the baseline analytical d ≈ 0.73, producing moderate-high extraction because the observer has no position in the constraint and maximum ambiguity about its structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The drop reading resolves the mandatrophy by showing that tangled rope is the correct classification when legitimacy and practice decouple without redefinition. The constraint has both genuine coordination (state monopoly on violence centralizes authority) and asymmetric extraction (honor claimants lose their available status mechanism). If only coordination were present, it would be rope. If only extraction were present, it would be snare. The mixture occurs because the state's coordination mechanism works via suppression of an alternative (honor-based) legitimacy claim. The mandatrophy is resolved by attending to the drop reading's key insight: that the constraint operates through the gap between what remains legitimate (honor-based status claims) and what becomes expensive (executing honor claims). This gap is what tangled rope captures. If the contraction reading were correct (honor itself was redefined), the constraint would be pure snare with lower suppression — the redefinition would solve the coordination problem by eliminating the honor claim entirely, leaving only the extractive machinery. The drop reading's tangled rope classification indicates that the coordination benefits (centralized authority) are genuine and substantial, not merely a cover story for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_versus_practice_decoupling,
    'Can a practice remain structurally legitimate (available as a claim within the cultural-legal framework) while becoming practically unexecutable (suppressed by external enforcement) without shifting the underlying conceptual framework?',
    'Textual and narrative analysis: Do legal codes, memoirs, literary works, and cultural documents of the period continue to describe dueling as a valid honor claim while documenting its practical suppression? Do individuals express the claim/suppression tension explicitly (e.g., ''I have the right to defend my honor, but the state will execute me for doing so'')?',
    'If yes: drop reading is confirmed — legitimacy decoupling is real, and the constraint can suppress practice while leaving the framework intact. If no: contraction reading may be better supported — conceptual redefinition occurred in parallel with external suppression, and the readings should be recombined as composite_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_versus_practice_decoupling, empirical, 'Whether legitimacy and practice can decouple without conceptual redefinition').

omega_variable(
    honor_redefinition_timeline,
    'At what point in the timeline of dueling suppression do legal/cultural texts begin redefining honor to explicitly exclude violence, rather than simply forbidding dueling while maintaining honor''s conceptual availability?',
    'Corpus linguistics on legal codes, philosophical works, military doctrine, and cultural commentaries from the suppression period (1750–1900). Identify the moment when texts shift from ''dueling is forbidden but honorable'' to ''honor means non-violent courage / loyalty / virtue.'' If such a shift occurs substantially after the initial suppression period, the drop reading (suppression without redefinition) preceded the contraction reading (redefinition).',
    'If redefinition post-dates suppression by 30+ years: drop reading temporal isolation confirmed. If simultaneous or redefinition precedes suppression: composite reading better explains the history.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_redefinition_timeline, empirical, 'Timeline of honor redefinition relative to dueling suppression').

omega_variable(
    external_cost_sufficiency,
    'Is state enforcement (execution, exile, social banishment of duelists'' families) a sufficient explanation for dueling''s decline, or does the decline require additional cultural mechanism (redefinition of honor itself)?',
    'Comparative historical analysis: Jurisdictions with strict dueling suppression vs. lenient enforcement; correlation between enforcement severity and actual practice frequency; identification of any regions where honor remained conceptually available but practice did NOT decline despite enforcement pressure (falsifying the drop reading) or where practice declined BEFORE legal suppression (supporting contraction reading over drop reading).',
    'If external cost alone explains decline: drop reading confirmed. If additional cultural mechanism needed: composite or contraction reading required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_cost_sufficiency, empirical, 'Whether external costs alone explain dueling decline or additional cultural mechanism is necessary').

omega_variable(
    identity_locked_mechanism_stability,
    'For honor claimants under the drop reading, does the identity lock (inability to imagine claiming honor through non-violent means) persist as long as dueling suppression remains active, or does the identity eventually shift to accept alternative honor claims (signaling a transition to the contraction reading)?',
    'Longitudinal textual analysis of memoirs, letters, and cultural narratives across generations of suppression (100+ years). Track whether individuals born after dueling suppression begins still experience identity conflict (honor-locked to dueling) or have internalized alternative honor claims as legitimate (identity shift to new honor frame).',
    'If identity lock persists across generations: drop reading stable. If identity lock erodes and new honor frames become psychologically available within a generation: contraction reading better explains long-term dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism_stability, empirical, 'Stability of identity lock across generations under external suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hvld_theater_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hvld_theater_t25, honor_violence_legitimacy__drop_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(hvld_theater_t50, honor_violence_legitimacy__drop_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(hvld_extract_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hvld_extract_t25, honor_violence_legitimacy__drop_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(hvld_extract_t50, honor_violence_legitimacy__drop_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hvld_suppress_t0, honor_violence_legitimacy__drop_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hvld_suppress_t25, honor_violence_legitimacy__drop_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(hvld_suppress_t50, honor_violence_legitimacy__drop_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The kernel honor_violence_legitimacy decomposes into three structurally distinct constraints based on the mechanism of dueling's decline. The drop reading (this file) posits external suppression of practice while legitimacy persists. The contraction reading posits conceptual redefinition of honor to exclude violence. The composite reading posits both mechanisms simultaneously. Each reading has its own extractiveness trajectory and perspectival structure. They are linked through network.affects_constraints because they are sibling readings of the same kernel and compete for explanatory priority in historical interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
