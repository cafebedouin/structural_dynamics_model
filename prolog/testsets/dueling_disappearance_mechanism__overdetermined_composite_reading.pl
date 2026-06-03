% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_overdetermined, []).

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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Dueling's Disappearance (Overdetermined Mechanism Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   Dueling's disappearance from American and Western European legal codes
 *   between 1790 and 1890 is a constraint that defies single-mechanism
 *   explanation. Four independent sufficient conditions acted simultaneously:
 *   (1) Legal prohibition through explicit criminalization; (2) Institutional
 *   modernization that substituted courts, banking systems, and libel law for
 *   honor-based dispute resolution; (3) Cultural shift from honor-culture to
 *   dignity-culture axioms, making dueling conceptually unthinkable; (4)
 *   Civil War trauma (in the American case) that delegitimized martial
 *   violence as dispute mechanism. The constraint is the collective
 *   enforcement of dueling's elimination—the structural pressure against the
 *   practice exercised by multiple institutions, laws, cultural narratives,
 *   and historical injuries. This reading asserts that the constraint cannot
 *   be decomposed into single mechanisms without losing explanatory power.
 *   Dueling died because ALL FOUR mechanisms converged, each sufficient in
 *   isolation but appearing together with overdetermined force. The challenge
 *   for the framework: overdetermined constraints have unstable ε because the
 *   causal attribution is non-separable. Which mechanism do we measure? The
 *   legal prohibition alone? The cultural shift alone? The institutional
 *   displacement alone? Or the composite interaction?
 *
 * KEY AGENTS:
 *   - Honor-culture aristocracy (powerless/trapped/identity_locked): Practitioners fused with honor-culture axioms; lose identity-constitutive practice through legal, institutional, and cultural erasure. Snare victim.
 *   - Modernizing legal institutions (institutional/arbitrage): Courts, legal codes, libel law. Capture dispute-resolution authority. Rope beneficiary—pure coordination from institutional perspective.
 *   - Republican state authority (organized/constrained): Centralize monopoly on violence; enforce dignity-culture legitimacy; suppress honor-culture claims to legitimate dispute mechanism. Mixed cost and benefit—tangled rope from this perspective.
 *   - Merchant-banking faction (institutional/arbitrage): Commercial elites benefiting from dueling's removal (stability, predictability, credit). Rope beneficiary—coordination benefit without extraction cost.
 *   - Bourgeois emerging elite (powerful/mobile): Voluntary participants in constraint enforcement (cultural shift, legal support). Benefit from dueling's decline; mobile enough to resist if they chose. Tangled rope—some coordination benefit, some extraction of honor-culture's legacy status.
 *   - Conflicted magistrates (moderate/constrained): Enforcing unpopular laws while some constituencies remain honor-bound. Caught between two legitimacy systems. Tangled rope—mixed benefits and costs.
 *   - Analytical observer (analytical/analytical): Risks naturalizing overdetermined outcome as historical inevitability. False summit candidate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.52).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.68).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Dueling's Disappearance (Overdetermined Mechanism Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '0b1b1c87-0a82-4787-a246-7607110a843d').
narrative_ontology:cs_kernel_codification('0b1b1c87-0a82-4787-a246-7607110a843d', fixed_text).
narrative_ontology:cs_authority_grounding('0b1b1c87-0a82-4787-a246-7607110a843d', distributed).
narrative_ontology:cs_reading_relation('0b1b1c87-0a82-4787-a246-7607110a843d', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b1b1c87-0a82-4787-a246-7607110a843d', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('0b1b1c87-0a82-4787-a246-7607110a843d', foundational, multiple_sufficient_mechanisms_non_separable).
narrative_ontology:cs_axiom_status(multiple_sufficient_mechanisms_non_separable, holdable).
narrative_ontology:cs_axiom_grounding('0b1b1c87-0a82-4787-a246-7607110a843d', multiple_sufficient_mechanisms_non_separable, empirically_contingent).
narrative_ontology:cs_axiom('0b1b1c87-0a82-4787-a246-7607110a843d', foundational, monocausal_reduction_obscures_complexity).
narrative_ontology:cs_axiom_status(monocausal_reduction_obscures_complexity, holdable).
narrative_ontology:cs_axiom_grounding('0b1b1c87-0a82-4787-a246-7607110a843d', monocausal_reduction_obscures_complexity, conventional).
narrative_ontology:cs_reference_frame('0b1b1c87-0a82-4787-a246-7607110a843d', causal_separability_presumption).
narrative_ontology:cs_drift_state('0b1b1c87-0a82-4787-a246-7607110a843d', contemporary_historical_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0b1b1c87-0a82-4787-a246-7607110a843d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, modernizing_legal_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_merchant_class).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, republican_political_authority).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, civil_stability_faction).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_aristocracy).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, martial_identity_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOUND DUELIST — Aristocratic practitioner trapped by identity fusion with honor-culture axioms. Structurally mobile (could refuse a challenge, flee jurisdiction) but identity-locked: honor-culture framing makes refusal unthinkable. Experiences the constraint as pure extraction — the legal, institutional, and cultural mechanisms converge to eliminate the social position the duelist's identity inhabits. No exit that preserves self-conception.
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__overdetermined_composite_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONFLICTED MAGISTRATE — Legal official caught between honor-culture legitimacy (some constituencies still honor-bound) and modernizing legal authority (institutional pressure to criminalize dueling). Constrained by conflicting legitimacy systems. Benefits from modernizing institutions (career advancement, professional prestige) but pays cost of enforcing unpopular laws. Mixed extraction and coordination.
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MODERNIZING LEGAL SYSTEM — Institutional beneficiary with arbitrage options. Legal modernization captures dispute-resolution authority (contracts, libel, property claims formerly settled by duel now settled by court). Experiences dueling prohibition as pure coordination: the constraint solves the collective action problem of displacing an alternative dispute mechanism. Net beneficiary — no victims from this perspective.
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__overdetermined_composite_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MERCHANT-BANKING FACTION — Commercial interests that benefit from honor-culture displacement. Dueling's decline removes violent dispute mechanism that destabilized commerce and credit. No extractive cost to this group — dueling prohibition is pure coordination from their perspective (removes risk, enables contract-based commerce). Benefits without burden.
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__overdetermined_composite_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUBLICAN STATE AUTHORITY — Organized institutional actor navigating the shift from honor-culture to dignity-culture legitimacy. Criminalizing dueling coordinates multiple mechanisms simultaneously: legal prohibition, institutional substitution (courts), cultural pressure. Experiences enforcement costs (resistance from honor-culture constituencies) but gains state sovereignty. Mixed coordination and extraction from enforcement overhead.
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EMERGING BOURGEOIS ELITE — Powerful agents with high exit options (could maintain honor-culture if they chose; could emigrate). Benefit from dueling's decline (removes violent threat to property accumulation, enables dignity-based status competition). Participate in constraint's enforcement (cultural shift, institutional support for legal prohibition). Mobile enough to resist if they opposed it; choose not to. Tangled rope: some coordination benefit (stability), some extraction of honor-culture's legacy.
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — Risk of naturalization: viewing dueling's decline as inevitable consequence of modernization itself (a law of historical development). The constraint appears as an immutable natural law — societies modernize, honor cultures give way to dignity cultures, violence is incompatible with market society. This perspective naturalizes what is actually a contingent overdetermined outcome orchestrated by multiple mechanisms. False summit candidate: benefits identifiable (state authority, commercial interests) and extraction is real (honor-culture displacement).
constraint_indexing:constraint_classification(dueling_disappearance_mechanism__overdetermined_composite_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dueling_disappearance_mechanism__overdetermined_composite_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dueling_disappearance_mechanism__overdetermined_composite_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint extracts honor-culture identity from aristocratic practitioners (high extraction for them) but coordinates benefits for multiple institutional beneficiaries (legal system, commercial class, state authority). The ε value represents the composite impact across all four mechanisms. The measurement trajectory shows rising extractiveness across the interval (0.18 → 0.64), reflecting the progressive strengthening of all four mechanisms simultaneously. Suppression (0.68): High. Multiple enforcement mechanisms converge: legal penalties, institutional barriers to honor-based dispute resolution, cultural delegitimization, and (in America) Civil War trauma that makes martial honor unthinkable. Suppression rises from 0.25 (prewar period, when legal prohibition was nascent but not yet enforced; cultural shift was incomplete) to 0.71 (postwar, when all mechanisms are consolidated and enforcement is total). Theater ratio (0.55): Moderate. The constraint contains both genuine institutional displacement (courts actually function as alternatives to dueling; this is not theater) and performative enforcement (legal prohibition without genuine alternative can appear performative). The moderate theater ratio reflects that the constraint mixes functional institutional innovation (low theater) with cultural performance of virtue through law-enforcement (higher theater). Tangled rope classification: The constraint requires active enforcement (legal penalties, institutional substitution, cultural messaging), has multiple beneficiaries (legal institutions, merchants, state, bourgeoisie) and identifiable victims (honor-culture practitioners). Extraction and coordination coexist—the state coordinates modernization while extracting from those whose identity depends on honor-culture practice.
 *
 * PERSPECTIVAL GAP:
 *   The bound duelist sees a snare (identity-locked trap with no exit that preserves self). The modernizing legal system sees a rope (pure coordination of dispute-resolution mechanisms). The republican state sees a tangled rope (coordination benefit with enforcement cost). The merchant sees a rope (pure coordination benefit, no cost). The bourgeois elite sees a tangled rope (benefits from modernization while extracting residual status from honor-culture displacement). The analytical observer risks seeing a mountain (historical inevitability) but structural data reveals this as a false summit—the overdetermined mechanisms are orchestrated by identifiable beneficiaries. The perpectival gap is extreme: from snare for practitioners to rope for institutional beneficiaries. No single classification is 'correct'—the divergence reveals the structural inequality: what appears as coordination (inevitable modernization) to the beneficiary appears as extraction (identity erasure) to the victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim status and exit options. Honor-culture practitioners are victims with trapped/identity_locked exit (high d → high extraction experienced). Modernizing legal institutions are beneficiaries with arbitrage exit (low d → low/negative extraction experienced). Republican state is both organizer of mechanisms and constrained enforcer (medium d from mixed status). The divergence in d values (0.05 for institutional beneficiaries, 0.95 for trapped practitioners) produces the perspectival gap. Chi scales by f(d): beneficiaries experience effectively negative extraction (the constraint subsidizes them); victims experience maximum extraction (the constraint extracts from them). Spatial scope (national) applies moderate scaling (σ = 1.0); if we considered this constraint at global scope (cross-cultural dueling disappearance), σ would rise and chi would be amplified.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by insisting on the overdetermined structure itself. The classical mandatrophy question is: 'Is dueling a snare (pure extraction) or a rope (coordination)?' This reading answers: 'Both and neither—it is a tangled rope whose classification depends on which mechanism is measured, and all mechanisms act together.' The constraint exhibits snare-like properties (identity destruction for practitioners, no escape) and rope-like properties (genuine institutional innovation, solution to real coordination problems). The mandatrophy is not resolvable by picking one type; it is resolved by showing that the constraint is constitutively hybrid. Four sufficient mechanisms create overdetermined impact: no single mechanism is necessary, yet none is contingent. This reading forecloses the reduction to a single primary mechanism (contraction reading or institutional displacement reading) and insists on the composite causal structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_separability_asymptote,
    'Can the causal contributions of four mechanisms (legal prohibition, institutional substitution, cultural shift, Civil War trauma) be empirically disentangled, or are they fundamentally non-separable?',
    'Comparative historical analysis: examine societies where mechanisms acted in different orders or combinations (e.g., cultural shift without legal prohibition, legal prohibition without Civil War). Identify effect sizes per mechanism where feasible; map counterfactual pathways.',
    'If separable: each mechanism''s ε is measurable; this reading decomposes into four distinct constraints. If non-separable: ε is an emergent property of the interaction (truly overdetermined); this reading stands as unified. Chi classification depends on which mechanisms are measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mechanism_separability_asymptote, empirical, 'Whether causal mechanisms in dueling''s decline are separable').

omega_variable(
    hierarchy_of_sufficiency,
    'Among the four sufficient mechanisms, which would have been sufficient alone, absent the others?',
    'Counterfactual historical reasoning: suppress each mechanism and assess whether dueling would have declined. Test predictions against analogous societies with different mechanism subsets. Look for critical junctures where one mechanism became binding constraint.',
    'If one mechanism is necessary while others are contingent accessories: primary ε inheres in the binding mechanism; constraint type reclassifies around it. If all four are truly independent sufficient conditions: overdetermined reading holds; no single mechanism is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hierarchy_of_sufficiency, conceptual, 'Relative sufficiency of component mechanisms').

omega_variable(
    civil_war_trauma_counterfactual,
    'How much of dueling''s decline would have occurred without Civil War trauma? Was the war a necessary accelerant or a sufficient-but-not-necessary contributor?',
    'Comparison of pre-war dueling decline trajectories in European societies (France, Germany, Denmark) without equivalent civil war trauma. Measure rate of decline absent war. Compare to American trajectory with war. Assess whether war compressed timeline or changed mechanisms.',
    'If war was accelerant only: mechanism remains tangled_rope but with different temporal dynamics. If war was necessary: overdetermined reading partially collapses (three mechanisms insufficient without war). If war was sufficient alternative: dueling would have died anyway on different timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_war_trauma_counterfactual, empirical, 'Necessity vs. contingency of Civil War trauma').

omega_variable(
    victim_set_ambiguity,
    'Who are the victims of dueling''s decline? Honor-culture practitioners who lost an identity-constitutive practice? Or prior victims of dueling (corpses) prevented by the constraint?',
    'Temporal analysis: count dueling deaths averted vs. social costs to honor-culture practitioners. Ethnographic reconstruction of how honor-culture persons experienced the constraint. Compare narratives of loss vs. narratives of liberation.',
    'If primary victims are honor-culture practitioners: constraint is extractive on them (snare from their perspective). If primary victims averted are dueling-death victims: constraint is protective. Ambiguity affects whether this is a snare or a protective institutional innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_ambiguity, conceptual, 'Identity of primary victims: practitioners or prevented casualties').

omega_variable(
    reading_separability_kernel,
    'Does the overdetermined reading foreclose the contraction_reading (dignity-culture displaces honor-culture) or coexist with it?',
    'Conceptual analysis: if dignity-culture shift is ONE of four sufficient mechanisms, does asserting multiple mechanisms preclude asserting cultural shift as THE primary mechanism? Or do these readings name different levels of analysis (overdetermined=causal mechanisms; contraction=cultural genealogy)?',
    'If overdetermined forecloses contraction: the two readings cannot both be true in the same framework; kernel has genuine logical structure. If they coexist: they are different descriptions of the same event at different analytical scales; both readings are live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_separability_kernel, conceptual, 'Logical relationship between overdetermined and contraction readings').

omega_variable(
    extractiveness_attribution_instability,
    'The base extractiveness (0.52) is assigned to the composite mechanism. But if mechanisms are non-separable, which agent experiences extraction?',
    'Detailed stakeholder analysis: for each of the four mechanisms, identify who benefits and who pays. If mechanisms create different victim sets (e.g., legal prohibition victimizes only practitioners; Civil War trauma victimizes soldiers; institutional displacement victimizes honor-based dispute adjudicators), then ε is distributed across mechanisms, not composite. Measure whether one beneficiary set is stable across all mechanisms or different.',
    'If beneficiary set is stable: one coordinated extraction mechanism (tangled_rope). If beneficiary sets differ: four separate constraints masquerading as one. Dissolution of this reading into four distinct overdetermined components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_attribution_instability, empirical, 'Stability of beneficiary set across mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1790, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dueling_theater_t0_prewar, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dueling_theater_t1_earlywar, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1, 0.48).

% Extraction over time
narrative_ontology:measurement(dueling_extract_t0_prewar, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(dueling_extract_t1_earlywar, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1, 0.31).
narrative_ontology:measurement(dueling_extract_t2_postwar, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(dueling_extract_t3_institutional_consolidation, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 3, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(dueling_suppress_t0_prewar, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(dueling_suppress_t1_earlywar, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1, 0.48).
narrative_ontology:measurement(dueling_suppress_t2_postwar, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(dueling_suppress_t3_consolidation, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 3, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% The dueling disappearance mechanism kernel has three distinct readings, each constituting a separate constraint with different ε values and causal structures. The overdetermined reading (this file) claims causal non-separability and composite ε (0.52). The contraction reading measures cultural shift alone (lower ε, snare to rope transition). The institutional displacement reading measures institutional substitution alone (moderate ε, rope with some coercion). All three readings describe the same historical event but decompose its causal structure differently. This reading argues the others underestimate mechanism interaction; the sibling readings argue this reading overestimates causal density. The network link is upstream (overdetermined) to siblings (contraction and institutional displacement) because if overdetermined is correct, the sibling mechanisms are partially spurious—they are effects of the composite, not independent causes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
