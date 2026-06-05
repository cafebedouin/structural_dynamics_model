% ============================================================================
% CONSTRAINT STORY: hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_amnesia_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_amnesia_reading
 *   human_readable: Hybrid Amnesia Reading: Lapsed Closure Enabling Beneficiary Capture
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'market_as_natural_default.' The hybrid amnesia reading captures a
 *   two-stage causal sequence: (1) genuine forgetting of Keynesian and
 *   administered-market frameworks during the paradigm transition of
 *   1930s–1970s, driven by authentic epistemic shifts and institutional
 *   decay; (2) subsequent weaponization of that pre-existing amnesia by
 *   finance sector beneficiaries from 1980s onward, enabling asymmetric
 *   extraction through maintenance of policy alternatives that can no longer
 *   be articulated. The constraint is 'hybrid' precisely because it is
 *   neither pure institutional amnesia (inherited passive forgetting) nor
 *   pure beneficiary capture (actively created suppression), but rather the
 *   conversion of inherited epistemic damage into an active extraction
 *   mechanism. The reading asserts that beneficiaries did not need to invent
 *   amnesia — they inherited a genuinely degraded epistemic commons — but
 *   they did systematically weaponize and defend that forgetting through
 *   gatekeeping, theoretical rationalization, and institutional rules that
 *   made alternative frameworks literally unthinkable within policy
 *   institutions. The extractiveness rises from 0.20 (1975: mere epistemic
 *   loss, some alternatives still discussible) to 0.45 (2005: amnesia fully
 *   weaponized, alternatives actively suppressed). The theater ratio rises
 *   from 0.35 to 0.64 as the need to justify disabled policy levers becomes
 *   ever more elaborate. This reading directly influences and coexists with
 *   the lapsed_alternative_reading (which prioritizes epistemic accident over
 *   intentional suppression) and forecloses the
 *   beneficiary_maintained_reading (which denies that genuine amnesia
 *   preceded capture).
 *
 * KEY AGENTS:
 *   - Keynesian and Administered-Market Traditions (pre-1975): Intellectual frameworks that governed economic policy; gradually lost institutional coherence during 1960s-1975 through genuine epistemic challenges, policy failures, and generational replacement
 *   - Finance Sector and Capital Concentration Agents (1980s-present): Institutional beneficiaries who inherited degraded alternatives and systematized their suppression through gatekeeping, policy frameworks, and theoretical rationalization
 *   - Central Banks and State Institutions (hybrid position): Inherited both the amnesia and the gatekeeping machinery; simultaneously experience coordination function loss (cannot deploy discretionary tools) and extraction benefit (cannot be held accountable for policy alternatives)
 *   - Policy Alternatives Epistemic Commons (powerless): Abstract collective stock of institutional memory and policy language; bears the cost of amnesia as irreversibility increases
 *   - Labor and Redistributive Constituencies (constrained): Structurally mobile at national level but unable to organize because the language and concepts for their demands have been forgotten or suppressed
 *   - Neoclassical Economic Orthodoxy as Institution (piton actor): Performs and rationalizes naturalness despite contradictions; maintains theater through journal gatekeeping, textbook canon, and methodological policing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(hybrid_amnesia_reading, 0.58).
domain_priors:theater_ratio(hybrid_amnesia_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(hybrid_amnesia_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hybrid_amnesia_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_amnesia_reading, "Hybrid Amnesia Reading: Lapsed Closure Enabling Beneficiary Capture").
narrative_ontology:topic_domain(hybrid_amnesia_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(hybrid_amnesia_reading, formalized).
narrative_ontology:cs_authority_grounding(hybrid_amnesia_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(hybrid_amnesia_reading).
narrative_ontology:cs_kernel_id(hybrid_amnesia_reading, market_as_natural_default).
narrative_ontology:cs_reading_relation(hybrid_amnesia_reading, lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation(hybrid_amnesia_reading, beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_axiom(hybrid_amnesia_reading, foundational, amnesia_preceded_and_enabled_extraction).
narrative_ontology:cs_axiom_status(amnesia_preceded_and_enabled_extraction, holdable).
narrative_ontology:cs_axiom_grounding(hybrid_amnesia_reading, amnesia_preceded_and_enabled_extraction, empirically_contingent).
narrative_ontology:cs_axiom(hybrid_amnesia_reading, foundational, beneficiary_intentionality_in_suppression).
narrative_ontology:cs_axiom_status(beneficiary_intentionality_in_suppression, holdable).
narrative_ontology:cs_axiom_grounding(hybrid_amnesia_reading, beneficiary_intentionality_in_suppression, empirically_contingent).
narrative_ontology:cs_axiom(hybrid_amnesia_reading, foundational, market_naturalness_contingent_on_epistemic_loss).
narrative_ontology:cs_axiom_status(market_naturalness_contingent_on_epistemic_loss, holdable).
narrative_ontology:cs_axiom_grounding(hybrid_amnesia_reading, market_naturalness_contingent_on_epistemic_loss, deontological).
narrative_ontology:cs_reference_frame(hybrid_amnesia_reading, market_naturalness_institutional).
narrative_ontology:cs_drift_state(hybrid_amnesia_reading, contemporary_post_crisis_rationalization, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_amnesia_reading, finance_sector).
narrative_ontology:constraint_beneficiary(hybrid_amnesia_reading, capital_concentration_agents).
narrative_ontology:constraint_victim(hybrid_amnesia_reading, policy_alternatives_epistemic_commons).
narrative_ontology:constraint_victim(hybrid_amnesia_reading, labor_and_redistributive_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLICY ALTERNATIVES EPISTEMIC COMMONS (SNARE) — The collective stock of institutional memory regarding Keynesian policy, administered markets, and redistributive mechanisms has been degraded to the point where alternative frameworks are literally unavailable to policy actors. The commons bears full cost of this amnesia (hyperinflation risks, inequality acceleration, policy gridlock) with no capacity to exit or recover lost frameworks.
constraint_indexing:constraint_classification(hybrid_amnesia_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LABOR AND REDISTRIBUTIVE CONSTITUENCIES (SNARE) — Structurally mobile at the national level but constrained by the epistemic collapse of alternative economic framings. Cannot articulate demands using forgotten policy languages; cannot even conceive redistributive alternatives because institutional memory has been actively suppressed. Exit from constraint requires recovering amnesia — but the mechanisms generating amnesia remain operational.
constraint_indexing:constraint_classification(hybrid_amnesia_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANKS AND STATE INSTITUTIONS (TANGLED ROPE) — Experience genuine coordination function (price stability, counter-cyclical policy capacity) layered with asymmetric extraction through rules-based frameworks that foreclose discretionary intervention. Constrained by the very amnesia they inherited and now enforce — unable to articulate why certain tools are 'forbidden' without revealing the contingency of the market-as-natural reading.
constraint_indexing:constraint_classification(hybrid_amnesia_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCE SECTOR AND CAPITAL CONCENTRATION (ROPE) — Benefits directly from the amnesia: lack of policy alternatives expands financial extraction capacity. Experiences the constraint as pure coordination — 'efficient markets require minimal intervention.' The beneficiary's position is structurally stable because the amnesia persists; weaponizing pre-existing forgetting requires no new extractive mechanisms, only active maintenance of epistemic barriers.
constraint_indexing:constraint_classification(hybrid_amnesia_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NEOCLASSICAL ORTHODOXY AS INSTITUTION (PITON) — The apparatus of neoclassical economics has become substantially performative: sophisticated mathematical models that assert naturalness while obscuring the contingent institutional choices embedded in their assumptions. Theater ratio elevated by the need to justify, after the fact, why certain policy levers have been permanently disabled. The orthodoxy persists through institutional inertia (gatekeeping in journals, textbooks, central banks) even as contradictions accumulate.
constraint_indexing:constraint_classification(hybrid_amnesia_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER - HYBRID AMNESIA READING (TANGLED ROPE) — This reading captures the constraint as a genuine two-stage process: (1) authentic institutional forgetting of alternative frameworks during the 1930s-1970s transition, driven by genuine epistemic shifts and Keynesian hegemony decline; (2) defensive rationalization and weaponization of that pre-existing amnesia by beneficiaries from 1980s onward. The constraint is hybrid: it inherited amnesia as a structural feature, then agents with asymmetric power converted inherited forgetting into an active extraction mechanism.
constraint_indexing:constraint_classification(hybrid_amnesia_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_amnesia_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_amnesia_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_amnesia_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_amnesia_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness trajectory (0.20 → 0.45): The constraint begins as moderate epistemic damage (0.20 in 1975) — alternative frameworks still exist in academic discourse, policy actors still reference them, institutional memory persists though weakening. By 1985, genuine transition to neoclassical dominance is complete; extractiveness rises as beneficiaries recognize that policy levers have been disabled and begin defending this as permanent (0.28). By 1995, the constraint enters active-extraction phase as beneficiaries explicitly rationalize why capital controls are forbidden, why discretionary spending is inflationary, why labor can have no wage-setting power (0.38). By 2005, the constraint has reached mature extraction (0.45) — alternatives are not merely rare but literally unthinkable; policy actors trained in new frameworks cannot even parse the institutional arrangements they are replacing. Suppression (0.58): High but not total. Labor constituencies face real barriers (unemployability if they demand 'forbidden' alternatives, academic gatekeeping that blocks heterodox hiring), but not absolute imprisonment — they can exit by accepting the market-as-natural frame or by organizing outside policy institutions. Theater (0.35 → 0.64): Initially low because genuine epistemic transition is occurring — people believe the shift is natural/scientific. Theater rises sharply as it becomes apparent that naturalness assertions require increasingly elaborate defensive rationalization. By 2005, performing neoclassical models' assumed naturalness while evidence of contingency accumulates generates substantial theater. Claimed type (tangled_rope): The constraint coordinates price stability and counter-cyclical macroeconomic capacity (real coordination function) while asymmetrically extracting from labor through foreclosure of wage-setting, redistribution, and full-employment as live policy options. This is not pure extraction (snare) because genuine coordination function persists; it is not pure coordination (rope) because asymmetric power extraction is structural.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is dramatic and reveals the constraint's hybrid nature. Finance sector (institutional/arbitrage) perceives rope — natural market coordination with minimal intervention. Labor constituencies (moderate/constrained) perceive snare — policy alternatives have been deleted and only market-dependent strategies remain. Central banks (institutional/constrained) perceive tangled_rope — they coordinate price stability while unable to deploy discretionary intervention, unable even to articulate why certain tools are forbidden. The policy alternatives commons (powerless/trapped) perceives snare with irreversibility — amnesia has become structural because institutions have been rebuilt to require forgetting. The analytical observer perceives that all five of these are simultaneously true from their respective positions, and that the constraint depends on each actor's epistemic position: beneficiaries have high d (extract from constrained/trapped agents), victims have low d (constrained in ways that limit their effective power). The gap between finance (rope experience) and labor (snare experience) is maximal — they are measuring the same constraint with radically different epistemic access.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim declarations and exit options. Finance sector (beneficiary + arbitrage) derives low d (~0.15), experiencing minimal effective extraction from their position; the constraint subsidizes them. Labor constituencies (victims + constrained) derive high d (~0.80), experiencing substantial effective extraction; the constraint extracts from their structural position. Policy alternatives commons (victim + trapped) derives maximal d (~0.95) — the abstract collective has no exit option and bears full amnesia cost. Central banks (constrained institutional position with both beneficiary access and victim exposure) derive moderate d (~0.55) — they benefit from discretionary capacity loss (reduces accountability pressure) but are victims of the inability to deploy counter-cyclical tools during crises. The sigmoid f(d) maps these d values to experienced extractiveness multipliers; beneficiaries' f(d) is near negative (extractiveness flows toward them), victims' f(d) is above 1.0 (extractiveness flows away from them). Scope is global (σ=1.2), amplifying extraction because the amnesia is universal across policy institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID READING RESOLVES MANDATROPHY: The constraint is neither pure coordination (rope) nor pure extraction (snare) because it performs both functions simultaneously. It genuinely coordinates price stability and international trade flows (coordination). It simultaneously forecloses wage-setting power, full-employment policy, and redistributive capacity for labor agents (extraction). The tension is not irresolvable — tangled rope is exactly the type for hybrid coordination-extraction — but the resolution requires understanding that beneficiaries experience the constraint as rope because they access the coordination benefits while avoiding the extraction costs, while victims experience it as snare because they bear the extraction costs while denied the coordination benefits. The constraint exists because the coordination function (price stability, capital mobility, trade efficiency) is real and valued, but the beneficiaries have structured access to asymmetric benefit distribution. Removing the extraction would require either decoupling coordination from asymmetric benefit distribution (difficult, requires institutional redesign) or introducing countervailing power that beneficiaries currently lack (requires recovering forgotten policy alternatives — the amnesia constraint). This story resolves the mandatrophy by clarifying that the constraint persists through active beneficiary defense of the amnesia, not through cooperation. The moment the amnesia breaks (policy alternatives become re-available), the constraint either becomes pure rope (if power is truly symmetric) or requires explicit new enforcement mechanisms (if extraction was disguised by epistemic asymmetry).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_weaponized_amnesia,
    'Is the forgetting of alternative economic frameworks a genuine epistemic transition (paradigm shift) or an actively maintained extraction mechanism enabled by beneficiaries?',
    'Historical archive analysis of policy debates, journals, and textbook content across three periods: (1) 1960s-1970s active discussion of alternatives, (2) 1980s-1995 rapid suppression and gatekeeping, (3) 1996-present defensive rationalization. Identify inflection points where suppression became active vs. passive forgetting.',
    'If genuine: constraint is closer to rope (coordination failure) with piton elements (degraded institutions). If weaponized: constraint is tangled_rope with snare elements (asymmetric extraction). Classification hinges on whether beneficiaries inherited amnesia (tangled_rope) or created it (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_weaponized_amnesia, empirical, 'Whether amnesia is genuine paradigm shift or actively maintained extraction').

omega_variable(
    policy_alternative_recovery_capacity,
    'Can policy actors recover forgotten institutional alternatives (Keynesian tools, capital controls, administered pricing) if epistemic barriers are removed, or has institutional memory degradation become irreversible?',
    'Test case analysis: post-2008 crisis adoption of unconventional monetary policy (QE, forward guidance), Modern Monetary Theory academic revival, COVID-era fiscal experimentation. If recovery is rapid and functional: amnesia is reversible (identity_locked exit applies). If recovery is blocked or leads to policy incoherence: amnesia has structural lock-in (trapped exit).',
    'If reversible: constraints on alternatives are primarily cognitive/identity-based (identity_locked focus). If irreversible: constraints are structural/material (trapped exit). Affects classification of labor constituencies'' exit options and the tightness of the snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(policy_alternative_recovery_capacity, empirical, 'Whether forgotten policy alternatives can be recovered').

omega_variable(
    reading_kernel_ambiguity,
    'Does the market-as-natural kernel admit a reading where beneficiaries created amnesia through active suppression (hybrid_amnesia_reading), or only inherited it from epistemic transition (lapsed_alternative_reading)?',
    'Comparative reading analysis: this reading asserts that beneficiary capture followed genuine forgetting (two-stage: amnesia then weaponization). The sibling lapsed_alternative_reading treats amnesia as primary and captures-as-secondary. The distinction turns on causal ordering: did beneficiaries inherit forgotten alternatives and weaponize the pre-existing condition, or did they create the forgetting? Archive evidence on actor intentionality, policy statements, and institutional design choices during 1975-1995 transition.',
    'If this reading holds: beneficiaries are architects of extraction using amnesia as tool (active extraction). If sibling holds: beneficiaries are opportunists exploiting accidental epistemic collapse (passive rent-seeking). Affects culpability framing and remediation: active suppression requires institutional reform; passive drift may auto-correct if epistemic alternatives re-emerge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Causal ordering between amnesia and beneficiary capture').

omega_variable(
    theater_mechanism_in_amnesia,
    'Is the elevated theater ratio (0.64) driven by neoclassical economics performing its own naturalness despite contradictions, or by beneficiaries performing justifications for disabled policy tools?',
    'Institutional discourse analysis: examine central bank policy statements, IMF and World Bank justifications for capital controls prohibition, academic economics journal editorial gatekeeping. Distinguish between (a) orthodox economics asserting market naturalness (self-performative), and (b) beneficiaries explicitly justifying why alternatives must remain forbidden (defensive performative). Archive papers from policy institutions during crisis periods (1997-1998 Asian crisis, 2008 financial crisis, 2020 pandemic).',
    'If self-performative: theater is structural cost of maintaining orthodoxy coherence despite empirical contradictions (piton dynamic). If defensive: theater is extraction cost — beneficiaries must continuously rationalize disabled alternatives (snare/tangled_rope dynamic). Affects whether the constraint is degraded institution (piton) or active extraction (tangled_rope/snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_mechanism_in_amnesia, empirical, 'Whether theater is self-performative orthodoxy or defensive beneficiary justification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_amnesia_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_amnesia_theater_t0_1975, hybrid_amnesia_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hybrid_amnesia_theater_t3_1985, hybrid_amnesia_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(hybrid_amnesia_theater_t6_1995, hybrid_amnesia_reading, theater_ratio, 6, 0.6).
narrative_ontology:measurement(hybrid_amnesia_theater_t9_2005, hybrid_amnesia_reading, theater_ratio, 9, 0.64).

% Extraction over time
narrative_ontology:measurement(hybrid_amnesia_extractiveness_t0_1975, hybrid_amnesia_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hybrid_amnesia_extractiveness_t3_1985, hybrid_amnesia_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(hybrid_amnesia_extractiveness_t6_1995, hybrid_amnesia_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(hybrid_amnesia_extractiveness_t9_2005, hybrid_amnesia_reading, base_extractiveness, 9, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_amnesia_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hybrid_amnesia_reading, 0.18).
narrative_ontology:affects_constraint(hybrid_amnesia_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(hybrid_amnesia_reading, beneficiary_maintained_reading).
narrative_ontology:affects_constraint(hybrid_amnesia_reading, epistemic_gatekeeping_neoclassical).
narrative_ontology:affects_constraint(hybrid_amnesia_reading, capital_controls_international_prohibition).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the 'market_as_natural_default' kernel. Sibling readings are lapsed_alternative_reading (amnesia as accidental drift) and beneficiary_maintained_reading (naturalness as proven, not contingent). All three share the same empirical domain but differ on the causal ordering of epistemic transition vs. active suppression. The hybrid amnesia reading has ε=0.45 reflecting two-stage: inherited amnesia (lower baseline) + weaponization (extraction rise). Lapsed alternative reading would show monotonic rise without intentionality markup. Beneficiary maintained reading would reject amnesia framing entirely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_amnesia_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
