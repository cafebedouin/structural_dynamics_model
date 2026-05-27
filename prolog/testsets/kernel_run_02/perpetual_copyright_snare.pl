% ============================================================================
% CONSTRAINT STORY: perpetual_copyright_snare
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perpetual_copyright_snare, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: perpetual_copyright_snare
 *   human_readable: Perpetual Copyright as Institutional Extraction Mechanism
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The perpetual copyright snare traces from the Statute of Anne (1710)
 *   through its metamorphosis into modern copyright regimes. The statute's
 *   genius was to reframe copyright from a Stationers' Company monopoly into
 *   an author's right — a shift that appeared to liberate creators but
 *   established the logical structure that perpetual control could be
 *   justified. Over three centuries, successive term extensions (Sonny Bono
 *   Act, TRIPS harmonization) have converted copyright from a term-limited
 *   coordination mechanism into a functional perpetuity. The constraint
 *   exhibits the full asymmetry of a snare: derivative creators cannot access
 *   works to build new creations, the public domain (the epistemic commons)
 *   cannot organize to resist enclosure, publishers extract monopoly rents
 *   without increasing total creation, and the system maintains itself
 *   through legal theater (copyright registries, licensing bureaus, term
 *   extension legislation) rather than functional necessity. The Statute of
 *   Anne's ambiguity between statutory monopoly and common-law right — never
 *   fully resolved by Donaldson v. Beckett — created the interpretive space
 *   within which perpetuity became thinkable as an extension of property
 *   rights. That ambiguity remains generative: the false natural law view
 *   (copyright as inherent property right) depends on treating the statutory
 *   choice as inevitable rather than contingent.
 *
 * KEY AGENTS:
 *   - Derivative Creators: Primary victims (powerless/trapped) — cannot build on existing works without licensing; exit is structural impossibility
 *   - Public Domain / Epistemic Commons: Primary victim (powerless/trapped) — abstract collective that cannot organize; conceptualized as shrinking rather than growing
 *   - Publishers and Estate Holders: Primary beneficiaries (institutional/arbitrage) — extract monopoly rents through licensing control and term extensions
 *   - Living Authors: Secondary stakeholders (powerful/constrained) — benefit from copyright protection but suppressed by licensing costs, litigation risk, and publisher power imbalance
 *   - Copyright Regime / Institutional Machinery: Secondary institutional actor (institutional/arbitrage) — perpetuates itself through legal theater; beneficiaries of status quo renewal
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as inevitable property right
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perpetual_copyright_snare, 0.68).
domain_priors:suppression_score(perpetual_copyright_snare, 0.72).
domain_priors:theater_ratio(perpetual_copyright_snare, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perpetual_copyright_snare, extractiveness, 0.68).
narrative_ontology:constraint_metric(perpetual_copyright_snare, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(perpetual_copyright_snare, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perpetual_copyright_snare, snare).
narrative_ontology:human_readable(perpetual_copyright_snare, "Perpetual Copyright as Institutional Extraction Mechanism").
narrative_ontology:topic_domain(perpetual_copyright_snare, "legal_history/intellectual_property/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(perpetual_copyright_snare, 'c1345e62-76da-460d-ad4f-4202f0677161').
narrative_ontology:cs_created_at('c1345e62-76da-460d-ad4f-4202f0677161', '').
narrative_ontology:cs_kernel_codification('c1345e62-76da-460d-ad4f-4202f0677161', formalized).
narrative_ontology:cs_authority_grounding('c1345e62-76da-460d-ad4f-4202f0677161', extraction).
narrative_ontology:cs_interpretation_layer_present('c1345e62-76da-460d-ad4f-4202f0677161').
narrative_ontology:cs_reading_relation('c1345e62-76da-460d-ad4f-4202f0677161', copyright_as_incentive_mechanism, coexists_with).
narrative_ontology:cs_reading_relation('c1345e62-76da-460d-ad4f-4202f0677161', copyright_as_natural_right, forecloses).
narrative_ontology:cs_axiom('c1345e62-76da-460d-ad4f-4202f0677161', foundational, copyright_is_statutory_contingent_monopoly).
narrative_ontology:cs_axiom_status(copyright_is_statutory_contingent_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('c1345e62-76da-460d-ad4f-4202f0677161', copyright_is_statutory_contingent_monopoly, empirically_contingent).
narrative_ontology:cs_axiom('c1345e62-76da-460d-ad4f-4202f0677161', foundational, perpetuity_exceeds_incentive_necessity).
narrative_ontology:cs_axiom_status(perpetuity_exceeds_incentive_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c1345e62-76da-460d-ad4f-4202f0677161', perpetuity_exceeds_incentive_necessity, empirically_contingent).
narrative_ontology:cs_axiom('c1345e62-76da-460d-ad4f-4202f0677161', foundational, perpetual_copyright_extracts_rents_from_derivative_creators).
narrative_ontology:cs_axiom_status(perpetual_copyright_extracts_rents_from_derivative_creators, holdable).
narrative_ontology:cs_axiom_grounding('c1345e62-76da-460d-ad4f-4202f0677161', perpetual_copyright_extracts_rents_from_derivative_creators, empirically_contingent).
narrative_ontology:cs_reference_frame('c1345e62-76da-460d-ad4f-4202f0677161', statutory_monopoly_with_renewable_terms).
narrative_ontology:cs_drift_state('c1345e62-76da-460d-ad4f-4202f0677161', contemporary_perpetual_copyright_regime, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perpetual_copyright_snare, book_publishers).
narrative_ontology:constraint_beneficiary(perpetual_copyright_snare, copyright_heirs).
narrative_ontology:constraint_beneficiary(perpetual_copyright_snare, institutional_monopolists).
narrative_ontology:constraint_victim(perpetual_copyright_snare, derivative_creators).
narrative_ontology:constraint_victim(perpetual_copyright_snare, literary_public_domain).
narrative_ontology:constraint_victim(perpetual_copyright_snare, future_knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(perpetual_copyright_snare, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(perpetual_copyright_snare, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

constraint_indexing:constraint_classification(perpetual_copyright_snare, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(perpetual_copyright_snare, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(perpetual_copyright_snare, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(perpetual_copyright_snare, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perpetual_copyright_snare_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(perpetual_copyright_snare, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perpetual_copyright_snare, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(perpetual_copyright_snare, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(perpetual_copyright_snare, TR),
    TR >= 0.70.

:- end_tests(perpetual_copyright_snare_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint exhibits asymmetric extraction across time horizons. Derivative creators face permanent suppression; future generations inherit locked-in copyright terms. Publishers capture disproportionate revenue relative to author compensation. The extractiveness value accounts for: (1) monopoly pricing on licenses, (2) forced exclusion of derivative creators, (3) zero-cost extension of copyright terms on already-published works, (4) institutional lock-in via international treaties. The trajectory from 0.35 (1710, term-limited coordination) to 0.68 (2025, effective perpetuity) reflects accumulating extraction as the system matures. Suppression (0.72): Very high. Multiple barriers prevent derivative creators and the public from accessing locked works: (1) legal prohibition (copyright law itself), (2) licensing monopoly (single rightsholder controls access), (3) cost barriers (licensing fees often prohibitive), (4) enforcement threat (litigation risk for unauthorized use), (5) epistemic barriers (orphaned works have no identifiable rightsholder; public has no mechanism to challenge copyright claims). Suppression is not partial — it is structural impossibility for the trapped agent. Theater ratio (0.58): Moderate-high. Contemporary copyright machinery (registries, licensing bureaus, term-extension legislation) is substantially performative. The work is already created; copyright does not make creation happen. Legislation extending copyright terms for already-published works is pure rent-seeking theater, not incentive-generation. The ratio was lower in 1710 (0.35) when registration and licensing courts handled substantive legal determination; it has risen as actual enforcement becomes dispersed and symbolic. Claimed type (Snare): Confirmed by victim presence (derivative creators, public domain), high extraction (0.68), and high suppression (0.72). The constraint exhibits all snare markers: existence depends on suppressing alternatives (creative reuse), minimal coordination benefit (the work is already created), and high coercion (legal prohibition, licensing monopoly, litigation risk). The machinery persists because it extracts value, not because it enables creation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of perspectival divergence. The publisher sees Rope — they experience copyright as fair coordination of author compensation and distribution. The living author sees Tangled Rope — they benefit from protection but are suppressed by licensing costs and publisher power. The derivative creator sees Snare — they are trapped with no escape from copyright prohibition. The public domain sees Snare — it cannot organize or exit; it is the victim of enclosure. The institutional regime sees itself as Piton — it maintains elaborate machinery (registries, licensing bureaus) through inertia rather than function. The analytical observer risks seeing Mountain (natural law) — framing perpetual copyright as an inevitable consequence of property rights — but the false summit detector reveals the frame as naturalization of a contingent institutional choice. The perspectival gaps reveal that all six types coexist in the same structure: which type you see depends on your structural position (beneficiary vs. victim), time horizon (immediate license decision vs. civilizational knowledge commons), and exit options (arbitrage vs. trapped). The snare classification is the deepest truth — it is the perspective of the powerless trapped agent. That this perspective is rational and not merely negative is the diagnostic signal that the constraint is genuinely extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   The perpetual copyright constraint drives directionality (d) through beneficiary/victim declarations and exit-option differentiation. Publishers and estate holders are beneficiaries with high arbitrage capacity — they can forum-shop, license strategically, and exit copyright regimes that don't serve them by relocating to stronger jurisdictions. Their d value is low (~0.15-0.25), producing negative or minimal effective extraction (chi). Derivative creators are victims with trapped exit — they cannot access locked works without violating law; they cannot organize to change copyright law; they cannot create derivative works as an alternative path to income. Their d value is high (~0.90), producing maximum experienced extraction (chi). Living authors occupy the intermediate position (d ~0.55-0.65) — they benefit from copyright protection but are suppressed by licensing monopolies and locked into unfavorable publishing relationships. The public domain (epistemic commons) is a powerless victim with zero exit options (trapped at d ~0.95) — it cannot organize, cannot lobby, cannot escape enclosure. The directionality cascade reveals that the constraint's extractiveness is asymmetric: it is minimal for beneficiaries (who actively deploy it) and maximum for victims (who have no choice). This asymmetry is the signature of a snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The perpetual copyright snare resolves the mandatrophy (tension between multiple valid classifications from different perspectives) by demonstrating that the perspectives reveal different structural aspects of a single mechanism. The publisher's 'Rope' perspective shows the coordination function (copyright does solve the problem of compensating authors and distributing works). The derivative creator's 'Snare' perspective shows the extraction function (copyright prevents adaptive creation and locks in monopoly rents). The living author's 'Tangled Rope' perspective shows the mixture. The institutional regime's 'Piton' perspective shows the degraded machinery (theater ratio rising, function declining). The analytical observer's 'Mountain' perspective shows the naturalization trap (framing contingent institutional choice as inevitable property right). No single type is correct; all are structural facts from their respective positions. The mandatrophy is resolved by recognizing that the constraint is BOTH coordination (authors need incentives; readers benefit from published works) AND extraction (perpetual duration prevents derivative creation; monopoly rents exceed incentive necessity; institutional machinery persists through inertia). The snare classification is dominant because the extraction mechanism (suppression of derivative creators) would not exist without the beneficiaries enforcing it — it is not an accidental side effect but the operational payoff that sustains the system. Living authors and publishers accept perpetual copyright despite its social cost because perpetual copyright transfers value from derivative creators (who cannot exit) to existing rightholders (who have arbitrage options). The snare exists to sustain this extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_vs_common_law_perpetuity,
    'Was the Statute of Anne (1710) creating a new statutory monopoly or recognizing a pre-existing common-law right to perpetual copyright?',
    'Historical documentary analysis: did pre-Anne licensing practices treat copyright as perpetual or term-limited? What did legal scholars immediately before 1710 claim about copyright ownership? Did the Statute itself present copyright as a novel creation or as codification of existing practice?',
    'If statutory creation: copyright is a contingent institutional choice, and perpetuity is itself a contingent design choice (not inherent). If common-law recognition: the constraint operates from natural law and has no identified beneficiaries (falsifies FSM). The Statute''s own framing is interpretively contested — Stationers argued pre-existing right; reformers argued for new author protections — suggesting the ambiguity IS the constraint''s generative mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_vs_common_law_perpetuity, empirical, 'Whether Anne statute created or recognized perpetual copyright').

omega_variable(
    perpetuity_as_design_choice,
    'Is perpetual copyright duration a necessary feature of copyright as an institution, or a contingent choice that could be reversed without destroying copyright''s coordination function?',
    'Comparative analysis: do jurisdictions with term-limited copyright (most nations now use finite terms after TRIPS harmonization) show measurably lower creation rates, shorter creative production cycles, or reduced author compensation relative to perpetual-copyright regimes? Do creators in finite-term systems produce less? Do publishers invest less? Cross-national empirical data on creative output, author income, and publisher profitability with finite vs perpetual terms.',
    'If perpetuity is necessary: copyright''s extractiveness is lower (some extraction is inherent to the coordination function). If perpetuity is contingent: copyright is a snare masquerading as necessary coordination — extractiveness increases, and alternative institutional designs become visible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(perpetuity_as_design_choice, empirical, 'Whether perpetuity is necessary or contingent feature of copyright').

omega_variable(
    beneficiary_capture_of_reform,
    'Why have copyright term extensions (Sonny Bono Act, TRIPS term harmonization) consistently expanded duration toward perpetuity rather than moving toward shorter terms, despite economic evidence that shorter terms reduce rent-seeking without reducing creation?',
    'Historical legislative analysis: who lobbied for copyright extensions? Who benefits from each extension? Did creator organizations support perpetual copyright, or did publisher/estate interests drive extensions while framing them as author protection? What do creators actually report wanting vs. what legislators enacted?',
    'If beneficiary capture: the constraint''s suppression and extraction are actively maintained and expanded by institutional forces, confirming snare classification. If creator preference: copyright extensions represent author-protective coordination, shifting classification toward rope or tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_capture_of_reform, empirical, 'Whether copyright extensions serve creator protection or beneficiary extraction').

omega_variable(
    public_domain_scarcity_mechanism,
    'Is the public domain shrinking because copyright terms are extended indefinitely, or is perpetual copyright justified by actual ongoing creation and innovation that requires the incentive?',
    'Empirical measurement: What fraction of current copyrighted works (those still under copyright as of 2025) are actively being exploited commercially vs. locked in archives or abandoned by rightholders? For the abandoned works, does copyright duration predict abandonment probability? If most historical works are abandoned but locked by copyright, that indicates extraction without coordination value.',
    'If public domain shrinking due to term extensions on abandoned works: the constraint is pure extraction (snare confirmed). If most copyrighted works are actively managed and generating value: the constraint is mixed coordination and extraction (tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_domain_scarcity_mechanism, empirical, 'Whether copyright duration correlates with active exploitation or abandonment').

omega_variable(
    derivative_creation_suppression_magnitude,
    'How many potentially valuable derivative works (sequels, adaptations, remixes, critical editions, scholarly commentary) are never created because copyright licensing is prohibitively expensive or refused?',
    'Behavioral research: survey authors and creators about copyright-prevented projects. Legal analysis: compare licensing denial rates across publishers and time periods. Historical counterfactual: what adaptations and remixes would have flourished under shorter copyright terms or broader fair use? Evidence from jurisdictions with shorter terms or broader fair use provisions.',
    'If licensing suppression is high: suppression gate is confirmed (≥0.60). If suppression is low: the trapped agent''s experience is overstated, and snare classification weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_creation_suppression_magnitude, empirical, 'Magnitude of copyright-prevented derivative creation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perpetual_copyright_snare, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perpcopy_tr_t0, perpetual_copyright_snare, theater_ratio, 0, 0.35).
narrative_ontology:measurement(perpcopy_tr_t1, perpetual_copyright_snare, theater_ratio, 1, 0.42).
narrative_ontology:measurement(perpcopy_tr_t2, perpetual_copyright_snare, theater_ratio, 2, 0.48).
narrative_ontology:measurement(perpcopy_tr_t3, perpetual_copyright_snare, theater_ratio, 3, 0.55).
narrative_ontology:measurement(perpcopy_tr_t4, perpetual_copyright_snare, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(perpcopy_be_t0, perpetual_copyright_snare, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(perpcopy_be_t1, perpetual_copyright_snare, base_extractiveness, 1, 0.42).
narrative_ontology:measurement(perpcopy_be_t2, perpetual_copyright_snare, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(perpcopy_be_t3, perpetual_copyright_snare, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(perpcopy_be_t4, perpetual_copyright_snare, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perpetual_copyright_snare, resource_allocation).
narrative_ontology:affects_constraint(perpetual_copyright_snare, patent_term_extension_rent_seeking).
narrative_ontology:affects_constraint(perpetual_copyright_snare, literary_public_domain_enclosure).
narrative_ontology:affects_constraint(perpetual_copyright_snare, derivative_creator_licensing_trap).

% DUAL FORMULATION NOTE:
% Perpetual copyright is downstream of multiple structural choices: (1) the Statute of Anne's shift from monopoly to author's right (enabling perpetuity framing), (2) international harmonization via Berne Convention and TRIPS (locking in strong protection globally), (3) repeated term extensions (Sonny Bono, Copyright Term Extension Act) that push copyright toward effective perpetuity. Each is a structurally distinct constraint with its own extractiveness value. The upstream constraint (author's right conceptual shift in 1710) has lower extractiveness but generative power — it created the logical space for perpetual copyright. The downstream constraint (contemporary perpetual-duration regime) has maximum extractiveness. The constraint family decomposes along the temporal dimension: the ambiguity in Anne's statute produces the Donaldson v Beckett litigation, which produces the institutional consolidation of copyright as statute-only, which produces the TRIPS harmonization, which produces contemporary term extensions. Each story has its own extractiveness trajectory and perspectival gap. Perpetual copyright snare is the contemporary endpoint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(perpetual_copyright_snare, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
