% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: Article II Non-Appropriation — Commons Conservation Reading
 *   domain: international law / treaty interpretation / commons governance
 *
 * SUMMARY:
 *   A fixed sentence of treaty text — Article II of the 1967 Outer Space
 *   Treaty, barring 'national appropriation' by claim of sovereignty, use,
 *   occupation, or 'any other means' — anchors a three-way contest over what
 *   celestial resource recovery is. This story instantiates the
 *   commons_conservation reading: that recovery of space resources without
 *   multilateral authorization is de facto appropriation, that the bar
 *   reaches private actors through the Article VI authorization duty, and
 *   that the common estate may be allocated only by negotiation rather than
 *   by capability. The standing arrangement this reading assesses is the
 *   current de facto governance patchwork: the treaty's anti-sovereignty
 *   taboo (still universally honored) overlaid, since 2015, by national
 *   licensing statutes, a bilateral instruments network reciting treaty
 *   fidelity while drawing de facto priority zones, and a sidelined 1979
 *   benefit-sharing regime. By this reading's lights the patchwork has a
 *   genuine coordination core being converted into extraction cover, so
 *   epsilon is authored high (0.74) for the standing arrangement — never for
 *   the multilateral conservation wall this reading would institute, which is
 *   not the referent. The wall itself, its stranded-investment incidence on
 *   first movers, and the veto it would hand non-spacefaring states are
 *   carried in the axioms, the structural-delta discussion, and the omega
 *   variables; the metrics and role declarations below describe the standing
 *   arrangement, which all three sibling readings share as referent. KEY
 *   AGENTS (by structural relationship): - extraction_legislating_states:
 *   agenda-setter and principal collector (institutional/arbitrage) — writes
 *   the licensing terms, leads the bilateral network, accrues the positional
 *   value - commercial_space_resource_prospectors: secondary beneficiary
 *   (powerful/constrained) — banks priority positions under the ambiguity;
 *   capital exposed if the wall lands - non_spacefaring_states: primary
 *   bearing class (organized/trapped) — formal voice, no capability, pays via
 *   exclusion from allocation - future_generations: silent residual claimant
 *   (powerless/trapped) — inherits a narrowed commons and settled precedent -
 *   moon_agreement_parties: sidelined regime-holders
 *   (organized/identity_locked) — maintain a ratified benefit-sharing
 *   framework the capable ignore - planetary_science_research_community:
 *   incidental beneficiary (moderate/constrained) — open access serves it;
 *   industrialization threatens it - global_commons_advocacy_networks:
 *   excluded voice (moderate/constrained) — presses moratoria from outside
 *   every table - copuos_legal_subcommittee: analytical observer
 *   (institutional/analytical) — hosts the debate it cannot decide
 *
 * KEY AGENTS:
 *   - extraction_legislating_states: agenda-setter and principal collector (institutional/arbitrage) — writes the licensing terms, leads the bilateral network, accrues the positional value
 *   - commercial_space_resource_prospectors: secondary beneficiary (powerful/constrained) — banks priority positions under the ambiguity; capital exposed if the wall lands
 *   - non_spacefaring_states: primary bearing class (organized/trapped) — formal voice, no capability, pays via exclusion from allocation
 *   - future_generations: silent residual claimant (powerless/trapped) — inherits a narrowed commons and settled precedent
 *   - moon_agreement_parties: sidelined regime-holders (organized/identity_locked) — maintain a ratified benefit-sharing framework the capable ignore
 *   - planetary_science_research_community: incidental beneficiary (moderate/constrained) — open access serves it; industrialization threatens it
 *   - global_commons_advocacy_networks: excluded voice (moderate/constrained) — presses moratoria from outside every table
 *   - copuos_legal_subcommittee: analytical observer (institutional/analytical) — hosts the debate it cannot decide
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.74).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.7).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.74).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "Article II Non-Appropriation — Commons Conservation Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international law / treaty interpretation / commons governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '38ce59cb-f362-4ee3-ab5c-e33b9699b6df').
narrative_ontology:cs_kernel_codification('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', fixed_text).
narrative_ontology:cs_authority_grounding('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', lineage).
narrative_ontology:cs_interpretation_layer_present('38ce59cb-f362-4ee3-ab5c-e33b9699b6df').
narrative_ontology:cs_reading_relation('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', foundational, unauthorized_extraction_constitutes_appropriation).
narrative_ontology:cs_axiom_status(unauthorized_extraction_constitutes_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', unauthorized_extraction_constitutes_appropriation, conventional).
narrative_ontology:cs_axiom('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', foundational, celestial_domains_are_fiduciary_commons_not_capability_claims).
narrative_ontology:cs_axiom_status(celestial_domains_are_fiduciary_commons_not_capability_claims, holdable).
narrative_ontology:cs_axiom_grounding('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', celestial_domains_are_fiduciary_commons_not_capability_claims, deontological).
narrative_ontology:cs_reference_frame('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', common_heritage_fiduciary_trust).
narrative_ontology:cs_drift_state('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', post_national_licensing_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('38ce59cb-f362-4ee3-ab5c-e33b9699b6df', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, extraction_legislating_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, commercial_space_resource_prospectors).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, planetary_science_research_community).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, future_generations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, moon_agreement_parties).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, celestial_sovereignty_taboo).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, capability_based_allocation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and administer national frameworks that license their citizens to locate, recover, and sell celestial resources — the 2015 United States commercial space statute, Luxembourg's 2017 venture framework, and parallel statutes in Japan and the UAE — while formally disclaiming sovereignty and leading a bilateral instruments network that recites treaty fidelity. They set which conduct counts as lawful, condition partnership and market access on acceptance of those terms, and the positional value of early licensing accrues under their jurisdictions.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, extraction_legislating_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, extraction_legislating_states, beneficiary).

% Hold prospecting contracts, lander manifests, and lunar-surface agreements issued under national licensing regimes. Their capital is committed to hardware and mission schedules that presume the permissive interpretation continues; a hard multilateral prohibition would strand that capital, while the current ambiguity lets them bank priority positions on attractive sites.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, commercial_space_resource_prospectors, beneficiary,
    powerful, biographical, constrained, global).

% Comprise the large majority of UN COPUOS membership. They hold formal equal voice and have insisted since 1967 that celestial resources belong to a common estate requiring collective benefit-sharing, but they possess no independent launch, landing, or processing capability and no seat in the bilateral instruments where operating terms are now written. Their leverage is the consensus rule: the ability to block, delay, or attach conditions to multilateral outcomes.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, payer,
    organized, generational, trapped, global).

% Hold the residual interest in the condition of the lunar and asteroid environment and in whatever precedent governs its division. They cannot transact, object, or consent; every license granted and every priority zone drawn before they arrive narrows what they inherit and on what terms.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations, payer,
    powerless, civilizational, trapped, global).

% Ratified the 1979 agreement that would have vested celestial resources in an international regime with mandatory benefit-sharing. The major spacefaring powers never joined, leaving their instrument without the actors it was written to bind; its parties nonetheless maintain delegations, reporting cycles, and anniversary conferences for a framework that no longer reaches the conduct it governs. Withdrawal would carry reputational cost, and their diplomatic identity is invested in the regime they built.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, moon_agreement_parties, payer,
    organized, generational, identity_locked, global).

% Depends on open access to celestial surfaces and on sites remaining scientifically intact — polar volatiles, lava tubes, historically significant landing zones. Open-access norms serve them well today; surface industrialization, site claims, and priority-zone exclusions threaten both their access and the integrity of the record they study.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, planetary_science_research_community, beneficiary,
    moderate, generational, constrained, global).

% Coalitions of space-sustainability organizations, environmental lawyers, and ethicists pressing for moratoria, protected-zone designations, and benefit-sharing funds. They publish critiques of the bilateral instruments and lobby delegations, but hold no seat in the COPUOS bureau, the bilateral instrument process, or any national licensing authority.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, global_commons_advocacy_networks, excluded,
    moderate, biographical, constrained, global).

% Hosts the annual debate over resource-utilization norms, produces consensus guidelines and working papers, and records the objections of every regional group. It can recommend and legitimize but not bind; its outputs are cited by all sides while its decisions are routed around by bilateral instruments.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, copuos_legal_subcommittee, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__commons_conservation, extraction_legislating_states).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__commons_conservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the 1967 settlement's core achievement: no state claims sovereignty over celestial territory, great-power territorial conflict off Earth is prevented, and formal access remains open to all states regardless of capability.
% TRANSFER_FUNCTION: Moves allocative control over celestial resource locations and eventual recovery value from the common estate to whichever states and firms hold near-term capability — via national licensing, bilateral instrument networks, and positional precedent rather than price, negotiation, or shared administration.
% ABSENT_VOICES: Future generations hold the largest silent stake and appear at no table; the publics invoked by 'province of all mankind' rhetoric are represented only by delegations they did not choose; space-sustainability advocacy networks sit outside the COPUOS bureau, the bilateral instrument process, and every national licensing authority.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the anti-sovereignty taboo it maintains would not survive long unpolicied: licensing regimes would convert into claim registries, priority zones into de facto territories, and the major powers would either scramble or be forced to negotiate a replacement regime under crisis conditions — every dependent structure (national statutes, bilateral instruments, COPUOS agendas, prospecting contracts) rearranges around its absence.
% FOUNDING_PROBLEM: The 1967 problem: extend the terrestrial great-power standoff to space without repeating the territorial partitions that produced centuries of conflict — freeze sovereignty claims on celestial bodies during an era when almost no state could reach them, keeping access formally open.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the COPUOS deliberative record, where every regional group reaffirms the no-sovereignty settlement; the diplomatic statements of the rival coalition, which disputes this reading's answer but not the founding problem's liveness; and the neutral legal literature tracking renewed cis-lunar competition. The corroboration attests the founding problem's liveness, not this reading's solution to it.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All three tracked series run on one shared eight-point grid (1967-2026) so the engine samples every metric at every examined time point; no metric is dated by another's gaps. Base extractiveness is authored flat and low through the cooperative decades (0.10 to 0.22): the taboo held, capability was absent, and the arrangement's operation was mostly the coordination it advertised. The reading dates the break to 2015, when national commercial space law began granting citizens rights to recovered resources: 0.44, then 0.58 with the 2020 bilateral instruments and their de facto priority zones, reaching 0.74 as prospecting contracts and licensing statutes proliferate — extraction accumulating on a coordination base, the ratchet this reading exists to contest. Theater_ratio crosses 0.5 in the last third of the interval: compliance recitals, sustainability guidelines, and anniversary diplomacy increasingly perform fidelity while allocation terms are set elsewhere — Goodhart drift, proxy language replacing the function it cites. Suppression_requirement is authored because the story specifically tracks enforcement-capacity change: cold-war mutual policing of the taboo (0.55) decayed through the stalled-ratification and cooperation years (trough 0.28 in 1998), then rebuilt along bilateral lines — accession conditionality, partnership gating, forum management (0.70 by 2026). The rebuild is the arrangement's suppressive force migrating from defending a taboo to disciplining dissent about allocation. Accessibility_collapse (0.48) and resistance (0.62) are scalars: alternatives — a multilateral regime, moratoria, benefit-sharing funds — remain constructible but are progressively preempted by fait accompli investment, and resistance is real, organized, and continuous, which is why the arrangement still requires active enforcement rather than passive acquiescence.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the divergence is the finding. From the extraction_legislating_states seat the arrangement is infrastructure they built and profit from: a stable no-sovereignty order plus licensing prerogatives — coordination with a favorable fee schedule. From the non_spacefaring_states seat the same structure operates as formal equality wrapping substantive exclusion: voice without capability, consensus leverage without reach. Prospector firms experience optionality with stranding tail-risk. Moon Agreement parties experience a piton-shaped grievance — their ratified instrument is maintained ceremonially, with reporting cycles and anniversary conferences, while functioning nowhere it was aimed; it is the clearest nested mandatrophy inside the arrangement. The engine computes these per-seat classifications from the structural data; this story's claimed type is the reading's diagnosis of the whole, not any single seat's experience of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations describe the standing arrangement — the same referent epsilon is authored over — so the derivation chain needs no overrides: extraction_legislating_states (declared beneficiary, agenda-setter, arbitrage exit) derive near the beneficiary pole; prospectors and the science community (beneficiaries with constrained exit) sit nearby; non_spacefaring_states, future_generations, and moon_agreement_parties (declared victims, trapped or identity-locked) derive toward the target pole, with future_generations furthest out — powerless, trapped, universal-stake. Suppression is authored as a raw structural property and is deliberately unscaled; scope amplification of extractiveness is the engine's arithmetic, not authored here. The reading's prescribed wall would invert several of these relationships — prospectors would become the wall's payers, non-spacefaring states its protected class — and that inversion is carried in the omegas and axioms rather than falsifying the declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing terrestrial rivalry from partitioning the celestial estate — is corroborated live by every regional group's continuing insistence on the taboo, so the arrangement's core mandate has not atrophied and no story-level mandatrophy resolution is declared. What has atrophied is a component: the 1979 benefit-sharing regime, whose mandate of collective allocation was never activated among the states that matter and whose maintenance is now theatrical — the nested piton noted above. The tangled_rope diagnosis prevents two mislabels: reading the whole arrangement as pure extraction would erase the live, corroborated coordination achievement (five decades without a sovereignty claim); reading it as pure coordination would erase the measured extraction ratchet and the theater ratio crossing 0.5. The classification keeps the coordination credit and the extraction debit on the same ledger — which is exactly the structure the conservation reading protests and the extraction_permissive sibling denies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the commons_conservation reading of the ost_article_ii_non_appropriation kernel; how would the classification change under the extraction_permissive or international_regime siblings?',
    'Cross-reading comparison within the constraint family: all three stories share one referent (the standing de facto allocation arrangement) and differ only in reading-indexed epsilon, victim sets, and diagnosis; the divergence locates the disagreement structurally.',
    'Under extraction_permissive, prospectors leave the victim set and epsilon falls toward the coordination floor; under international_regime, epsilon indexes indeterminacy cost and the wall dissolves into agenda. Classification is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: this file is one reading of a contested fixed-text treaty kernel, not the topic whole.').

omega_variable(
    article_vi_private_actor_chain,
    'Does the Article VI state-responsibility chain actually extend the non-appropriation bar to private actors, or does national licensing create a lawful loophole class the reading''s coverage claim misses?',
    'Crystallizing state practice or authoritative adjudication on whether domestic extraction licenses satisfy or breach the Article VI duty of authorization and supervision.',
    'If the chain holds, the wall covers private actors as the reading claims; if not, the victim set is incomplete and the standing arrangement''s epsilon is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_private_actor_chain, empirical, 'Whether the private-actor half of the reading''s coverage claim is structurally sound.').

omega_variable(
    veto_power_realism,
    'Is the non-spacefaring states'' consensus veto a real brake on enclosure, or a procedural illusion once bilateral instruments and licensing practice reach critical mass?',
    'Track whether any COPUOS consensus block has measurably altered a bilateral instrument trajectory, a national licensing statute, or a prospecting contract pipeline.',
    'If illusory, the arrangement drifts toward pure extraction with leverage-less victims; if real, the coordination core is genuine and the tangled diagnosis holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_power_realism, empirical, 'Whether the protected class''s procedural leverage constrains the capable coalition.').

omega_variable(
    stranded_investment_magnitude,
    'If the conservation wall were enforced tomorrow, would first-mover extraction investments be proportionally redirected or catastrophically stranded?',
    'Actuarial audit of committed capital in lunar prospecting contracts, lander programs, and site agreements against moratorium scenarios.',
    'Determines whether the wall''s incidence on prospectors is a manageable coordination cost or an expropriation-scale transfer — the difference between a firm wall and a deepening tangle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_investment_magnitude, empirical, 'Magnitude of the stranding cost the reading''s prescribed wall would impose on first movers.').

omega_variable(
    benefit_sharing_baseline_contest,
    'Against what baseline is the arrangement''s extraction measured — the conservation reading assumes a negotiable equitable-sharing benchmark that its rivals reject outright?',
    'Preference elicitation across the three readings'' sponsor coalitions; no empirical test settles a baseline choice, only which coalition holds sway.',
    'Epsilon magnitude is baseline-sensitive: a capability-desert baseline drives it toward zero (the extraction_permissive result over the same referent); a strict common-estate baseline drives it higher. Readings share the referent and diverge here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_sharing_baseline_contest, preference, 'The evaluative baseline over which reading-indexed epsilon is computed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_cc_tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.08).
narrative_ontology:measurement_basis(ost_cc_tr_t1967, observed).
narrative_ontology:measurement(ost_cc_tr_t1979, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1979, 0.22).
narrative_ontology:measurement_basis(ost_cc_tr_t1979, observed).
narrative_ontology:measurement(ost_cc_tr_t1984, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1984, 0.31).
narrative_ontology:measurement_basis(ost_cc_tr_t1984, observed).
narrative_ontology:measurement(ost_cc_tr_t1998, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1998, 0.24).
narrative_ontology:measurement_basis(ost_cc_tr_t1998, observed).
narrative_ontology:measurement(ost_cc_tr_t2015, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2015, 0.36).
narrative_ontology:measurement_basis(ost_cc_tr_t2015, observed).
narrative_ontology:measurement(ost_cc_tr_t2020, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2020, 0.46).
narrative_ontology:measurement_basis(ost_cc_tr_t2020, observed).
narrative_ontology:measurement(ost_cc_tr_t2023, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2023, 0.51).
narrative_ontology:measurement_basis(ost_cc_tr_t2023, observed).
narrative_ontology:measurement(ost_cc_tr_t2026, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2026, 0.52).
narrative_ontology:measurement_basis(ost_cc_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ost_cc_be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.1).
narrative_ontology:measurement_basis(ost_cc_be_t1967, observed).
narrative_ontology:measurement(ost_cc_be_t1979, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1979, 0.14).
narrative_ontology:measurement_basis(ost_cc_be_t1979, observed).
narrative_ontology:measurement(ost_cc_be_t1984, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1984, 0.18).
narrative_ontology:measurement_basis(ost_cc_be_t1984, observed).
narrative_ontology:measurement(ost_cc_be_t1998, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement_basis(ost_cc_be_t1998, observed).
narrative_ontology:measurement(ost_cc_be_t2015, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement_basis(ost_cc_be_t2015, observed).
narrative_ontology:measurement(ost_cc_be_t2020, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(ost_cc_be_t2020, observed).
narrative_ontology:measurement(ost_cc_be_t2023, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2023, 0.67).
narrative_ontology:measurement_basis(ost_cc_be_t2023, observed).
narrative_ontology:measurement(ost_cc_be_t2026, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2026, 0.74).
narrative_ontology:measurement_basis(ost_cc_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ost_cc_su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement_basis(ost_cc_su_t1967, observed).
narrative_ontology:measurement(ost_cc_su_t1979, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1979, 0.48).
narrative_ontology:measurement_basis(ost_cc_su_t1979, observed).
narrative_ontology:measurement(ost_cc_su_t1984, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1984, 0.38).
narrative_ontology:measurement_basis(ost_cc_su_t1984, observed).
narrative_ontology:measurement(ost_cc_su_t1998, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1998, 0.28).
narrative_ontology:measurement_basis(ost_cc_su_t1998, observed).
narrative_ontology:measurement(ost_cc_su_t2015, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2015, 0.46).
narrative_ontology:measurement_basis(ost_cc_su_t2015, observed).
narrative_ontology:measurement(ost_cc_su_t2020, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2020, 0.61).
narrative_ontology:measurement_basis(ost_cc_su_t2020, observed).
narrative_ontology:measurement(ost_cc_su_t2023, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2023, 0.66).
narrative_ontology:measurement_basis(ost_cc_su_t2023, observed).
narrative_ontology:measurement(ost_cc_su_t2026, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2026, 0.7).
narrative_ontology:measurement_basis(ost_cc_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, enforcement_mechanism).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% Constraint family: one colloquial label ('Article II non-appropriation') decomposes into three structurally distinct claims per the epsilon-invariance principle — this conservation reading (epsilon 0.74; victims include non-spacefaring states and future generations), the extraction_permissive sibling (epsilon near the coordination floor; prospectors exit the victim set), and the international_regime sibling (epsilon indexes indeterminacy cost; classification deferred). All three share one referent — the standing de facto allocation arrangement — and differ only in reading-indexed assessment. The fixed treaty text is upstream of all three; conservation and permissive form a forecloses pair; the regime reading mediates and is pressured by this reading's negotiating red lines.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
