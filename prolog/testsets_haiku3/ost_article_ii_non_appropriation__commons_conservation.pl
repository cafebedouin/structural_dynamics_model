% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: OST Article II Non-Appropriation Principle (Commons Conservation Reading)
 *   domain: international/legal/commons_governance
 *
 * SUMMARY:
 *   The Outer Space Treaty (1967) Article II states that 'outer space,
 *   including the moon and other celestial bodies, is not subject to national
 *   appropriation by claim of sovereignty, by means of use or occupation, or
 *   by any other means.' This constraint instantiates the
 *   commons-conservation reading: the clause 'use or occupation' is read to
 *   prohibit de facto appropriation via resource extraction without prior
 *   multilateral authorization. Under this reading, a mining company's
 *   unilateral resource extraction — even without a sovereignty claim —
 *   constitutes prohibited appropriation because extraction creates effective
 *   control and economic benefit capture. This reading preserves a veto for
 *   non-spacefaring states over enclosure and mandates that appropriation
 *   questions be resolved through negotiation rather than unilateral
 *   capability. The sibling extraction_permissive reading interprets the same
 *   Article II language to permit unilateral resource extraction by private
 *   actors (states do not appropriate, individuals and corporations do, and
 *   the treaty does not govern private property). The international_regime
 *   reading treats Article II as deferring the appropriation question to a
 *   future institutional regime. These three readings all cite the same
 *   treaty text but deploy different framings to support structurally
 *   incompatible legal consequences.
 *
 * KEY AGENTS:
 *   - non_spacefaring_states: majority of Earth's states; no current access to space mining; benefit from veto over enclosure; political power moderate, technological power minimal, exit options trapped (cannot unilaterally pursue space extraction; depend on regime negotiation)
 *   - first_mover_mining_investors: advanced-nation corporations and space agencies with early-stage mining technology; invest under extraction_permissive framing; bear stranded costs under commons_conservation reading; powerful institutionally, high technological exit optionality via regime negotiation or investment diversification
 *   - spacefaring_commercial_operators: private space companies from technologically advanced nations; economically benefit under extraction_permissive reading; constrained under commons_conservation; institutional power high, organized, geographic scope global
 *   - international_community: aggregated states as a governance body; benefits under commons_conservation reading from veto power and negotiated benefit-sharing; bears coordination costs of regime negotiation
 *   - future_generations: not present at negotiation; benefit from commons preservation; voice absent from current treaty enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.68).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.72).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.68).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "OST Article II Non-Appropriation Principle (Commons Conservation Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international/legal/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '2811e056-9d93-4b5b-b967-41a35d9c3232').
narrative_ontology:cs_kernel_codification('2811e056-9d93-4b5b-b967-41a35d9c3232', fixed_text).
narrative_ontology:cs_authority_grounding('2811e056-9d93-4b5b-b967-41a35d9c3232', extraction).
narrative_ontology:cs_reading_relation('2811e056-9d93-4b5b-b967-41a35d9c3232', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('2811e056-9d93-4b5b-b967-41a35d9c3232', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('2811e056-9d93-4b5b-b967-41a35d9c3232', foundational, extraction_constitutes_appropriation).
narrative_ontology:cs_axiom_status(extraction_constitutes_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('2811e056-9d93-4b5b-b967-41a35d9c3232', extraction_constitutes_appropriation, deontological).
narrative_ontology:cs_axiom('2811e056-9d93-4b5b-b967-41a35d9c3232', foundational, multilateral_authorization_requirement).
narrative_ontology:cs_axiom_status(multilateral_authorization_requirement, holdable).
narrative_ontology:cs_axiom_grounding('2811e056-9d93-4b5b-b967-41a35d9c3232', multilateral_authorization_requirement, conventional).
narrative_ontology:cs_reference_frame('2811e056-9d93-4b5b-b967-41a35d9c3232', commons_preservation_framework).
narrative_ontology:cs_drift_state('2811e056-9d93-4b5b-b967-41a35d9c3232', contemporary_post_iridium, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2811e056-9d93-4b5b-b967-41a35d9c3232', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, international_community).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_investors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_commercial_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, spacefaring_commercial_operators).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, international_community).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, commons_preservation_doctrine).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, multilateral_authorization_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Majority of Earth's states with no current space extraction capability. Under the commons-conservation reading, they gain veto power over enclosure and the right to negotiate benefit-sharing from any extraction regime. They hold voting power in the UN and can collectively threaten non-recognition, sanctions, or legal challenges to enforce the non-appropriation principle. They depend entirely on multilateral negotiation for their seat at the extraction-authorization table and cannot exit the constraint (they cannot pursue unilateral extraction). The constraint preserves their option value indefinitely.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, analytical, global).

% Private corporations and advanced-nation space agencies with early-stage mining technology and invested capital in space exploration. Under the commons-conservation reading, their extraction plans face legal prohibition absent multilateral authorization they do not control. They must either seek authorization through regime negotiation (delaying return on investment), abandon the investment (sunk-cost loss), or challenge the reading's validity (high diplomatic/legal cost). They possess exit options: capital can shift to Earth-based mining, technology development, or other jurisdictions. However, space infrastructure represents significant path-dependent investment, so exit is costly but available.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_investors, payer,
    powerful, biographical, arbitrage, global).

% Private space companies and technology providers from technologically advanced nations. They benefit if the extraction-permissive reading prevails, permitting private resource ownership. Under commons-conservation, they face authorization requirements but also potential licensing opportunities and technology-service contracts if a regime is built. They are constrained by dependence on spacefaring-nation regulatory frameworks and international legal legitimacy for their operations. Their power is significant (technology and capital) but tied to their home nation's diplomatic position.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_commercial_operators, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, spacefaring_commercial_operators, payer).

% Aggregated states and international governance bodies (UN, COPUOS, etc.) that bear the responsibility for negotiating a multilateral extraction regime if commons-conservation reading prevails. They benefit from the constraint's veto structure (preserves collective decision-making) but pay the cost of intensive, long-term negotiation. They are excluded from the direct extraction benefit but carry the coordination burden. Exit options are analytical: they cannot opt out of the OST without collectively renegotiating the treaty.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_community, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, international_community, payer).

% Not represented at the OST negotiation or contemporary regime discussions. Benefit from commons preservation (if achieved), but have no voice in current authorization processes and no exit options. Entirely dependent on current states' choice to adopt the commons-conservation reading. They bear the risk that negotiation fails and extraction proceeds without preservation constraints.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% USA, Russia, China, ESA members: states with space launch, orbital infrastructure, and technological capacity for extraction. They set the political/diplomatic agenda for how Article II is interpreted and whether regime negotiation proceeds. Under commons-conservation, they bear pressure from both non-spacefaring states (demanding veto rights) and their own commercial operators (demanding extraction access). They can choose to accelerate regime negotiation, block it, or adopt extraction-permissive framing domestically. Their exit options are mobile (they can reinterpret Article II, withdraw from the OST via Article XVI, or pursue extraction de facto while claiming private-actor status).
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, advanced_spacefaring_nations, agenda_setter,
    institutional, generational, mobile, global).

% Emerging spacefaring nations and space agencies from non-advanced countries (India, Japan, private operators from developing economies). Excluded from early extraction planning due to technological and capital constraints, despite the commons-conservation reading's promise of veto rights in multilateral authorization. They would benefit if regime negotiation includes capacity-building provisions but are trapped in dependence on advanced nations' technological choices and investment decisions. Their absence from first-mover mining investment means their veto is theoretical unless regime negotiation actively incorporates them.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, developing_spacefaring_actors, excluded,
    moderate, biographical, trapped, global).

% NGOs, scientific institutions, and conservation advocates monitoring space resource governance. They observe the constraint's enforcement but lack direct legal standing in OST dispute mechanisms. They advocate for commons-conservation framing and provide expert testimony and legal briefs in multilateral forums. Their power is indirect (agenda-setting through advocacy and knowledge production) but their exit options are mobile (they can shift focus to other governance domains).
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, environmental_and_scientific_advocates, observer,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__commons_conservation, diffuse).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__commons_conservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates an outer-space resource governance system that prevents unilateral appropriation and preserves multilateral consent-based access to extraction benefits. Solves the collective-action problem: without coordination, states fear first-mover unilateral enclosure will lock in unequal access; with coordination (multilateral authorization requirement), all states retain voice over appropriation and can negotiate benefit-sharing.
% TRANSFER_FUNCTION: Transfers veto power from spacefaring operators (who would possess technological capacity to extract unilaterally) to the international community (non-spacefaring states plus spacefaring states acting collectively). Also transfers authorization authority from unilateral state appropriation claims to multilateral regime decisions. Benefits (veto power, negotiation seats, future benefit-sharing) accrue to non-spacefaring and developing states. Costs (investment delay, authorization uncertainty, stranded capital) accrue to first-mover mining investors and spacefaring operators.
% ABSENT_VOICES: Future generations (not represented in OST negotiation or contemporary regime discussions); developing-economy spacefaring actors (excluded by technological barriers from early extraction planning); lunar settlements and long-term space-resource users (not yet in existence, no voice in appropriation framework design). These absent voices would argue for commons preservation with equitable access pathways and long-term sustainability constraints, but are kept out by present-actor focus and short-term extraction economics.
% DISAPPEARANCE_RATIONALE: If the commons-conservation constraint disappeared and Article II were reinterpreted as extraction-permissive, spacefaring operators and first-mover mining companies would immediately pursue unilateral extraction without authorization delays, stranded investments would be recovered, and benefit-sharing would flow to operators rather than being negotiated multilaterally. Non-spacefaring states would lose veto power and would face de facto enclosure. The international resource-governance landscape would reorganize around technological capability rather than multilateral consent. Conversely, if the constraint held and a multilateral regime materialized, extraction would proceed under authorization with benefit-sharing mechanisms, and the world would stabilize into a negotiated governance structure. The disappearance of the constraint as the decision-determinant is what makes the world rearrange.
% FOUNDING_PROBLEM: Uncontrolled space resource extraction creates unilateral appropriation risk: spacefaring nations and operators can claim and extract resources unilaterally, locking in unequal benefit-distribution and foreclosing non-spacefaring states from access. This violates the principle of 'province of all mankind' and destabilizes international cooperation. The OST Article II was written to prevent de facto appropriation through 'use or occupation' — to preserve the commons by prohibiting unilateral enclosure.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live: first-mover mining projects are being proposed and capital is accumulating; technological capacity for unilateral extraction is rising; and no multilateral regime has materialized to govern appropriation (Article XI remains unimplemented 57 years after OST entry into force). Non-spacefaring states' diplomatic statements and UN COPUOS positions consistently invoke the non-appropriation principle as the governing rule, indicating they perceive the founding problem as unresolved. Advanced spacefaring nations acknowledge the problem rhetorically (they cite Article II) but resist regime negotiation, suggesting political unwillingness to operationalize the solution. Independent legal scholars and space governance experts (outside the benefiting parties) attest that the appropriation question remains the central unresolved issue in space law, and that first-mover extraction without multilateral authorization would represent appropriation contrary to Article II under the commons-conservation reading.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 endpoint) because the commons-conservation reading strands first-mover investments by redefining extraction as appropriation; the reading assigns the stranded costs to investors while distributing benefits (veto power, negotiation access) to non-spacefaring states. Suppression is elevated (0.72) because the reading's enforcement depends on active international consensus against unilateral extraction — without multilateral institutional machinery in place, suppression relies on state-to-state pressure, sanctions threats, and moral/legal claims rather than structural barriers. Theater is moderate (0.42) and rising: the formal prohibition on 'use or occupation' is stated but lacks operational enforcement machinery; compliance relies on states choosing to respect each other's veto rather than on institutional mechanisms that would automatically block extraction. The measurement series tracks rising extractiveness (as first-mover investors accumulate capital and pressure for regime loosening) and rising theater (as enforcement becomes increasingly performative — states issue statements and threaten dispute settlement, but extraction capacity outpaces institutional capacity to block it). The shared time grid runs all three metrics at the same six points (t = 0, 4, 8, 12, 16, 20) so temporal divergence and synchronization are measurable.
 *
 * PERSPECTIVAL GAP:
 *   From the non-spacefaring-state seat, this reading is a coordination mechanism that preserves their voice and prevents unilateral enclosure; extractiveness is moderate-to-low from their seat (they benefit from the veto). From the first-mover-investor seat, the reading is extractive (their investments are stranded absent negotiated authorization); extractiveness is high (they bear the cost of delay and the requirement to seek multilateral permission). Spacefaring operators sit between: they benefit if extraction-permissive reading prevails, but must absorb uncertainty and negotiation costs under commons-conservation. The engine computes per-seat directionality from beneficiary/victim status: non-spacefaring states derive d near 0.0 (beneficiaries), investors derive d near 1.0 (victims), international-community aggregate derives d near 0.5 (both benefits and costs). The reading's type classification diverges sharply across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states are structural beneficiaries (role: beneficiary): they gain veto power, negotiation access, and option value on future benefit-sharing without current extraction capability. Their exit options are analytical (they cannot unilaterally opt out of the treaty or extract independently); their power is organized/diplomatic rather than technological, so they benefit from a rule that ties appropriation to multilateral agreement. Directionality for this seat: d ≈ 0.05 (strong beneficiary). First-mover mining investors are structural victims (role: payer): their investments are stranded unless they negotiate multilateral authorization; they bear the cost of delay and the risk that multilateral negotiation may impose sharing or prohibition. Their exit options are arbitrage (they can shift capital to other technologies or jurisdictions, including Earth-based mining and manufacturing) and powerful (they command significant institutional and technical resources). Directionality for this seat: d ≈ 0.85 (strong target). Spacefaring commercial operators are ambiguously positioned (role: beneficiary with secondary payer risk): they benefit from the framing that private extraction is permitted, but face uncertain authorization pathways and potential retroactive claims. Exit options for spacefaring operators are constrained (path dependency on space infrastructure, institutional relationships); power is institutional. Directionality: d ≈ 0.55 (near-symmetric, with upside if regime negotiation succeeds). International community/future generations: benefit from commons preservation but bear coordination costs and the risk that negotiation fails to produce a regime; d ≈ 0.35 (moderate beneficiary, with diffuse costs).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is extraction governance: without coordination, states fear unilateral appropriation will lock in unequal access and enclosure. The commons-conservation reading asserts that Article II solves this by prohibiting appropriation absent multilateral authorization. The mandatrophy question: does the founding problem remain live, or has it been displaced by technological/economic facts on the ground? Evidence for mandatrophy: (a) No international regime for space mining authorization has materialized in 60 years despite Article XI's apparent invitation; (b) Extraction technology has advanced while multilateral negotiation remains stalled; (c) The constraint's enforcement mechanism (state-to-state pressure, threat of sanctions) has not visibly blocked any major extraction project — theater has risen as enforcement costs accumulate without observable prevention. Evidence against mandatrophy: (a) The constraint DOES preserve the legal opening for multilateral negotiation; (b) No unilateral extraction on a scale that would trigger state-to-state enforcement has yet occurred (first-mover projects are still in proposal/negotiation phases, suggesting the constraint's legal force matters); (c) The rising theater ratio reflects increasing enforcement pressure, not absence of enforcement — suppression is rising in response to accumulating first-mover capital. Assessment: the founding problem remains live in legal form (unauthorized extraction is prohibited) but degraded in institutional form (no regime exists to operationalize multilateral authorization). Classification stands as tangled_rope: the constraint coordinates a benefit (veto power for non-spacefaring states) and extracts from a cost-bearer (first-mover investors stranded pending authorization), but the active enforcement mechanism is increasingly theatrical — it relies on threat and diplomatic pressure rather than structural barriers. If technological advance outpaces regime negotiation further, the reading should be reassessed as piton (preserved theater of a dead coordination problem).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    use_vs_appropriation_boundary,
    'Does the reading''s distinction between ''use or occupation'' (permitted) and ''appropriation'' (prohibited) hold as a workable legal boundary, or does de facto resource extraction collapse the distinction by creating effective control indistinguishable from ownership?',
    'Case law from OST-dispute settlement or ICJ advisory opinions; state practice on unauthorized extraction claims; whether extraction without title-claim remains distinguishable from territorial appropriation or generates equivalent economic sovereignty.',
    'If extraction collapses into appropriation, the reading''s enforcement mechanism holds. If extraction can remain extraction without crossing into appropriation, the boundary erodes and enforcement becomes theater — first-mover advantage prevails regardless of the formal rule. Classification diverges: snare (reading fails) vs. tangled_rope (reading holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_vs_appropriation_boundary, conceptual, 'Whether use/occupation and appropriation are structurally distinguishable or operationally equivalent.').

omega_variable(
    multilateral_authorization_feasibility,
    'Can the non-appropriation principle be operationalized without a multilateral authorization regime that does not yet exist, or does the constraint''s enforceability depend on the future creation of institutional machinery the OST Article II does not provide?',
    'Implementation of an actual international regime (per Article XI or negotiated successor); empirical track record of whether states successfully coordinate collective enforcement absent formal institutional structure; state practice on challenge and resolution.',
    'If operationalization is feasible via customary practice and state consensus, the constraint is enforceable as written. If it requires formal regime creation, the constraint becomes aspirational — effective extraction moves faster than multilateral negotiation, and the reading becomes piton (theater of enforcement without functional block). This omega splits tangled_rope from piton depending on what ''active enforcement'' resolves to.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilateral_authorization_feasibility, empirical, 'Whether the non-appropriation principle can be enforced absent a formal international regime.').

omega_variable(
    first_mover_stranding_vs_adaptation,
    'Do first-mover mining investments become genuinely stranded under this reading, or do investors adapt by seeking multilateral authorization, altering extraction economics rather than eliminating extraction?',
    'Empirical tracking of actual first-mover investments post-OST signature; whether negotiations for multilateral authorization regime succeed in converting stranded investments into licensed extraction or fail, leaving investments abandoned.',
    'True stranding makes the reading maximally extractive from the investor seat (d → 1.0, high χ) and proves the constraint blocks first-mover advantage. Adaptation and negotiated licensing would reduce extractiveness from the investor seat and blur the distinction between the conservation and extraction_permissive readings — both would then permit extraction via authorization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(first_mover_stranding_vs_adaptation, empirical, 'Whether the constraint eliminates first-mover extraction advantage or merely delays it pending negotiation.').

omega_variable(
    kernel_reading_decomposition_signal,
    'Is the three-way contest between commons_conservation, extraction_permissive, and international_regime readings a matter of textual ambiguity in Article II itself, or do the readings differ primarily in which downstream institutional choice they endorse?',
    'Linguistic analysis of Article II''s actual text; travaux préparatoires from the OST negotiation; whether the treaty language favors one reading''s semantic interpretation or whether all three readings find plausible textual support and diverge instead in their institutional commitments (regime building vs. laissez-faire).',
    'If textually ambiguous with readings as legitimate framings, this is a genuine kernel reading and sibling readings coexist. If one reading wins the semantic contest, the others are merely different policy preferences attached to a settled constraint — reclassification from reading-family to single constraint with disputed application. The omega documents whether this is a kernel-reading situation or a preference dispute over an established constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition_signal, conceptual, 'Whether the three readings instantiate a contested kernel or represent policy disputes over a settled constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(ost__tr_t0, observed).
narrative_ontology:measurement(ost__tr_t4, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 4, 0.24).
narrative_ontology:measurement_basis(ost__tr_t4, observed).
narrative_ontology:measurement(ost__tr_t8, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(ost__tr_t8, observed).
narrative_ontology:measurement(ost__tr_t12, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(ost__tr_t12, observed).
narrative_ontology:measurement(ost__tr_t16, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 16, 0.4).
narrative_ontology:measurement_basis(ost__tr_t16, observed).
narrative_ontology:measurement(ost__tr_t20, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(ost__tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(ost__be_t0, observed).
narrative_ontology:measurement(ost__be_t4, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 4, 0.48).
narrative_ontology:measurement_basis(ost__be_t4, observed).
narrative_ontology:measurement(ost__be_t8, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(ost__be_t8, observed).
narrative_ontology:measurement(ost__be_t12, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 12, 0.61).
narrative_ontology:measurement_basis(ost__be_t12, observed).
narrative_ontology:measurement(ost__be_t16, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 16, 0.65).
narrative_ontology:measurement_basis(ost__be_t16, observed).
narrative_ontology:measurement(ost__be_t20, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(ost__be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t0, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0, 0.51).
narrative_ontology:measurement_basis(ost__su_t0, observed).
narrative_ontology:measurement(ost__su_t4, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 4, 0.58).
narrative_ontology:measurement_basis(ost__su_t4, observed).
narrative_ontology:measurement(ost__su_t8, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 8, 0.64).
narrative_ontology:measurement_basis(ost__su_t8, observed).
narrative_ontology:measurement(ost__su_t12, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(ost__su_t12, observed).
narrative_ontology:measurement(ost__su_t16, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(ost__su_t16, observed).
narrative_ontology:measurement(ost__su_t20, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(ost__su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, global_infrastructure).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__commons_conservation, 0.18).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, moon_agreement_benefit_sharing).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, iridium_spectrum_allocation_precedent).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-member kernel family decomposed from the OST Article II appropriation question. Each reading (commons_conservation, extraction_permissive, international_regime) is a separate constraint story with its own epsilon value, beneficiary/victim structure, and type classification. They share the same foundational kernel (the Article II text) but diverge in legal interpretation and operational consequence. All three members are linked via network.affects_constraints to enable contamination analysis: if one reading's legitimacy erodes, the others face structural pressure. The upstream story is iridium_spectrum_allocation_precedent, which established that unilateral enclosure of global commons without multilateral authorization violates customary international law — this precedent influences the commons_conservation reading's plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
