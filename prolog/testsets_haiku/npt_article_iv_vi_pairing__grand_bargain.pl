% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Article IV–VI Grand Bargain: Reciprocal Obligation Pairing (Weapon State Disarmament Conditionality)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   The NPT establishes a legal bargain between weapon states (P5) and
 *   non-weapon states (NNWS): in exchange for non-proliferation commitments
 *   (Article III) and non-acquisition obligations (Article IV conditionality
 *   on peaceful use), non-weapon states receive access to civilian nuclear
 *   technology and the promise of weapon-state disarmament progress (Article
 *   VI). The 'grand-bargain' reading interprets Article IV and Article VI as
 *   mutually conditioning: NNWS restraint is contingent on weapon-state
 *   disarmament progress, and weapon-state breach of Article VI undermines
 *   the legitimacy of Article IV restrictions. This reading stands against
 *   two sibling interpretations: the nonproliferation_primary reading treats
 *   Article VI as aspirational and Article IV as unconditional on disarmament
 *   progress (weapon states set the terms unilaterally); the abolitionist
 *   reading treats Article IV as illegitimate (perpetuating dual-use
 *   proliferation risk) and Article VI as mandating complete disarmament (no
 *   exception for deterrence). The grand-bargain reading is the one advanced
 *   by non-aligned states, disarmament advocates, and legal scholars who see
 *   the treaty's founding premise as reciprocal obligation. It is
 *   structurally distinct from the other two in its enforcement mechanism
 *   (makes disarmament justiciable), its victim class (NNWS shift from purely
 *   payers to conditionally-obligated beneficiaries), and its breach
 *   condition (weapon-state non-compliance becomes actionable, not merely
 *   rhetorical).
 *
 * KEY AGENTS:
 *   - weapon_states: institutional power, agenda-setter role; set and enforce NPT terms; commit nominally to Article VI while resisting binding verification; extract asymmetric benefit (nuclear deterrent without restriction, ability to condition NNWS technology access)
 *   - non_weapon_states: organized power, beneficiary and payer; receive technology but restrain proliferation; under grand-bargain reading, their restraint is conditional on weapon-state disarmament progress (rather than unconditional)
 *   - iaea_and_verification_apparatus: institutional power, payer role; implement asymmetric verification on NNWS; under grand-bargain reading, would expand to symmetric verification of weapon-state disarmament (orders of magnitude more complex)
 *   - non_aligned_movement_and_disarmament_advocates: moderate power, excluded role (not treaty-setting parties but influential in legitimacy discourse); champion the grand-bargain reading; would object to suppression of Article VI enforceability
 *   - potential_proliferators: powerful but excluded; behavior responds to NPT legitimacy; grand-bargain reading operationalization would reinforce NNWS compliance; reading suppression weakens restraint incentives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.62).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.71).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV–VI Grand Bargain: Reciprocal Obligation Pairing (Weapon State Disarmament Conditionality)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, '0c7b0c0c-596a-447c-8f57-bd0b1968ec8b').
narrative_ontology:cs_kernel_codification('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', fixed_text).
narrative_ontology:cs_authority_grounding('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', extraction).
narrative_ontology:cs_interpretation_layer_present('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b').
narrative_ontology:cs_reading_relation('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', foundational, article_vi_enforceable_obligation).
narrative_ontology:cs_axiom_status(article_vi_enforceable_obligation, holdable).
narrative_ontology:cs_axiom_grounding('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', article_vi_enforceable_obligation, deontological).
narrative_ontology:cs_axiom('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', foundational, reciprocal_conditionality_structural).
narrative_ontology:cs_axiom_status(reciprocal_conditionality_structural, holdable).
narrative_ontology:cs_axiom_grounding('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', reciprocal_conditionality_structural, conventional).
narrative_ontology:cs_reference_frame('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', reciprocal_bargain_1968_founding).
narrative_ontology:cs_drift_state('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0c7b0c0c-596a-447c-8f57-bd0b1968ec8b', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, nnws_civil_nuclear_sector).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, iaea_and_verification_apparatus).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, treaty_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, pacta_sunt_servanda_conditional_performance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commit to non-proliferation (Article III verification, no military nuclear programs) in exchange for access to civilian nuclear technology and the security guarantee of weapon states' disarmament progress (Article VI). They benefit from the technology transfer and the legal framework binding weapon states. They also bear the cost of the non-proliferation restraint and face the extraction of bearing asymmetric verification burdens; their exit (withdrawal) is theoretically available but politically costly (alienation, security isolation) and is further constrained by dependency on nuclear technology suppliers. The grand-bargain reading positions their restraint as conditional on measurable disarmament progress.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states, payer).

% Set and enforce the terms of the NPT: commit nominally to disarmament (Article VI) while maintaining nuclear arsenals and resisting binding verification. They enforce Article IV technology-transfer conditionality and Article III verification requirements on non-weapon states. Under the grand-bargain reading, their disarmament obligation becomes enforceable and their breach of Article VI undercuts their authority to enforce Article IV. They extract legitimacy and strategic advantage from the asymmetry: non-weapon states cannot proliferate, but weapon states are not bound by the same enforcement machinery. Their exit from the treaty is de facto available (they can withdraw) but carries geopolitical cost.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Receives technology, fuel, and expertise under Article IV. Their benefit depends on continuous access to the non-proliferation regime; exit (for their host state) would block supplier relationships and trigger supplier sanctions. They benefit materially from the bargain but do not directly bear the disarmament conditionality—that is borne by their host states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nnws_civil_nuclear_sector, beneficiary,
    organized, biographical, constrained, global).

% Implements inspection and verification for non-weapon states under Article III, bearing the operational cost and technical complexity. Under the grand-bargain reading, they would also be tasked with verifying weapon-state disarmament, which is orders of magnitude more complex and resource-intensive. Their current role is asymmetric: verifying restraint in non-weapon states, not in weapon states. The grand-bargain reading would demand they expand to symmetric verification, which they lack the mandate and capacity to perform.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea_and_verification_apparatus, payer,
    institutional, generational, constrained, global).

% Advocate for interpreting Article VI as enforceable and Article IV as conditional on disarmament progress. They would object to the suppression of this reading in favor of the nonproliferation_primary reading, where Article VI is treated as aspirational. They are excluded from the treaty-setting process and from formal verification, though they conduct independent monitoring and documentation. The grand-bargain reading is the interpretive frame many of them champion but lack institutional standing to enforce.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, ngos_and_disarmament_advocates, excluded,
    moderate, biographical, constrained, global).

% States (or non-state actors with state-development aspirations) who might seek nuclear weapons if the non-proliferation regime's legitimacy erodes. Under the grand-bargain reading, if weapon states breach Article VI without consequence, the constraint on NNWS commitment weakens, and potential proliferators gain argument and motivation. They are structurally excluded from the treaty's beneficiary class (not parties) but their behavior shifts with the treaty's legitimacy.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, potential_proliferators, excluded,
    powerful, biographical, identity_locked, national).

% Document and analyze the reading contest: whether Article IV and VI are reciprocal or independent, whether breach of VI undermines IV's legitimacy, whether disarmament is enforceable or hortatory. They produce the legal scholarship and testimony that informs state positions and NGO advocacy. They take no direct rents or costs but have reputational investment in their reading.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, analytical_observer_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__grand_bargain, weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__grand_bargain, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal framework for technology sharing and peaceful nuclear energy development (Article IV) by binding non-weapon states to non-proliferation commitments (Article III), contingent on weapon states pursuing nuclear disarmament (Article VI). The grand-bargain reading frames this as a coordination mechanism for global nuclear governance: non-weapon states agree to restraint in exchange for access to technology and the security guarantee of a disarming world.
% TRANSFER_FUNCTION: Moves sovereign military capacity (the right to possess nuclear weapons) from non-weapon states to the NPT regime, where it is held in abeyance. Moves technology, fuel, and technical expertise from weapon states and advanced non-weapon states to NNWS under Article IV conditionality. Moves verification burden and transparency asymmetrically onto non-weapon states (Article III inspections are intrusive, weapon-state declarations unverified). Under the grand-bargain reading, it should also move disarmament obligation (and verification burden) onto weapon states, but this transfer is not actualized—it remains contested.
% ABSENT_VOICES: Non-signatory states (India, Pakistan, Israel) would argue the NPT regime is illegitimate—NNWS are bound while weapon states retain arsenals; they are excluded from the negotiation frame. Potential proliferators (Iran, North Korea at different times) argue the regime is discriminatory. Civil-society groups from the Global South argue NNWS bear asymmetric verification costs and technology dependence without equivalent security guarantees. The grand-bargain reading depends on these voices being brought into the legitimacy debate; their suppression is itself an enforcement mechanism.
% DISAPPEARANCE_RATIONALE: If the NPT's reciprocal obligation structure (Article IV conditionality on Article VI progress) formally disappeared—if the grand-bargain reading were completely displaced by the nonproliferation_primary reading (Article VI aspirational, Article IV unconditional)—non-weapon states would face a different legitimacy question: why restrain proliferation if disarmament is not a binding obligation? NNWS would likely demand renegotiation of technology access, expand domestic enrichment capacity under Article IV rights, or withdraw. The global security architecture built on the assumption of NPT commitment would undergo stress-testing. Conversely, if the abolitionist reading were adopted (Article IV illegitimate, Article VI absolute), the treaty structure would be delegitimized from above, triggering either radical reformation or withdrawal cascades.
% FOUNDING_PROBLEM: The founding problem (1968) was horizontal nuclear proliferation: preventing non-weapon states from developing nuclear weapons while accommodating the legitimate needs of weapon states to retain deterrent arsenals temporarily. The bargain was explicit in the treaty's language: NNWS restraint in exchange for weapon-state disarmament progress and technology sharing.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and many non-weapon states (aligned with IAEA, NATO allies) attest that the founding problem was horizontal proliferation and that the regime has succeeded: far fewer states possess or actively pursue weapons than 1968 projections. Disarmament advocates, abolitionist movements, and NNWS governments (Iran, NAM leadership, Global South coalitions) attest that the founding problem was DUAL: proliferation containment AND disarmament progress—and the regime has failed the second half. Non-aligned movement documents (NAM statements from 2010 NPT Review Conference) and expert testimony from outside the weapon-state beneficiary set corroborate the contested reading: the treaty's premise was reciprocity, and reciprocity has not been honored.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62 at interval end) reflects the asymmetric enforcement burden: NNWS are verified intrusively and unconditionally under Article III; weapon states are verified weakly and conditionally (if at all) on Article VI. The constraint extracts sovereignty from NNWS (limits their military options) without equivalent restriction on weapon states. Suppression is higher (0.71) because operationalizing the grand-bargain reading is actively resisted: weapon states and their allies promote the nonproliferation_primary reading instead, and institutional structures (IAEA mandate, Security Council composition) reinforce asymmetric verification. Theater ratio is moderate-high (0.48 at interval end) and rising: disarmament negotiations produce declarations and treaties (START, TPNW, ICAN campaigns) that appear to honor Article VI while arsenals are maintained, reconsidered, or redeployed. The measurement series show extraction and suppression both rising from 1968 (T=0, just after NPT entry into force) through 2010 (T=42, NPT Review Conference where reciprocity arguments crystallized) and plateau thereafter (2010–2024, T=56): extraction plateaus at 0.62, suppression at 0.71, theater at 0.48. The plateau reflects an equilibrium: the grand-bargain reading is stably suppressed but not foreclosed, weapon states extract stable asymmetric benefit, and NNWS accept conditional restraint without operationalizing the reciprocity condition. Coercion grid shows structural and organizational levels experience higher suppression (0.68 and 0.72 at interval end) than class or individual levels (0.74 and 0.65): institutional suppression (weapon-state forum control, IAEA mandate limits) is more significant than grass-roots resistance, though NAM coalitions and civil society mount steady class-level opposition.
 *
 * PERSPECTIVAL GAP:
 *   Weapon states (agenda-setter seat) view this constraint as rope: they set the verification rules, share technology selectively, and maintain strategic deterrent—coordination with asymmetric benefit. Non-weapon states (payer seat) experience this as tangled_rope or snare depending on whether they credit the grand-bargain reading (mutual conditionality, beneficiary potential) or the nonproliferation_primary reading (unconditional restraint, extraction without reciprocal disarmament). The IAEA experiences this as pure coordination cost: verification is genuinely resource-intensive and necessary for the regime's technical function, but the asymmetry (NNWS inspected, weapon states not) makes it a payer role with limited beneficiary upside. NGOs and disarmament advocates experience this as snare: the constraint's enforcement (suppression of the grand-bargain reading, institutional reinforcement of asymmetry) depends on coercion and alternative suppression (the nonproliferation_primary reading as the dominant institutional norm). The engine computes these divergences from the structural data: weapon-state directionality (low d toward extraction, high effective extraction χ from commanding power and exit arbitrage) versus NNWS directionality (high d toward target, extraction modulated by the contingent beneficiary role the grand-bargain reading assigns them).
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states hold the agenda-setter role, institutional power, and arbitrage exit (can withdraw from the treaty, though at geopolitical cost; can maintain deterrents regardless of article VI compliance, since verification is weak). Their directionality is toward beneficiary (d near 0.0–0.2): they extract asymmetric verification advantage and strategic legitimacy from the regime. Non-weapon states hold organized power, constrained exit (withdrawal is theoretically possible but institutionally/economically costly—loss of technology access, isolation), and bifurcated role: beneficiary under the grand-bargain reading (receive technology, restrain is conditional on weapon-state progress), payer under the nonproliferation_primary reading (restrain unconditionally, receive technology at weapon-state discretion). Under the grand-bargain reading, NNWS directionality shifts from high-target (d ~0.7–0.8) to moderate-target (d ~0.55–0.65): the conditionality adds beneficiary structure. The authoring decision not to use a directionality override reflects this: the structural data (bifurcated beneficiary/victim declaration, conditional exit option mediated by article VI enforceability) drives the split directionality naturally. The IAEA sits at moderate directionality (d ~0.55–0.60): genuine verification cost (payer side) coupled with technical indispensability (beneficiary side) and constrained exit (institutional mandate-dependence).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was horizontal proliferation prevention (1968 founding) and weapon-state disarmament (stated in both Article IV and Article VI). The constraint's mandate has undergone bifurcation and partial decay: (1) horizontal proliferation prevention is live and effectively performed—few states acquire weapons, verification catches hidden programs, supplier controls work. (2) Weapon-state disarmament is dead—arsenals have been reduced from Cold War peaks but remain stable at thousands, no agreement on timelines, no enforcement mechanism, no acceleration toward abolition. The grand-bargain reading asserts that the bifurcation is itself a breach: if disarmament is dead, Article IV legitimacy erodes, and the horizontal proliferation prevention mandate cannot survive without the disarmament-progress condition. The nonproliferation_primary reading treats the mandate as successfully narrowed: prevent horizontal proliferation; the disarmament aspiration was always secondary and is properly downgraded to hortatory language. The constraint story authored here reflects the grand-bargain reading's claim that mandatrophy is both active (the disarmament mandate has atrophied) and violation-consequential (breach of Article VI undermines Article IV legitimacy). The suppression and theater measurements capture how this mandatrophy is institutionally managed: high theater ratio (disarmament rhetoric, treaties, declarations) coupled with high suppression (the grand-bargain reading is excluded from operational interpretation) preserves the facade of a dual mandate while functionally downgrading disarmament to performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_enforceability_status,
    'Is Article VI enforceable as a binding obligation on weapon states, or is it a non-justiciable aspirational commitment?',
    'A definitive International Court of Justice opinion on the NPT''s legally binding character; amendment via a NPT Review Conference that explicitly clarifies Article VI status; or a state''s invocation of Article VI breach as grounds for treaty withdrawal under VCLT Article 60 (material breach), forcing adjudication.',
    'If VI is enforceable, the grand-bargain reading becomes operative: weapon-state disarmament progress becomes a condition of NNWS Article IV obligations, and breach of VI licenses NNWS withdrawal or non-compliance. If VI is aspirational, the nonproliferation_primary reading holds: NNWS restraint is unconditional, and Article IV technology transfer is independent of disarmament progress. This resolves the core structural ambiguity of the grand-bargain reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_enforceability_status, conceptual, 'Whether Article VI is a binding legal obligation or an aspirational political commitment.').

omega_variable(
    reciprocity_doctrine_adoption_path,
    'Will the reciprocity doctrine (that Article IV and VI are mutually conditioning) become the dominant reading in international law and state practice, or remain minoritarian?',
    'Accumulation of NPT Review Conference resolutions (2015, 2020, 2026 cycles) that explicitly affirm reciprocity; adoption by the Non-Aligned Movement and NNWS coalitions as formal negotiating position; successful invocation by a state in a WTO or UN legal forum; or reverse—formal rejection by weapon states and allied NNWS in successive review conferences.',
    'Adoption of reciprocity would operationalize the grand-bargain reading, shift the burden of proof onto weapon states to demonstrate disarmament progress, and enable NNWS legal claims to withdraw or condition compliance. Persistent minoritarian status would leave the reading as NGO and scholar advocacy without institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_doctrine_adoption_path, empirical, 'Whether reciprocity doctrine becomes institutionalized or remains scholarly/activist framing.').

omega_variable(
    weapon_state_disarmament_measurement,
    'What constitutes measurable disarmament progress sufficient to sustain NNWS Article IV obligations? What metrics, timelines, and verification thresholds apply?',
    'Negotiated protocols establishing disarmament verification procedures and progress benchmarks; unilateral declarations by weapon states (credibility-dependent); independent verification by IAEA or UN mechanisms; or contested dispute over whether existing reductions (SALT, START, INF legacy) count as ''progress toward disarmament'' under Article VI.',
    'Without agreed metrics, the grand-bargain reading cannot operationalize: NNWS cannot condition compliance on progress they cannot measure. With metrics, the constraint becomes machine-readable: weapon-state arsenal reductions (or lack thereof) can be compared against NNWS restrained behavior, and divergence can trigger Article IV withdrawal claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weapon_state_disarmament_measurement, empirical, 'Whether disarmament progress can be defined and measured operationally.').

omega_variable(
    asynchronous_treaty_interpretation_contest,
    'Is this constraint a kernel reading of a single contested commitment (the NPT) or multiple distinct constraints emerging from different interpretive traditions?',
    'Meta-analysis of whether the grand_bargain, nonproliferation_primary, and abolitionist readings are alternative coherent framings of a single treaty text or three distinct constraints with incompatible ε values, beneficiary structures, and persistence mechanisms.',
    'If a single kernel with multiple readings, the three constraints form a family linked by network.affects_constraints and interpretive contention. If three structurally distinct constraints, they should be authored separately with different ε, beneficiaries, victims, and measures—the contest is then between independent constraints, not readings of one. The committer-frame choice determines whether the constraint family is structured as readings (one kernel, three interpretations) or as independent constraints (three kernels, each instantiating a different claim).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asynchronous_treaty_interpretation_contest, conceptual, 'Whether the grand-bargain, nonproliferation-primary, and abolitionist framings are readings of one kernel or three independent constraints.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of the grand-bargain reading structural (legal barriers, institutional exclusion, enforcement machinery blocking its adoption) or internalized (NNWS internalize the nonproliferation_primary reading as legitimate, discouraging reciprocity claims)?',
    'Post-institutional-change trajectory: if suppression persists after formal legal barriers are removed (e.g., if a NPT amendment explicitly authorizes the reciprocity reading), the suppression is substantially internalized. If suppression declines sharply once barriers fall, it was primarily structural.',
    'If internalized, NNWS carry the suppression forward even if institutional blocks are removed—the constraint''s effective suppression (0.71 at interval end) is underestimated; the reading requires both legal framework change and norm-shift among NNWS to operationalize. If structural, institutional reform sufficient to operationalize it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of the grand-bargain reading is maintained by formal barriers or by internalized norm acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t8, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(npt__tr_t8, observed).
narrative_ontology:measurement(npt__tr_t16, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(npt__tr_t16, observed).
narrative_ontology:measurement(npt__tr_t24, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(npt__tr_t24, observed).
narrative_ontology:measurement(npt__tr_t32, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 32, 0.46).
narrative_ontology:measurement_basis(npt__tr_t32, observed).
narrative_ontology:measurement(npt__tr_t40, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(npt__tr_t40, observed).
narrative_ontology:measurement(npt__tr_t48, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 48, 0.48).
narrative_ontology:measurement_basis(npt__tr_t48, observed).
narrative_ontology:measurement(npt__tr_t56, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 56, 0.48).
narrative_ontology:measurement_basis(npt__tr_t56, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t8, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(npt__be_t8, observed).
narrative_ontology:measurement(npt__be_t16, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 16, 0.48).
narrative_ontology:measurement_basis(npt__be_t16, observed).
narrative_ontology:measurement(npt__be_t24, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 24, 0.54).
narrative_ontology:measurement_basis(npt__be_t24, observed).
narrative_ontology:measurement(npt__be_t32, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 32, 0.59).
narrative_ontology:measurement_basis(npt__be_t32, observed).
narrative_ontology:measurement(npt__be_t40, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(npt__be_t40, observed).
narrative_ontology:measurement(npt__be_t48, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 48, 0.62).
narrative_ontology:measurement_basis(npt__be_t48, observed).
narrative_ontology:measurement(npt__be_t56, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 56, 0.62).
narrative_ontology:measurement_basis(npt__be_t56, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t8, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(npt__su_t8, observed).
narrative_ontology:measurement(npt__su_t16, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 16, 0.58).
narrative_ontology:measurement_basis(npt__su_t16, observed).
narrative_ontology:measurement(npt__su_t24, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 24, 0.64).
narrative_ontology:measurement_basis(npt__su_t24, observed).
narrative_ontology:measurement(npt__su_t32, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 32, 0.69).
narrative_ontology:measurement_basis(npt__su_t32, observed).
narrative_ontology:measurement(npt__su_t40, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(npt__su_t40, observed).
narrative_ontology:measurement(npt__su_t48, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 48, 0.71).
narrative_ontology:measurement_basis(npt__su_t48, observed).
narrative_ontology:measurement(npt__su_t56, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 56, 0.71).
narrative_ontology:measurement_basis(npt__su_t56, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=56
narrative_ontology:measurement(npt__grid_01, npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(npt__grid_02, npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse(class), 56, 0.7).
narrative_ontology:measurement(npt__grid_03, npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(npt__grid_04, npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse(individual), 56, 0.62).
narrative_ontology:measurement(npt__grid_05, npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse(organizational), 0, 0.62).
narrative_ontology:measurement(npt__grid_06, npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse(organizational), 56, 0.68).
narrative_ontology:measurement(npt__grid_07, npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(npt__grid_08, npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse(structural), 56, 0.65).
narrative_ontology:measurement(npt__grid_09, npt_article_iv_vi_pairing__grand_bargain, resistance(class), 0, 0.82).
narrative_ontology:measurement(npt__grid_10, npt_article_iv_vi_pairing__grand_bargain, resistance(class), 56, 0.82).
narrative_ontology:measurement(npt__grid_11, npt_article_iv_vi_pairing__grand_bargain, resistance(individual), 0, 0.68).
narrative_ontology:measurement(npt__grid_12, npt_article_iv_vi_pairing__grand_bargain, resistance(individual), 56, 0.72).
narrative_ontology:measurement(npt__grid_13, npt_article_iv_vi_pairing__grand_bargain, resistance(organizational), 0, 0.78).
narrative_ontology:measurement(npt__grid_14, npt_article_iv_vi_pairing__grand_bargain, resistance(organizational), 56, 0.8).
narrative_ontology:measurement(npt__grid_15, npt_article_iv_vi_pairing__grand_bargain, resistance(structural), 0, 0.72).
narrative_ontology:measurement(npt__grid_16, npt_article_iv_vi_pairing__grand_bargain, resistance(structural), 56, 0.75).
narrative_ontology:measurement(npt__grid_17, npt_article_iv_vi_pairing__grand_bargain, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(npt__grid_18, npt_article_iv_vi_pairing__grand_bargain, stakes_inflation(class), 56, 0.68).
narrative_ontology:measurement(npt__grid_19, npt_article_iv_vi_pairing__grand_bargain, stakes_inflation(individual), 0, 0.45).
narrative_ontology:measurement(npt__grid_20, npt_article_iv_vi_pairing__grand_bargain, stakes_inflation(individual), 56, 0.54).
narrative_ontology:measurement(npt__grid_21, npt_article_iv_vi_pairing__grand_bargain, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(npt__grid_22, npt_article_iv_vi_pairing__grand_bargain, stakes_inflation(organizational), 56, 0.64).
narrative_ontology:measurement(npt__grid_23, npt_article_iv_vi_pairing__grand_bargain, stakes_inflation(structural), 0, 0.48).
narrative_ontology:measurement(npt__grid_24, npt_article_iv_vi_pairing__grand_bargain, stakes_inflation(structural), 56, 0.58).
narrative_ontology:measurement(npt__grid_25, npt_article_iv_vi_pairing__grand_bargain, suppression(class), 0, 0.52).
narrative_ontology:measurement(npt__grid_26, npt_article_iv_vi_pairing__grand_bargain, suppression(class), 56, 0.74).
narrative_ontology:measurement(npt__grid_27, npt_article_iv_vi_pairing__grand_bargain, suppression(individual), 0, 0.4).
narrative_ontology:measurement(npt__grid_28, npt_article_iv_vi_pairing__grand_bargain, suppression(individual), 56, 0.65).
narrative_ontology:measurement(npt__grid_29, npt_article_iv_vi_pairing__grand_bargain, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(npt__grid_30, npt_article_iv_vi_pairing__grand_bargain, suppression(organizational), 56, 0.72).
narrative_ontology:measurement(npt__grid_31, npt_article_iv_vi_pairing__grand_bargain, suppression(structural), 0, 0.42).
narrative_ontology:measurement(npt__grid_32, npt_article_iv_vi_pairing__grand_bargain, suppression(structural), 56, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__grand_bargain, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested NPT Article IV–VI kernel. The grand_bargain reading asserts reciprocal obligation (disarmament conditions restraint); nonproliferation_primary asserts asymmetric obligation (restraint unconditional on disarmament); abolitionist asserts complete abolition mandate. All three share the same treaty text but instantiate different constraints with different ε, beneficiary structures, and enforcement mechanisms. The network links capture the interpretive contest: each reading's operationalization would preclude or materially pressure the others (foreclosure or influence relationships). They are not independent constraints—they are genealogically bound through the kernel and mutually reinforcing/exclusive in their legitimacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__grand_bargain, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
