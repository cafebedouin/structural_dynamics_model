% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing — Abolitionist Reading (Disarmament-Mandate / Prohibition-Norm)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the NPT Article IV/VI
 *   kernel: Article VI is read as an unconditional, categorical mandate to
 *   complete disarmament, not a good-faith aspirational commitment; Article
 *   IV's peaceful-use guarantee is read as illegitimate to the extent it
 *   perpetuates dual-use proliferation-relevant technology transfer; and
 *   legal authority is drawn from humanitarian law and from the Treaty on the
 *   Prohibition of Nuclear Weapons (TPNW), which this reading treats as
 *   having established that nuclear weapon possession itself is categorically
 *   unlawful, collapsing the peaceful/military use distinction the NPT relies
 *   on. Under this reading the standing arrangement — the NPT's Article IV/VI
 *   pairing as actually practiced since 1968 — is the referent for ε,
 *   assessed by the abolitionist reading's own lights: high extraction,
 *   because weapon states retain arsenals and dual-use cooperation while
 *   treating disarmament as non-binding.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: institutional beneficiaries who retain arsenals under a bargain this reading holds they have never honored
 *   - nuclear_supplier_group_members: organized beneficiaries controlling dual-use technology transfer under Article IV
 *   - non_nuclear_weapon_states: constrained payers who accepted permanent restraint for an unfulfilled promise
 *   - tpnw_signatory_states: the authority-bearing constituency this reading recognizes, structurally excluded from binding influence over weapon states
 *   - populations_near_test_and_production_sites: powerless, trapped payers bearing the direct physical costs of continued weapons infrastructure
 *   - international_court_of_justice: analytical observer whose 1996 opinion is cited as corroborating authority for the disarmament-mandate reading
 *   - iaea_safeguards_regime: agenda-setting verification body whose safeguards this reading treats as necessary but incapable of resolving dual-use ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.81).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.81).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing — Abolitionist Reading (Disarmament-Mandate / Prohibition-Norm)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '53699527-bfe2-41e9-bf2d-ec7abfd29b4a').
narrative_ontology:cs_kernel_codification('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', fixed_text).
narrative_ontology:cs_authority_grounding('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', distributed).
narrative_ontology:cs_reading_relation('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_axiom('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', foundational, possession_categorically_unlawful).
narrative_ontology:cs_axiom_status(possession_categorically_unlawful, holdable).
narrative_ontology:cs_axiom_grounding('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', possession_categorically_unlawful, deontological).
narrative_ontology:cs_axiom('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', foundational, peaceful_military_distinction_incoherent).
narrative_ontology:cs_axiom_status(peaceful_military_distinction_incoherent, holdable).
narrative_ontology:cs_axiom_grounding('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', peaceful_military_distinction_incoherent, empirically_contingent).
narrative_ontology:cs_axiom('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', secondary, article_vi_unconditional_mandate).
narrative_ontology:cs_axiom_status(article_vi_unconditional_mandate, holdable).
narrative_ontology:cs_axiom_grounding('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', article_vi_unconditional_mandate, deontological).
narrative_ontology:cs_reference_frame('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', npt_1968_grand_bargain_framework).
narrative_ontology:cs_drift_state('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', post_tpnw_adoption_2017, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('53699527-bfe2-41e9-bf2d-ec7abfd29b4a', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_supplier_group_members).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, tpnw_signatory_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, populations_near_test_and_production_sites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NPT-recognized weapon states retain arsenals while citing Article IV's peaceful-use guarantee to justify continued fuel-cycle and technology cooperation among themselves and allies. Under this reading, their non-fulfillment of Article VI's disarmament mandate is treated as the arrangement's central, unremedied breach — they set the pace of disarmament negotiations and face no binding enforcement mechanism compelling divestment of arsenals.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, agenda_setter).

% Control export of enrichment and reprocessing technology under Article IV cooperation commitments. They profit from technology transfer arrangements and from being gatekeepers of what counts as 'peaceful' nuclear cooperation, a status this reading holds is inherently compromised because the same fuel-cycle capabilities enable weapons breakout.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_supplier_group_members, beneficiary,
    organized, generational, mobile, global).

% Accepted permanent non-acquisition obligations in exchange for a disarmament promise now four decades unfulfilled and for peaceful-use guarantees this reading holds are structurally poisoned by dual-use risk. Their exit options are limited: withdrawal from the NPT carries severe diplomatic and security costs, and Article IV cooperation is often the only route to civilian nuclear energy technology, making continued membership a trap disguised as a benefit.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, national).

% Adopted the Treaty on the Prohibition of Nuclear Weapons as the authority this reading recognizes for categorical illegality of possession. They bear the cost of parallel-regime friction — weapon states and their allies refuse to engage with TPNW obligations, so signatories pay a diplomatic and legal-legitimacy cost for holding the position this reading treats as authoritative, without the material power to compel compliance from anyone outside their own ranks.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_signatory_states, payer,
    moderate, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, tpnw_signatory_states, excluded).

% Bear the direct physical and environmental costs of weapons production, testing, and fuel-cycle infrastructure that the Article IV/VI pairing perpetuates by legitimizing continued nuclear activity under a 'peaceful use' framing this reading rejects as a meaningful moral distinction. They have essentially no voice in treaty negotiation and no exit from contaminated land or bodies.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, populations_near_test_and_production_sites, payer,
    powerless, generational, trapped, regional).

% Issued the 1996 Advisory Opinion holding that Article VI creates an obligation to pursue negotiations in good faith to a conclusion — language this reading treats as a binding categorical mandate rather than aspirational guidance. Has no enforcement power and cannot compel weapon-state compliance; its opinion is invoked as legal authority by abolitionist advocates but not treated as dispositive by weapon states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% Administers Article III verification that underwrites the Article IV promise of peaceful-use cooperation. Under this reading, the safeguards regime is treated as necessary but insufficient — it can detect diversion after the fact but cannot resolve the underlying dual-use ambiguity of enrichment and reprocessing technology, meaning the 'peaceful' guarantee it certifies is definitionally unstable.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, iaea_safeguards_regime, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, iaea_safeguards_regime, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__abolitionist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The pairing was originally meant to coordinate a bargain: non-weapon states forgo acquisition and gain access to peaceful nuclear technology and cooperation, while weapon states move toward disarmament — solving a mutual-assurance problem that would otherwise produce an arms race.
% TRANSFER_FUNCTION: Moves security guarantees and reputational legitimacy to weapon states and technology-supplier states, while moving permanent constraint, unfulfilled disarmament promises, and dual-use proliferation risk onto non-weapon states and populations near nuclear infrastructure. Article IV's technology-sharing flow itself transfers latent weapons-capable knowledge without transferring the disarmament weapon states owe in return.
% ABSENT_VOICES: Populations near test, mining, and production sites are almost never seated at treaty-review conferences. TPNW signatory states are present in UN fora but are treated as outside the operative legal conversation by weapon states, who do not attend TPNW meetings and do not recognize its authority — this reading holds that exclusion is itself illegitimate, not merely inconvenient.
% DISAPPEARANCE_RATIONALE: If the Article IV/VI pairing vanished overnight, weapon states would lose their primary legal cover for indefinite arsenal retention paired with technology-sharing privileges, and the field would likely consolidate around either the TPNW's categorical prohibition or a return to unregulated proliferation risk. Non-weapon states dispute whether this would improve or worsen their position: some see the pairing as a fragile brake on horizontal proliferation worth preserving despite its failures; abolitionist readings hold the pairing's disappearance would simply reveal the prohibition norm this reading already treats as governing.
% FOUNDING_PROBLEM: In 1968, the founding problem was preventing horizontal proliferation of nuclear weapons to additional states while allowing broad access to civilian nuclear energy, in exchange for a credible path toward eventual disarmament by existing weapon states.
% FOUNDING_PROBLEM_CORROBORATION: The International Court of Justice's 1996 Advisory Opinion and subsequent NPT Review Conference final documents (repeatedly failing consensus since 2005) are cited by non-weapon states and disarmament NGOs — parties outside the weapon-state beneficiary set — as evidence the Article VI bargain was never substantively performed. Weapon states themselves do not dispute the absence of complete disarmament; they dispute only whether Article VI was ever meant to require it on any fixed timeline, which is itself evidence the founding problem's resolution mechanism has failed to bind.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, contested).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because, under this reading, weapon states and supplier states capture the ongoing benefits of the arrangement — legal legitimacy, technology cooperation, deterrence status — while non-weapon states and TPNW signatories bear a restraint obligation whose reciprocal consideration (complete disarmament) has not been delivered in over five decades. Suppression is elevated (0.72) because the arrangement is maintained through active diplomatic and legal machinery: weapon states' non-recognition of TPNW authority, NPT review-conference procedures that require consensus (giving weapon states an effective veto over binding disarmament language), and export-control regimes that gatekeep dual-use technology. Theater ratio is substantial and rising (0.58 by 2024) because much of the NPT review-conference apparatus — working groups, disarmament pledges, side events — under this reading functions increasingly as performative reaffirmation of Article VI language without operational content, while the underlying arsenals and dual-use cooperation continue unchanged. Accessibility collapse is moderate (0.4), not high, because this reading holds a live alternative institutional path exists and has been taken — the TPNW itself is a functioning alternative legal framework, so alternatives have not collapsed; they have been actively suppressed from binding recognition rather than foreclosed as unavailable. Resistance is high (0.75): TPNW ratification, ICJ advisory engagement, and non-weapon-state coalition activity at review conferences represent substantial active contestation of the arrangement as this reading characterizes it.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states and supplier-group members sit near the beneficiary end of directionality: they collect the legal legitimacy and cooperative-technology benefits of the pairing while bearing no binding disarmament cost under current enforcement. Non-weapon states and TPNW signatories sit near the target end: they accepted permanent constraint in exchange for a promise this reading holds has not been performed, and their exit options are constrained by the security and diplomatic costs of NPT withdrawal or by exclusion from the parallel TPNW regime's practical force. Populations near test and production sites are the most fully targeted: trapped, powerless, and bearing direct physical costs with no seat at treaty negotiations at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure coordination (a rope) by requiring the story to name concrete beneficiaries and victims and to show active enforcement machinery (consensus-blocking at review conferences, export-control gatekeeping, non-recognition of TPNW) sustaining the asymmetry — this is what makes it a tangled rope rather than a snare: a genuine coordination function (preventing horizontal proliferation, enabling civilian nuclear cooperation) did exist and partially persists, but under this reading the disarmament half of the bargain has decayed into non-performance while the restraint half remains fully enforced, producing structural extraction riding on the coordination story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_displacement_completeness,
    'Does TPNW ratification by a minority of UN member states (none of them nuclear-armed) actually displace NPT authority as a matter of customary international law, or does it remain a parallel, non-binding normative claim that weapon states can simply decline to recognize?',
    'Track whether TPNW prohibition norms achieve customary-international-law status through consistent state practice and opinio juris, including ICJ engagement, over the coming decades; track whether any weapon or umbrella state''s practice shifts in response to TPNW pressure even absent ratification.',
    'If TPNW achieves customary status, this reading''s authority claim strengthens substantially and Article IV''s legitimacy gap becomes a matter of binding law rather than contested legal theory. If TPNW remains a minority-treaty position with no customary uptake, this reading''s authority claim remains aspirational despite its internal coherence, and the abolitionist classification of the standing arrangement as tangled_rope (rather than a fully displaced illegitimate regime) understates how contested its own legal grounding is.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_displacement_completeness, conceptual, 'Whether TPNW-derived authority genuinely displaces or merely contests NPT legal authority.').

omega_variable(
    dual_use_inseparability,
    'Is the technical inseparability of peaceful and military nuclear fuel-cycle capability (enrichment, reprocessing) a fixed physical fact that makes Article IV''s peaceful/military distinction incoherent, or is it a matter of degree that safeguards and verification can manage to an acceptable residual risk?',
    'Technical assessment of breakout timelines under varying safeguards regimes; comparison of proliferation outcomes in states with extensive Article IV cooperation versus states without it.',
    'If inseparability is near-total, this reading''s claim that Article IV is structurally illegitimate wherever exercised gains strong empirical support. If verification can manage the risk to a low residual level, the abolitionist reading''s categorical rejection of the peaceful/military distinction would be assessed as overstated relative to the grand_bargain reading''s conditional-restraint framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_inseparability, empirical, 'Whether dual-use risk is a categorical or a manageable-degree problem.').

omega_variable(
    review_conference_consensus_veto_intent,
    'Was the NPT review conference''s consensus requirement designed, or has it functioned in practice, as a deliberate veto mechanism protecting weapon-state interests, or is it a neutral procedural norm whose blocking effect is an unintended byproduct?',
    'Historical analysis of NPT negotiating records from 1968 and subsequent review-conference procedural votes to determine whether consensus was adopted with foreseeable blocking effects in mind.',
    'If deliberately designed as a veto, this strengthens the case for suppression as an intentional enforcement mechanism rather than incidental friction, supporting the tangled_rope classification''s active-enforcement requirement more strongly. If unintended, the suppression measurement should be read as partly emergent rather than fully engineered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_conference_consensus_veto_intent, empirical, 'Whether procedural consensus rules function as intentional or incidental enforcement of the status quo.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(npt__tr_t1995, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(npt__tr_t2005, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(npt__tr_t2017, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2017, 0.53).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(npt__be_t1995, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(npt__be_t2005, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(npt__be_t2017, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2017, 0.77).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(npt__su_t1995, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(npt__su_t2005, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(npt__su_t2017, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2017, 0.69).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__abolitionist, 0.1).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_prohibition_norm).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the npt_article_iv_vi_pairing kernel. 'nonproliferation_primary' authors Article IV as conditional on Article III verification and Article VI as non-justiciable, grounding authority in weapon-state security interest — it would author a substantially lower ε for the same standing arrangement because it does not treat non-fulfillment of disarmament as a legitimacy-undermining breach. 'grand_bargain' treats the two articles as genuinely reciprocal and conditional, authoring a moderate ε reflecting partial but incomplete performance on both sides. This reading ('abolitionist') authors the highest ε among the three because it treats the entire framework as insufficient against a categorical prohibition norm it holds already governs. Each reading shares the same underlying treaty text and historical record but differs in which authority structure and interpretive premises govern legitimacy — per the kernel/reading discipline, these are three distinct constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
