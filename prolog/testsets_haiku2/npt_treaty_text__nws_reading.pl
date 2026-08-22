% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Non-Proliferation Binding on NNWS; Disarmament Aspirational (NWS Reading)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty (1968) contains two structural asymmetries
 *   read very differently across the fault line of this reading: Article II
 *   requires non-nuclear weapons states (NNWS) to renounce weapons 'in
 *   perpetuity' (binding, present tense, concrete), while Article VI requires
 *   nuclear weapons states (NWS) to pursue disarmament 'at an early date'
 *   (aspirational, future-oriented, undefined). This reading—instantiated by
 *   NWS governments, IAEA interpretation practice, UN Security Council
 *   resolutions, and most NPT Review Conference outcomes—treats
 *   non-proliferation as the binding quid pro quo and disarmament as a
 *   long-term aspiration without enforcement. The sibling nnws_reading
 *   inverts the normative priority: disarmament is the binding obligation
 *   (the payment NNWS purchase with their non-proliferation commitment), and
 *   non-proliferation is conditional on NWS compliance. The kernel itself is
 *   the treaty text and its formal authority structure; the reading is how
 *   NWS interpret that text to preserve asymmetry. This reading benefits NWS
 *   by freezing the strategic status quo (arsenals retained) while binding
 *   NNWS in perpetuity (no acquisition ever). The reading is not a lie about
 *   what the text says; it is a defensible but contestable interpretation of
 *   the text's legal force.
 *
 * KEY AGENTS:
 *   - NWS (P-5 + France): interpret treaty, control IAEA Board, enforce Security Council vetoes on investigations and sanctions
 *   - NNWS: bound by Article II, subject to full-scope safeguards, locked into non-acquisition
 *   - IAEA: verifies NNWS compliance, safeguards budget concentrated on horizontal proliferation, operates under Board control (NWS-dominated)
 *   - Disarmament advocates / NNWS coalitions: excluded from interpretive authority, attempt Article VI reframing at Review Conferences
 *   - Threshold states: constrained by fuel supply, inspection regimes, and sanctions threat
 *   - Global South: powerless NNWS without advanced capacity or security alliances, permanently subordinated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.72).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.68).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Non-Proliferation Binding on NNWS; Disarmament Aspirational (NWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, 'fec85511-5c53-4bf9-88c3-4be3cba14eca').
narrative_ontology:cs_kernel_codification('fec85511-5c53-4bf9-88c3-4be3cba14eca', formalized).
narrative_ontology:cs_authority_grounding('fec85511-5c53-4bf9-88c3-4be3cba14eca', extraction).
narrative_ontology:cs_interpretation_layer_present('fec85511-5c53-4bf9-88c3-4be3cba14eca').
narrative_ontology:cs_reading_relation('fec85511-5c53-4bf9-88c3-4be3cba14eca', npt_treaty_text__nnws_reading, forecloses).
narrative_ontology:cs_reading_relation('fec85511-5c53-4bf9-88c3-4be3cba14eca', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('fec85511-5c53-4bf9-88c3-4be3cba14eca', foundational, article_vi_non_binding_aspiration).
narrative_ontology:cs_axiom_status(article_vi_non_binding_aspiration, holdable).
narrative_ontology:cs_axiom_grounding('fec85511-5c53-4bf9-88c3-4be3cba14eca', article_vi_non_binding_aspiration, conventional).
narrative_ontology:cs_axiom('fec85511-5c53-4bf9-88c3-4be3cba14eca', foundational, nws_arsenal_retention_justified_by_deterrence).
narrative_ontology:cs_axiom_status(nws_arsenal_retention_justified_by_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('fec85511-5c53-4bf9-88c3-4be3cba14eca', nws_arsenal_retention_justified_by_deterrence, instrumental).
narrative_ontology:cs_axiom('fec85511-5c53-4bf9-88c3-4be3cba14eca', secondary, horizontal_proliferation_threat_primacy).
narrative_ontology:cs_axiom_status(horizontal_proliferation_threat_primacy, holdable).
narrative_ontology:cs_axiom_grounding('fec85511-5c53-4bf9-88c3-4be3cba14eca', horizontal_proliferation_threat_primacy, empirically_contingent).
narrative_ontology:cs_reference_frame('fec85511-5c53-4bf9-88c3-4be3cba14eca', nws_asymmetric_deterrence_preservation).
narrative_ontology:cs_drift_state('fec85511-5c53-4bf9-88c3-4be3cba14eca', contemporary_disarmament_abandonment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fec85511-5c53-4bf9-88c3-4be3cba14eca', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapons_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapons_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, global_south_development_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, iaea_verification_community).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, iaea_verification_community).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, regional_rivals_without_nukes).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, threshold_states_in_transition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and administer the NPT through UN Security Council permanent membership, IAEA Board of Governors membership, and treaty negotiation leadership. Maintain nuclear arsenals while requiring NNWS non-acquisition. Control the meaning of 'at an early date' in Article VI disarmament language through interpretive practice and treaty review conference outcomes. Collect strategic advantage from the asymmetric legal structure: bound to aspire to disarmament (unenforceable), while enforcing non-proliferation (highly enforceable) against NNWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapons_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Renounce nuclear weapons acquisition in perpetuity under Article II. Subject to intrusive IAEA full-scope safeguards inspections (nuclear material accountancy, facility monitoring, environmental sampling). Forgo a credible deterrent against regional nuclear-armed states and great-power coercion. Their exit option is withdrawal under Article X, which requires three months' notice and carries the cost of international isolation, sanctions regime exposure, and permanent security disadvantage. Identity lock is profound: the NNWS designation becomes institutionalized in the regime and in geopolitical positioning; reverting to NWS status is treated as regime violation, not normative choice.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapons_states, payer,
    organized, generational, identity_locked, global).

% IAEA inspectors and analysts conduct the verification labor that enforces non-proliferation: they have budgetary and operational constraints that concentrate verification effort on horizontal proliferation risk (Iraq, Iran, Libya precursors). Tasking is set by the Board of Governors (NWS-dominated); IAEA Director-General operates under effective veto from the Security Council permanent members on investigation scope and enforcement. They bear the cost of intrusive inspections and carry reputational stakes in verification failures. Secondary benefit: their institutional role and budget depend on the non-proliferation regime's persistence and their perceived competence within it.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_verification_community, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, iaea_verification_community, beneficiary).

% States like Japan, South Korea, Turkey, and Germany remain NNWS despite regional security threats (China, Russia, Iran). They are locked into conventional deterrence or extended nuclear umbrellas (US extended deterrence commitments), unable to acquire independent nuclear capacity for self-defense. The constraint transfers strategic vulnerability to them; their options are strengthening alliances with NWS patrons or challenging the regime (both costly).
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, regional_rivals_without_nukes, payer,
    moderate, generational, constrained, regional).

% States with advanced nuclear technology (Brazil, Argentina pre-1990s, South Africa pre-1991, Iran) that could acquire weapons but are constrained by the regime. They pay through technology restrictions, fuel bank dependencies, and external inspection. Their exclusion from NWS status is enforced; any move toward weaponization triggers international sanctions and isolation. Secondary role: they carry technological capacity that makes them potential threats, so their payer status arises from the regime's need to constrain them continuously.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, threshold_states_in_transition, payer,
    powerful, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, threshold_states_in_transition, excluded).

% NNWS without advanced nuclear capacity or security alliance networks. They pay through: restricted access to peaceful nuclear energy (fuel supply control, technology transfer restrictions); resource diversion to verification compliance (hosting IAEA inspections, maintaining safeguards infrastructure); and permanent strategic subordination (no credible deterrent, vulnerability to coercion from NWS and regional powers). Exit is formally available but informationally and practically unavailable: withdrawal invokes international condemnation, sanctions, and development isolation; they lack the military capacity to create a credible deterrent anyway.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, global_south_development_economies, payer,
    powerless, generational, constrained, global).

% Civil society, humanitarian organizations, and NNWS-coalition diplomats advocate for NWS disarmament as binding obligation (not aspirational goal). They are excluded from treaty interpretation authority; their reading of Article VI is not incorporated into IAEA safeguards protocols, Security Council resolutions, or NPT review conference outcomes. They hold the opposite claim about what the treaty binds: they would say disarmament is mandatory and non-proliferation is conditional, whereas this reading inverts it. Their exclusion is structural, enforced through decision-making architecture (NWS vetoes in Security Council, NWS board control in IAEA).
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, disarmament_advocacy_coalitions, excluded,
    organized, generational, constrained, global).

% NNWS and sympathetic observers at quinquennial NPT Review Conferences attempt to reframe the treaty toward disarmament as binding (shifting the interpretation axis). They have institutional presence (voting power in conference committees) but lack enforcement power: NWS can block consensus, veto resolutions on substantive matters, and hold veto threat over conference closure. The observer position captures this paradox: formal inclusion but structural impotence in outcome determination.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, npt_review_conference_majority, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nuclear_weapons_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binding legal architecture for horizontal non-proliferation: NNWS renounce weapons acquisition, agree to full-scope IAEA safeguards, and create an international verification infrastructure that deters or detects weapons programs across 191 countries. The NWS accept the same IAEA safeguards framework on civil nuclear materials only (not military arsenals), preserving asymmetry. The coordination solves: (a) mutual fear that without a binding regime, proliferation cascades and destabilizes global security; (b) verification infrastructure that was not coordinated before the NPT.
% TRANSFER_FUNCTION: Moves strategic capacity asymmetrically: from NNWS to NWS. The transfer mechanisms are: (1) permanent renunciation of weapons development for NNWS (strategic capacity transferred by forgoing it); (2) NWS retention of arsenals (strategic capacity retained by exemption); (3) IAEA inspection labor concentrated on NNWS (verification asymmetry, inspection frequency ratios 10:1+ favoring scrutiny of NNWS over NWS civil fuel); (4) UN Security Council permanent seat preservation and veto power for NWS (institutional decision-making power concentrated). The transfer is enforced through security dependence: NNWS depend on NWS extended deterrence or conventional alliances, which NWS can threaten to withdraw.
% ABSENT_VOICES: Disarmament advocacy coalitions, humanitarian organizations, and NNWS blocs that read Article VI as binding are structurally excluded from interpretive authority. They cannot shape IAEA safeguards protocols, NPT review conference outcomes do not bind without NWS consensus, and Security Council resolutions (the only binding mechanisms) carry NWS vetoes. A reading that inverted this one—treating disarmament as mandatory and non-proliferation as conditional—would be held by many NNWS and civil society actors but is not present in the regime's interpretive machinery.
% DISAPPEARANCE_RATIONALE: If this constraint and its enforcement machinery vanished, the international legal architecture for non-proliferation would collapse overnight. NNWS would no longer face legal or inspectorial obligation to forgo weapons; NWS arsenal retention would become purely de facto, not treaty-codified; IAEA safeguards would lose legal standing; and regional conflicts would accelerate toward proliferation (Israel, Iran, Pakistan, North Korea precedents establish that absent treaty constraint, weapons acquisition accelerates). NWS strategic leverage through the NPT would evaporate: the treaty is the instrument through which NWS enforce non-proliferation while preserving their own arsenals. A world without this constraint is a world without the current architecture of asymmetric arms control.
% FOUNDING_PROBLEM: The founding problem (1968) was the prospect of nuclear proliferation among non-aligned states during the Cold War: if every regional power acquired nuclear weapons, the calculus of deterrence would fragment, accidental escalation risks would multiply, and NWS strategic stability would erode. The NPT was built as a bargain: NNWS renounce weapons (horizontal non-proliferation secured), and NWS promise to disarm (the quid pro quo, Article VI). This reading interprets the bargain as: NWS purchase NNWS non-proliferation commitment with an aspirational disarmament pledge, which is not binding and carries no enforcement.
% FOUNDING_PROBLEM_CORROBORATION: NWS governments attest the founding problem (proliferation risk among non-aligned states) was the historical driver and remains a live concern, especially regarding Iran, North Korea, and other threshold states. NNWS and disarmament advocates contest this reading of the problem's status, arguing the founding problem INCLUDES NWS disarmament as a binding reciprocal obligation that has NOT been met and is now overdue (over 50 years of alleged non-compliance). Independent analyses (International Court of Justice advisory opinions on nuclear weapons, Comprehensive Nuclear-Test-Ban Treaty Organization, disarmament research institutes) document deep disagreement on whether the founding problem's solution includes binding NWS disarmament. The ICJ's 1996 advisory opinion stated that NWS are obligated to pursue disarmament 'in good faith' but did not require specific arsenals reductions or timelines—a formulation that preserves the ambiguity this reading relies on.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint transfers strategic capacity asymmetrically: NWS retain arsenals while NNWS forgo them in perpetuity. The asymmetry is encoded in the treaty text itself, but this reading amplifies it by treating Article VI as non-binding aspiration. Suppression is substantial (0.68) because NNWS cannot exit without sanctions isolation and permanent security disadvantage (Article X withdrawal cost is prohibitive for most). Theater is moderate (0.42) and has risen over 58 years: the initial coordination function (preventing proliferation cascade during Cold War) was real, but as the founding problem has evolved (Cold War ended, proliferation did not cascade as feared, disarmament stalled indefinitely), a growing share of enforcement activity defends the asymmetry itself, not the original coordination goal. Accessibility collapse (0.58) reflects the regime's pervasive institutional structure: NNWS have limited technical or diplomatic paths to acquire weapons, verification is intrusive and continuous, and international isolation follows any attempt to weaponize. Resistance (0.71) is substantial because disarmament advocates, humanitarian organizations, many NNWS blocs, and Iran all contest this reading: Iran's position (disarmament is binding or I will pursue weapons anyway) is direct resistance to this reading's premise.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, the constraint is essential security architecture: NNWS renounce weapons, NWS maintain deterrence, proliferation is contained. From this reading, disarmament is aspirational because binding disarmament would require NWS arsenals to be eliminated—which NWS view as destabilizing and unilateral (Russia, China, others would not comply, leaving only Western NWS disarmed). From the NNWS seat, the constraint is asymmetric extraction: we renounce weapons permanently while you keep yours and offer no binding path to disarmament. The disarmament advocate seat reads the treaty as a broken bargain: the NWS bought NNWS non-proliferation with a disarmament promise that was never honored. The engine computes these divergences from the structural data: power asymmetry (institutional vs organized), exit options (arbitrage for NWS, identity-locked for NNWS), and beneficiary/victim declarations. The NWS agenda-setter seat and the NNWS payer seats should compute to different type assessments.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS directionality is near the beneficiary end (d~0.15-0.25): they set the rules, interpret the text, retain strategic capacity, and collect the benefit of NNWS renunciation without surrendering their own arsenals. Their exit options are arbitrage (they can withdraw, face some costs, but retain power and weapons; they can reinterpret the treaty in their favor). NNWS directionality is near the target end (d~0.80-0.90): they renounce weapons capacity forever, submit to intrusive inspections, and cannot exit without isolation. Identity-locked exit means withdrawal is not a real option for most NNWS—it means security abandonment. Threshold states (Iran, Pakistan precedents, Brazil pre-1990) have higher d than average NNWS (d~0.85) because they possess technical capacity and face higher stakes: the regime constrains them most directly. Disarmament advocates are excluded (d varies per seat, but their exclusion from decision-making is structural). IAEA carries d~0.70 on the enforcement side (they bear the labor cost and carry reputational stakes) but benefit modestly from institutional growth (d~0.40 on the coordination benefit side); the net is moderate to high d, making IAEA partially a payer, partially a beneficiary through institutional dependence on the regime.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids Tangled Rope mandatrophy by anchoring the coordination function in real historical problem-solving: preventing proliferation cascades among non-aligned states during Cold War was a genuine coordination problem that the NPT solved. The foundational mandate (horizontal non-proliferation verification) remains active. However, mandatrophy is latent and rising: disarmament, the explicit reciprocal obligation in the treaty, has been abandoned as a binding mandate—it is now purely aspirational. The theater_ratio measurement shows this progression: starting at 0.15 in 1968 (real coordination work on both axes), rising to 0.42 by 2026 (disarmament theater persists at Review Conferences but enforcement is nil; non-proliferation enforcement intensifies). The gap between founding mandate (disarmament binding) and current function (disarmament abandoned) is growing. If disarmament theater vanishes entirely—if NWS stop even pretending to pursue disarmament—the regime would expose itself as pure NNWS constraint without reciprocal NWS obligation, which would collapse NNWS consent. The regime persists by maintaining the fiction of binding disarmament commitment while never acting on it. This is the Piton transition risk: the regime could degrade into inertial maintenance if the founding mandate is abandoned openly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status,
    'Does Article VI''s ''at an early date'' language constitute a binding legal obligation on NWS to disarm, or merely a non-binding aspiration?',
    'International Court of Justice binding interpretation (contentious case, not advisory opinion), or amendment to the treaty clarifying the text. Alternatively, settlement of the treaty dispute through an authoritative NWS-NNWS negotiation that formally declares the binding status.',
    'If binding: the constraint becomes Snare (pure extraction under cover of broken reciprocal bargain); if aspirational (as this reading claims): the constraint is Tangled Rope (real coordination with asymmetric terms). The classification pivots entirely on this determination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_binding_status, conceptual, 'The legal force of Article VI disarmament language is the hinge the reading rests on.').

omega_variable(
    nws_interpretation_authority,
    'Who has the authority to interpret the NPT binding text—NWS exclusively (as this reading assumes), NNWS-inclusive bodies (as nnws_reading claims), or the treaty text''s plain language read by all parties equally?',
    'Formal amendment to the NPT establishing an authoritative interpretation body with NNWS representation and NWS non-veto; or a new binding protocol clarifying that treaty interpretation is NNWS-inclusive. The current situation (NWS de facto control of interpretation through IAEA Board, Security Council vetoes, Review Conference veto threat) is path-dependent and not formally codified.',
    'If NWS lose interpretive monopoly, this reading collapses and nnws_reading gains structural footing. The constraint would shift from NWS-beneficiary to NNWS-beneficiary or become a genuine Rope with symmetric terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_interpretation_authority, conceptual, 'Whether the reading''s authority claims are structurally entrenched or contingent on NWS institutional dominance.').

omega_variable(
    proliferation_cascade_falsification,
    'Did the NPT actually prevent proliferation cascades among non-aligned states (founding problem solved), or did proliferation happen anyway despite the treaty, indicating the founding mandate was never effective?',
    'Counterfactual historical analysis: if the NPT had not existed, would more countries have weaponized? Empirical research on threshold states (Brazil, Argentina, South Africa, Iraq, Iran, Libya, Syria) and their technology development paths. Expert assessment of whether treaty constraints or other factors (regional alliances, domestic politics, technology access limits) were the binding constraints on weaponization.',
    'If proliferation was prevented: the coordination mandate is vindicated and the regime''s persistence is justified. If proliferation happened anyway or was constrained by other factors: the founding mandate is partially failed, and the regime now functions as pure NNWS constraint without efficacy on the original problem, suggesting Piton degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_cascade_falsification, empirical, 'Whether the founding problem was solved by the treaty or by other factors, indicating whether the coordination function remains live.').

omega_variable(
    iaea_safeguards_asymmetry_intentionality,
    'Is the concentration of IAEA safeguards budget and inspection effort on NNWS (10:1+ inspection ratios vs. NWS) an intentional design choice of this reading, or a neutral technical consequence of having more NNWS to verify?',
    'Historical archive of IAEA Board decision-making on budget allocation (1968-present); NWS government statements on verification priorities; technical analysis of what symmetric inspection would require (NWS military arsenal accounting would be intrusive and difficult; is the asymmetry budget-neutral or by design?).',
    'If intentional: the reading deliberately maximizes NNWS extraction by concentrating verification resources; if technical artifact: the asymmetry is a side effect of scope (more NNWS to verify). The intentionality distinction affects whether the reading represents NWS strategy or structural accident.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iaea_safeguards_asymmetry_intentionality, empirical, 'Whether the verification asymmetry is a designed feature of this reading or a side effect of scope differences.').

omega_variable(
    identity_lock_reversibility,
    'Is NNWS identity-locked status (once non-nuclear, permanently non-nuclear) reversible through state choice, or is it structurally irreversible?',
    'Test case: if a current NNWS announced withdrawal and weaponization, what would the international response be? Could the state execute this choice, or would sanctions, military intervention, or regime collapse prevent it? North Korea''s 2003 withdrawal provides one precedent; Iran''s persistent threshold status provides another.',
    'If reversible through state choice: NNWS exit_options are better than identity_locked (they are constrained or trapped). If irreversible: identity_locked is accurate, and the payer position is even more extractive than claimed. The exit classification affects directionality and whether the constraint is truly a Tangled Rope or a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether NNWS identity-lock is a legal designation or a structural irreversibility, affecting the exit analysis.').

omega_variable(
    reading_kernel_contest_scope,
    'Is the contest between these three readings primarily a matter of different interpretations of the same underlying treaty (all parties reading the fixed text), or a contest over WHAT COUNTS as authoritative interpretation in the first place?',
    'Linguistic/textual analysis of Article II and Article VI plain language; historical intent reconstruction from drafting records; formal logic applied to the treaty''s conditional structure (what does non-proliferation condition NNWS receiving? What does disarmament condition NWS?).',
    'If primarily textual interpretation: the readings are all coherent readings of ambiguous text, and the classification as Tangled Rope vs. Snare depends on which reading you adopt. If primarily a contest over interpretive authority: the NWS reading wins not because it is more textually sound, but because NWS control the institutions that codify meaning. This affects whether the reading is a natural interpretation or an enforced one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest_scope, conceptual, 'The nature of the kernel contest itself: textual ambiguity or institutional power over meaning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1968, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__nws_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nws_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nws_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nws_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_text__nws_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(npt__tr_t2026, npt_treaty_text__nws_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__nws_reading, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nws_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nws_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nws_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_text__nws_reading, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement(npt__be_t2026, npt_treaty_text__nws_reading, base_extractiveness, 2026, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_text__nws_reading, suppression_requirement, 1968, 0.35).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__nws_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nws_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nws_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_text__nws_reading, suppression_requirement, 2020, 0.67).
narrative_ontology:measurement(npt__su_t2026, npt_treaty_text__nws_reading, suppression_requirement, 2026, 0.68).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1968, tn=2026
narrative_ontology:measurement(npt__grid_01, npt_treaty_text__nws_reading, accessibility_collapse(class), 1968, 0.35).
narrative_ontology:measurement(npt__grid_02, npt_treaty_text__nws_reading, accessibility_collapse(class), 2026, 0.55).
narrative_ontology:measurement(npt__grid_03, npt_treaty_text__nws_reading, accessibility_collapse(individual), 1968, 0.25).
narrative_ontology:measurement(npt__grid_04, npt_treaty_text__nws_reading, accessibility_collapse(individual), 2026, 0.48).
narrative_ontology:measurement(npt__grid_05, npt_treaty_text__nws_reading, accessibility_collapse(organizational), 1968, 0.38).
narrative_ontology:measurement(npt__grid_06, npt_treaty_text__nws_reading, accessibility_collapse(organizational), 2026, 0.62).
narrative_ontology:measurement(npt__grid_07, npt_treaty_text__nws_reading, accessibility_collapse(structural), 1968, 0.42).
narrative_ontology:measurement(npt__grid_08, npt_treaty_text__nws_reading, accessibility_collapse(structural), 2026, 0.68).
narrative_ontology:measurement(npt__grid_09, npt_treaty_text__nws_reading, resistance(class), 1968, 0.6).
narrative_ontology:measurement(npt__grid_10, npt_treaty_text__nws_reading, resistance(class), 2026, 0.78).
narrative_ontology:measurement(npt__grid_11, npt_treaty_text__nws_reading, resistance(individual), 1968, 0.48).
narrative_ontology:measurement(npt__grid_12, npt_treaty_text__nws_reading, resistance(individual), 2026, 0.7).
narrative_ontology:measurement(npt__grid_13, npt_treaty_text__nws_reading, resistance(organizational), 1968, 0.55).
narrative_ontology:measurement(npt__grid_14, npt_treaty_text__nws_reading, resistance(organizational), 2026, 0.73).
narrative_ontology:measurement(npt__grid_15, npt_treaty_text__nws_reading, resistance(structural), 1968, 0.5).
narrative_ontology:measurement(npt__grid_16, npt_treaty_text__nws_reading, resistance(structural), 2026, 0.65).
narrative_ontology:measurement(npt__grid_17, npt_treaty_text__nws_reading, stakes_inflation(class), 1968, 0.4).
narrative_ontology:measurement(npt__grid_18, npt_treaty_text__nws_reading, stakes_inflation(class), 2026, 0.6).
narrative_ontology:measurement(npt__grid_19, npt_treaty_text__nws_reading, stakes_inflation(individual), 1968, 0.3).
narrative_ontology:measurement(npt__grid_20, npt_treaty_text__nws_reading, stakes_inflation(individual), 2026, 0.52).
narrative_ontology:measurement(npt__grid_21, npt_treaty_text__nws_reading, stakes_inflation(organizational), 1968, 0.45).
narrative_ontology:measurement(npt__grid_22, npt_treaty_text__nws_reading, stakes_inflation(organizational), 2026, 0.68).
narrative_ontology:measurement(npt__grid_23, npt_treaty_text__nws_reading, stakes_inflation(structural), 1968, 0.5).
narrative_ontology:measurement(npt__grid_24, npt_treaty_text__nws_reading, stakes_inflation(structural), 2026, 0.72).
narrative_ontology:measurement(npt__grid_25, npt_treaty_text__nws_reading, suppression(class), 1968, 0.3).
narrative_ontology:measurement(npt__grid_26, npt_treaty_text__nws_reading, suppression(class), 2026, 0.65).
narrative_ontology:measurement(npt__grid_27, npt_treaty_text__nws_reading, suppression(individual), 1968, 0.2).
narrative_ontology:measurement(npt__grid_28, npt_treaty_text__nws_reading, suppression(individual), 2026, 0.62).
narrative_ontology:measurement(npt__grid_29, npt_treaty_text__nws_reading, suppression(organizational), 1968, 0.35).
narrative_ontology:measurement(npt__grid_30, npt_treaty_text__nws_reading, suppression(organizational), 2026, 0.68).
narrative_ontology:measurement(npt__grid_31, npt_treaty_text__nws_reading, suppression(structural), 1968, 0.4).
narrative_ontology:measurement(npt__grid_32, npt_treaty_text__nws_reading, suppression(structural), 2026, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nws_reading, 0.18).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, iaea_safeguards_asymmetry).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, un_security_council_veto_proliferation).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, fuel_supply_control_constraint).

% DUAL FORMULATION NOTE:
% The NPT kernel (treaty text and authority structure) is read three ways: nws_reading (this constraint, NWS-beneficiary) treats non-proliferation as binding and disarmament as aspirational. nnws_reading (sibling) inverts the priority. withdrawal_threshold_reading (sibling) focuses on Article X interpretation. These are not one constraint viewed from different angles—they are three distinct constraints arising from three incompatible readings of the same kernel. Each has its own epsilon, beneficiary/victim structure, and legal standing. The epsilon values differ substantially: nws_reading has high extractiveness (0.72, asymmetric), nnws_reading would have lower extractiveness (disarmament binding reduces NWS benefit), withdrawal_threshold_reading has different extractiveness (depends on whether high or low threshold). The framework model these as three stories linked via network.affects_constraints, not as one story with a measurement parameter. The kernel contest is real: no single NWS or NNWS party can hold both nws_reading and nnws_reading simultaneously as true, although different parties do hold different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nws_reading, institutional, 0.22).
constraint_indexing:directionality_override(npt_treaty_text__nws_reading, organized, 0.84).
constraint_indexing:directionality_override(npt_treaty_text__nws_reading, moderate, 0.76).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
