% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA Binding Multilateral Treaty Constraint (Multilateral Reading)
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA (Joint Comprehensive Plan of Action, 2015) is a multilateral
 *   treaty constraining Iran's nuclear program in exchange for sanctions
 *   relief. The binding multilateral reading instantiates one contested
 *   interpretation of the agreement: as a legally binding multilateral
 *   covenant that requires consensus for modification or dissolution, where
 *   unilateral withdrawal carries reputational cost and snapback sanctions
 *   require UNSC verification and consensus. This reading emphasizes the
 *   treaty as a commitment device that constrains all signatories equally —
 *   most significantly, constraining U.S. unilateral action. The constraint's
 *   claimed type is tangled rope: the agreement genuinely coordinates
 *   multilateral nonproliferation verification (coordination function), but
 *   it asymmetrically distributes enforcement authority (veto power to UNSC
 *   members, exclusion to regional actors), and it persists through active
 *   enforcement of the consensus requirement against unilateral defection.
 *   The claim/metric gap is deliberate: the constraint is claimed as tangled
 *   rope (a hybrid coordination-extraction arrangement); the metrics reflect
 *   substantially extractive operation at 0.68 extractiveness, where the
 *   extraction emerges from veto-point consolidation and the constraint's
 *   rigidity in the face of Iranian technical violations. The measurement
 *   series track the constraint's enforcement intensification over the
 *   observed interval: extractiveness rises as contested compliance
 *   assessments accumulate, theater increases as performative
 *   compliance-review sessions substitute for binding remedial action, and
 *   suppression rises as the consensus mechanism hardens into a de facto
 *   enforcement veto.
 *
 * KEY AGENTS:
 *   - United States: Signatory with maximum withdrawal authority under its constitutional reading; constrained by the binding reading's consensus requirement; highest incentive to escape the arrangement's binding character
 *   - Iran: Obligated party with identity-locked exit (nuclear isolation if exit); faces diffuse enforcement authority and graduated dispute resolution rather than automatic sanctions
 *   - Russia and China: UNSC veto-holders; benefit from veto authority over sanctions; have geopolitical incentive to exercise veto (sanctions relief for Iran, regime stability)
 *   - European signatories (U.K., France, Germany): Benefit from consensus requirement preventing U.S. unilateral action; constrained by veto-dependent enforcement
 *   - IAEA: Technical inspector; verification authority; subordinated to multilateral dispute resolution
 *   - Regional actors (Israel, Gulf states): Excluded from formal dispute resolution; bear highest regional security risk; trapped in the arrangement's structural logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.68).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.62).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA Binding Multilateral Treaty Constraint (Multilateral Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '9404c52c-3273-4e50-adfc-727c8b058c9c').
narrative_ontology:cs_kernel_codification('9404c52c-3273-4e50-adfc-727c8b058c9c', fixed_text).
narrative_ontology:cs_authority_grounding('9404c52c-3273-4e50-adfc-727c8b058c9c', lineage).
narrative_ontology:cs_interpretation_layer_present('9404c52c-3273-4e50-adfc-727c8b058c9c').
narrative_ontology:cs_reading_relation('9404c52c-3273-4e50-adfc-727c8b058c9c', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('9404c52c-3273-4e50-adfc-727c8b058c9c', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('9404c52c-3273-4e50-adfc-727c8b058c9c', foundational, multilateral_consensus_modification_binding).
narrative_ontology:cs_axiom_status(multilateral_consensus_modification_binding, holdable).
narrative_ontology:cs_axiom_grounding('9404c52c-3273-4e50-adfc-727c8b058c9c', multilateral_consensus_modification_binding, conventional).
narrative_ontology:cs_axiom('9404c52c-3273-4e50-adfc-727c8b058c9c', foundational, unilateral_withdrawal_violates_covenant_law).
narrative_ontology:cs_axiom_status(unilateral_withdrawal_violates_covenant_law, holdable).
narrative_ontology:cs_axiom_grounding('9404c52c-3273-4e50-adfc-727c8b058c9c', unilateral_withdrawal_violates_covenant_law, deontological).
narrative_ontology:cs_reference_frame('9404c52c-3273-4e50-adfc-727c8b058c9c', multilateral_legally_binding_covenant).
narrative_ontology:cs_drift_state('9404c52c-3273-4e50-adfc-727c8b058c9c', post_2018_us_withdrawal_context, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9404c52c-3273-4e50-adfc-727c8b058c9c', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_institution_architecture).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime_stability).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, unilateral_state_withdrawal_authority).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, rapid_sanctions_reimposition_capability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, european_signatories_uk_france_germany).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, european_signatories_uk_france_germany).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A signatory with maximum structural withdrawal authority under its own constitutional reading (executive agreement framework, not Senate-ratified treaty). Under the binding multilateral reading, unilateral withdrawal is constrained by reputational cost and UNSC consensus requirements for sanctions reimposition. The constraint's enforcement machinery specifically targets U.S. unilateral capability — the veto-point structure prevents the U.S. from moving unilaterally to sanctions escalation. The U.S. bears the cost of consensus navigation and potential veto obstruction by Russia and China. The 2018 withdrawal attempt demonstrated that despite signing, the U.S. can adopt the competing transactional_provisional_reading, but doing so incurs diplomatic cost and UNSC resistance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states, payer).

% Obligated to maintain uranium enrichment below 3.65% (for reactor fuel) and below 300 kg of low-enriched uranium stockpile under the binding reading. Constrained to allow IAEA inspectors continuous access to declared nuclear sites. In exchange, Iran receives sanctions relief and international legitimacy for its civilian nuclear program. The identity-lock status derives from Iran's position as a non-aligned nuclear aspirant — exit from the arrangement means loss of sanctions relief AND loss of the legitimacy framework that distinguishes Iran's program as civilian rather than weapons-oriented. Iran is trapped between enrichment constraint and economic isolation. Under the binding reading, Iran cannot be unilaterally sanctioned for violations without UNSC consensus; technical breaches (exceeding enrichment caps) trigger graduated dispute resolution rather than automatic snapback. Iran has violated the 300 kg stockpile cap and exceeded 3.65% enrichment since 2019 (in response to U.S. withdrawal and European failure to provide promised economic benefits), but formal snapback has been blocked by UNSC veto.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, payer,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, beneficiary).

% Hold veto authority over snapback sanctions and any formal amendment to the agreement under the binding multilateral reading. Russia and China benefit from sanctions relief for Iran (geopolitical leverage, economic opportunity) and have consistently exercised veto to block snapback proposals. Their arbitrage exit option derives from their ability to switch alignment — they can leave the agreement entirely (losing UNSC veto over its operation) or they can continue as signatories while obstructing enforcement (preserving veto leverage). They have effectively chosen the latter, converting veto authority into geopolitical rent collection. The constraint's binding character is maintained precisely because Russia and China exercise veto to prevent unilateral U.S. enforcement; their obstruction proves the multilateral binding is operative.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, permanent_unsc_members_russia_china, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the binding reading's consensus requirement, which prevents U.S. unilateral action that might overturn the agreement. They bear the cost of veto-dependent enforcement — when Russia and China block snapback, European signatories cannot act unilaterally without violating the binding multilateral framework. They have attempted to preserve the agreement through the Instrument in Support of Trade Exchanges (INSTEX), a financial mechanism to circumvent U.S. sanctions, but this has been ineffectual against U.S. secondary sanctions. European signatories extract legitimacy and institutional participation from the binding reading; they are constrained by the consensus requirement's rigidity when that consensus fails.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, european_signatories_uk_france_germany, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, european_signatories_uk_france_germany, payer).

% Technical inspector tasked with verifying Iranian compliance. The IAEA's role is to report factually on enrichment levels, stockpile measurements, and access to declared sites. Under the binding multilateral reading, IAEA findings feed a dispute-resolution process; they do not automatically trigger sanctions. The IAEA has documented Iranian violations (exceeding enrichment caps, exceeding stockpile limits) since 2019, but these findings have not translated into enforcement because UNSC consensus is blocked. The IAEA carries no enforcement authority — verification is subordinated to multilateral dispute resolution.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea, observer,
    institutional, generational, analytical, global).

% Are excluded from formal JCPOA dispute resolution and enforcement architecture despite bearing the highest regional security risk from Iranian nuclear escalation. Israel has conducted strikes on Iranian nuclear facilities and maintains a stated policy of preventing Iranian nuclear weapons; Gulf states (Saudi Arabia, UAE, etc.) face direct ballistic missile threat from Iran. Under the binding multilateral reading, their exclusion is structural — the JCPOA is a multilateral state agreement, not a regional security arrangement. They would argue for lower enrichment thresholds, faster dispute resolution, and explicit provisions linking Iranian violations to regional deterrence escalation. Instead, they are trapped in a framework they cannot influence, which forces them toward independent deterrence postures (Israeli strikes, Gulf ballistic missile acceleration) outside the constraint's scope. Their exclusion is both a feature (keeps the constraint multilateral rather than U.S.-regional) and an extractive mechanism (binds regional powers to multilateral outcomes that leave them strategically vulnerable).
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_actors_israel_gulf_states, excluded,
    powerful, generational, trapped, regional).

% The binding multilateral reading benefits the abstraction 'multilateral institutional order' — the norm that great-power coalitions should coordinate through consensus-based frameworks rather than unilateral action. This is not an actor; it is a vindicated proposition. The JCPOA's binding character instantiates the principle that nuclear treaties can constrain all signatories equally through multilateral consensus. The constraint's persistence vindicates the multilateral consensus doctrine even when enforcement is paralyzed.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_institution_architecture, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_institution_architecture).

% The binding multilateral reading benefits the nonproliferation regime itself — the international legal and institutional framework that treats nuclear weapons as collectively constrained, subject to verification, and subject to multilateral enforcement. Even when the JCPOA's enforcement is paralyzed, the binding reading maintains the regime's structural legitimacy: Iran remains formally obligated to constraints (IAEA verification continues, enrichment caps are acknowledged); signatories remain formally committed to the agreement. The regime persists in attenuated form, which is better for regime stability than its wholesale collapse.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime_stability, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime_stability).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_institution_architecture).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates verification of Iranian nuclear compliance through a unified IAEA inspection protocol; coordinates sanctions relief against verifiable constraint compliance; coordinates multilateral enforcement through UNSC consensus rather than unilateral decisionmaking — solves the problem of how to bind a nuclear aspirant state without imposing terms that demand unilateral submission to a single power's judgment.
% TRANSFER_FUNCTION: Transfers Iranian uranium enrichment capacity (from weapons-grade weaponizable levels to civil reactor levels) to the IAEA and the signatories as a collective. Transfers veto authority over sanctions reimposition to UNSC permanent members (Russia, China), away from U.S. unilateral action. Transfers geopolitical leverage from regional actors (who cannot formally object) to multilateral institutions.
% ABSENT_VOICES: Regional security actors (Israel, Gulf states) who bear the highest material risk from Iranian violations are excluded from formal dispute resolution; they would argue for lower enforcement thresholds and faster response mechanisms. Non-nuclear-armed powers would argue for stronger enforcement guarantees than consensus allows. Iran would argue for lower enrichment caps and shorter sunshine clauses.
% DISAPPEARANCE_RATIONALE: If the binding multilateral reading's consensus requirement disappeared and replaced it with unilateral U.S. authority to reimpose sanctions, Iran would either resume weapons-grade enrichment (concluding the constraint is illusory) or accelerate to breakout, UNSC permanent members would lose veto leverage and would potentially withdraw from enforcement cooperation, and the architecture would shift from a binding multilateral treaty to a provisional transactional framework dependent on U.S. enforcement will. The agreement's actual operation would reorganize around the distribution of enforcement authority.
% FOUNDING_PROBLEM: The founding problem is the bilateral credibility trap: how to commit a nuclear aspirant state (Iran) to verifiable constraints without granting unilateral veto to any single power (the U.S.), while simultaneously committing the great powers (UNSC members) to honor the agreement and not exploit it for regime change once compliance is demonstrated.
% FOUNDING_PROBLEM_CORROBORATION: All original signatories (U.K., France, Germany, Russia, China, EU) attested to this problem during negotiations (recorded in JCPOA preamble and technical annexes). The IAEA attested to the verification challenge. Iran attested to the credibility problem in its initial demands for reciprocal commitment structures. Independent scholarship on multilateral treaty design confirms the problem is live wherever nuclear aspirant states negotiate with great-power coalitions. The problem persists as a structural feature of any nuclear diplomacy framework.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the constraint binds unilateral withdrawal authority and creates structural veto-points that extract rents from the signatories in the form of delayed response, paralyzed enforcement, and geopolitical leverage concentration among UNSC permanent members. Suppression is 0.62 because the constraint's persistence depends on active enforcement of the consensus requirement (the consensus mechanism itself must be maintained as binding) and exclusion of alternative interpretation pathways (the competing readings — transactional_provisional_reading, graduated_compliance_reading — must be suppressed for the binding reading to hold). Theater_ratio is moderate (0.28) because the constraint includes genuine coordination (IAEA verification, multilateral inspection protocols) but increasingly supplements real enforcement with performative compliance-review sessions that produce no binding remedial action. Accessibility_collapse is 0.71 because once the binding multilateral reading is adopted, alternatives to consensus-based modification are effectively foreclosed within the framework (unilateral withdrawal carries reputational cost; unilateral sanctions reimposition violates the reading; exit becomes identity-locked for Iran). Resistance is 0.58 because substantial resistance exists: the U.S. administration adopted the transactional_provisional_reading (authorizing unilateral withdrawal in 2018); Iran periodically violates enrichment caps; regional actors resist exclusion and argue for faster enforcement thresholds.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. seat and the UNSC veto-holder seats experience sharply different constraint types from this same arrangement. From the U.S. perspective, the binding reading is a snare: it binds U.S. action through veto-point navigation, it extracts compliance from Iran through consensus mechanisms the U.S. cannot unilaterally trigger, and it distributes enforcement authority away from the actor with the greatest capacity to enforce. From the Russian and Chinese seats, the arrangement is a rope: it coordinates multilateral nonproliferation verification, it legitimates their veto authority, and it produces geopolitical leverage. The engine computes these divergent types from the structural data (power atom, exit options, beneficiary/victim position) without requiring reconciliation at the story level. The narrative explains the divergence; the metrics feed the computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from the constraint's asymmetric distribution of benefits and costs. The U.S. is near the target end (d ≈ 0.75) because the binding reading constrains its maximum structural power (unilateral action); the veto authority concentration among UNSC permanent members carries d downward for those actors (d ≈ 0.35 for Russia and China), as they gain leverage. Iran sits near symmetric (d ≈ 0.50) — genuine coordination benefit (verification legitimacy, sanctions relief tied to compliance) balanced against diffuse enforcement risk and enrichment constraint. European signatories sit slightly beneficiary-ward (d ≈ 0.40) because the consensus requirement prevents U.S. unilateral overruling, but they bear the cost of veto-dependent enforcement. Regional actors are effectively excluded, so directionality is inapplicable (they are not seats in the constraint's operative framework, despite bearing the highest material risk). The IAEA is observer-positioned (d ≈ 0.0) — verification authority without enforcement veto.
 *
 * MANDATROPHY ANALYSIS:
 *   The binding multilateral reading contains an incipient mandatrophy conflict: the founding problem (credibility trap requiring consensus-based binding) remains live, but the constraint's function has degraded into veto-dependent paralysis. Iran continues enrichment violations; the U.S. has adopted the competing transactional_provisional_reading and withdrawn; Russia and China have exercised veto to prevent sanctions escalation; and European signatories are trapped in a consensus framework that cannot enforce against any of the actors that actually matter. The constraint persists not because it solves the original credibility problem but because multilateral institutions maintain the formal structure theatrically (compliance reviews, technical working groups) while actual enforcement authority is diffused away. The mandatrophy is not yet resolved — the constraint is still invoked (Iran claims binding status, signatories cite it in diplomatic discourse) — but the divergence between claimed function (binding multilateral constraint) and actual operation (a framework whose enforcement is paralyzed by consensus requirements it cannot overcome) indicates mandatrophy is live and deepening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_provisional_contest,
    'Is the JCPOA a binding legal covenant requiring consensus modification, or a provisional transactional framework voidable on unilateral determination of breach?',
    'International Court of Justice advisory opinion on the JCPOA''s legal character under Vienna Convention on the Law of Treaties; examination of negotiation records for intent regarding amendment procedures and withdrawal conditions.',
    'If binding-multilateral is established, unilateral U.S. withdrawal violated treaty law and sanctions reimposition requires UNSC consensus (shifts d upward for U.S., downward for UNSC veto-holders). If provisional-transactional is established, unilateral withdrawal is legal and snapback is immediate (shifts d downward for U.S., upward for Iran). The classification diverges fundamentally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binding_vs_provisional_contest, conceptual, 'Whether the JCPOA''s legal binding character is multilaterally codified or provisionally conditional on transactional good faith assessment.').

omega_variable(
    consensus_enforcement_viability,
    'Can a consensus-based enforcement mechanism credibly deter Iranian enrichment violations when UNSC veto-holders have geopolitical incentive to exercise veto against sanctions escalation?',
    'Empirical test: if Iran violates enrichment caps and Russia/China exercise veto to prevent snapback, the mechanism''s viability is falsified. Counterfactual: if a mechanism requiring consensus among actors with conflicting interests could never operate as designed, the constraint''s claimed coordination function is theatrical.',
    'If viability is falsified, the constraint''s extractiveness increases (veto-dependent paralysis becomes the operative feature) and theater_ratio rises sharply (compliance reviews substitute for binding remedial action). Classification might shift from tangled_rope (hybrid coordination-extraction) toward piton (atrophied function maintained theatrically).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_enforcement_viability, empirical, 'Whether consensus-based multilateral enforcement can operate against signatories with veto authority and geopolitical interest in Iranian sanctions relief.').

omega_variable(
    regional_exclusion_sustainability,
    'Can a multilateral nuclear treaty credibly constrain regional nuclear aspirants (Iran) when the actors bearing the highest regional security risk (Israel, Gulf states) are excluded from formal enforcement authority?',
    'Empirical: if regional actors pursue independent deterrence escalation (e.g., Israeli strikes, Gulf ballistic missile acceleration) because they lack confidence in multilateral enforcement, the exclusion structure fails. Counterfactual: if the exclusion is structural to the constraint''s viability (regional veto would paralyze consensus further), the constraint extracts stability from regional powers by locking them into multilateral frameworks they cannot govern.',
    'If regional exclusion fails, the constraint''s accessibility_collapse decreases (alternatives to Iranian nuclearization via regional deterrence become salient) and resistance increases (regional actors move to unilateral deterrence). If regional exclusion is structurally necessary, it is an extractive feature, not a failure — the constraint binds regional actors to multilateral outcomes they oppose, which is pure extraction from their perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_exclusion_sustainability, empirical, 'Whether the exclusion of regional actors from enforcement is a structural necessity or a credibility failure of multilateral constraint.').

omega_variable(
    identity_lock_mechanism_interpersonal_dynamics,
    'For Iran, is the identity-lock status (exit_options: identity_locked) primarily structural (economic dependency, nuclear isolation) or internalized (revolutionary ideological commitment to nuclear independence as state identity)?',
    'Post-constraint counterfactual: if Iran gained access to nuclear capacity without constraint abandonment (e.g., via technological breakthrough), would Iran abandon the constraint voluntarily? If internalized, Iran would maintain constraint compliance out of ideological commitment; if structural, Iran would exit to pursue nuclear sovereignty.',
    'If primarily internalized, the constraint''s suppression is lower than measured (Iran suppresses exit autonomously); if primarily structural, the measured suppression is accurate (Iran carries the suppression with them, external barriers). If internalized, Iran could be identity-deconstructed through regime change or ideological evolution, destabilizing the constraint; if structural, Iran remains locked regardless of ideology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal_dynamics, empirical, 'Whether Iran''s exit constraint is structurally imposed (economic/security dependency) or identity-fused (ideological commitment to nuclear independence).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_binding_multilateral_tr_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(jcpoa_binding_multilateral_tr_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 3, 0.21).
narrative_ontology:measurement(jcpoa_binding_multilateral_tr_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(jcpoa_binding_multilateral_tr_t12, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(jcpoa_binding_multilateral_tr_t18, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(jcpoa_binding_multilateral_tr_t25, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(jcpoa_binding_multilateral_be_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(jcpoa_binding_multilateral_be_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(jcpoa_binding_multilateral_be_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(jcpoa_binding_multilateral_be_t12, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(jcpoa_binding_multilateral_be_t18, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(jcpoa_binding_multilateral_be_t25, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_binding_multilateral_su_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(jcpoa_binding_multilateral_su_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 3, 0.57).
narrative_ontology:measurement(jcpoa_binding_multilateral_su_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(jcpoa_binding_multilateral_su_t12, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(jcpoa_binding_multilateral_su_t18, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(jcpoa_binding_multilateral_su_t25, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_consensus_requirement_nuclear_verification).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_enrichment_cap_constraint).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, sanctions_snapback_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the binding_multilateral_reading of the kernel jcpoa_treaty_bindingness. Sibling readings (transactional_provisional_reading, graduated_compliance_reading) are separate constraint stories with different ε values, beneficiary/victim structures, and accessibility_collapse profiles. All three readings reference the same JCPOA text but instantiate structurally distinct constraints. The divergence is not measurement ambiguity — it is genuinely different commitments the signatories hold simultaneously about what the agreement binds them to. Network links record that each reading's persistence affects the others' operative scope: if the binding_multilateral reading holds, the transactional_provisional_reading is foreclosed as a unilateral option; if the transactional_provisional_reading is adopted, the binding_multilateral reading's legitimacy erodes. The constraint family is linked via affects_constraints as a Markov chain of competing institutional readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__binding_multilateral_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
