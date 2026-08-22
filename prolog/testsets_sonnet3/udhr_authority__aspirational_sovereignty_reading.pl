% ============================================================================
% CONSTRAINT STORY: udhr_authority__aspirational_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__aspirational_sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: udhr_authority__aspirational_sovereignty_reading
 *   human_readable: UDHR as Aspirational Moral Guidance Requiring State Consent (Sovereigntist Reading)
 *   domain: international_law/political_philosophy/human_rights_doctrine
 *
 * SUMMARY:
 *   This constraint models the sovereigntist reading of the UDHR's authority:
 *   the document is a moral and aspirational text adopted by General Assembly
 *   resolution, not a treaty, and therefore does not bind states absent their
 *   own subsequent consent through ratification of covenants such as the
 *   ICCPR and ICESCR. On this reading, international tribunals have no
 *   coercive jurisdiction over non-consenting states, and individuals harmed
 *   by rights violations in such states have no enforceable international
 *   forum — their remedy, if any, runs through domestic law or diplomatic
 *   pressure. This is a low-extraction, low-suppression reading precisely
 *   because it denies the UDHR any binding force to extract compliance with
 *   in the first place; what extraction exists is diffuse and reputational
 *   (states that ignore the UDHR's aspirations pay soft costs in
 *   international standing) rather than coercive. This story is ONE of three
 *   linked readings of the same kernel (udhr_authority): the
 *   binding_universalism_reading claims the UDHR creates justiciable rights
 *   regardless of consent (high extraction on non-complying states, low
 *   extraction on rights-holders); the customary_emergence_reading claims the
 *   UDHR has evolved into binding custom through state practice and opinio
 *   juris (a moderate, rising-extraction middle path). Each reading is
 *   authored as its own constraint with its own stable epsilon; this file
 *   does not average across them or hedge between them.
 *
 * KEY AGENTS:
 *   - sovereign_states: agenda_setter (institutional/arbitrage) — control ratification and reservation decisions that determine binding force
 *   - non_ratifying_states: beneficiary (institutional/arbitrage) — shielded from coercive obligation by the consent requirement
 *   - individual_rights_claimants: payer (powerless/trapped) — bear the cost of the absent enforceable forum
 *   - persecuted_minorities_without_treaty_recourse: payer (powerless/trapped) — live inside the gap between aspiration and remedy
 *   - international_tribunals: excluded (institutional/constrained) — denied coercive jurisdiction absent consent
 *   - human_rights_scholars: observer (analytical/analytical) — debate the kernel's true structural status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__aspirational_sovereignty_reading, 0.18).
domain_priors:suppression_score(udhr_authority__aspirational_sovereignty_reading, 0.12).
domain_priors:theater_ratio(udhr_authority__aspirational_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__aspirational_sovereignty_reading, rope).
narrative_ontology:human_readable(udhr_authority__aspirational_sovereignty_reading, "UDHR as Aspirational Moral Guidance Requiring State Consent (Sovereigntist Reading)").
narrative_ontology:topic_domain(udhr_authority__aspirational_sovereignty_reading, "international_law/political_philosophy/human_rights_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__aspirational_sovereignty_reading, 'bb371213-5fdc-42f9-a5b0-a10694300aa4').
narrative_ontology:cs_kernel_codification('bb371213-5fdc-42f9-a5b0-a10694300aa4', fixed_text).
narrative_ontology:cs_authority_grounding('bb371213-5fdc-42f9-a5b0-a10694300aa4', distributed).
narrative_ontology:cs_reading_relation('bb371213-5fdc-42f9-a5b0-a10694300aa4', udhr_authority__binding_universalism_reading, forecloses).
narrative_ontology:cs_reading_relation('bb371213-5fdc-42f9-a5b0-a10694300aa4', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('bb371213-5fdc-42f9-a5b0-a10694300aa4', foundational, state_consent_is_necessary_condition_for_binding_obligation).
narrative_ontology:cs_axiom_status(state_consent_is_necessary_condition_for_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('bb371213-5fdc-42f9-a5b0-a10694300aa4', state_consent_is_necessary_condition_for_binding_obligation, conventional).
narrative_ontology:cs_axiom('bb371213-5fdc-42f9-a5b0-a10694300aa4', secondary, general_assembly_resolutions_lack_treaty_force_absent_ratification).
narrative_ontology:cs_axiom_status(general_assembly_resolutions_lack_treaty_force_absent_ratification, holdable).
narrative_ontology:cs_axiom_grounding('bb371213-5fdc-42f9-a5b0-a10694300aa4', general_assembly_resolutions_lack_treaty_force_absent_ratification, conventional).
narrative_ontology:cs_reference_frame('bb371213-5fdc-42f9-a5b0-a10694300aa4', id_1948_declaratory_non_treaty_status).
narrative_ontology:cs_drift_state('bb371213-5fdc-42f9-a5b0-a10694300aa4', contemporary_near_universal_ratification_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bb371213-5fdc-42f9-a5b0-a10694300aa4', '').
narrative_ontology:cs_kernel_id(udhr_authority__aspirational_sovereignty_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, non_ratifying_states).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, diplomatic_negotiators).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, individual_rights_claimants).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, persecuted_minorities_without_treaty_recourse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Treat the UDHR as a non-binding declaration of shared moral aspiration adopted by the UN General Assembly in 1948 without treaty force. Retain full discretion over whether to ratify subsequent covenants (ICCPR, ICESCR) that would create binding obligations, and can enter reservations or decline ratification entirely. Cite the UDHR rhetorically while resisting any reading that would make its provisions enforceable against them absent explicit consent.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Have declined to ratify one or more binding human rights covenants, or have ratified with extensive reservations. Under this reading, the UDHR's moral force does not convert into legal exposure; they face reputational costs but no adjudicable liability, and no tribunal can compel them absent their own prior consent.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, non_ratifying_states, beneficiary,
    institutional, generational, arbitrage, national).

% Use the UDHR's aspirational status as negotiating room: they can invoke its language for legitimacy while preserving flexibility to trade away specific commitments in treaty negotiations. The consent requirement is the lever that lets them shape binding text before it applies to their state.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, diplomatic_negotiators, beneficiary,
    powerful, biographical, mobile, global).

% Individuals suffering rights violations in states that have not ratified relevant covenants, or that have entered reservations covering the violated right. Under this reading they have no standing before international tribunals — the UDHR's language offers moral vindication but no forum with coercive jurisdiction, since the tribunal's authority depends on the violating state's own prior consent.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, individual_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Groups facing systematic rights violations within states that treat the UDHR as non-binding. They bear the practical cost of the consent requirement directly: the gap between the UDHR's stated protections and any enforceable remedy is a gap they live inside, with exit blocked by the same sovereignty the reading protects.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, persecuted_minorities_without_treaty_recourse, payer,
    powerless, biographical, trapped, national).

% Bodies such as the ICJ or UN treaty committees that might otherwise adjudicate UDHR-based claims. Under this reading their jurisdiction is contingent entirely on state ratification and consent to jurisdiction; against a non-consenting state they have no coercive authority and can issue only non-binding findings or moral condemnation.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, international_tribunals, excluded,
    institutional, generational, constrained, global).

% Debate whether the UDHR's status has shifted from aspiration to binding custom. This reading represents the sovereigntist pole of that debate — scholars in this camp emphasize the drafting history (a General Assembly resolution, not a treaty) and consistent state practice of treating ratification as the operative legal threshold.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, human_rights_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_authority__aspirational_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_authority__aspirational_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared moral vocabulary and aspirational benchmark that states can invoke in diplomacy and domestic legitimation without surrendering sovereign control over which obligations bind them — coordinating around common language while preserving decentralized consent as the mechanism for converting aspiration into law.
% TRANSFER_FUNCTION: Moves rhetorical and reputational currency (legitimacy, moral standing) toward states that can claim UDHR-aligned values, while withholding enforceable remedy from individuals harmed by states that decline to ratify binding instruments — the transfer is from claimants' access to a forum toward states' retained discretion.
% ABSENT_VOICES: Individual rights claimants and persecuted minorities have no seat in the intergovernmental process that determines which covenants get ratified or how reservations are drafted; their interests are represented, if at all, by NGOs and treaty-body shadow reports, not by direct participation in the consent decision.
% DISAPPEARANCE_RATIONALE: If the sovereigntist reading of the UDHR's authority disappeared overnight (i.e., if consent-based limits were simply abandoned), sovereign states would lose their primary shield against externally imposed obligations, and diplomatic negotiation dynamics around treaty ratification would be transformed. Whether the world 'rearranges' or stays the same is exactly the kernel's contest: sovereigntists say the consent requirement is what keeps the system stable and functioning at all; universalist readers say removing it would simply align practice with rights that already exist morally.
% FOUNDING_PROBLEM: In 1948, no consensus existed among UN member states — spanning colonial powers, newly independent states, and different legal and religious traditions — for a binding international rights instrument with coercive enforcement. The UDHR was drafted as a declaration precisely because treaty-level obligation was unachievable at the time; the consent-based architecture solved the problem of getting near-universal adoption of a shared moral text without requiring immediate legal submission.
% FOUNDING_PROBLEM_CORROBORATION: The drafting history itself (Eleanor Roosevelt's Human Rights Commission records, and the explicit decision to pursue a non-binding resolution rather than a treaty in 1948) corroborates that the founding problem was real and that consent-based non-bindingness was the deliberate solution. However, human rights scholars and treaty-body practice outside any single state's own justification increasingly attest that the founding problem (lack of consensus) has been substantially resolved through seven decades of near-universal ratification of core covenants — meaning the sovereigntist reading's founding rationale is, by that outside corroboration, largely obsolete even though states asserting it continue to rely on it.
narrative_ontology:disappearance_verdict(udhr_authority__aspirational_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(udhr_authority__aspirational_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__aspirational_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_authority__aspirational_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__aspirational_sovereignty_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__aspirational_sovereignty_reading_tests).
:- end_tests(udhr_authority__aspirational_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at 2024) because this reading, by its own structural logic, denies the UDHR any binding force to extract with — the cost borne by rights claimants is an absence of remedy rather than an active transfer captured by an identifiable extractor, and it drifts only slightly upward over the interval as the diplomatic-reputational cost of ignoring the UDHR has modestly increased. Suppression is authored low (0.12) because the reading imposes no coercive machinery on states; the only 'suppression' is soft reputational pressure. Theater ratio is authored moderate (0.4, rising slightly to 0.4 by 2024) because significant diplomatic and rhetorical activity invokes the UDHR ceremonially in venues where it produces no binding legal effect under this reading — states cite it in speeches and resolutions while the actual work of obligation happens in the separate, opt-in covenant-ratification track. Accessibility collapse is low (0.2): alternative frameworks (regional human rights courts, domestic constitutional protections, ad hoc political pressure) remain fully available and are not foreclosed by this reading. Resistance is moderate (0.35): this reading is actively contested by universalist scholars, some treaty bodies, and rights claimants who argue the consent gate produces unconscionable outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and non-ratifying states sit near the beneficiary end: the consent requirement is the very thing that protects their discretion, and their exit options are effectively arbitrage-grade (they can invoke the UDHR's moral authority selectively while declining its legal consequences). Individual rights claimants and persecuted minorities sit near the full-target end: they are powerless, trapped within domestic jurisdictions, and bear the cost of the enforcement gap this reading declares structurally necessary. Diplomatic negotiators are a secondary beneficiary class — not the ones directly extracting, but professionally advantaged by the flexibility the reading preserves. International tribunals are structurally excluded rather than coordinated or extracted from: their absence from binding authority is the reading's central architectural feature, not an oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of 1948 consensus for binding law) is authored as contested/largely resolved by outside corroboration, while the arrangement (consent-gated authority) persists and is actively defended by the states it benefits. This is the classic mandatrophy signature: a scaffold-shaped justification (interim solution while consensus builds) sustained past the point its own proponents' evidence suggests the underlying problem has been substantially addressed by seven decades of near-universal covenant ratification. The classification here is claimed as rope (genuine coordination value in preserving a workable, near-universal moral text) rather than snare, because the reading does not suppress alternative avenues (regional courts, domestic law) and the cost to rights claimants, while real, is diffuse rather than actively engineered — the engine's computed type from the authored metrics is the actual test of whether this rope classification holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the UDHR''s authority genuinely indeterminate between the sovereigntist, universalist, and customary-emergence readings, or has state practice since 1948 settled the question in favor of one reading in a way this story''s low-epsilon assessment understates?',
    'Track ICJ and domestic court citations of the UDHR as evidence of opinio juris over time; a rising trend of courts treating specific UDHR provisions as customary law independent of ratification would favor the customary_emergence_reading over this one.',
    'If customary emergence has substantially displaced the consent requirement in practice, this reading''s claimed low extractiveness understates the actual binding pressure states face, and the sovereigntist framing becomes increasingly a rhetorical holdout rather than an accurate description of the doctrine''s operative status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the sovereigntist reading remains descriptively accurate or has been overtaken by customary international law developments.').

omega_variable(
    consent_requirement_natural_or_constructed,
    'Is the state-consent requirement for binding international obligation a structural feature of an international system with no supranational sovereign (a near-mountain), or is it a constructed doctrinal choice that primarily serves the interests of currently powerful non-ratifying states?',
    'Compare consent-requirement enforcement across power asymmetries: if powerful states face the same practical consequences for non-ratification as weak states, the requirement functions more like a structural feature; if enforcement and reputational cost fall disproportionately on weaker states while powerful states escape both legal and reputational consequence, the requirement functions as a constructed shield for the powerful.',
    'If constructed and asymmetrically protective, this reading would resemble a false-summit dynamic (naturalized state sovereignty benefiting identifiable powerful-state beneficiaries) more than a genuine mountain-like feature of a consent-based international order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_requirement_natural_or_constructed, conceptual, 'Whether the sovereignty/consent requirement is structural necessity or constructed doctrine benefiting powerful states.').

omega_variable(
    reservation_practice_erosion,
    'Do extensive treaty reservations by ratifying states functionally replicate non-ratification, meaning the consent requirement extends its shielding effect even into nominally ''binding'' regimes?',
    'Audit the scope and substance of reservations entered against ICCPR/ICESCR by major states and assess whether reserved provisions correspond to the rights most frequently violated in practice.',
    'If reservations systematically exempt the most contested rights, the sovereigntist reading''s practical reach is broader than formal ratification statistics suggest, meaning the true extractiveness gap for claimants is understated by this story''s epsilon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reservation_practice_erosion, empirical, 'Whether reservation practice extends consent-based non-bindingness beyond formal non-ratification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__aspirational_sovereignty_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(udhr_tr_t1963, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1963, 0.32).
narrative_ontology:measurement(udhr_tr_t1978, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1978, 0.34).
narrative_ontology:measurement(udhr_tr_t1993, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1993, 0.36).
narrative_ontology:measurement(udhr_tr_t2008, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(udhr_tr_t2024, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1948, 0.08).
narrative_ontology:measurement(udhr_be_t1963, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1963, 0.1).
narrative_ontology:measurement(udhr_be_t1978, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1978, 0.12).
narrative_ontology:measurement(udhr_be_t1993, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1993, 0.14).
narrative_ontology:measurement(udhr_be_t2008, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2008, 0.16).
narrative_ontology:measurement(udhr_be_t2024, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2024, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(udhr_authority__aspirational_sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__aspirational_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_authority__aspirational_sovereignty_reading, 0.05).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__binding_universalism_reading).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__customary_emergence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the udhr_authority kernel. aspirational_sovereignty_reading (this file) authors low, stable extractiveness (~0.18) reflecting the sovereigntist premise that no binding obligation exists absent consent. binding_universalism_reading authors substantially higher extractiveness on non-complying states, treating UDHR rights as justiciable regardless of consent. customary_emergence_reading authors a moderate, rising extractiveness trajectory tracking the accumulation of state practice and opinio juris toward binding custom. The three are not the same constraint measured three ways — they are three structurally distinct claims about where legal authority resides, sharing a textual kernel but diverging on the authority_grounding and beneficiary/victim structure. Each is generated independently per the epsilon-invariance principle and linked here for contamination-propagation analysis: a documented empirical shift toward customary emergence would apply downstream pressure on this reading's continued descriptive accuracy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
