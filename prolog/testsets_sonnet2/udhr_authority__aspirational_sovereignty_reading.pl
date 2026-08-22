% ============================================================================
% CONSTRAINT STORY: udhr_authority__aspirational_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: udhr_authority__aspirational_sovereignty_reading
 *   human_readable: UDHR as Aspirational Moral Guidance Requiring State Consent
 *   domain: international_law/political_philosophy/human_rights_doctrine
 *
 * SUMMARY:
 *   This story instantiates the aspirational sovereignty reading of the UDHR
 *   authority kernel: the 1948 Universal Declaration of Human Rights is a
 *   moral touchstone that requires state consent (via subsequent treaty
 *   ratification) before it generates binding legal obligation. Under this
 *   reading, the UDHR's General Assembly resolution status was a deliberate
 *   design choice to secure cross-ideological consensus, not an oversight
 *   later corrected by binding universalism. States retain a veto over which
 *   obligations bind them; tribunals invoking the UDHR have interpretive but
 *   not coercive authority absent a ratified instrument. This is a distinct
 *   constraint from the binding_universalism_reading (which treats UDHR
 *   rights as justiciable regardless of consent, and would author much higher
 *   extractiveness on state autonomy) and from the
 *   customary_emergence_reading (which treats bindingness as an emergent
 *   property of state practice over time, occupying an intermediate ε). Each
 *   reading is authored as its own file with its own stable ε, per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - sovereign_states: primary beneficiary (institutional/arbitrage) — retains full discretion over binding obligation
 *   - individuals_subject_to_state_power: primary bearer of the gap between aspiration and enforcement (powerless/trapped)
 *   - international_tribunals: agenda_setter with only derivative authority (institutional/constrained)
 *   - un_general_assembly: founding institutional agenda_setter (institutional/analytical)
 *   - human_rights_advocates: excluded from the consent mechanism despite highest stake in enforcement (organized/constrained)
 *   - legal_positivist_scholars: analytical observer corroborating the reading's drafting-history basis
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
narrative_ontology:human_readable(udhr_authority__aspirational_sovereignty_reading, "UDHR as Aspirational Moral Guidance Requiring State Consent").
narrative_ontology:topic_domain(udhr_authority__aspirational_sovereignty_reading, "international_law/political_philosophy/human_rights_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__aspirational_sovereignty_reading, 'd44368cc-5484-4e27-82ce-c1be42f3337d').
narrative_ontology:cs_kernel_codification('d44368cc-5484-4e27-82ce-c1be42f3337d', fixed_text).
narrative_ontology:cs_authority_grounding('d44368cc-5484-4e27-82ce-c1be42f3337d', distributed).
narrative_ontology:cs_reading_relation('d44368cc-5484-4e27-82ce-c1be42f3337d', udhr_authority__binding_universalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('d44368cc-5484-4e27-82ce-c1be42f3337d', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('d44368cc-5484-4e27-82ce-c1be42f3337d', foundational, state_consent_necessary_for_binding_obligation).
narrative_ontology:cs_axiom_status(state_consent_necessary_for_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('d44368cc-5484-4e27-82ce-c1be42f3337d', state_consent_necessary_for_binding_obligation, conventional).
narrative_ontology:cs_axiom('d44368cc-5484-4e27-82ce-c1be42f3337d', secondary, declaration_status_reflects_deliberate_non_binding_design).
narrative_ontology:cs_axiom_status(declaration_status_reflects_deliberate_non_binding_design, holdable).
narrative_ontology:cs_axiom_grounding('d44368cc-5484-4e27-82ce-c1be42f3337d', declaration_status_reflects_deliberate_non_binding_design, empirically_contingent).
narrative_ontology:cs_reference_frame('d44368cc-5484-4e27-82ce-c1be42f3337d', westphalian_consent_based_international_law).
narrative_ontology:cs_drift_state('d44368cc-5484-4e27-82ce-c1be42f3337d', contemporary_human_rights_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d44368cc-5484-4e27-82ce-c1be42f3337d', '').
narrative_ontology:cs_kernel_id(udhr_authority__aspirational_sovereignty_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, non_ratifying_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, individuals_subject_to_state_power).
narrative_ontology:constraint_vindicates(udhr_authority__aspirational_sovereignty_reading, state_consent_doctrine).
narrative_ontology:constraint_vindicates(udhr_authority__aspirational_sovereignty_reading, westphalian_sovereignty_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Treats the UDHR as a non-binding declaration of moral aspiration. Retains full discretion over whether to ratify subsequent covenants that would create binding treaty obligations, and can enter reservations even when it does ratify. Cites the UDHR in diplomatic rhetoric while declining enforcement mechanisms that would bind its domestic conduct.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, sovereign_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__aspirational_sovereignty_reading, sovereign_states, agenda_setter).

% Cite the UDHR in litigation and advocacy campaigns seeking to hold states accountable for rights violations, but under this reading have no forum with coercive jurisdiction absent the state's own consent to a binding instrument. Their appeals to the UDHR carry moral weight but no legal teeth in this framework.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, human_rights_advocates, excluded,
    organized, generational, constrained, global).

% Live under the jurisdiction of a state that may violate UDHR principles without facing binding international consequence, because the state has not consented to an enforceable mechanism. Bear the practical cost of the gap between declared aspiration and enforceable obligation, though this reading treats that gap as the correct and intended structure, not a defect.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, individuals_subject_to_state_power, payer,
    powerless, biographical, trapped, national).

% Can invoke UDHR language interpretively but lack coercive jurisdiction over a state unless that state has ratified a treaty conferring it. Under this reading, tribunals are advisory or interpretive bodies whose authority is entirely derivative of prior state consent, never original to the UDHR text itself.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, international_tribunals, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__aspirational_sovereignty_reading, international_tribunals, excluded).

% Adopted the UDHR in 1948 as a resolution, not a treaty, deliberately withholding binding legal force to secure broad consensus across ideologically divided member states. Continues to reference the UDHR as a moral touchstone without asserting it independently binds member conduct.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, un_general_assembly, agenda_setter,
    institutional, civilizational, analytical, global).

% Analyze the UDHR's drafting history and General Assembly resolution status to argue that its authority was designed to be persuasive rather than obligatory, and that subsequent claims of binding force require independent legal grounding through treaty or custom.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, legal_positivist_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_authority__aspirational_sovereignty_reading, sovereign_states).
narrative_ontology:fixing_cost_class(udhr_authority__aspirational_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared moral vocabulary and reference point that lets states with radically different legal, political, and religious systems affirm common aspirational standards without surrendering domestic legal sovereignty — enabling broad 1948 consensus that a binding instrument could never have achieved.
% TRANSFER_FUNCTION: Moves rhetorical and diplomatic legitimacy toward states that can credibly invoke UDHR language, without moving any enforceable legal obligation from states to individuals or to international bodies; the only 'transfer' is reputational, not coercive.
% ABSENT_VOICES: Individuals harmed by rights violations and human rights advocates seeking enforceable remedies are structurally absent from the consent mechanism — the instrument's binding status is negotiated entirely among states, and those most affected by its non-binding character have no seat in deciding that character.
% DISAPPEARANCE_RATIONALE: Advocates for this reading argue that if the UDHR's aspirational, consent-requiring status vanished and were replaced by automatic binding force, the diplomatic consensus of 1948 would never have been reachable and many states would have refused to engage with the framework at all — so removing the consent requirement destroys the coordination function retroactively. Critics of this reading argue the world would barely notice at the state level (since enforcement is already largely absent) but would rearrange significantly for individuals who gained an enforceable claim.
% FOUNDING_PROBLEM: In 1948, states with deeply incompatible legal and political systems (liberal democracies, Soviet-bloc states, newly decolonizing nations) needed a shared statement of human rights aspirations without any single bloc being able to impose binding legal obligations on the others through the instrument.
% FOUNDING_PROBLEM_CORROBORATION: The UDHR's own drafting history (Eleanor Roosevelt's Third Committee negotiations, the deliberate choice of 'declaration' over 'covenant' or 'treaty') corroborates the consent-requiring reading from outside any single beneficiary state. However, subsequent international law scholarship and customary-international-law tribunals attest that significant portions have since acquired binding customary status independent of ongoing state consent — a corroboration this reading disputes as premature or overstated.
narrative_ontology:disappearance_verdict(udhr_authority__aspirational_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(udhr_authority__aspirational_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__aspirational_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.18) because under this reading no coercive transfer occurs from states to any party — the UDHR's operation is genuinely voluntary and consent-gated, and the mild upward drift across the interval reflects only the accumulating rhetorical cost states pay when their non-ratification becomes more visible in an increasingly networked diplomatic environment, not a growing coercive extraction. Suppression is authored low (0.12) because the reading's own logic holds that no state is coerced into anything; whatever pressure exists is reputational, not legal. Theater ratio is moderate and rising (0.30 to 0.40) because ceremonial invocation of the UDHR in diplomatic settings has increased over decades even as the underlying consent-gated structure has not changed — more performance, same substance, which this reading treats as expected rather than as decay of a coercive function (there was no coercive function to decay).
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign_states seat, this reading computes as close to rope: genuine coordination benefit (shared moral vocabulary, diplomatic legitimacy) at low personal cost, entered into voluntarily. From the individuals_subject_to_state_power seat, the same structure may compute quite differently even within this reading's own terms — bearing the practical consequence of unenforceability while having no vote in whether their state ratifies binding instruments. The engine should register this seat divergence structurally; this reading's authors regard the divergence as the correct and stable design, not as evidence of extraction, but the JSON does not resolve that dispute — it only supplies the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states are declared beneficiaries because the consent-requirement itself is what they collect: freedom from binding obligation absent their own ratification, with arbitrage-grade exit (they can decline any covenant). Individuals subject to state power are declared payers because they bear the practical consequence of non-enforcement, despite having no formal victim status in this reading's own framework — trapped exit reflects that individuals cannot exit their state's jurisdiction to escape the consequence of its non-ratification. No group is declared a formal victim (base_properties.victims is empty) because this reading's own logic holds that consent-gating is not extraction — it is the coordination mechanism working as designed. This is a deliberate authoring choice reflecting the reading's internal perspective, distinct from how the binding_universalism_reading would author victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing cross-ideological 1948 consensus without any bloc imposing binding obligations on others) is genealogically well-corroborated by the drafting history itself — this is not a manufactured origin myth. Whether that founding problem remains live in 2025, when far greater ideological convergence on baseline rights exists among many UN member states, is contested: this reading holds the problem is still live (deep disagreement persists, e.g., over religious law versus secular rights frameworks), while the customary_emergence and binding_universalism readings hold the problem has been substantially superseded by decades of practice. The mismatch-consumer here would watch for founding_problem_status=dead paired with disappearance_verdict=world_rearranges as a capture signal; this story authors status=contested and verdict=contested precisely because the reading itself acknowledges the genealogy is live-disputed, not settled in its own favor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the UDHR''s authority genuinely gated by state consent (this reading), automatically binding regardless of consent (binding_universalism_reading), or bootstrapped into binding custom through decades of state practice (customary_emergence_reading)? The 1948 text and drafting history underdetermine which reading is correct, and different international law traditions and tribunals have endorsed different readings.',
    'No single resolution mechanism exists because this is a live jurisprudential dispute rather than an empirical unknown; the closest available evidence is comparative analysis of how international tribunals (ICJ, regional human rights courts) have actually treated UDHR provisions as binding or non-binding over time, and whether that treatment has hardened into consistent customary practice (opinio juris) or remained genuinely contested and inconsistent.',
    'If tribunal practice has hardened into consistent binding treatment across a critical mass of provisions, the customary_emergence_reading displaces this reading as the descriptively accurate account, and this reading''s low extractiveness figure would understate the actual current constraint on state autonomy. If tribunal practice remains genuinely inconsistent and consent-dependent, this reading''s low-extraction authoring is descriptively correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which of the three kernel readings (aspirational-consent, binding-universal, customary-emergent) best describes the UDHR''s actual current legal status is unresolved and may not be resolvable in the abstract — it may vary provision by provision.').

omega_variable(
    consent_requirement_as_cover_for_impunity,
    'Does the state-consent requirement genuinely reflect a principled sovereignty norm necessary for the 1948 coordination achievement, or has it become a structural shield that powerful states use selectively to avoid accountability for violations while still claiming UDHR-derived moral legitimacy in their foreign policy rhetoric?',
    'Comparative case analysis of which states invoke the UDHR rhetorically in condemning others'' conduct while declining to ratify or accept jurisdiction under corresponding binding instruments themselves — a pattern of selective invocation would indicate the consent requirement functions asymmetrically rather than as a neutral sovereignty principle.',
    'If selective invocation is the dominant pattern, this reading''s low extractiveness score understates a hidden extraction: powerful states extract legitimacy benefits from UDHR rhetoric while extracting impunity benefits from the consent requirement, at the expense of individuals in weaker or less strategically important states. This would push the constraint toward a tangled_rope or snare classification at the level of powerful-state conduct specifically, even while remaining low-extraction as a general design principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_requirement_as_cover_for_impunity, empirical, 'Whether the consent requirement is applied as a neutral sovereignty principle or selectively as a shield by powerful states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__aspirational_sovereignty_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(udhr_tr_t1963, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1963, 0.32).
narrative_ontology:measurement(udhr_tr_t1978, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1978, 0.35).
narrative_ontology:measurement(udhr_tr_t1993, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1993, 0.36).
narrative_ontology:measurement(udhr_tr_t2008, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(udhr_tr_t2025, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1948, 0.1).
narrative_ontology:measurement(udhr_be_t1963, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1963, 0.12).
narrative_ontology:measurement(udhr_be_t1978, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1978, 0.14).
narrative_ontology:measurement(udhr_be_t1993, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1993, 0.15).
narrative_ontology:measurement(udhr_be_t2008, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2008, 0.16).
narrative_ontology:measurement(udhr_be_t2025, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2025, 0.18).

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
% This story is one of three linked readings of the udhr_authority kernel. The kernel is the 1948 UDHR text and its adoption circumstances; the three readings (aspirational_sovereignty, binding_universalism, customary_emergence) assign structurally different authority claims to that same text, yielding different ε values, different beneficiary/victim structures, and different classifications. This story (aspirational_sovereignty_reading) authors the lowest extractiveness of the three, treating state consent as a genuine, non-extractive design feature. The binding_universalism_reading would author substantially higher extractiveness on state autonomy (states are bound regardless of consent — a target relationship, not a beneficiary one). The customary_emergence_reading occupies an intermediate position, treating bindingness as a gradually accreted property. All three should be read together to understand the full contested structure of UDHR authority; no single file should be read as 'the' UDHR constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
