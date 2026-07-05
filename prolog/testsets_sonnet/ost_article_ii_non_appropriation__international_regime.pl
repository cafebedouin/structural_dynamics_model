% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Deferred to Future International Regime (Article XI Analogue Reading)
 *   domain: international_law/space_governance/commons
 *
 * SUMMARY:
 *   The Outer Space Treaty's Article II bars national appropriation 'by claim
 *   of sovereignty, by means of use or occupation, or by any other means,'
 *   but is silent on whether private commercial extraction of space resources
 *   constitutes appropriation. Rather than resolving this ambiguity toward
 *   permission or prohibition, this reading holds that the treaty framework
 *   itself commits the question to a future international regime — a position
 *   with real textual and historical support (the 1979 Moon Agreement's
 *   Article 11 attempted exactly this for celestial bodies, though it failed
 *   to attract ratification by spacefaring states). The structural effect of
 *   this deferral is a scaffold: a coordination mechanism meant to be
 *   transitional, whose sunset condition is the eventual conclusion of a
 *   binding multilateral resource-rights regime. In the absence of that
 *   regime, however, the deferral does not remain neutral — capability
 *   differentials mean first-mover states and firms accumulate operational
 *   precedent while the negotiation stalls, converting a nominally symmetric
 *   deferral into an asymmetric benefit for those already capable of
 *   extraction.
 *
 * KEY AGENTS:
 *   - first_mover_resource_firms: primary beneficiary (powerful/arbitrage) — operates in the grey zone the deferral creates
 *   - spacefaring_states_with_domestic_licensing_regimes: agenda_setter/beneficiary (institutional/mobile) — sets domestic law citing the deferral as cover, slow-walks binding regime negotiation
 *   - non_spacefaring_states: primary payer (moderate/trapped) — dependent on a regime that never arrives while precedent hardens against them
 *   - future_multilateral_regime_negotiators: payer (institutional/constrained) — inherits a fait accompli despite formal future authority
 *   - copuos_legal_subcommittee: analytical observer (institutional/analytical) — documents the stalemate without power to resolve it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.42).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.28).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Deferred to Future International Regime (Article XI Analogue Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_law/space_governance/commons").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, 'f3136180-4df2-4046-be72-16bd0754d183').
narrative_ontology:cs_kernel_codification('f3136180-4df2-4046-be72-16bd0754d183', fixed_text).
narrative_ontology:cs_authority_grounding('f3136180-4df2-4046-be72-16bd0754d183', distributed).
narrative_ontology:cs_reading_relation('f3136180-4df2-4046-be72-16bd0754d183', ost_article_ii_non_appropriation__extraction_permissive, influences).
narrative_ontology:cs_reading_relation('f3136180-4df2-4046-be72-16bd0754d183', ost_article_ii_non_appropriation__commons_conservation, influences).
narrative_ontology:cs_axiom('f3136180-4df2-4046-be72-16bd0754d183', foundational, appropriation_question_requires_multilateral_resolution).
narrative_ontology:cs_axiom_status(appropriation_question_requires_multilateral_resolution, holdable).
narrative_ontology:cs_axiom_grounding('f3136180-4df2-4046-be72-16bd0754d183', appropriation_question_requires_multilateral_resolution, conventional).
narrative_ontology:cs_axiom('f3136180-4df2-4046-be72-16bd0754d183', foundational, treaty_text_alone_underdetermines_extraction_rights).
narrative_ontology:cs_axiom_status(treaty_text_alone_underdetermines_extraction_rights, holdable).
narrative_ontology:cs_axiom_grounding('f3136180-4df2-4046-be72-16bd0754d183', treaty_text_alone_underdetermines_extraction_rights, conventional).
narrative_ontology:cs_reference_frame('f3136180-4df2-4046-be72-16bd0754d183', deferred_multilateral_negotiation_framework).
narrative_ontology:cs_drift_state('f3136180-4df2-4046-be72-16bd0754d183', post_commercial_capability_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f3136180-4df2-4046-be72-16bd0754d183', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_resource_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, spacefaring_states_with_domestic_licensing_regimes).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, future_multilateral_regime_negotiators).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, smaller_commercial_entrants).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, treaty_text_underdetermines_appropriation_rule).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, multilateral_regime_is_the_proper_forum).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate in the grey zone the deferred question creates: no treaty rule affirmatively permits resource extraction, but none clearly forbids it either, so these firms proceed under domestic licensing (US Commercial Space Launch Competitiveness Act, Luxembourg's space resources law, UAE's framework) while the multilateral question stays unresolved. The longer the regime negotiation stalls, the more their operational precedent hardens into de facto practice that future negotiators must accommodate.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_resource_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Author domestic legislation authorizing their nationals to extract and own space resources, explicitly citing the absence of a controlling multilateral rule as legal cover. They participate in COPUOS and Moon Agreement discussions but have incentive to slow-walk any binding regime that would constrain what their licensed firms are already doing. They set the negotiating agenda by virtue of being the only parties currently capable of extraction.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, spacefaring_states_with_domestic_licensing_regimes, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, spacefaring_states_with_domestic_licensing_regimes, beneficiary).

% Lack extraction capability and depend entirely on a future international regime to secure any share of common-heritage benefits. Every year the regime question stays deferred, first-mover practice accumulates and shrinks the range of terms a future regime could plausibly impose without disrupting established operations. Their formal equality in UN forums does not translate into leverage against the physical fact of who is already extracting.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states, payer,
    moderate, generational, trapped, global).

% Whoever eventually negotiates the Article XI-analogue regime inherits a fait accompli: capital sunk, precedent set, and a distributional conflict that has calcified rather than softened. They are structurally disadvantaged by every year of delay even though the deferral was framed as preserving their future authority.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, future_multilateral_regime_negotiators, payer,
    institutional, civilizational, constrained, universal).

% Face the same legal uncertainty as first movers but lack the capital reserves, insurance access, or state backing to operate profitably inside a grey zone; the ambiguity that first movers can absorb as risk is often a market-entry barrier for them. They cannot obtain financing against property claims that no regime has validated.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, smaller_commercial_entrants, payer,
    moderate, biographical, constrained, global).

% Convenes working groups on space resource governance, produces reports and draft principles, but has no binding authority to resolve the appropriation question absent state consensus. Documents the stalemate without being able to end it.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, copuos_legal_subcommittee, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely defers a distributionally explosive question — how to allocate rights to extraterrestrial resources among states with wildly unequal extraction capacity — to a future negotiated framework, avoiding a premature lock-in that any single reading (extraction-permissive or conservationist) would otherwise impose unilaterally.
% TRANSFER_FUNCTION: Moves de facto priority and precedent-setting power from the deferred negotiation to whichever actors can operate now: capital, licensing infrastructure, and operational practice flow to first-mover states and firms, while non-spacefaring states and future negotiators bear the cost of accumulating fait accompli.
% ABSENT_VOICES: Non-spacefaring states voice objections in COPUOS plenary sessions but hold no capability-based leverage; future negotiators are not yet identifiable individuals and cannot object at all; the interests of a not-yet-existing multilateral regime are represented, if at all, only by present-day diplomats with no binding mandate.
% DISAPPEARANCE_RATIONALE: If the deferral vanished and a rule were imposed by fiat (either reading), first-mover firms would either gain confirmed property rights (extraction-permissive win) or lose sunk investment (conservation win) — the world clearly rearranges for them. Non-spacefaring states dispute whether the deferral itself is doing anything protective for them or merely running out the clock in favor of those with capability; some diplomats argue the scaffold is the only thing preventing unilateral extraction-permissive practice from becoming customary law by default, others argue the deferral IS how that customary law is quietly forming.
% FOUNDING_PROBLEM: In 1967 no state had the capability to extract space resources, so Article II's non-appropriation principle could be drafted broadly without needing to resolve a genuine appropriation-rights allocation question; the framers deferred the harder distributive question to a future regime (as later made explicit in the 1979 Moon Agreement's Article 11) because it was not yet urgent and consensus was not achievable.
% FOUNDING_PROBLEM_CORROBORATION: UN COPUOS Legal Subcommittee working papers (2016–present) and independent international law scholarship (e.g., commentary from non-aligned bloc delegations and academic treatises on the Moon Agreement's failure to attract spacefaring-state ratification) attest that the appropriation question remains genuinely unresolved and increasingly urgent as extraction capability matures — this is not a claim asserted only by the states currently benefiting from the deferral.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, contested).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.05 to 0.45 over the interval) because the deferral itself extracts relatively little directly — no party is compelled to pay a rent to another under this reading's terms — but the accumulating asymmetry of who can act during the deferral period produces a growing effective transfer from non-spacefaring states and future negotiators to first movers. Theater ratio is substantial and rising (0.10 to 0.60): COPUOS working groups, expert panels, and repeated 'principles' drafts perform the appearance of active regime-building while producing no binding text, especially post-2015 as national legislation on space resources proliferated faster than multilateral consensus. Suppression is comparatively low (0.28) because no party is coercively prevented from advocating either substantive reading — the mechanism operates through capability asymmetry and negotiation deadlock rather than active suppression of alternatives. Accessibility collapse is moderate (0.35): the deferral formally keeps both extraction-permissive and conservation outcomes open, but the growing weight of first-mover precedent progressively narrows what a future regime could realistically impose. Resistance is substantial (0.61): non-spacefaring states and international law scholars actively contest the sufficiency of the deferral and argue it functions as a de facto extraction-permissive default.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a spacefaring state with an active licensing regime, Article II's deferral looks like prudent scaffolding — a genuine coordination problem (avoiding premature multilateral lock-in) correctly left open pending better information and broader buy-in. From the seat of a non-spacefaring state or future negotiator, the same deferral looks increasingly indistinguishable from acquiescence to an extraction-permissive default achieved by delay rather than by treaty text. The engine should compute these as genuinely different per-seat classifications from the same structural facts — that divergence is the point of a scaffold whose sunset condition has not yet been met and shows no clear path to being met.
 *
 * DIRECTIONALITY LOGIC:
 *   First-mover firms and licensing states sit near the beneficiary end: they act now, under domestic law, while the multilateral question remains open, and lose nothing from continued deferral — every additional year without a binding regime is a year of uncontested operational precedent. Non-spacefaring states and future regime negotiators sit near the target end: they bear the cost of the deferral in the form of eroding future leverage, despite having no present recourse, because their exit option is 'trapped' or 'constrained' relative to the accumulating fact pattern. This is not a case of active extraction from a payer to a beneficiary through enforcement — it is closer to a asymmetric race where the deferral's neutrality is only formal; the real-world capability gap converts nominal symmetry into structural advantage for the capable party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to allocate appropriation rights to a resource no one could yet extract — was live in 1967 and remains live today; it has not become a dead mandate maintained by inertia. What has drifted is the balance of urgency: the deferral was tenable when extraction was purely theoretical, but as commercial capability has matured (Article II's practical stakes rising from near-zero in 1967 to substantial by 2025), the same deferral increasingly functions as a default answer favoring whoever can act first, even though its formal justification (avoid premature lock-in) has not changed. This is precisely the scaffold/snare boundary case an honest classification must not paper over: the coordination function is real and the mandate is not dead, but the failure to sunset on any defined schedule is allowing an asymmetric outcome to accrete under cover of continued 'transitional' status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deferral_neutrality_or_default,
    'Does Article II''s deferral to a future international regime function as genuinely neutral scaffolding, or does it operate as a de facto extraction-permissive default given capability asymmetry among states?',
    'Track whether a binding multilateral regime is concluded before or after first-mover extraction becomes commercially routine and politically difficult to unwind; if commercial practice hardens into customary international law before any regime is negotiated, the deferral will have functioned as a default rather than a neutral placeholder.',
    'If the deferral is genuinely neutral, this constraint remains a scaffold whose sunset is simply delayed. If it is a de facto default, this reading is functionally indistinguishable from the extraction_permissive reading and the international_regime framing is itself part of the cover story for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_neutrality_or_default, conceptual, 'Whether the deferral reading is analytically distinct from the extraction-permissive reading in practice, not just in text.').

omega_variable(
    moon_agreement_precedent_weight,
    'How much interpretive weight should the 1979 Moon Agreement''s Article 11 (which explicitly established a ''common heritage of mankind'' international regime requirement) carry for interpreting Article II of the 1967 Outer Space Treaty, given that no major spacefaring state has ratified the Moon Agreement?',
    'Comparative treaty interpretation analysis of whether non-ratification of a related instrument weakens or is irrelevant to interpreting the intent behind the earlier, widely-ratified instrument; state practice and opinio juris surveys among spacefaring vs. non-spacefaring states.',
    'If the Moon Agreement''s non-ratification signals rejection of the international-regime reading by capable states, this reading''s authority is weaker than claimed and the deferral is more contested than the text suggests. If non-ratification is explained by unrelated political factors, the international-regime reading retains stronger textual support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moon_agreement_precedent_weight, empirical, 'Whether the Moon Agreement''s failure to attract ratification undermines the international-regime reading''s claim to being the treaty framework''s intended resolution.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the international-regime reading the most textually supportable of the three sibling readings, or is it a diplomatically convenient middle position that avoids resolving genuine ambiguity in the treaty text?',
    'None conclusively available absent either a binding multilateral regime being concluded (which would validate the deferral reading as the operative one) or an authoritative international tribunal ruling on Article II''s scope (no such ruling has occurred).',
    'If the deferral reading is itself a diplomatic compromise rather than the treaty''s actual meaning, all three readings should be treated as genuinely coexisting unresolved positions rather than one being structurally privileged — which is in fact how this story treats them, but the omega flags that this framing choice was itself contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether treating international_regime as a distinct third reading (rather than folding it into extraction_permissive as the practical default) is the correct decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 1967, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1967, 0.1).
narrative_ontology:measurement_basis(ost__tr_t1967, observed).
narrative_ontology:measurement(ost__tr_t1979, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1979, 0.2).
narrative_ontology:measurement_basis(ost__tr_t1979, observed).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(ost__tr_t2015, observed).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2020, 0.48).
narrative_ontology:measurement_basis(ost__tr_t2020, observed).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2025, 0.55).
narrative_ontology:measurement_basis(ost__tr_t2025, observed).
narrative_ontology:measurement(ost__tr_t2030, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2030, 0.58).
narrative_ontology:measurement_basis(ost__tr_t2030, projected).
narrative_ontology:measurement(ost__tr_t2035, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2035, 0.6).
narrative_ontology:measurement_basis(ost__tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1967, 0.05).
narrative_ontology:measurement_basis(ost__be_t1967, observed).
narrative_ontology:measurement(ost__be_t1979, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1979, 0.08).
narrative_ontology:measurement_basis(ost__be_t1979, observed).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement_basis(ost__be_t2015, observed).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2020, 0.31).
narrative_ontology:measurement_basis(ost__be_t2020, observed).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2025, 0.38).
narrative_ontology:measurement_basis(ost__be_t2025, observed).
narrative_ontology:measurement(ost__be_t2030, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2030, 0.42).
narrative_ontology:measurement_basis(ost__be_t2030, projected).
narrative_ontology:measurement(ost__be_t2035, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2035, 0.45).
narrative_ontology:measurement_basis(ost__be_t2035, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ost_article_ii_non_appropriation__international_regime, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__international_regime, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the ost_article_ii_non_appropriation kernel. The extraction_permissive reading holds Article II bars only sovereign territorial claims, not private resource ownership — a Tangled Rope or Snare-leaning reading benefiting first-mover firms directly. The commons_conservation reading holds Article II's 'use or occupation' language prohibits de facto appropriation by extraction, covering private actors — a Rope-leaning reading protecting non-spacefaring states' future claims. This international_regime reading occupies the deferred middle: a Scaffold whose sunset condition (conclusion of a binding multilateral regime) has not been met and shows no clear timeline for being met, which is structurally why the deferral increasingly functions in practice like the extraction_permissive reading despite its distinct textual basis. All three stories share the same treaty text and historical interval but diverge sharply in claimed_type, beneficiary/victim structure, and epsilon because they are answers to different structural questions the same label conflates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
