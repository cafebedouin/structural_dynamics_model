% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Outer Space Treaty Article II — Extraction-Permissive Reading
 *   domain: international_law/space_governance/resource_extraction
 *
 * SUMMARY:
 *   The 1967 Outer Space Treaty's Article II prohibits national appropriation
 *   of celestial bodies 'by claim of sovereignty, by means of use or
 *   occupation, or by any other means.' This story instantiates the
 *   extraction-permissive reading: the text bars STATE territorial
 *   sovereignty claims but is silent on whether private (or state-licensed)
 *   extraction of resources followed by domestic legal recognition of title
 *   constitutes 'appropriation.' Under this reading, states with launch
 *   capability (the US, Luxembourg, UAE, Japan) have enacted domestic
 *   legislation granting title to extracted space resources and coordinated
 *   bilateral recognition through instruments like the Artemis Accords,
 *   treating the treaty's silence as permission. This is one of three
 *   readings of the same kernel text; the conservation reading treats the
 *   same silence as insufficient to overcome the non-appropriation
 *   principle's broader intent, and the international-regime reading holds
 *   the question undecided pending a multilateral framework analogous to the
 *   Moon Agreement's Article XI. Each reading is authored as its own
 *   constraint story with its own epsilon; this file is the
 *   extraction-permissive reading only.
 *
 * KEY AGENTS:
 *   - spacefaring_launch_states: primary agenda-setters and beneficiaries (institutional/arbitrage) — write the operative interpretation via domestic law and bilateral accord
 *   - commercial_extraction_firms: direct beneficiaries (organized/mobile) — hold extracted-resource title under flag-state licensing
 *   - non_spacefaring_states: primary payers (powerless/trapped) — formally equal signatories excluded from the benefit stream
 *   - future_generations_of_claimants: temporally displaced payers (powerless/trapped) — inherit a pre-allocated solar system
 *   - un_committee_on_peaceful_uses_of_outer_space: excluded multilateral venue — bypassed by unilateral and bilateral action
 *   - international_legal_scholars: analytical observers — document but do not adjudicate the interpretive contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.71).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.58).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.71).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Outer Space Treaty Article II — Extraction-Permissive Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_law/space_governance/resource_extraction").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '1c9136ec-073a-4d39-a2c3-aae85248c1d4').
narrative_ontology:cs_kernel_codification('1c9136ec-073a-4d39-a2c3-aae85248c1d4', fixed_text).
narrative_ontology:cs_authority_grounding('1c9136ec-073a-4d39-a2c3-aae85248c1d4', distributed).
narrative_ontology:cs_reading_relation('1c9136ec-073a-4d39-a2c3-aae85248c1d4', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('1c9136ec-073a-4d39-a2c3-aae85248c1d4', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('1c9136ec-073a-4d39-a2c3-aae85248c1d4', foundational, sovereignty_and_ownership_are_legally_severable).
narrative_ontology:cs_axiom_status(sovereignty_and_ownership_are_legally_severable, holdable).
narrative_ontology:cs_axiom_grounding('1c9136ec-073a-4d39-a2c3-aae85248c1d4', sovereignty_and_ownership_are_legally_severable, conventional).
narrative_ontology:cs_axiom('1c9136ec-073a-4d39-a2c3-aae85248c1d4', foundational, textual_silence_on_extraction_constitutes_permission).
narrative_ontology:cs_axiom_status(textual_silence_on_extraction_constitutes_permission, holdable).
narrative_ontology:cs_axiom_grounding('1c9136ec-073a-4d39-a2c3-aae85248c1d4', textual_silence_on_extraction_constitutes_permission, conventional).
narrative_ontology:cs_reference_frame('1c9136ec-073a-4d39-a2c3-aae85248c1d4', cold_war_sovereignty_prohibition_framework).
narrative_ontology:cs_drift_state('1c9136ec-073a-4d39-a2c3-aae85248c1d4', commercial_extraction_capability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c9136ec-073a-4d39-a2c3-aae85248c1d4', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_launch_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, commercial_extraction_firms).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations_of_claimants).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, sovereign_non_appropriation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess launch capability and domestic legal frameworks (e.g. national space resource acts) that authorize their firms to extract and own celestial resources without claiming sovereign territory. They author the interpretation that Article II's silence on private appropriation is permission, and they enforce this reading through bilateral agreements (e.g. Artemis Accords) among themselves, effectively writing the operative rule without amending the treaty.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_launch_states, agenda_setter,
    institutional, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_launch_states, beneficiary).

% Operate under flag-state licensing to prospect and extract lunar and asteroid resources. They receive legal title to extracted material under domestic statute, backed by their state's diplomatic and military weight, and can relocate operations or incorporate wherever licensing is most permissive.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, commercial_extraction_firms, beneficiary,
    organized, biographical, mobile, universal).

% Lack the technological capability to extract resources themselves and have no seat at the bilateral agreements that operationalize the extraction-permissive reading. They are formally equal signatories to the Outer Space Treaty but structurally excluded from the benefit stream; their consent to the treaty's non-appropriation language did not anticipate resource enclosure via domestic legislation and flag-state recognition.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states, payer,
    powerless, generational, trapped, global).

% Will inherit a solar system where first-mover extraction has already allocated the most accessible resource-rich sites to whichever states had launch capability first. They have no representation in the current interpretive contest and cannot retroactively contest allocations made under a permissive reading that predates any multilateral resource regime.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations_of_claimants, payer,
    powerless, civilizational, trapped, universal).

% The multilateral body theoretically charged with developing a resource-governance regime under Article XI-style processes. Its deliberations are slow and consensus-bound; spacefaring states proceed with domestic legislation and bilateral accords while COPUOS discussion continues, rendering its eventual output likely to ratify a fait accompli rather than shape it.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, un_committee_on_peaceful_uses_of_outer_space, excluded,
    institutional, generational, analytical, global).

% Debate whether Article II's prohibition on 'national appropriation by claim of sovereignty, by means of use or occupation, or by any other means' extends to resource extraction by private actors under state authorization. They document the interpretive contest but hold no enforcement power over which reading prevails in practice.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal certainty for capital-intensive extraction ventures by settling (for launch-capable states and their firms) that resource ownership can attach without triggering the treaty's sovereignty prohibition, enabling investment in extraction technology.
% TRANSFER_FUNCTION: Moves de facto control over accessible extraterrestrial resource sites from the community of treaty signatories as a whole to the subset of states with launch capability, via domestic legislation and bilateral recognition rather than treaty amendment.
% ABSENT_VOICES: Non-spacefaring signatory states and future generations have no forum in which the extraction-permissive reading is actually negotiated; COPUOS is formally the venue but is bypassed in practice by domestic statute and bilateral accord, so objection is structurally foreclosed even though the treaty nominally bound all parties equally.
% DISAPPEARANCE_RATIONALE: If the extraction-permissive reading were displaced (e.g. by a binding multilateral resource regime or a tribunal ruling that private appropriation is itself prohibited use), current extraction licensing schemes would lose their legal foundation, firms would need to renegotiate title under a different framework, and the current first-mover advantage of launch-capable states would be substantially diminished or redistributed.
% FOUNDING_PROBLEM: Article II was drafted to prevent Cold War superpowers from planting flags and claiming sovereign territory on the Moon or other celestial bodies, averting a repeat of terrestrial colonial land-grabs in a new domain.
% FOUNDING_PROBLEM_CORROBORATION: Spacefaring states and their firms attest the founding problem (sovereign territorial claims) is fully solved by the treaty's plain text and that resource extraction was never within its scope. Independent legal scholars and non-spacefaring states dispute this, arguing the founding problem was broader — preventing de facto appropriation of the commons by any means — and that the extraction-permissive reading revives the underlying harm through a different legal instrument (corporate title instead of a flag).
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71 by interval end because the reading permits resource enclosure gated purely by technological capability and flag-state legal recognition, with no compensation or benefit-sharing mechanism running to non-spacefaring signatories — the treaty's 'province of all mankind' language in Article I is not operationalized under this reading. Suppression is moderate (0.58): there is no direct coercive enforcement against dissenting states, but the fait accompli structure of bilateral accords and domestic legislation forecloses meaningful contest — by the time any multilateral body could act, extraction infrastructure and legal precedent will already be established. Theater ratio is moderate (0.42) and rising: COPUOS deliberation and treaty-fidelity rhetoric ('we fully comply with Article II') continue as a performative layer over a substantive practice of unilateral rule-writing.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this looks like a rope: a sensible resolution of textual ambiguity that unlocks investment and coordinates emerging spacefaring activity through mutually recognized domestic frameworks. From the payer seats, the same structure computes as extraction: an interpretation that was never negotiated multilaterally, that forecloses the 'province of all mankind' promise, and that transfers the practical benefit of a common resource to whichever states got there first. The engine's per-seat computation should register this divergence directly from the power/exit asymmetry, not from any claim about which reading is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   Spacefaring launch states and their licensed firms sit near the full-beneficiary end: they collect legal certainty and eventual resource title, and they hold arbitrage-grade exit (they can forum-shop jurisdictions for the most permissive licensing regime). Non-spacefaring states and future claimants sit near the full-target end: they bear the cost of foreclosed access with no compensation mechanism, and their exit options are trapped — the treaty framework was the only lever they held, and it is being interpreted around them, not by them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing Cold War-style flag-planting sovereignty claims) is largely dead as a live risk — no state today seriously threatens to annex the Moon by sovereign claim. But the extraction-permissive reading does not retire the treaty; it redirects its remaining force toward validating a different enclosure mechanism (corporate title via domestic statute) that the original drafters did not anticipate and may not have endorsed. Classifying this as tangled_rope rather than snare or mountain captures that there IS a genuine coordination function (legal certainty enabling investment) riding alongside genuine asymmetric extraction (foreclosure without compensation) — collapsing it to pure extraction would miss the real investment-enabling function; collapsing it to pure coordination would launder the distributive harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    silence_as_permission_ambiguity,
    'Does Article II''s silence on private resource extraction constitute affirmative permission, or does the non-appropriation principle''s evident purpose (preventing enclosure of a shared domain) extend by implication to cover extraction regardless of the formal absence of a sovereignty claim?',
    'An International Court of Justice advisory opinion, a binding multilateral resource-governance treaty, or sustained and unrebutted state practice (opinio juris) treating extraction-based title as either valid or invalid under customary international law.',
    'If silence is genuinely permissive, this reading''s classification as tangled_rope (real coordination function plus real extraction) stands as the operative structure. If the conservation reading''s broader-purpose argument prevails, current extraction licensing regimes would be retroactively delegitimized and the tangled_rope structure would collapse toward snare, since the coordination function would no longer have a valid legal basis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silence_as_permission_ambiguity, conceptual, 'Whether treaty silence on extraction is affirmative permission or an unaddressed gap governed by the treaty''s broader non-appropriation purpose.').

omega_variable(
    committer_structure_which_reading_prevails,
    'This constraint is one of three declared readings (extraction_permissive, commons_conservation, international_regime) of the same kernel text. Which reading will actually govern state and firm behavior as extraction capability matures, and does the extraction_permissive reading''s current practical dominance (via domestic legislation and bilateral accords) reflect legal correctness or simply first-mover fait accompli?',
    'Track whether COPUOS or a successor body produces a binding multilateral resource regime (favoring the international_regime reading), whether non-spacefaring states successfully litigate or negotiate recognition of the conservation reading, or whether the extraction_permissive reading becomes entrenched customary practice through unrebutted state conduct.',
    'If international_regime prevails, this story''s beneficiary/victim structure would be superseded by a negotiated allocation mechanism, likely reducing extractiveness substantially. If commons_conservation prevails, current extraction licensing would be rendered legally void, converting apparent beneficiaries into holders of contested or void title.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_which_reading_prevails, preference, 'Which of the three kernel readings will structure actual resource governance as extraction capability matures — a question of unresolved legal and political contest, not settled fact.').

omega_variable(
    fait_accompli_vs_formal_annexation,
    'Is enclosure via extraction-and-domestic-title functionally equivalent to the sovereign territorial appropriation Article II was drafted to prevent, even though it takes a different legal form?',
    'Comparative analysis of practical control and exclusion effects: does flag-state-recognized extraction title produce the same exclusionary access effects on a resource-rich site as a sovereign claim would, notwithstanding the absence of a formal sovereignty assertion?',
    'If functionally equivalent, the extraction-permissive reading is a workaround that achieves the treaty''s prohibited outcome through an unprohibited mechanism, strengthening the case for reclassifying this constraint as substantially closer to snare. If meaningfully distinct (e.g., extraction title does not exclude others from the same site the way sovereignty would), the tangled_rope classification''s coordination component is more clearly justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fait_accompli_vs_formal_annexation, conceptual, 'Whether extraction-based title produces the same practical enclosure effects the treaty''s sovereignty prohibition was meant to prevent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ost__tr_t0, observed).
narrative_ontology:measurement(ost__tr_t8, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(ost__tr_t8, observed).
narrative_ontology:measurement(ost__tr_t16, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 16, 0.3).
narrative_ontology:measurement_basis(ost__tr_t16, observed).
narrative_ontology:measurement(ost__tr_t24, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(ost__tr_t24, observed).
narrative_ontology:measurement(ost__tr_t32, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 32, 0.38).
narrative_ontology:measurement_basis(ost__tr_t32, observed).
narrative_ontology:measurement(ost__tr_t40, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(ost__tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(ost__be_t0, observed).
narrative_ontology:measurement(ost__be_t8, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 8, 0.4).
narrative_ontology:measurement_basis(ost__be_t8, observed).
narrative_ontology:measurement(ost__be_t16, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 16, 0.48).
narrative_ontology:measurement_basis(ost__be_t16, observed).
narrative_ontology:measurement(ost__be_t24, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(ost__be_t24, observed).
narrative_ontology:measurement(ost__be_t32, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 32, 0.66).
narrative_ontology:measurement_basis(ost__be_t32, observed).
narrative_ontology:measurement(ost__be_t40, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 40, 0.71).
narrative_ontology:measurement_basis(ost__be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t0, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(ost__su_t0, observed).
narrative_ontology:measurement(ost__su_t8, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 8, 0.36).
narrative_ontology:measurement_basis(ost__su_t8, observed).
narrative_ontology:measurement(ost__su_t16, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 16, 0.43).
narrative_ontology:measurement_basis(ost__su_t16, observed).
narrative_ontology:measurement(ost__su_t24, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 24, 0.49).
narrative_ontology:measurement_basis(ost__su_t24, observed).
narrative_ontology:measurement(ost__su_t32, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 32, 0.54).
narrative_ontology:measurement_basis(ost__su_t32, observed).
narrative_ontology:measurement(ost__su_t40, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(ost__su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposed from the natural-language label 'Article II non-appropriation and resource extraction,' per the epsilon-invariance principle: measuring the same treaty text under the extraction-permissive reading yields substantially higher extractiveness (0.71) than the commons_conservation reading would (where extraction itself is treated as prohibited, collapsing the beneficiary structure this story authors) or the international_regime reading would (where the allocation question is unresolved and pending multilateral negotiation, producing a different and likely lower or contested epsilon). Each sibling carries its own epsilon, its own beneficiary/victim structure, and its own claimed_type; they are linked here via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
