% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: NPT Article IV-VI Grand Bargain: Reciprocal Disarmament Obligation
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty (1970) encodes a reciprocal obligation
 *   between non-weapon states (NNWS) and weapon states (WS): Article IV
 *   grants NNWS the right to peaceful nuclear technology; Article VI commits
 *   WS to 'pursue in good faith' nuclear disarmament. This constraint
 *   investigates the GRAND BARGAIN READING: the pairing of Articles IV and VI
 *   is a genuine quid pro quo — NNWS restraint is conditional on WS
 *   disarmament progress, and breach of Article VI undermines Article IV
 *   legitimacy. Over 50 years, disarmament progress has stalled while
 *   non-proliferation compliance has held, creating a structural gap: NNWS
 *   continue to honor Article IV restraint despite WS non-compliance with
 *   Article VI. The constraint manifests as Tangled Rope from WS perspective
 *   (genuine but asymmetric security coordination), Snare from NNWS
 *   perspective (extraction without reciprocal obligation), and theater-based
 *   degradation (Piton) in the Treaty Review Conference apparatus. The core
 *   empirical question is whether the 'grand bargain' framing is a structural
 *   fact about the treaty or a committer narrative that masks one-way
 *   extraction.
 *
 * KEY AGENTS:
 *   - Non-Weapon States (NNWS / 188 parties): Primary victims (powerless/trapped) — bound by Article IV restraint indefinitely; lack enforcement mechanism for Article VI; exit costs (sanctions, security isolation) are prohibitive
 *   - Weapon States (P5 + declared WS): Primary beneficiaries (institutional/constrained) — benefit from NNWS restraint; face reciprocal verification constraints under Article VI but retain discretion on disarmament timeline; exit option is international isolation (costly but available)
 *   - Non-Aligned Movement Coalition: Organized secondary actor (organized/constrained) — collectively voice NNWS concerns; extract limited concessions via NPT Review Conferences; constrained by free-rider dynamics and great-power veto
 *   - International Atomic Energy Agency: Verification authority (institutional/arbitrage) — benefits from monopoly on NPT safeguards authority; core coordination function (inspections, data-sharing); minimal extraction experienced
 *   - NPT Review Conference Apparatus: Institutional ritual (institutional/arbitrage) — maintains treaty legitimacy through theater; reproduces disarmament language without binding change; functions as pressure-release valve preventing regime collapse
 *   - Global Disarmament Timeline: Analytical victim (analytical/analytical) — abstract collective good that bears the extraction cost of indefinite disarmament delay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.58).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.68).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV-VI Grand Bargain: Reciprocal Disarmament Obligation").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, '14719f56-39ae-4aea-b7d9-e9df47202121').
narrative_ontology:cs_kernel_codification('14719f56-39ae-4aea-b7d9-e9df47202121', fixed_text).
narrative_ontology:cs_authority_grounding('14719f56-39ae-4aea-b7d9-e9df47202121', extraction).
narrative_ontology:cs_interpretation_layer_present('14719f56-39ae-4aea-b7d9-e9df47202121').
narrative_ontology:cs_reading_relation('14719f56-39ae-4aea-b7d9-e9df47202121', nonproliferation_primary, influences).
narrative_ontology:cs_reading_relation('14719f56-39ae-4aea-b7d9-e9df47202121', abolitionist, coexists_with).
narrative_ontology:cs_axiom('14719f56-39ae-4aea-b7d9-e9df47202121', foundational, article_vi_binding_disarmament).
narrative_ontology:cs_axiom_status(article_vi_binding_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('14719f56-39ae-4aea-b7d9-e9df47202121', article_vi_binding_disarmament, deontological).
narrative_ontology:cs_axiom('14719f56-39ae-4aea-b7d9-e9df47202121', foundational, reciprocal_restraint_condition).
narrative_ontology:cs_axiom_status(reciprocal_restraint_condition, holdable).
narrative_ontology:cs_axiom_grounding('14719f56-39ae-4aea-b7d9-e9df47202121', reciprocal_restraint_condition, deontological).
narrative_ontology:cs_reference_frame('14719f56-39ae-4aea-b7d9-e9df47202121', reciprocal_disarmament_quid_pro_quo).
narrative_ontology:cs_drift_state('14719f56-39ae-4aea-b7d9-e9df47202121', contemporary_post_2020, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('14719f56-39ae-4aea-b7d9-e9df47202121', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, international_verification_regimes).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, global_disarmament_timeline).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_proliferation_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-WEAPON STATE / TRAPPED (SNARE) — Perpetually bound by Article IV non-acquisition pledge while weapon states accumulate, upgrade, and extend arsenals. Exit options are illusory: withdrawal triggers sanctions, isolation, and security cascades. Trapped in a grand bargain where the reciprocal obligation (Article VI disarmament) has no enforcement mechanism or timeline. Experiences maximal extraction — restraint without reciprocal constraint.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__grand_bargain, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ALIGNED MOVEMENT / ORGANIZED (TANGLED ROPE) — Collective voice has extracted limited concessions (indefinite extension conditioned on NPT Review Conference commitments, renewed promises of disarmament progress). But extraction persists: disarmament pledges remain unenforceable; verification mechanisms are asymmetric; NNWS collective action is constrained by free-rider dynamics and great-power pressure. Mixed: genuine coordination function (collective review process) embedded in asymmetric extraction (binding commitments vs. non-binding pledges).
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__grand_bargain, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: IAEA / VERIFICATION AUTHORITY (ROPE) — Experiences the constraint as a pure coordination mechanism: safeguards agreements enable inspections, data-sharing, and verification protocols that benefit all parties. The IAEA's extraction is minimal — the constraint enables its core function. Exit options are arbitrage: the IAEA benefits from its monopoly on NPT verification authority; withdrawal would require renegotiation of the entire inspection regime. Net beneficiary in a low-extraction coordination role.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__grand_bargain, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WEAPON STATES / P5 POWERS (TANGLED ROPE) — Genuine coordination function (mutual vulnerability reduction through transparency protocols, confidence-building measures, arms-control frameworks). But extraction is embedded: Article VI language is aspirational ('pursued in good faith') not mandatory; disarmament timelines are conditional on 'appropriate' verification, defined by the weapon states themselves. Constrained by verification reciprocity demands and escalation risks, but with far greater exit latitude (ability to withdraw with less international penalty). Mixed: genuine security coordination + asymmetric delay of reciprocal obligations.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__grand_bargain, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TREATY REVIEW CONFERENCE APPARATUS (PITON) — The NPT Review Conference every five years has devolved into theater: elaborate negotiations on 'final documents' that reproduce the same language (disarmament 'commitments,' NNWS 'concerns,' verification 'mechanisms') without binding change. Theater ratio high (0.62+): the ritual maintains legitimacy of the treaty while the substantive gap widens. The review process persists through institutional inertia, not because it produces disarmament progress. Functionally degraded coordination.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__grand_bargain, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL DISARMAMENT TIMELINE / ANALYTICAL (SNARE) — From the civilizational view, the 50-year record shows weapon states have not pursued disarmament 'in good faith' per Article VI language. Strategic parity, deterrence doctrine, and modernization programs have sustained and extended arsenals. The constraint extracts from future generations by deferring disarmament obligations indefinitely while cementing non-weapon-state restraint. The analytical observer sees pure extraction: the 'grand bargain' is honored in reciprocal non-weapon-state restraint but routinely breached by weapon states. Non-enforcement makes the extraction systematic.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__grand_bargain, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__grand_bargain, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__grand_bargain, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, TR),
    TR >= 0.70.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from NNWS through indefinite restraint without reciprocal WS obligation (measured by the 50-year stall in disarmament). But the extraction is not absolute (0.66+/Snare-threshold) because: (1) NNWS technically retain the threat of withdrawal, which constrains WS options; (2) IAEA verification benefits are genuine and some NNWS pursue peaceful nuclear programs successfully; (3) the constraint functions partially as intended (non-proliferation has held). The increase from 0.35 (1970) to 0.58 (2020) reflects accumulating extraction: the 'good faith' disarmament language has degraded as WS have continued modernization without numerical reduction. Suppression (0.68): High. Multiple mechanisms prevent NNWS exercise of Article IV exit: (1) sanctions and international isolation for withdrawal; (2) security dependencies on WS nuclear umbrella; (3) technical barriers to independent nuclear programs; (4) NPT Review Conference consensus requirements that give WS veto power. Theater ratio (0.62): Moderate-high. The NPT Review Conference has evolved into ritual: 50 years of final documents reproduce the same language ('deep concern,' 'unequivocal commitment,' 'steps forward') without binding disarmament progress. The performative content has increased as the gap between language and reality has widened. The theater masks the constraint's extraction function — the ritual maintains treaty legitimacy while the substantive obligation remains unenforced.
 *
 * PERSPECTIVAL GAP:
 *   The grand bargain reading produces a perspectival gap between WS (Tangled Rope: genuine but asymmetric security coordination, bounded exit) and NNWS (Snare: extraction without reciprocal remedy, trapped exit). WS experience the constraint as coordination (security architecture that reduces mutual vulnerability) with embedded extraction (they control disarmament timeline and verification standards). NNWS experience it as extraction with coordination veneer — the peaceful-technology access is real but perpetually conditional on non-proliferation restraint that WS do not reciprocate with disarmament. The Review Conference (Piton) hides this gap through theater. The analytical observer (Snare at civilizational scope) sees the 50-year record: disarmament commitments unmet, WS arsenals modernized, NNWS restraint perfect, and the grand bargain increasingly incoherent. This reading specifically claims the gap is structural, not observational — the grand bargain is asymmetric by design.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply across institutional contexts. Non-weapon states are victims with trapped exit (d ≈ 0.92), experiencing maximal chi via the f(d) sigmoid. Weapon states are beneficiaries with constrained exit (d ≈ 0.35-0.40), experiencing reduced chi despite high base extractiveness. The IAEA occupies an unusual position: nominally enforcing reciprocal obligations, but actually benefiting from the asymmetry (its authority derives from NNWS compliance, not WS constraint). This produces the 'verification reciprocity asymmetry' omega: IAEA inspections of NNWS are intrusive and continuous; verification of WS disarmament is declaratory and voluntary. The Organized coalition (NAM) has d ≈ 0.55-0.60, intermediate between victim and beneficiary — they have extracted concessions (extended NPT with Review Conference language) but remain systematically disadvantaged. The analytical observer at civilizational scope assigns NNWS and the disarmament timeline to the victim set, computing high d for both.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that the reading claim is: 'The Article IV-VI pairing is a GENUINE RECIPROCAL OBLIGATION that has been BREACHED by weapon states.' This reading commits to enforcement asymmetry as the problem, not the bargain itself. If Article VI were binding and enforced, the constraint would shift to Tangled Rope across all perspectives (genuine mixed coordination + extraction, but with remedies). The Snare classification at NNWS perspective and analytical scope derives from non-enforceability of Article VI, which makes NNWS restraint unreciprocated. The mandatrophy resolves by distinguishing: (1) the structural commitment (Articles IV and VI are reciprocal — this is the reading's axiom); (2) the enforcement gap (Article VI language is aspirational, not binding — this is the empirical breach); (3) the classification consequence (Snare from powerless/trapped perspective due to breach; Tangled Rope from WS/institutional perspective due to coordinated security interests that override reciprocity). The reading's coherence depends on maintaining the distinction between 'the bargain was intended as reciprocal' (commitment) and 'the bargain has failed as reciprocal' (empirical fact).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    good_faith_enforceability,
    'Is Article VI''s ''good faith'' standard for disarmament enforceable as a binding legal obligation, or merely a non-binding aspiration?',
    'International Court of Justice interpretation; comparison with other treaty language (e.g., CFE Treaty numerical commitments); analysis of state practice and opinio juris on Article VI compliance',
    'If enforceable: weapon states are in material breach; non-weapon states have legal grounds for Article IV withdrawal or expansion. If aspiration only: the constraint becomes pure Snare — extraction without legal remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(good_faith_enforceability, conceptual, 'Whether Article VI imposes binding disarmament obligation or non-binding aspiration').

omega_variable(
    verification_reciprocity_asymmetry,
    'Are verification standards and timelines symmetric between weapon-state disarmament (Article VI) and non-weapon-state non-proliferation (Article IV)?',
    'Comparative analysis of IAEA safeguards (intrusive, continuous, mandatory) vs. weapon-state verification protocols (declaratory, sporadic, voluntary); measurement of verification resource asymmetry',
    'If asymmetric: the constraint is Snare-classified from NNWS perspective (bearing reciprocal burden without reciprocal verification). If symmetric: classification shifts toward Tangled Rope (genuine mutual constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_reciprocity_asymmetry, empirical, 'Symmetry of verification obligations between Article IV and Article VI').

omega_variable(
    reading_identity_ambiguity,
    'Is the Article IV-VI pairing a genuine ''grand bargain'' (reciprocal quid pro quo) or a structural cover story masking one-way non-weapon-state restraint?',
    'Historical analysis of treaty negotiation: what obligations did weapon states believe they were undertaking? How did non-weapon states understand reciprocity? Comparison with actual disarmament progress (numerical reductions, timelines, verification) vs. treaty language.',
    'If genuinely reciprocal: the constraint is Tangled Rope (mixed coordination + asymmetric extraction, but both parties agree on the quid pro quo). If cover story: the constraint is Snare (NNWS restraint without reciprocal weapon-state obligation). This omega determines the reading''s coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Whether Article IV-VI pairing represents genuine reciprocal bargain or structural cover story').

omega_variable(
    npt_regime_sustainability,
    'Can the NPT regime persist indefinitely with non-binding Article VI language, or does breach of disarmament reciprocity eventually trigger Article IV collapse (NNWS withdrawal cascades)?',
    'Longitudinal tracking of NNWS withdrawal threats, Article IV expansion declarations, and nuclear programs motivated by NPT credibility loss. Correlation between disarmament progress and NPT compliance rates.',
    'If collapse is probable: the constraint''s classification may shift from Snare (extraction persists) to temporary instability (regime failure is foreseeable). If regime persists: Snare classification is stable (perpetual extraction without crisis).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(npt_regime_sustainability, empirical, 'Long-term sustainability of NPT regime under non-binding Article VI language').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_iv_vi_theater_1970, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0, 0.4).
narrative_ontology:measurement(npt_iv_vi_theater_1995, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 25, 0.58).
narrative_ontology:measurement(npt_iv_vi_theater_2020, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(npt_iv_vi_extract_1970, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(npt_iv_vi_extract_1995, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(npt_iv_vi_extract_2020, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt_iv_vi_suppress_1970, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(npt_iv_vi_suppress_1995, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(npt_iv_vi_suppress_2020, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, article_iv_peaceful_access_asymmetry).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, nnws_disarmament_verification_burden).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, ws_arsenal_modernization_hedge).

% DUAL FORMULATION NOTE:
% The grand bargain reading decomposes from a simpler constraint (Article IV-VI pairing exists) into two structurally distinct obligations: Article IV (peaceful access, approximately binding) and Article VI (disarmament, empirically non-binding). The sibling readings distribute enforcement and reciprocity differently: nonproliferation_primary makes Article IV binding and Article VI aspirational (ε ≈ 0.30, Rope); abolitionist makes both binding with Article VI dominant (ε ≈ 0.72, Snare). The grand bargain reading (ε ≈ 0.58) claims both are binding but Article VI has been breached, creating Tangled Rope that has degraded toward Snare from NNWS perspective. Each reading has different downstream effects on verification burden (affects_constraints entries reflect which reading's enforcement architecture is assumed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__grand_bargain, institutional, 0.38).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__grand_bargain, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
