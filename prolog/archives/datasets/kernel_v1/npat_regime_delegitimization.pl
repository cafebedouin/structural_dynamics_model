% ============================================================================
% CONSTRAINT STORY: npat_regime_delegitimization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npat_regime_delegitimization, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npat_regime_delegitimization
 *   human_readable: NPAT Regime Delegitimization and Narrative Collapse
 *   domain: institutional/epistemic
 *
 * SUMMARY:
 *   NPAT regime delegitimization describes the structural constraint created
 *   when an epistemic regime (a set of normative standards, evidence
 *   hierarchies, institutional authorities, and professional credentialing
 *   systems) undergoes coordinated narrative and institutional collapse. The
 *   regime—whether a scientific paradigm, medical evidence hierarchy,
 *   academic disciplinary standard, or policy framework—becomes the target of
 *   delegitimization attacks that destabilize foundational consensus. This
 *   constraint exhibits the full range of DR classification types depending
 *   on structural position: practitioners experience it as a Snare (trapped,
 *   career-ending institutional collapse); beneficiary institutions
 *   experience it as Rope (clearing space for alternative frameworks); the
 *   epistemic commons experiences it as extraction (suppression of dissent,
 *   narrative weaponization); and the civilizational observer risks
 *   naturalizing it as an immutable feature of knowledge succession rather
 *   than a contingent institutional contestation. The extractiveness value
 *   (0.58) reflects that delegitimization creates real asymmetric costs:
 *   practitioners lose credentials, institutional prestige, and funding;
 *   alternative regime beneficiaries capture authority, resources, and
 *   narrative power. The suppression (0.65) indicates significant barriers to
 *   exit: practitioners cannot easily transition to alternative epistemic
 *   frameworks because their entire toolkit, credential value, and
 *   professional identity are regime-specific. The theater ratio (0.68)
 *   captures that delegitimization campaigns often employ performative
 *   elements—spectacularized criticism, media narratives disconnected from
 *   detailed epistemic analysis, ritual dismantling of procedural
 *   authorities—even when underlying epistemological issues exist.
 *
 * KEY AGENTS:
 *   - Regime Practitioners: Primary victims (powerless/trapped) — career professionals whose credentials, funding, and institutional identity are constituted through the regime. Exit means professional dissolution.
 *   - Institutional Insiders: Secondary victims (moderate/identity_locked) — moderate structural exit options but identity fused with regime. Cognitive capture prevents exit despite material feasibility.
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good that bears cost of regime collapse (uncertainty, lost standards, narrative weaponization).
 *   - Institutional Defender Coalition: Mixed agent (organized/constrained) — professional associations, regime-aligned funders, institutional leadership. Both coordinate to defend regime AND benefit from extracted value (turf consolidation, funding control).
 *   - Alternative Regime Beneficiaries: Primary beneficiary (institutional/arbitrage) — funding agencies, policy makers, alternative epistemic institutions that capture authority and resources from regime's collapse.
 *   - Independent Researchers: Mixed agent (powerful/mobile) — access to multiple frameworks, mobile funding, but face uncertainty during transition.
 *   - Procedural Authorities: Degraded agent (institutional/arbitrage, Piton) — editorial boards, accreditation agencies, funding councils that maintain formal regime authority through ritual even as epistemic foundation erodes.
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional contestation as immutable feature of regime succession
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npat_regime_delegitimization, 0.58).
domain_priors:suppression_score(npat_regime_delegitimization, 0.65).
domain_priors:theater_ratio(npat_regime_delegitimization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npat_regime_delegitimization, extractiveness, 0.58).
narrative_ontology:constraint_metric(npat_regime_delegitimization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(npat_regime_delegitimization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npat_regime_delegitimization, tangled_rope).
narrative_ontology:human_readable(npat_regime_delegitimization, "NPAT Regime Delegitimization and Narrative Collapse").
narrative_ontology:topic_domain(npat_regime_delegitimization, "institutional/epistemic").

domain_priors:requires_active_enforcement(npat_regime_delegitimization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npat_regime_delegitimization, alternative_epistemic_regime).
narrative_ontology:constraint_beneficiary(npat_regime_delegitimization, institutional_challengers).
narrative_ontology:constraint_victim(npat_regime_delegitimization, regime_practitioners).
narrative_ontology:constraint_victim(npat_regime_delegitimization, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIME PRACTITIONER (SNARE) — Professionals and researchers whose careers, credentials, and reputation are constituted through the regime face maximum extraction. Abandoning the regime means losing institutional identity, funding streams, and professional standing. No viable alternative exists within their original framework. The regime's delegitimization traps them: staying within a collapsing institution carries reputational cost; exiting means professional death.
constraint_indexing:constraint_classification(npat_regime_delegitimization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL INSIDER / IDENTITY-LOCKED (SNARE) — Agents with moderate structural options (funding diversification, lateral career moves) but whose identity is fused with the regime experience maximum extraction via cognitive capture. The regime's delegitimization assaults their self-concept. Exiting would require becoming a different person — abandoning professional identity, ideological commitment, or fused relational identity with institutional community. High suppression despite moderate exit optionality.
constraint_indexing:constraint_classification(npat_regime_delegitimization, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL DEFENDER COALITION (TANGLED ROPE) — Organized groups (professional associations, institutional leadership, regime-aligned funders) benefit from the regime's persistence AND coordinate to defend it. They experience both genuine coordination (maintaining epistemic standards, funding allocation) and asymmetric extraction (power consolidation, narrative control). Active enforcement is required to maintain regime authority.
constraint_indexing:constraint_classification(npat_regime_delegitimization, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ALTERNATIVE REGIME BENEFICIARY (ROPE) — Institutional actors (funding agencies, policy makers, alternative institutions) who benefit from the regime's delegitimization experience it as pure coordination: clearing intellectual space for new paradigms, capturing funding streams redirected from the failing regime, establishing new evidence hierarchies. Net beneficiary with maximum arbitrage capacity.
constraint_indexing:constraint_classification(npat_regime_delegitimization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDEPENDENT RESEARCHER (TANGLED ROPE) — Powerful agents with mobility (tenure, independent funding, cross-disciplinary credentials) experience mixed dynamics. The regime's delegitimization creates opportunities (new research directions, grants) and threats (loss of established standards, epistemic chaos). Both genuine coordination and extractive pressure operate — from narrative collapse uncertainty and from incumbent regime actors defending turf.
constraint_indexing:constraint_classification(npat_regime_delegitimization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PROCEDURAL AUTHORITY (PITON) — Institutional bodies (editorial boards, accreditation agencies, funding councils) that formally maintain regime standards perform largely ritually as the regime's epistemic foundation erodes. The procedures persist through institutional inertia — journals continue peer review, accreditors continue credentialing — but the underlying legitimacy has degraded. Theater ratio remains high even as functional authority declines.
constraint_indexing:constraint_classification(npat_regime_delegitimization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational/universal perspective, epistemic regime succession is an immutable feature of knowledge systems: all regimes eventually delegitimize, new paradigms emerge, institutional turnover is inevitable. This view sees the NPAT constraint as a natural law of institutional dynamics. However, the structural data contradicts this — identifiable beneficiaries, coordinated delegitimization attacks, and extraction mechanisms reveal this as a false summit naturalizing contingent institutional contestation.
constraint_indexing:constraint_classification(npat_regime_delegitimization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npat_regime_delegitimization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npat_regime_delegitimization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npat_regime_delegitimization, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(npat_regime_delegitimization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(npat_regime_delegitimization, TR),
    TR >= 0.70.

:- end_tests(npat_regime_delegitimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The measurement trajectory (0.32 → 0.48 → 0.58) shows rising extraction as delegitimization accelerates. Initial phase (t=0): regime challenges are epistemically grounded and distributional (some agents benefit from paradigm shift, costs are real but not existential). Mid-phase (t=3): alternative regime consolidates power and begins capturing resources, creating asymmetry where practitioners face institutional loss while beneficiaries gain authority. Final phase (t=6): extraction peaks as regime institutional infrastructure crumbles but practitioners remain trapped. Extractiveness plateaus at 0.58 rather than spiking to 0.70+ because regime collapse is also genuine institutional transition (not pure extraction) — beneficiaries experience legitimate epistemic coordination benefits, not purely extractive gain. The regime's institutional death is real; the extraction lies in the asymmetry of transition costs. Suppression (0.65): High. Multiple mechanisms operate: (1) Structural barriers—practitioners cannot easily acquire alternative credentials, funding agencies bias toward new regime, institutional gatekeepers prevent regime defense. (2) Narrative suppression—delegitimization campaigns limit space for regime-consistent claims in public discourse. (3) Internalized suppression—practitioners internalize the regime's delegitimization narrative and self-silence (identity_locked dynamic). (4) Institutional suppression—regime-aligned institutions are defunded, editorial policies shift against regime-affiliated authors, publication bias emerges. The 0.65 value reflects that exit barriers are high but not absolute — some practitioners can transition (constrained rather than trapped), some alternative perspectives find platforms, some institutional defenders maintain residual authority. Theater ratio (0.68): Moderate-high and rising (0.52 → 0.62 → 0.68). Initial delegitimization often employs detailed epistemic critique (lower theater), but as contestation intensifies, media narratives, spectacularized criticisms, and ritual authority-stripping dominate (higher theater). Procedural authorities (journals, accreditors) maintain formal procedures but the underlying epistemic authority has degraded — reviews continue but lack meaningful gatekeeping function (piton classification). Some genuine epistemic work occurs (lower theater component), but the delegitimization campaign's dominant mechanism is narrative and institutional, not analytical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The regime practitioner sees a Snare (career destruction, institutional collapse, no exit). The institutional insider sees a Snare via identity lock (moderate structural options blocked by cognitive capture and identity fusion). The defender coalition sees a Tangled Rope (both defending epistemic standards AND consolidating power). The alternative regime beneficiary sees a Rope (pure coordination — clearing space for new paradigms). The independent researcher sees a Tangled Rope (opportunities and threats coexist). The procedural authority sees a Piton (performing regime authority through ritual despite epistemic degradation). The analytical observer risks seeing a Mountain (regime succession is inevitable)—but the structural data reveals this as a false summit. The perspectival gaps are not measurement artifacts; they reflect real differences in experienced extractiveness determined by beneficiary/victim status and exit optionality. A practitioner with zero exit options (d → 1.0) experiences high chi; a beneficiary with arbitrage options (d → 0.0) experiences negative chi (benefit). The same structural constraint produces opposite experienced outcomes based on position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's beneficiary/victim status, power level, and exit options. Regime practitioners: victim status + powerless + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extractiveness. Institutional insiders: victim status + moderate + identity_locked → d ≈ 0.89 → f(d) ≈ 1.28 → high experienced extractiveness despite moderate structural exit options (cognitive lock-in raises effective d). Defender coalition: mixed beneficiary/victim + organized + constrained → d ≈ 0.55 → f(d) ≈ 0.75 → moderate extraction; beneficiary component lowers d relative to pure victim. Alternative regime beneficiary: beneficiary status + institutional + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 → negative chi (net benefit). Independent researcher: both + powerful + mobile → d ≈ 0.48 → f(d) ≈ 0.60 → moderate experienced extractiveness despite powerful status (mobile exit reduces impact of extraction mechanisms). Procedural authority: beneficiary (maintains formal authority) + institutional + arbitrage → low d → low/negative chi; piton classification derives from high theater ratio, not from experienced extraction. Analytical observer: observer position → d ≈ 0.72 → f(d) ≈ 1.15 → moderate analytical extractiveness; the observer's native instruments (regime-consistent reasoning) are compromised by the oracle gap (Theorem 4) during regime transition. No directionality overrides required — the derivation chain produces accurate d values from the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY NOT FULLY RESOLVED — This constraint sits at extractiveness 0.58, above the Tangled Rope/Snare boundary (0.46), but below the terminal mandatrophy threshold (0.70). The constraint resolves partial mandatrophy through the seven-perspective structure: Tangled Rope is validated as the claimed type because genuine coordination (institutional succession, epistemic reorganization, resource allocation) coexists with asymmetric extraction (practitioner dispossession, commons suppression, narrative control). The constraint is NOT a disguised Snare because beneficiaries experience real coordination function — they are not merely extracting value but rebuilding epistemic institutions. The constraint is NOT a pure Rope because victims experience material harm with suppression mechanisms (institutional barriers, narrative suppression, cognitive lock-in). The Tangled Rope classification holds across the empirical interval: suppression (0.65) exceeds the snare threshold (0.60) but coordination function remains present (beneficiaries consolidate authority through institutional work, not pure coercion). REMAINING OMEGA: The unresolved mandatrophy lies in the regime epistemic validity question (omega_regime_epistemic_validity). If the delegitimization is justified by genuine epistemic failure, the constraint is adaptive institutional transition (Rope ascends). If it is coordinated attack on sound frameworks, the constraint is extractive replacement (Snare dominates). If mixed, the constraint's true type is the proportional mix. The analytical observer's classification as mountain is a false summit candidate, triggered by attempted naturalization. The constraint's extractiveness will likely trend upward (toward 0.70+) during the interval 6-12 if regime collapse accelerates without alternative regime consolidation (epistemic commons vacuum intensifies extraction). If alternative regime stabilizes institutional authority during this interval, extractiveness may plateau or decline as beneficiaries shift from pure capture to genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_epistemic_validity,
    'Is the delegitimization driven by genuine epistemic failure of the regime''s core claims, or by coordinated narrative attacks on institutionally sound frameworks?',
    'Independent evaluation of the regime''s empirical track record, predictive capacity, and anomaly resolution vs. alternative frameworks; analysis of attack timing relative to epistemic events vs. institutional opportunities',
    'If driven by epistemic failure: the delegitimization is justified regime succession (Rope from many perspectives). If driven by coordinated attacks on sound frameworks: the delegitimization is extractive constraint (Snare/Tangled Rope strengthened). If mixed: the constraint''s classification reflects the proportional mix.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_epistemic_validity, empirical, 'Whether delegitimization reflects genuine epistemic failure or coordinated narrative attacks').

omega_variable(
    alternative_regime_credibility,
    'Does the alternative regime possess comparable epistemic rigor, empirical track record, and institutional capacity to the displaced regime?',
    'Comparative evaluation of prediction accuracy, anomaly resolution rate, institutional stability, and evidence standards between incumbent and alternative regimes; analysis of whether benefits flow from superior epistemic performance or from power consolidation',
    'If alternative has superior epistemic profile: the constraint is adaptive (Rope at collective level). If alternative has inferior profile: the constraint is extraction masquerading as progress (Snare/Tangled Rope intensified). If comparable but different: the constraint is pure institutional competition (Rope disguised as Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_regime_credibility, empirical, 'Comparative epistemic performance of alternative vs incumbent regime').

omega_variable(
    suppression_mechanism_internalization,
    'What proportion of observed suppression is structural (institutional barriers, funding cuts, publication bias) vs. internalized (practitioners'' cognitive lock-in, identity fusion, epistemic closure)?',
    'Post-exit trajectory analysis: do practitioners who leave the regime retain internalized suppression (adopt regime-consistent reasoning frameworks, defend regime concepts)? Do structural barriers persist after regime''s formal collapse, or do they dissolve?',
    'If primarily structural: suppression decays after regime institutional collapse. If primarily internalized: suppression persists in practitioner behavior, identity, and epistemic choices years after institutional exit. High internalization suggests identity_locked classification is accurate; low internalization suggests trapped/constrained are more appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Proportion of suppression that is structural vs. internalized').

omega_variable(
    narrative_weaponization_coordination,
    'Are delegitimization narratives emergent from distributed criticism, or are they coordinated attacks orchestrated by alternative regime beneficiaries?',
    'Temporal analysis of narrative emergence: do attack patterns show centralized coordination (funding source, strategic timing aligned with institutional opportunities) or distributed organic emergence? Network analysis of narrative propagation vectors and source institutions.',
    'If emergent/distributed: delegitimization reflects genuine epistemic consensus shift (Rope/Mountain from powerful perspective). If coordinated: delegitimization is extraction mechanism requiring active enforcement (Tangled Rope/Snare strengthened). Coordination evidence strengthens beneficiary declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_weaponization_coordination, empirical, 'Whether delegitimization narratives are coordinated or emergent').

omega_variable(
    regime_succession_reversibility,
    'Is the delegitimization process reversible — can the regime''s epistemic authority be restored if empirical or institutional conditions change?',
    'Historical analysis of regime reversals; evaluation of whether regime practitioners retain capacity to re-establish authority, whether institutional infrastructure is preserved or permanently dismantled, whether alternative regime has sufficient lock-in to prevent reversal',
    'If reversible: the delegitimization constraint is temporary (Scaffold trajectory). If irreversible: the constraint operates as regime termination (Snare/Tangled Rope to terminal state). If reversibility depends on external conditions: the constraint is contingent on political/institutional factors (affects classification sensitivity to context).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_succession_reversibility, empirical, 'Whether regime delegitimization is reversible or terminal').

omega_variable(
    epistemic_commons_recovery_lag,
    'What is the lag time between regime delegitimization and stabilization of a new epistemic commons — during which period no authoritative evidence hierarchy exists?',
    'Longitudinal measurement of epistemic uncertainty during transition: publication retraction rates, citation entropy, funding allocation volatility, institutional credibility surveys during delegitimization interval vs. post-stabilization',
    'Short lag (< 2 years): the epistemic commons is resilient; alternative regime rapidly provides organizational structure. Long lag (> 10 years): the epistemic commons experiences extended chaos; suppression and extraction intensify during the vacuum. Lag duration directly affects the constraint''s magnitude and duration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_commons_recovery_lag, empirical, 'Duration of epistemic uncertainty gap during regime transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npat_regime_delegitimization, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npat_tr_t0, npat_regime_delegitimization, theater_ratio, 0, 0.52).
narrative_ontology:measurement(npat_tr_t3, npat_regime_delegitimization, theater_ratio, 3, 0.62).
narrative_ontology:measurement(npat_tr_t6, npat_regime_delegitimization, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(npat_be_t0, npat_regime_delegitimization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(npat_be_t3, npat_regime_delegitimization, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(npat_be_t6, npat_regime_delegitimization, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npat_su_t0, npat_regime_delegitimization, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(npat_su_t3, npat_regime_delegitimization, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(npat_su_t6, npat_regime_delegitimization, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npat_regime_delegitimization, resource_allocation).
narrative_ontology:affects_constraint(npat_regime_delegitimization, institutional_legitimacy_crisis).
narrative_ontology:affects_constraint(npat_regime_delegitimization, practitioner_credential_collapse).
narrative_ontology:affects_constraint(npat_regime_delegitimization, epistemic_commons_fragmentation).

% DUAL FORMULATION NOTE:
% NPAT regime delegitimization is part of a constraint family decomposed from the general institutional authority collapse. The parent constraint (institutional_legitimacy_crisis) has higher-level institutional dynamics; this constraint focuses on the specific extractive mechanisms operating on individual practitioners and the epistemic commons. The downstream constraint (practitioner_credential_collapse) isolates the career-specific extraction; epistemic_commons_fragmentation isolates the commons-level extraction. Each story in the family has its own ε value reflecting the specific observable: regime delegitimization focuses on narrative and institutional mechanisms (ε=0.58); practitioner collapse focuses on career dispossession (ε higher, closer to 0.72); commons fragmentation focuses on loss of standards (ε moderate, near 0.40). All three are linked by network.affects_constraints. Do not merge them — the ε-invariance principle requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npat_regime_delegitimization, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
