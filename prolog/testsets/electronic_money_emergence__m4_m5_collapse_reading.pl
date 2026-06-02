% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: Electronic Money Emergence as M4/M5 Statistical Collapse (Piton Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   In the 1980s-90s, the empirical relationship between the money supply and
 *   inflation — the foundational assumption of monetarism — broke down. M1
 *   and M2 stopped reliably predicting inflation. Rather than acknowledging
 *   theoretical crisis, central banks retroactively invented new measurement
 *   categories (M4, M5, and variants across jurisdictions) whose boundaries
 *   were explicitly constructed to produce aggregates that 'better explained'
 *   inflation dynamics. This constraint models that the retroactive creation
 *   of the M4/M5 distinction itself instantiated the category of 'electronic
 *   money' — the boundary was not discovered but invented to rescue a failing
 *   measurement regime. The reading you are instantiating is the M4/M5
 *   Collapse reading: the distinction is a measurement artifact (Piton) whose
 *   persistence depends on institutional inertia and suppression of
 *   alternative theoretical frameworks (Snare from the powerless
 *   perspective). Sibling readings — the 'became thinkable' reading and the
 *   'first held' reading — model the constraint as an epistemic breakthrough
 *   (genuine emergence of a new category) and as an empirical discovery
 *   (electronic money always existed, we just learned to measure it). This
 *   reading forecloses both: no genuine emergence occurred, no empirical
 *   discovery was made. What happened was measurement regime collapse and
 *   retroactive redefinition.
 *
 * KEY AGENTS:
 *   - Central Banking Authority (institutional/arbitrage): Primary beneficiary — maintains policy legitimacy through measurement flexibility and retroactive regime change; experiences constraint as pure coordination
 *   - Monetary Measurement Regime (institutional/arbitrage): Secondary beneficiary — the apparatus itself is the beneficiary; sustains institutional authority through boundary maintenance
 *   - Heterodox Economists (powerless/trapped and powerless/identity_locked): Primary victims — their theoretical frameworks are retroactively invalidated; lack exit options (career, publication venues, funding all depend on acceptance of new regime)
 *   - Policy Makers (moderate/constrained): Secondary victims — dependent on central bank measurement categories for policy guidance; constrained by opacity of regime change rationale
 *   - Monetary Theory Coherence (powerless/trapped): Abstract victim — the coherence of monetary theory as a discipline is fragmented across incompatible frameworks (Keynesian, Monetarist, Post-Keynesian, Modern Monetary Theory) each with different assumptions about what counts as money; no framework can coherently accommodate retroactive redefinition
 *   - Analytical Observer (analytical/analytical): Sees the constraint as either a natural feature of electronic payment systems (false summit) or as a constructed measurement artifact maintained through institutional inertia (accurate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.58).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.65).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, snare).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "Electronic Money Emergence as M4/M5 Statistical Collapse (Piton Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__m4_m5_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '11bfa5a7-83ad-433a-9318-e98621ecbc15').
narrative_ontology:cs_kernel_codification('11bfa5a7-83ad-433a-9318-e98621ecbc15', formalized).
narrative_ontology:cs_authority_grounding('11bfa5a7-83ad-433a-9318-e98621ecbc15', extraction).
narrative_ontology:cs_interpretation_layer_present('11bfa5a7-83ad-433a-9318-e98621ecbc15').
narrative_ontology:cs_reading_relation('11bfa5a7-83ad-433a-9318-e98621ecbc15', electronic_money_emergence__became_thinkable_reading, forecloses).
narrative_ontology:cs_reading_relation('11bfa5a7-83ad-433a-9318-e98621ecbc15', electronic_money_emergence__first_held_reading, forecloses).
narrative_ontology:cs_axiom('11bfa5a7-83ad-433a-9318-e98621ecbc15', foundational, m4_m5_boundary_post_hoc_construction).
narrative_ontology:cs_axiom_status(m4_m5_boundary_post_hoc_construction, holdable).
narrative_ontology:cs_axiom_grounding('11bfa5a7-83ad-433a-9318-e98621ecbc15', m4_m5_boundary_post_hoc_construction, empirically_contingent).
narrative_ontology:cs_axiom('11bfa5a7-83ad-433a-9318-e98621ecbc15', foundational, measurement_regime_collapse_as_extraction_mechanism).
narrative_ontology:cs_axiom_status(measurement_regime_collapse_as_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('11bfa5a7-83ad-433a-9318-e98621ecbc15', measurement_regime_collapse_as_extraction_mechanism, instrumental).
narrative_ontology:cs_reference_frame('11bfa5a7-83ad-433a-9318-e98621ecbc15', monetarist_consensus_crisis).
narrative_ontology:cs_drift_state('11bfa5a7-83ad-433a-9318-e98621ecbc15', contemporary_post_consensus_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('11bfa5a7-83ad-433a-9318-e98621ecbc15', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_banking_authority).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_measurement_regime).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, monetary_theory_coherence).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, policy_makers).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, heterodox_economists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HETERODOX ECONOMIST (SNARE) — Trapped within a measurement regime that retroactively erases the validity of their theoretical frameworks. The M4/M5 distinction was invented to resolve an empirical crisis (the money supply aggregates ceased predicting inflation), but its retroactive application delegitimizes prior analysis. No exit: accepting the new regime means abandoning prior work; rejecting it means being excluded from policy-relevant discourse. Maximum extraction with no coordination benefit.
constraint_indexing:constraint_classification(electronic_money_emergence__m4_m5_collapse_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CENTRAL BANK TECHNICIAN (TANGLED ROPE) — Constrained by employment and expertise dependence on the regime they maintain, but also genuinely benefits from the clarifying function the new measurement boundaries provide. The regime solves a real coordination problem (what counts as money?) while extracting from those whose frameworks it retroactively invalidates. Significant suppression through career dependence and methodological closure, but also genuine coordination value.
constraint_indexing:constraint_classification(electronic_money_emergence__m4_m5_collapse_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANKING AUTHORITY (ROPE) — Primary beneficiary. The M4/M5 distinction allows the authority to claim that inflation targeting has 'worked' (by redefining which aggregates matter), and to maintain policy regime legitimacy through measurement flexibility. Experiences the constraint as a pure coordination mechanism for maintaining institutional authority — the distinction enables smooth policy narrative without requiring fundamental reform.
constraint_indexing:constraint_classification(electronic_money_emergence__m4_m5_collapse_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MONETARY MEASUREMENT APPARATUS (PITON) — The M4/M5 distinction persists as institutional ritual despite having been constructed specifically to resolve the breakdown of the prior regime's empirical predictions. The theater ratio is high: enormous effort is spent maintaining and defending measurement categories (M0, M1, M2, M3, M4, M5) whose boundary rationales have eroded as theoretical frameworks have diverged. The distinction was functional in the 1980s-90s (coordinating policy under monetarism); now it persists through inertia, defended through increasingly complex narratives about 'broad money' that satisfy no theoretical framework completely.
constraint_indexing:constraint_classification(electronic_money_emergence__m4_m5_collapse_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the M4/M5 distinction might appear to capture an immutable feature of modern monetary systems: the distinction between 'narrow' and 'broad' money reflects a genuine structural boundary in how electronic payment systems work. Money is 'just' whatever resolves transactions; the boundary between electronic and non-electronic is 'just' a technological fact. This perspective risks naturalizing what is actually a constructed measurement convention that was retroactively imposed to rescue a failing theoretical framework.
constraint_indexing:constraint_classification(electronic_money_emergence__m4_m5_collapse_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: POST-STRUCTURALIST MONETARY THEORIST (SNARE / IDENTITY_LOCKED) — A more severe variant of the heterodox trap. This theorist's professional identity has been constructed through critique of the prior monetary aggregate framework (M1-M3) — their career capital and epistemic authority are fused with that critique. The retroactive M4/M5 innovation does not just invalidate their prior work; it invalidates their identity as a theorist. Even if they recognize the regime change as extractive, exit would require abandoning their identity as a monetary theorist entirely. Structurally mobile (could switch fields, adopt new frameworks) but identity-locked (doing so would dissolve who they have become in the discipline). This instantiates the oracle gap: recognition of the trap does not equal freedom from it.
constraint_indexing:constraint_classification(electronic_money_emergence__m4_m5_collapse_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(electronic_money_emergence__m4_m5_collapse_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electronic_money_emergence__m4_m5_collapse_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, TR),
    TR >= 0.70.

:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The central banking authority extracts significant benefit from the regime change — it allows them to claim monetary policy 'works' (by choosing which aggregates to measure) while avoiding fundamental theoretical or institutional reform. The extraction is not as severe as a pure Snare (0.72) because some genuine coordination value exists: any monetary system must draw some boundary between what counts as money and what doesn't. The high value reflects that the particular boundaries chosen were constructed post-hoc to rescue a failing framework, not discovered empirically. Suppression (0.65): High. The regime is actively defended through institutional closure: heterodox frameworks are excluded from policy-relevant discourse; alternative measurement approaches (cryptographic, decentralized, asset-based) are marginalized; career incentives punish those who question the boundaries. Suppression is not total (academic heterodoxy persists in some institutions) but sufficient to prevent meaningful challenge to the regime. Theater ratio (0.78): High. The temporal trajectory shows increasing theater over the interval. In the 1980s (theater=0.55), the regime change was justified as technical response to empirical failure — there was genuine substantive debate about what aggregates matter. By the 2000s (theater=0.78), the boundaries are defended largely through institutional ritual. Central banks publish detailed M1-M5 statistics weekly despite the distinctions having lost predictive validity; policy discourse invokes the categories without examining their foundations; academic papers treat the boundaries as given rather than constructed. The ritual persists because the institutional apparatus depends on it, not because it solves the underlying problem.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival gap because it involves a retroactive regime change that benefits one agent at the expense of others' epistemic coherence. The central banking authority (Rope perspective) experiences the M4/M5 distinction as coordination — a useful boundary that clarifies policy space. The heterodox economist (Snare/identity_locked perspective) experiences the same distinction as extraction and epistemic erasure — their prior theoretical work is retroactively invalidated. The piton perspective reveals that the institution itself has degraded into ritual: enormous effort maintaining categories whose rationale has eroded. The mountain perspective risks naturalizing the regime by treating the M4/M5 boundary as inherent to electronic payment systems — this reading forecloses that interpretation. The gap exists because the regime change was not the result of consensus-building or empirical discovery; it was imposed by institutional authority to rescue its own legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations. The central banking authority benefits from the regime (d ≈ 0.05, low extraction experienced). Heterodox economists are victims without exit options (d ≈ 0.95, maximum extraction experienced). This produces a perspectival gap: the same constraint (ε=0.58) produces different chi values for different observers. The beneficiary with arbitrage options derives d from their structural mobility and benefit flow — they can exit by adopting alternative measurement approaches, but doing so costs them policy relevance, so they stay (arbitrage exit option). The victim with identity lock derives d from their inability to exit even though structurally mobile — they could adopt new theoretical frameworks or switch disciplines, but doing so would dissolve their identity in the field of monetary economics. This combination (victim+identity_locked) produces high d, revealing that the constraint binds primarily through internalized identity rather than external material barriers. Overrides: none declared — the automatic derivation captures the structure accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by foregrounding that the classification depends entirely on which reading of the kernel is correct. If the 'became thinkable' or 'first held' readings are true, the constraint approaches Rope — a genuine coordination innovation. If this reading (m4_m5_collapse) is true, the constraint is Snare/Piton hybrid — extraction through institutional regime change and theater. The mandatrophy is not 'which type is the right one?' but 'which reading of the kernel's genesis is correct?' The structural data support this reading: the measurements show rising theater_ratio and rising suppression_requirement as the regime solidified, consistent with a piton trajectory (functional regime degrading into ritual). The omega variables identify the empirical and conceptual questions that would distinguish this reading from its siblings. The analytical observer's mountain perspective represents the false summit: treating the M4/M5 boundary as 'just how electronic money works' naturalizes what is actually a constructed measurement convention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_boundary_vs_constructed_distinction,
    'Is the M4/M5 boundary capturing a genuine empirical distinction in monetary dynamics, or is it a retrospective classification imposed to resolve theoretical crisis?',
    'Historical analysis of the distinction''s invention: was the boundary derived from data patterns discovered independently, or was it constructed specifically to accommodate post-hoc the failure of M1-M3 to predict inflation? Cross-sectional analysis of whether the M4/M5 boundary maps onto behavioral monetary phenomena (transaction propensities, velocity patterns, policy effectiveness) or merely onto institutional categorization decisions.',
    'If empirical: the constraint is coordinate-system-dependent but corresponds to underlying structure — classification as Rope is defensible. If constructed: the boundary is a measurement artifact maintained through convention — Piton classification is justified, and the snare reading is correct that the regime extracts from theorists whose frameworks it retroactively invalidates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_boundary_vs_constructed_distinction, empirical, 'Whether M4/M5 distinction is empirically grounded or constructed post-hoc').

omega_variable(
    retroactive_regime_change_legitimacy,
    'What constitutes legitimate grounds for retroactively redefining measurement categories to rescue a theoretical framework?',
    'Normative analysis of scientific practice: when does measurement innovation become extractive redefinition? Comparative analysis of other historical regime changes (the shift from caloric to thermodynamic frameworks, from Ptolemaic to Heliocentric astronomy, from Newtonian to Einsteinian mechanics) and what made them acceptable vs. extractive.',
    'If retroactive redefinition is legitimate when empirically motivated: the heterodox economists'' loss of epistemic authority is regrettable but acceptable cost of progress. If retroactive redefinition is extractive unless prior frameworks are explicitly acknowledged as still-valid competing readings: central banking authority has enacted a snare by erasing the legitimacy of prior theorists. This omega is partially conceptual, partially empirical — empirical reconstruction of the regime change''s history, normative judgment of its legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retroactive_regime_change_legitimacy, conceptual, 'Whether retroactive measurement redefinition to rescue failing frameworks is legitimate innovation or extractive regime change').

omega_variable(
    measurement_convention_immunity_to_falsification,
    'Can a measurement convention be falsified, or does its institutional adoption make it definitionally true?',
    'Philosophy of science analysis: examination of whether the M4/M5 distinction could in principle be falsified (shown to be wrong by evidence) or whether its adoption as institutional standard makes it logically immune to falsification. Comparison with other measurement conventions (SI units, inflation baskets, unemployment definitions) and how they have responded to empirical challenge.',
    'If measurement conventions are falsifiable: the M4/M5 distinction is subject to empirical test; failure to improve policy outcomes should trigger revision or abandonment. If they are immune to falsification by adoption: the distinction persists through institutional inertia regardless of empirical performance — Piton classification is correct. This determines whether the theater ratio (0.78) can decrease if empirical performance improves, or whether it is structurally locked at high values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_convention_immunity_to_falsification, conceptual, 'Whether measurement conventions can be empirically falsified or are immune to falsification by institutional adoption').

omega_variable(
    identity_lock_mechanism_in_academic_economics,
    'To what extent is the heterodox economist''s inability to exit the regime due to identity fusion with pre-regime theoretical frameworks versus rational material incentives (career, funding, publication venues)?',
    'Cognitive science analysis: interview and discourse analysis of heterodox economists to identify whether perceived constraints are structural (job market hostility, funding exclusion, publication barriers) or internalized (belief that acceptance of new regime constitutes intellectual betrayal, identity dissolution in the discipline). Comparison of exit rates among theorists with high vs. low identity fusion with prior frameworks.',
    'If material constraints dominate: classify as trapped, not identity_locked; the heterodox economist has real material barriers that could be addressed by structural reform. If identity fusion dominates: classify as identity_locked; exit would require the theorist to become a different kind of scholar, and this is genuinely beyond what material reform can address. Affects which intervention pathways (career protection, publication access) would actually reduce the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_academic_economics, empirical, 'Whether heterodox economists'' inability to exit is material or identity-based').

omega_variable(
    m4_m5_boundary_stability,
    'Is the M4/M5 boundary itself stable, or does it show signs of further fragmentation under stress?',
    'Historical trend analysis: documentation of whether central banks have modified, subdivided, or supplemented the M4/M5 categories since their introduction. Examination of whether alternative monetary aggregates (crypto, stablecoins, overnight repo markets) are forcing boundary redefinitions. Projection of whether the digital currency transition will render the M4/M5 distinction obsolete within 10-20 years.',
    'If stable: the piton reading is solid — the regime persists through inertia. If unstable: the regime may be approaching another phase transition, at which point the current beneficiaries (central banking authority) and victims (heterodox theorists) may need to renegotiate. This affects whether the snare persists indefinitely or is transitional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(m4_m5_boundary_stability, empirical, 'Stability of the M4/M5 boundary under technological and theoretical pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(em_m4m5_theater_1980s, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(em_m4m5_theater_1990s, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 10, 0.68).
narrative_ontology:measurement(em_m4m5_theater_2000s, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 20, 0.78).

% Extraction over time
narrative_ontology:measurement(em_m4m5_extract_1980s, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(em_m4m5_extract_1990s, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(em_m4m5_extract_2000s, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(em_m4m5_suppress_1980s, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(em_m4m5_suppress_1990s, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(em_m4m5_suppress_2000s, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__m4_m5_collapse_reading, 0.08).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, monetary_policy_effectiveness__inflation_targeting_regime).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, central_bank_measurement_authority__institutional_closure).

% DUAL FORMULATION NOTE:
% The electronic_money_emergence kernel decomposes into three readings with different ε values and structural properties. The m4_m5_collapse_reading (this story) treats the M4/M5 distinction as a piton artifact of institutional regime collapse. Sibling readings model the emergence as genuine epistemic breakthrough (became_thinkable_reading, lower ε) or as empirical discovery (first_held_reading, lower ε). All three readings are linked through the same kernel but have different beneficiary/victim structures and classifications. The network edges indicate that this reading directly influences sibling readings: if the collapse reading is correct, both sibling readings require substantial empirical work to sustain their narratives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
