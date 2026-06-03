% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Marriage Commitment Reversal via Endogenous Divine Revelation (Woodruff's 1890 Vision)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   In 1890, Wilford Woodruff, president of The Church of Jesus Christ of
 *   Latter-day Saints, issued an announcement framed as a divine revelation
 *   that suspended the church's public practice of plural marriage while
 *   formally preserving the doctrine (D&C 132) in the canonical text. This
 *   constraint models the structural consequence of reversing a constitutive
 *   practice through endogenous divine revelation — a reinterpretation of
 *   God's will presented as internal to the faith tradition rather than
 *   imposed by external coercion. The constraint manifests as a tangled
 *   hybrid: genuine coordination function (institutional survival during
 *   federal persecution requires a mechanism to exit practice while
 *   preserving authority) AND asymmetric extraction (the membership's
 *   doctrinal ground is erased while the leadership's interpretive authority
 *   is preserved). The extractiveness value (0.48) reflects moderate
 *   extraction that is legitimated through revelation framing rather than
 *   acknowledged as political necessity. The suppression value (0.62)
 *   reflects that the practice reversal is enforced through doctrinal
 *   authority — members are suppressed not by legal force alone but by
 *   theological delegitimization. The theater ratio (0.68) reflects that
 *   Section 132 is preserved as a doctrinal text but functions performatively
 *   — it is cited for institutional legitimacy and theological argument but
 *   stripped of prescriptive force over actual practice. This reading models
 *   the constraint AS IT APPEARS FROM WITHIN THE ENDOGENOUS REINTERPRETATION
 *   FRAME — the revelation is taken as genuine divine communication that
 *   explains the practice reversal. The sibling readings (exogenous_override
 *   and practice_doctrine_gap) model alternative framings that would produce
 *   different structural analyses and different ε values.
 *
 * KEY AGENTS:
 *   - Wilford Woodruff (Institutional Leadership): President of church, author of revelation announcement. Primary beneficiary (institutional/arbitrage) — preserves interpretive authority and institutional legitimacy while solving political crisis via revelation framing.
 *   - Affected Membership (Powerless/Identity-Locked): Primarily women and men in existing plural marriages; members who had internalized Section 132 as eternal covenant. Primary victims — face practice reversal without doctrinal explanation; identity-locked because exiting the church dissolves their identity frame entirely.
 *   - Theological Consistency (Abstract Principle): The epistemic integrity of the doctrinal system. Victim set — the unresolved contradiction (why did God's will change?) burdens the theological legitimacy structure.
 *   - Federal Government (Exogenous Pressure Agent): Not a perspective in this story, but the structural antagonist. External coercion drives the political necessity for practice reversal.
 *   - Section 132 Doctrine (Textual Artifact): The canonical teaching on plural marriage as eternal covenant. Neither pure beneficiary nor victim — its preservation as doctrine serves institutional legitimacy while its suspension as practice obscures the extraction mechanism.
 *   - Analytical Observer (Civilizational): Sees the full structure — coordination function (institutional survival) nested inside asymmetric extraction (authority preservation at the cost of theological consistency).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.48).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.62).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Marriage Commitment Reversal via Endogenous Divine Revelation (Woodruff's 1890 Vision)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, '8daf1356-3db7-4156-a0e4-edc2d8e25bd6').
narrative_ontology:cs_kernel_codification('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', fixed_text).
narrative_ontology:cs_authority_grounding('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', lineage).
narrative_ontology:cs_interpretation_layer_present('8daf1356-3db7-4156-a0e4-edc2d8e25bd6').
narrative_ontology:cs_reading_relation('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', foundational, revelation_as_sufficient_authority).
narrative_ontology:cs_axiom_status(revelation_as_sufficient_authority, holdable).
narrative_ontology:cs_axiom_grounding('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', revelation_as_sufficient_authority, deontological).
narrative_ontology:cs_axiom('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', secondary, doctrine_practice_alignment_via_revelation).
narrative_ontology:cs_axiom_status(doctrine_practice_alignment_via_revelation, holdable).
narrative_ontology:cs_axiom_grounding('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', doctrine_practice_alignment_via_revelation, deontological).
narrative_ontology:cs_reference_frame('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', prophetic_revelation_authority).
narrative_ontology:cs_drift_state('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', contemporary_historical_scholarship, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8daf1356-3db7-4156-a0e4-edc2d8e25bd6', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, doctrinal_continuity).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, affected_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, epistemic_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED MEMBERSHIP (SNARE) — Members whose identity and kinship claims are constituted through Section 132 covenant doctrine face a structural inversion without doctrinal resolution. Exit from the church dissolves their identity frame entirely; remaining requires acceptance of the revelation reinterpretation without internal doctrinal consistency. Identity lock prevents seeing the doctrine-practice reversal as a contingent institutional choice — it appears as God's will, immutable. High experienced extraction because the constraint preserves institutional authority while erasing the theological ground of their original commitment.
constraint_indexing:constraint_classification(marriage_commitment_reversal__endogenous_reinterpretation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: THEOLOGICAL CONSISTENCY PRINCIPLE (TANGLED ROPE) — The abstract epistemic good of doctrinal coherence benefits from revelation framing (legitimacy mechanism) yet bears the cost of the unresolved contradiction (why did God's will reverse?). This is a genuine victim — the constraint serves coordination (preserving institutional continuity during political pressure) while extracting from the epistemic integrity of the system. Moderate power but constrained exit: one cannot leave theology without leaving the faith community.
constraint_indexing:constraint_classification(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — The prophet and senior leadership experience the constraint as a pure coordination mechanism: the revelation narrative solves the political problem (federal coercion requiring practice reversal) while preserving the institutional authority structure that the leadership depends on. The revelation reinterpretation enables the leadership to exit federal persecution while maintaining their claim to divine mandated authority. Experiences low or negative extraction because the constraint explicitly benefits the leadership's legitimacy — they are the authors of the revelation.
constraint_indexing:constraint_classification(marriage_commitment_reversal__endogenous_reinterpretation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOCTRINAL STATEMENT — SECTION 132 PRESERVED (PITON) — The doctrine of plural marriage as eternal covenant remains formally codified (D&C 132) even as practice is suspended. The doctrinal text persists through institutional inertia and theological preservation claims, but its functional authority over practice has been inverted by the revelation reinterpretation. Theater ratio high because the text is preserved and cited in theological argument while its prescriptive force is negated. This is a degraded former Rope (coordination around eternal marriage principle) now maintained for legitimacy theater rather than functional guidance.
constraint_indexing:constraint_classification(marriage_commitment_reversal__endogenous_reinterpretation_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POLITICAL PRESSURE RESPONSE — SUNSET LOGIC (SCAFFOLD) — From the perspective of organized institutional survival, the revelation-framed practice reversal is a temporary coordination mechanism with a built-in exit condition: if federal pressure subsides, the revelation could be reinterpreted again, restoring practice. Extractiveness is bounded by the political-necessity framing — this is presented not as a permanent doctrinal shift but as a divinely mandated adaptation to persecution. The sunset is neither explicit nor firm, creating ambiguity whether this is a true scaffold or a permanent tangled rope masked as temporary.
constraint_indexing:constraint_classification(marriage_commitment_reversal__endogenous_reinterpretation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — ENDOGENOUS REINTERPRETATION READING (TANGLED ROPE) — From a civilizational view, this constraint exhibits genuine coordination function (institutional survival under persecution requires some mechanism for reversing practice while preserving authority claims) AND asymmetric extraction (the leadership preserves legitimacy while the membership's doctrinal ground is erased without theological explanation). The endogenous reinterpretation reading classifies as tangled rope because it keeps both elements: this is neither pure coordination nor pure extraction, but a hybrid where the coordination benefit to the institution structurally requires the membership's theological consistency to be treated as expendable.
constraint_indexing:constraint_classification(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_commitment_reversal__endogenous_reinterpretation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_commitment_reversal__endogenous_reinterpretation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. This reading assumes the revelation is genuine divine communication (the endogenous reinterpretation frame). Under that assumption, the constraint's extractiveness is moderate rather than severe because the practice reversal is framed as divinely mandated, not as political capitulation. The extraction is real (the membership's doctrinal ground is erased, their identity commitments are inverted) but legitimated through authority claims. If the revelation were revealed to be politically motivated (exogenous override reading), extractiveness would increase to 0.60+. Suppression (0.62): High. Plural marriage practice is suppressed through doctrinal authority — members are prevented from continuing the practice not merely by law but by theological delegitimization. The suppression is effective because it operates through the same authority structure (the prophet's interpretive mandate) that legitimates the faith community. Alternatives to compliance are structurally unavailable within the faith frame. Theater ratio (0.68): High. Section 132 is formally preserved as doctrinal text but functions almost entirely performatively. The doctrine is cited in theological argument and institutional history, invoked to explain why the church ever practiced plural marriage, but carries no prescriptive force. The theatrical preservation serves institutional legitimacy — maintaining the claim that the church did not reverse doctrine, merely interpreted God's will differently. This is high-theater institutional communication: the text is preserved to create the appearance of doctrinal continuity while practice is inverted.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across a single structural arrangement. The institutional leadership (Rope perspective) sees pure coordination — a divinely mandated mechanism for preserving the church during persecution. The affected membership (Snare perspective with identity lock) sees extraction — their doctrinal commitment is inverted without explanation, and exit is impossible because it would dissolve their identity. The theological consistency principle (Tangled Rope perspective, abstract victim) sees a mixed arrangement — genuine coordination function (preserving institutional survival) nested within extraction (erasing the theological ground of the prior doctrine). The Piton perspective observes that Section 132 is preserved purely theatrically — the doctrine persists through institutional inertia and legitimacy claims while its prescriptive force is negated. The Scaffold perspective (organized institutional actors viewing political pressure response) sees temporary coordination with ambiguous sunset — the revelation reinterpretation is framed as necessary adaptation to persecution, potentially subject to future reinterpretation if circumstances change. The analytical observer at civilizational scope (this reading's perspective) sees Tangled Rope — the endogenous reinterpretation reading classifies the constraint as a genuine hybrid of coordination and extraction, where the coordination benefit to the institution structurally requires the membership's theological consistency to be treated as expendable.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary group (institutional_leadership) derives d ≈ 0.15-0.25 (beneficiary status + arbitrage exit options) yielding low f(d), thus low χ experienced. The leadership experiences the constraint as coordination, not extraction, because they author the revelation that solves their political problem. The victim groups (theological_consistency, affected_membership, epistemic_clarity) derive d ≈ 0.75-0.95 (victim status + constrained or identity_locked exit options) yielding high f(d), thus high χ experienced. The affected membership experiences high extraction because they face practice reversal without doctrinal resolution, and exit options are constrained by identity lock (leaving the church dissolves their identity frame). At the institutional power level with arbitrage exit, directionality is toward the leadership — they are positioned to extract legitimacy value from the revelation framing while externalizing the theological consistency cost. At the powerless/identity_locked level, directionality is toward the victims — the membership bears the cost of the unresolved contradiction while the leadership's authority is preserved.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (the tension between coordination and extraction functions) by explicitly modeling the coupling: the coordination mechanism (institutional survival via revelation-framed practice reversal) is structurally dependent on the extraction mechanism (erasing the membership's doctrinal ground without explanation). From the leadership perspective, this is pure coordination — they are solving a genuine institutional survival problem. From the membership perspective, this is pure extraction — their foundational commitment (Section 132 as eternal covenant) is inverted without doctrinal coherence. The constraint is neither misnamed nor misclassified — it is genuinely a tangled rope, where the beneficiary group's coordination benefit is purchased by the victim group's theological consistency cost. The mandatrophy is resolved by recognizing that mandates (the leadership's institutional survival mandate and the membership's doctrinal coherence mandate) are in structural conflict, and the constraint serves one at the expense of the other. From the institutional perspective, this is successful constraint (solves the survival problem). From the theological perspective, this is extractive constraint (solves the institutional problem by erasing the doctrinal problem rather than resolving it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity_vs_political_utility,
    'Is the Woodruff revelation (September 23, 1890 vision authorizing practice suspension) an authentic divine communication, or an institutional innovation legitimated through revelation framing to solve a political crisis?',
    'Historical analysis: timing relative to federal coercion intensification; internal institutional documents discussing political necessity; comparison to other revelation-framed institutional changes during persecution periods; theological methodology (how revelation validation occurs within the tradition vs how it is claimed post-hoc)',
    'If authentic: endogenous reinterpretation reading holds — God''s will genuinely changed, and the constraint is legitimate, reducing extractiveness to 0.25-0.35 (pure coordination). If instrumentalized: extractiveness increases to 0.60+ (snare) — revelation framing is cover story for political capitulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_authenticity_vs_political_utility, empirical, 'Whether the Woodruff revelation is authentic divine communication or politically-motivated institutional innovation').

omega_variable(
    doctrine_practice_reconciliation_mechanism,
    'What theological mechanism explains why God''s will regarding plural marriage as eternal covenant (Section 132) changed from mandate to prohibition within a single institutional framework without doctrinal revision?',
    'Systematic theology: analysis of official doctrinal explanations post-1890; examination of whether this represents (a) genuine revelation of new will overriding prior will, (b) reinterpretation of eternal principle in light of changed temporal circumstances, (c) acknowledgment of the prior doctrine as culture-bound rather than eternal, or (d) acceptance of an unresolved theological contradiction',
    'If (a): legitimate authority inversion, extractiveness 0.35-0.45. If (b): doctrine-practice gap acknowledged, extractiveness 0.50-0.60. If (c): prior doctrine delegitimized, high victim set burden, extractiveness 0.55-0.70. If (d): unresolved contradiction, maximal theological consistency cost, extractiveness 0.65-0.80.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_reconciliation_mechanism, conceptual, 'Theological mechanism explaining doctrine-practice reversal without doctrinal revision').

omega_variable(
    membership_epistemic_access_asymmetry,
    'Do members have reliable access to the reasons for the practice reversal (federal coercion, institutional survival logic) or is the revelation framing presented as sufficient explanation, obscuring the political necessity beneath?',
    'Analysis of contemporaneous institutional communications: are political pressures discussed openly in leadership addresses, or is the revelation narrative presented as self-contained? Comparison of internal vs external messaging; examination of what members are taught about the revelation''s grounding.',
    'If access is transparent: members can maintain psychological coherence (recognizing institutional necessity while accepting authority), reducing suppression and identity lock. If asymmetric: members are incentivized to internalize the revelation framing uncritically, increasing identity lock and suppression. Higher epistemic asymmetry correlates with higher extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_epistemic_access_asymmetry, empirical, 'Asymmetry in member access to political necessity vs revelation framing for practice reversal').

omega_variable(
    reading_dependent_extractiveness,
    'This constraint instantiates the ENDOGENOUS REINTERPRETATION READING of the marriage_commitment_reversal kernel. The exogenous_override_reading (federal coercion without doctrinal revision) and practice_doctrine_gap reading (structural ambiguity) would produce different ε values. Is the extractiveness measured here (0.48) an artifact of this reading''s framing, or a structural property independent of the reading chosen?',
    'Decomposition check: compare ε across three readings (this one at 0.48, exogenous at 0.60+, gap reading at 0.55). If ε differs substantially across readings (by > 0.12), each reading measures a structurally distinct constraint — they are not alternative framings of the same constraint but different constraints that share the historical event as their occasion. The three readings should be separate JSON files linked via network.affects_constraints.',
    'If readings differ substantively: confirm that this story and sibling stories are each ε-invariant, and that network linking is declared. If readings converge on ε: the three readings are genuine alternatives for a single constraint, and the perspectival gap is correctly modeled within one story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_dependent_extractiveness, conceptual, 'Whether this reading''s ε value is an artifact of framing or evidence that multiple readings represent structurally distinct constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcr_endo_tr_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mcr_endo_tr_t4, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(mcr_endo_be_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mcr_endo_be_t2, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(mcr_endo_be_t4, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(mcr_endo_be_t6, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 6, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(mcr_endo_su_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(mcr_endo_su_t2, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(mcr_endo_su_t4, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 4, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% The marriage_commitment_reversal kernel decomposes into three structurally distinct constraint stories with different ε values: (1) endogenous_reinterpretation_reading (ε=0.48, Tangled Rope) — practice reversal framed as divine revelation; (2) exogenous_override_reading (ε=0.60+, Snare) — practice reversal understood as federal coercion without doctrinal revision; (3) practice_doctrine_gap (ε=0.55, Tangled Rope) — structural ambiguity where doctrine and practice invert. These are not alternative perspectives on a single constraint but genuinely distinct constraints produced by different framings of the historical event. The three stories are linked via network.affects_constraints to show the constraint family structure. The endogenous reading assumes the revelation is genuine; the exogenous reading reinterprets it as political cover; the gap reading treats both the revelation and its denial as symptoms of an underlying theological incoherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
