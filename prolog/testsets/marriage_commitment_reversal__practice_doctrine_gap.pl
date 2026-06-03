% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Reversal: Practice-Doctrine Gap (1890-1904)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   Between 1890 and 1904, the Church of Jesus Christ of Latter-day Saints
 *   maintained a structural ambiguity at its institutional core: the
 *   principle authorizing plural marriage (Section 132 of the Doctrine and
 *   Covenants) remained canonical doctrine, formally preserved and
 *   doctrinally binding, while the practice of plural marriage was publicly
 *   suspended through the Manifesto of 1890 and increasingly enforced against
 *   through institutional discipline, denial of temple access, and eventual
 *   doctrinal revocation in 1904. This constraint represents not a simple
 *   policy reversal but a period of strategic institutional ambiguity where
 *   the authority structure preserved the doctrinal principle while
 *   suspending its practice, enabling the institution to maintain multiple
 *   legitimation tracks simultaneously: formal coherence with doctrine for
 *   internal consumption and fundamentalist believers; compliance with
 *   federal law for external legitimacy; and operational flexibility that
 *   could be reframed later as either temporary suspension or permanent
 *   doctrinal evolution. The practice-doctrine gap created distinct victim
 *   and beneficiary structures. The general membership experienced
 *   incoherence — told to hold Section 132 as eternal principle while being
 *   punished for living it. Fundamentalist dissidents experienced forced
 *   schism — holding the doctrine coherently required leaving the
 *   institution. The institutional leadership experienced the constraint as
 *   enabling survival through ambiguity. The accommodationist coalition saw
 *   the gap as a temporary expedient with a sunset (divine reinterpretation).
 *   Institutional historians observe it as a degraded constraint persisting
 *   through inertia. The analytical observer risks naturalizing it as an
 *   inherent feature of institutional life under pressure. This story
 *   instantiates ONE reading of the contested marriage_commitment_reversal
 *   kernel: the practice_doctrine_gap reading, which emphasizes the
 *   structural logic of preserving doctrine while suspending practice as a
 *   strategy for institutional flexibility under conflicting imperatives.
 *
 * KEY AGENTS:
 *   - Institutional Leadership (First Presidency & Quorum of the Twelve): Primary beneficiary (institutional/arbitrage) — gains flexibility, preserves authority discretion, maintains credibility with both federal government and membership; navigates between federal prohibition and fundamentalist expectation through strategic ambiguity
 *   - General Membership: Primary victim (powerless/trapped) — caught in incoherent position: doctrine says Section 132 is eternal principle; practice says it is suspended; no authority frame permits coherence; social/familial/economic costs of exit are prohibitive
 *   - Fundamentalist Dissidents: Secondary victim (moderate/constrained) — insist on doctrinal coherence; face forced choice between accepting contradiction (cognitive capture) or schism (exit); suppression operates through framing dissent as rebellion
 *   - Doctrinal Coherence (abstract victim): Structural victim — the institutional commitment system's internal logical consistency is sacrificed for external flexibility; the principle-practice gap reflects the cost of maintaining institutional survival at the expense of coherence
 *   - Accommodationist Coalition: Organized agents (organized/constrained) — support practice reversal and see the gap as temporary; believe internal reinterpretation will eventually resolve it; hold 'sunset clause' understanding of the ambiguity
 *   - Institutional Historian: Observer (institutional/arbitrage) — perceives the constraint as degraded form of living doctrine; Section 132 preserved through inertia, not belief; notes that formal disavowal would require acknowledging that the principle was not eternal
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks naturalizing the gap as inevitable feature of institutional authority under pressure; sees the constraint as Mountain rather than contingent institutional strategy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.62).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.68).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Reversal: Practice-Doctrine Gap (1890-1904)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '96f67ae6-22f7-440b-802f-c6d0b7fdf30a').
narrative_ontology:cs_kernel_codification('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', fixed_text).
narrative_ontology:cs_authority_grounding('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', extraction).
narrative_ontology:cs_interpretation_layer_present('96f67ae6-22f7-440b-802f-c6d0b7fdf30a').
narrative_ontology:cs_reading_relation('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', marriage_commitment_reversal__endogenous_reinterpretation_reading, influences).
narrative_ontology:cs_axiom('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', foundational, section_132_preserved_for_institutional_flexibility).
narrative_ontology:cs_axiom_status(section_132_preserved_for_institutional_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', section_132_preserved_for_institutional_flexibility, instrumental).
narrative_ontology:cs_axiom('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', foundational, practice_suspension_operates_through_ambiguity_not_clarification).
narrative_ontology:cs_axiom_status(practice_suspension_operates_through_ambiguity_not_clarification, holdable).
narrative_ontology:cs_axiom_grounding('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', practice_suspension_operates_through_ambiguity_not_clarification, empirically_contingent).
narrative_ontology:cs_reference_frame('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', section_132_as_eternal_binding_principle).
narrative_ontology:cs_drift_state('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', post_manifesto_1904, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('96f67ae6-22f7-440b-802f-c6d0b7fdf30a', '2026-02-26T14:23:18Z').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissidents).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, doctrinal_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL MEMBERSHIP (SNARE) — Ordinary adherents face a structurally inescapable contradiction: the principle (Section 132) remains canonical doctrine, yet the practice is suspended without internal doctrinal revision. They cannot exit the jurisdictional frame (departure incurs social/familial/economic costs in closed communities), cannot resolve the contradiction (the authority structure refuses to acknowledge the gap), and cannot organize (dissent is suppressed as doubt). Maximum extraction: membership clarity sacrificed, identity rendered incoherent, no agency to resolve the bind.
constraint_indexing:constraint_classification(marriage_commitment_reversal__practice_doctrine_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FUNDAMENTALIST DISSIDENTS (SNARE) — Agents who insist on doctrinal coherence face forced choice: accept the practice-doctrine gap and experience cognitive dissonance, or exit the institution (schism). The cost of exit is extreme (loss of community, family ties, religious home). The cost of staying is identity fracture. Suppression is severe — dissent is framed as faithlessness; questioning the gap is portrayed as rebellion against legitimate authority. No stable position exists between acceptance and schism.
constraint_indexing:constraint_classification(marriage_commitment_reversal__practice_doctrine_gap, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (TANGLED ROPE) — The leadership structure experiences this as coordination with extraction layered on top. The genuine coordination problem: how to maintain doctrinal continuity while adapting to external (federal) pressure without explicitly rescinding canonical commitments. The leadership solves this through strategic ambiguity — doctrine is preserved in formal statement; practice is suspended in application; the gap itself is never officially acknowledged. This enables temporal arbitrage: preserving legitimacy with the membership while complying with external coercion. Net beneficiary position — the constraint allows institutional survival and preserves the authority structure's discretion. Low suppression from leadership perspective (they control the narrative frame); moderate extraction (they bear the cost of maintaining the ambiguity, managing the cognitive dissonance among members).
constraint_indexing:constraint_classification(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ACCOMMODATIONIST COALITION (SCAFFOLD) — Organized agents within the institutional system who support adaptive practice (ending plural marriage) see the practice-doctrine gap as a temporary expedient with a sunset clause. The gap is meant to persist only until doctrinal reinterpretation can catch up (the Woodruff Manifesto of 1890 was framed as temporary suspension pending divine guidance). From this perspective, the constraint is a transition mechanism — maintain the institution through external pressure while pursuing internal reframing. Exit from the gap is hypothetically possible once doctrinal revision is complete. Sunset timing: internal revelation/reinterpretation would formally collapse the gap, eliminating the constraint entirely. Theater is present (public compliance with federal law while doctrine is preserved) but functional — the theater buys time for reframing. This perspective sees extractiveness as moderate and temporary.
constraint_indexing:constraint_classification(marriage_commitment_reversal__practice_doctrine_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL HISTORIAN (PITON) — Over civilizational timescale, the practice-doctrine gap is a degraded form of the constraint that once functioned as living doctrine. Plural marriage as commanded principle once organized social practice coherently; the gap represents the constraint in decay — Section 132 is preserved as canonical text but is functionally inert, maintained through institutional inertia and formal statement rather than active enforcement or genuine belief. The constraint persists because formal disavowal would require explicit doctrinal rejection (which would involve acknowledging that Section 132 is not eternal principle but historical command), creating institutional crisis. Theater ratio is extremely high (0.81) — the constraint's function is almost entirely performative, preserving the formal coherence of doctrine while actual practice has moved on.
constraint_indexing:constraint_classification(marriage_commitment_reversal__practice_doctrine_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, the practice-doctrine gap appears as an immutable structural feature of commitment systems under external pressure: when an authority structure receives contradictory imperatives (preserve doctrinal coherence AND comply with external coercion), some gap between principle and practice becomes logically inevitable. This perspective naturalizes the gap as an irreducible feature of how institutional authority operates when pressed. However, this reading obscures the contingent institutional choices that created the gap — the choice to preserve Section 132 in doctrine rather than formally rescind it; the choice to frame compliance as 'suspension' rather than revocation; the choice not to communicate the gap to the membership. The mountain classification is a false summit that naturalizes what is actually a contingent institutional strategy.
constraint_indexing:constraint_classification(marriage_commitment_reversal__practice_doctrine_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_commitment_reversal__practice_doctrine_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_commitment_reversal__practice_doctrine_gap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, TR),
    TR >= 0.70.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The institutional leadership extracts substantial benefit from the ambiguity — they gain temporal flexibility, preserve authority discretion, avoid forced choice between federal compliance and doctrinal coherence. The general membership bears the cost of incoherence. The extractiveness value reflects that the leadership is not capturing the full rent (they are constrained by the need to maintain internal legitimacy and prevent schism), but they are capturing enough to classify this as significant extraction. The trajectory shows rising extractiveness over the interval (0.48 → 0.62) reflecting that the initial suspension (1890) was presented as temporary pending reinterpretation, while by 1904 it was becoming permanent, increasing the cost to the membership of maintaining belief in Section 132. Suppression (0.68): High. Structural barriers to addressing the contradiction include: the authority structure's formal control over doctrine and interpretation (members cannot unilaterally reinterpret); the framing of Section 132 as eternally valid principle (questioning it becomes doctrinal heresy); the social/familial/economic integration of the institution (exit is catastrophic); and the active suppression of dissent (fundamentalist movements are excommunicated, exiled, or forced underground). The rising suppression trajectory (0.58 → 0.68) reflects that as the gap became harder to deny, the institution increased enforcement against fundamentalist dissidents and enforced compliance with the practice suspension more strictly. Theater ratio (0.81): Very high. The entire architecture is performative: the Manifesto publicly claims suspension (compliance theater for federal observers); doctrine formally preserves Section 132 (legitimacy theater for the membership); internal leadership communications reveal the strategic ambiguity while external communications maintain coherence theater. By 1904, the theater is nearly complete — everyone knows the practice is gone, yet the doctrine persists formally. The rising theater trajectory (0.52 → 0.81) reflects that as the practice suspension became more permanent, the gap between the performative preservation of doctrine and the operative abandonment of practice grew wider.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The institutional leadership sees coordination with extraction: solving the legitimate problem of institutional survival under conflicting imperatives, with asymmetric benefit. The accommodationist coalition sees a temporary scaffold with sunset: the ambiguity is a bridge to internal reinterpretation. The institutional historian sees a piton: the constraint is degraded doctrine maintained through inertia. The fundamentalist dissidents see a snare: forced choice between cognitive dissonance and schism. The general membership sees a snare: incoherent position, no exit, no resolution. The analytical observer risks a mountain: naturalizing the gap as inherent to institutional life. The perspectival gaps reflect genuine differences in structural position, not differences in perspective alone. The leadership has exit options (arbitrage through ambiguity); the membership does not (trapped by social integration). The beneficiary and victim positions are structurally distinct, not perspectival artifacts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim structure plus exit options. The institutional leadership occupies d ≈ 0.15 (beneficiary status + arbitrage exit): they benefit from the constraint and can exercise discretion through reframing. The general membership occupies d ≈ 0.88 (victim status + trapped exit): they bear the cost of incoherence and have no structural exit from the jurisdictional frame. The fundamentalist dissidents occupy d ≈ 0.75 (victim status + constrained exit): they face forced schism or compromise. These d values map through the sigmoid f(d) to produce the experienced extractiveness (chi) in each perspective. The leadership's low d produces low chi (they experience the constraint as manageable coordination); the membership's high d produces high chi (they experience maximal extraction). The sigmoid's curvature means that small differences in d among beneficiaries (e.g., leadership at 0.15 vs. accommodationists at 0.25) produce large differences in chi experience, explaining why the accommodationist coalition perceives the constraint differently from the leadership despite both having beneficiary/coordinated roles.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that high extractiveness and real coordination can coexist. The institutional leadership genuinely solves a coordination problem (how to maintain institutional coherence under contradictory pressures). This is a real function, not pretense. But this coordination function is layered with asymmetric extraction: the leadership's solution preserves their discretion at the cost of the membership's coherence. The constraint is Tangled Rope from the leadership perspective (genuine coordination + extraction), Snare from the membership perspective (extraction without coordinating benefit), and Scaffold from the accommodationist perspective (coordination with sunset). Mandatrophy is resolved by recognizing that the same constraint exhibits different functional profiles from different structural positions. From the beneficiary's position (leadership), the extraction is the cost of solving the coordination problem they perceive. From the victim's position (membership), there is no coordination benefit — only incoherence imposed from above. The classification resolves by showing that the 'mandatrophy' was an artifact of treating the constraint as having a single function. The constraint has multiple functions, layered on top of each other, depending on whose problem-space you inhabit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_formality_vs_operationality,
    'Is Section 132 preserved in doctrine as genuine principle or as hollow formality maintained for institutional credibility?',
    'Historical analysis of internal institutional discourse (private letters, leadership councils, administrative decisions) comparing doctrine-in-principle to doctrine-in-operation. Examination of whether leadership attempted to reinterpret Section 132 internally or simply suspended it without engagement.',
    'If principle: the gap represents temporary suspension pending reinterpretation (scaffold narrative is correct). If formality: the gap represents permanent extraction disguised as temporary measure (snare narrative is correct). Classification would shift from Tangled Rope (leadership perspective) to pure Snare if doctrine is hollow formality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_formality_vs_operationality, empirical, 'Whether Section 132 is preserved as genuine principle or hollow formality').

omega_variable(
    membership_awareness_of_contradiction,
    'Did the general membership perceive and experience the practice-doctrine gap, or was the contradiction successfully suppressed from consciousness through framing and narrative control?',
    'Analysis of membership writings (diaries, letters, oral histories) to assess whether the contradiction was explicitly recognized or remained implicit/unacknowledged. Examination of how the institution''s framing (suspension vs. revocation) shaped member comprehension.',
    'If contradiction was explicit: membership experienced maximum extraction and cognitive dissonance (snare classification confirmed). If contradiction was suppressed from consciousness: extraction operated at the level of cognitive capture (identity_locked exit option becomes relevant). Demonstrates whether suppression was structural (no information available) or internalized (information available but reframed as non-contradiction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_awareness_of_contradiction, empirical, 'Whether membership perceived the practice-doctrine gap or was it suppressed from consciousness').

omega_variable(
    exogenous_vs_endogenous_causation,
    'Was the practice reversal driven primarily by exogenous federal coercion (external override reading) or by endogenous divine revelation/reinterpretation (endogenous reinterpretation reading) or by structural institutional logic (this reading)?',
    'Historical analysis of causation chain: sequencing of external pressure vs. internal revelation; internal leadership documents revealing decision logic; counterfactual analysis of whether the institution would have reversed practice without federal pressure.',
    'If exogenous dominates: the gap is an imposed constraint (snare narrative). If endogenous dominates: the gap is an internally coherent reframing (rope narrative becomes possible). If institutional logic dominates: the gap reflects the institution''s strategic choice to preserve ambiguity for flexibility (this reading''s tangled rope for leadership is correct). Determines which sibling reading most accurately captures the structural driver.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_causation, empirical, 'Whether practice reversal was exogenously driven or endogenously reinterpreted').

omega_variable(
    kernel_reading_contest_resolution,
    'Which reading of the marriage_commitment_reversal kernel best explains the observable institutional behavior: exogenous override (external pressure without internal doctrinal change), endogenous reinterpretation (internal revelation reinterpreting God''s will), or practice-doctrine gap (strategic ambiguity enabling dual-track legitimation)?',
    'Integration of omega_doctrine_formality, omega_membership_awareness, and omega_exogenous_vs_endogenous evidence. Comparison of predictive fit: which reading best explains the subsequent institutional trajectory (slow doctrinal adaptation, persistent fundamentalist tension, eventual formal doctrinal revision in 1904)? Which reading best explains why the institution chose preservation-with-ambiguity over formal rescission?',
    'This omega routes to the engine''s reading-relations validation: if exogenous_override and endogenous_reinterpretation both have high evidence, all three readings coexist_with each other. If practice_doctrine_gap (this reading) has strongest evidence, the sibling readings either coexist_with or influences this reading rather than foreclosing it. The resolution determines the network topology of the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, conceptual, 'Which reading best explains the institutional behavior in the marriage commitment reversal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcr_pdg_theater_1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0, 0.52).
narrative_ontology:measurement(mcr_pdg_theater_1897, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 7, 0.76).
narrative_ontology:measurement(mcr_pdg_theater_1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 14, 0.81).

% Extraction over time
narrative_ontology:measurement(mcr_pdg_extract_1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(mcr_pdg_extract_1897, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 7, 0.6).
narrative_ontology:measurement(mcr_pdg_extract_1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 14, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(mcr_pdg_suppress_1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(mcr_pdg_suppress_1897, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 7, 0.66).
narrative_ontology:measurement(mcr_pdg_suppress_1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 14, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_institutional_schism__1890_1904).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, doctrine_operationalization_gap__general_religious_institutions).

% DUAL FORMULATION NOTE:
% The marriage commitment reversal kernel has three structurally distinct readings: exogenous_override (external pressure model), endogenous_reinterpretation (internal revelation model), and practice_doctrine_gap (strategic ambiguity model). Each reading produces a different constraint story with different ε values and victim/beneficiary structures. This story instantiates the practice_doctrine_gap reading (ε=0.62). The sibling readings are separate constraint files (not yet authored). The network links show structural dependence: the practice_doctrine_gap constraint influences (and partially enables) the institutional schism outcome, and the general pattern of doctrine-operationalization gaps appears in other religious institutional histories with different ε and suppression values depending on whether the gap is strategic (ambiguity) or accidental (institutional drift).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__practice_doctrine_gap, institutional, 0.15).
constraint_indexing:directionality_override(marriage_commitment_reversal__practice_doctrine_gap, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
