% ============================================================================
% CONSTRAINT STORY: institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_pragmatism_reading, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto: Doctrinal Legitimation of Institutional Capitulation
 *   domain: religious_institutional_history/commitment_systems
 *
 * SUMMARY:
 *   On September 24, 1890, the institutional leadership announced the
 *   Manifesto: an official policy document declaring that the plural marriage
 *   practice, formerly understood as a divine mandate, was now ended by
 *   higher revelation. From the pragmatism reading, this was not primarily a
 *   doctrinal breakthrough but a strategic institutional response to coercive
 *   federal pressure. The U.S. government had spent two decades prosecuting
 *   polygamists, seizing property, and threatening institutional dissolution.
 *   The institution faced a choice: capitulate to federal demands or risk
 *   dissolution. The Manifesto enabled capitulation while maintaining a
 *   legitimacy frame — the institution could claim that God had revealed the
 *   end of the practice, not that the institution had surrendered to federal
 *   coercion. The reading's distinctive feature: the M-set gap between
 *   doctrine and practice. The Manifesto is the public document claiming the
 *   end of polygamy. Institutional records and members' testimonies reveal
 *   that secret polygamous marriages continued under leadership permission
 *   until at least 1904. The institution operated a bifurcated regime:
 *   official doctrine said plural marriage has ended; actual practice
 *   permitted secret continuations. Beneficiaries were church leadership
 *   (institutional survival, restoration of political rights, resumption of
 *   property rights). Victims were coerced polygamists (forced to choose
 *   between public renunciation and secret continuation under sufferance) and
 *   deceived monogamists (whose understanding of divine will was reorganized
 *   by a doctrine whose legitimacy claim was undercut by institutional
 *   practice). This constraint is one reading of a contested kernel: the
 *   plural_marriage_mandate. The pragmatism reading emphasizes strategic
 *   institutional adaptation as the primary causal driver, with doctrinal
 *   legitimation as the mechanism for making adaptation publicly acceptable.
 *   Sibling readings (exogenous override and endogenous reinterpretation)
 *   propose different primary causes and produce different classifications.
 *
 * KEY AGENTS:
 *   - Church Leadership: Primary beneficiary (institutional/arbitrage) — institutional survival, restored political and property rights, resumed institutional legitimacy. Strategic actor negotiating with federal government.
 *   - Practicing Polygamists: Primary victim (powerless/trapped) — coerced by federal pressure and institutional demand for public compliance; trapped between public renunciation and secret continuation under institutional permission with discovery risk.
 *   - Monogamist Believers: Secondary victim (powerless/trapped) — deceived by the Manifesto's revelation claim; internalize doctrine as genuine divine instruction while institutional practice contradicts the claim. Trapped in epistemic asymmetry.
 *   - Institutional Dissenters: Organized agent (organized/constrained) — mid-level leaders who understand institutional strategy but are constrained by belief commitment and career dependence. See genuine coordination value in institutional survival but experience doctrinal revision as extractive.
 *   - Federal Government: External coercive actor (powerful/analytical) — applies institutional pressure through prosecution and property seizure; creates the condition that makes institutional capitulation appear necessary.
 *   - Formal Doctrine (post-1890): Institutional construct (institutional/analytical) — persists through inertia; functions theatrically to legitimate compliance while secret practice contradicts the doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_pragmatism_reading, 0.58).
domain_priors:suppression_score(institutional_pragmatism_reading, 0.65).
domain_priors:theater_ratio(institutional_pragmatism_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_pragmatism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_pragmatism_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_pragmatism_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(institutional_pragmatism_reading, "1890 Manifesto: Doctrinal Legitimation of Institutional Capitulation").
narrative_ontology:topic_domain(institutional_pragmatism_reading, "religious_institutional_history/commitment_systems").

domain_priors:requires_active_enforcement(institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(institutional_pragmatism_reading, fixed_text).
narrative_ontology:cs_authority_grounding(institutional_pragmatism_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(institutional_pragmatism_reading).
narrative_ontology:cs_kernel_id(institutional_pragmatism_reading, plural_marriage_mandate).
narrative_ontology:cs_reading_relation(institutional_pragmatism_reading, exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation(institutional_pragmatism_reading, endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom(institutional_pragmatism_reading, foundational, institutional_survival_as_primary_motive).
narrative_ontology:cs_axiom_status(institutional_survival_as_primary_motive, holdable).
narrative_ontology:cs_axiom_grounding(institutional_pragmatism_reading, institutional_survival_as_primary_motive, empirically_contingent).
narrative_ontology:cs_axiom(institutional_pragmatism_reading, foundational, doctrinal_revision_as_strategic_deployment).
narrative_ontology:cs_axiom_status(doctrinal_revision_as_strategic_deployment, holdable).
narrative_ontology:cs_axiom_grounding(institutional_pragmatism_reading, doctrinal_revision_as_strategic_deployment, empirically_contingent).
narrative_ontology:cs_reference_frame(institutional_pragmatism_reading, doctrinal_continuity_obligation).
narrative_ontology:cs_drift_state(institutional_pragmatism_reading, post_manifesto_period, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_victim(institutional_pragmatism_reading, coerced_polygamists).
narrative_ontology:constraint_victim(institutional_pragmatism_reading, deceived_monogamists).
narrative_ontology:constraint_victim(institutional_pragmatism_reading, doctrinal_consistency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICING POLYGAMISTS (SNARE) — Trapped agents experiencing maximum extraction. The Manifesto creates the institutional demand for public compliance (apparent monogamy) while secretly permitting private continuation under sufferance. Polygamists face the choice: abandon plural marriage publicly and often actually, or continue secretly while supporting a doctrine that condemns their practice. No legitimate exit exists — federal pressure was coercive, the institutional path demands public capitulation, and secret continuation carries discovery risk. Suppression is maximal: legal jeopardy, social shame if discovered, institutional control over legitimacy frame.
constraint_indexing:constraint_classification(institutional_pragmatism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DECEIVED MONOGAMIST BELIEVERS (SNARE) — Trapped in cognitive asymmetry. They accept the Manifesto's revelation claim at face value — that God genuinely commanded the end of plural marriage — while the institutional leadership operates under the knowledge that secret continuations are permitted and managed. These believers are extraction victims: they reorganize their understanding of divine will based on a doctrine whose legitimacy claim is undercut by institutional practice that contradicts it. They also bear the social reconstruction cost of the 'new' doctrine. No exit: they cannot access the leadership's actual operating frame, and exiting the belief community means abandoning their entire identity structure.
constraint_indexing:constraint_classification(institutional_pragmatism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL DISSENTERS (TANGLED ROPE) — Organized agents who understand the Manifesto as institutional capitulation but see genuine coordination value in institutional survival. These agents (local leaders, mid-level administrators) experience both extraction and coordination: the institution's survival is a good they value, but the method — doctrinal revision under coercive pressure — extracts from the consistency of their authority structure. They are constrained by career dependence and belief commitment but have enough organizational position to understand the mechanism. Mixed experience: they support the Manifesto as necessary coordination for survival but experience the doctrinal tension as extractive.
constraint_indexing:constraint_classification(institutional_pragmatism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CHURCH LEADERSHIP (ROPE) — Pure coordination framing from the top. Leadership sees the Manifesto as solving the survival problem: federal coercion threatened institutional dissolution. The Manifesto is the coordination device that restored political rights, ended prosecution, and preserved the institution. From this perspective, doctrinal revision is a solution, not extraction — the institution survives, property is restored, political participation resumes. Leadership has arbitrage access to alternative jurisdictions and frames; they chose institutional survival as the goal and the Manifesto as the means. No extraction from their standpoint: they got what they negotiated for. The cost (doctrinal revision) is a legitimate coordination tradeoff.
constraint_indexing:constraint_classification(institutional_pragmatism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL DOCTRINE (PITON) — The Manifesto's doctrinal claim persists through institutional inertia despite being undercut by institutional practice. Formal doctrine insists plural marriage has ended. Secret continuations (documented through 1904 and beyond) reveal that the formal statement functions theatrically: it performs compliance to external observers while the institution manages an internal exception regime. The doctrine is maintained not because it is functionally true or universally enforced, but because institutional inertia prevents doctrinal revision in the direction of truthfulness. Theater ratio is high: the Manifesto is a legitimacy performance, not a complete description of institutional practice.
constraint_indexing:constraint_classification(institutional_pragmatism_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Manifesto might be read as responding to an immutable political reality: institutions cannot maintain coerced practices under determined federal opposition; capitulation is structurally inevitable. This perspective sees institutional adaptation to coercive pressure as a natural law — institutions either adapt or dissolve. However, this framing naturalizes what is actually a contingent political choice by the federal government and a contingent institutional strategy. The engine will identify this as a false summit: the 'inevitable' adaptation is legitimate resistance to federal overreach, not a natural law.
constraint_indexing:constraint_classification(institutional_pragmatism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_pragmatism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_pragmatism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_pragmatism_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_pragmatism_reading, TR),
    TR >= 0.70.

:- end_tests(institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Manifesto functions as a tangled_rope because it serves two structural purposes simultaneously: (1) institutional coordination — solving the survival problem through strategic adaptation to coercive pressure, and (2) asymmetric extraction — using doctrinal revision as the legitimacy mechanism while secretly managing exception regimes. The extractiveness rises over the interval (0.42 → 0.58) as the gap between doctrine and practice widens (secret continuations continue 1890-1904). Suppression (0.65): High. Multiple suppression mechanisms operate: federal legal jeopardy (structural), institutional authority claims (institutional), and epistemic closure for members unaware of the strategy (cognitive). Polygamists face legal risk, institutional pressure, and social shame. Monogamists face cognitive capture through the Manifesto's revelation claim. Suppression does not decline over the interval because the regime persists. Theater ratio (0.68): High and rising. The Manifesto is performative: it performs compliance to federal observers (official doctrine says plural marriage ended) while the institution manages an internal exception regime (secret continuations permitted). Theater increases as institutional practice diverges from formal doctrine. The theater is not theatrical in the Piton sense (inertia) but in the tangled_rope sense (legitimacy claim deployed strategically).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Church leadership sees coordination (Rope) — solving the survival problem through strategic adaptation. Practicing polygamists see pure extraction (Snare) — trapped between public renunciation and secret continuation under institutional sufferance. Monogamists see pure extraction through epistemic capture (Snare) — their understanding of divine will is reorganized by a doctrine whose legitimacy claim is false. Institutional dissenters see mixed extraction and coordination (Tangled Rope) — institutional survival is good, but the method (doctrinal revision) is extractive. The formal doctrine persists through inertia (Piton) — maintained not because true but because institutional machinery has difficulty retracting. The civilizational analytical observer might see capitulation as natural law (Mountain) — institutions must adapt to coercive pressure or die. But this naturalizes what is actually a political choice: the federal government applied coercion, and the institution chose capitulation with doctrinal legitimation rather than resistance or dissolution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for each perspective are derived from structural position: power level, exit options, and relationship to extraction flow. Church leadership (institutional/arbitrage) experiences low d — they are beneficiaries with options, so the sigmoid f(d) produces a low effective extraction term. Practicing polygamists (powerless/trapped) experience high d — they are victims with no exit, so f(d) produces high effective extraction. Monogamists (powerless/trapped) also experience high d through epistemic entrapment — they are structurally trapped by cognitive capture even if materially they have some mobility. Institutional dissenters (organized/constrained) experience moderate d — they have some organizational capacity but are constrained by commitment and career. The directionality derivation automatically captures the asymmetry: beneficiaries derive low chi, victims derive high chi, mixed agents derive moderate chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the Manifesto serves both a genuine coordination function (institutional survival) and asymmetric extraction (doctrinal legitimation that misrepresents institutional practice). The constraint is not a snare disguised as rope, nor rope disguised as snare. It is a tangled_rope — both functions are real, neither is parasitic on the other. The coordination function (solving the survival problem through strategic adaptation) is legitimate from the institutional perspective. The extraction function (using doctrinal claims to legitimize suppression of polygamists and epistemic capture of monogamists) is real from the victim perspective. Neither function can be eliminated without eliminating the other: if the institution had not coordinated institutional survival, there would be no constraint; if the institution had not deployed doctrinal legitimation, the survival coordination would have required a different mechanism. The mandatrophy is resolved by recognizing that tangled_rope is the correct classification precisely because both functions are essential to the constraint's operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_sincerity_ambiguity,
    'Did church leadership sincerely believe the Manifesto represented a genuine doctrinal revelation, or was it a pure strategic performance undertaken with the knowledge that secret continuations would persist?',
    'Analysis of leadership contemporaneous writings, private correspondence, internal institutional guidance; correlation between official doctrine and documented practice patterns post-1890; testimonies from insiders regarding the coherence or tension between private understanding and public claim.',
    'If sincere belief: the constraint is an institutional response to coercive pressure where doctrine genuinely changed. Classification remains Tangled Rope but the extractive component shifts toward institutional survival necessity. If pure performance: the constraint is an intentional doctrinal deception where leadership knowingly misrepresented institutional practice. Classification remains Tangled Rope but the extractive component is maximal — the doctrine is instrumentally deployed to legitimate survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_sincerity_ambiguity, empirical, 'Whether leadership sincerely believed or strategically performed the Manifesto''s doctrinal claim').

omega_variable(
    secret_continuations_scope_and_duration,
    'What proportion of polygamists actually continued secret polygamy post-1890, and how long did the institutional permission regime persist?',
    'Genealogical and demographic analysis of post-1890 plural marriages; court records and federal monitoring reports; institutional archives regarding guidance to bishops on managing secret continuations; comparison of official doctrine claims with actual marriage practices in membership records.',
    'If continuations were rare (< 10%) and brief (ended by 1895): the constraint''s extractive component is limited to institutional transition costs and doctrinal inconsistency. If continuations were widespread (> 40%) and prolonged (continued to 1904+): the constraint is more clearly extractive — the institution maintained a bifurcated regime where official doctrine contradicted official practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secret_continuations_scope_and_duration, empirical, 'Proportion and duration of secret polygamy continuations post-1890').

omega_variable(
    member_awareness_of_institutional_strategy,
    'To what extent were rank-and-file members aware that the Manifesto represented institutional capitulation rather than genuine doctrinal revelation?',
    'Analysis of member testimonies, private diaries, internal sermons; comparison of knowledge levels across hierarchical tiers (leadership, local administrators, rank-and-file); oral history projects documenting member understanding of the 1890 period.',
    'If awareness was high: suppression operates primarily through institutional authority and legal jeopardy rather than epistemic closure. Members knew the doctrine was instrumental but were suppressed by other mechanisms. If awareness was low: suppression operates through epistemic capture — members internalized the Manifesto''s legitimacy claim as genuine revelation and did not perceive the institutional strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_awareness_of_institutional_strategy, empirical, 'Member awareness of the Manifesto as strategic adaptation vs. genuine revelation').

omega_variable(
    reading_kernel_contest,
    'Which reading of the plural_marriage_mandate kernel is historically accurate: institutional pragmatism (this reading), exogenous override (federal coercion as decisive), or endogenous reinterpretation (doctrinal evolution as primary)?',
    'Historical analysis of the causal sequence: Did federal pressure and institutional threat drive the Manifesto (pragmatism/exogenous override), or did internal doctrinal reconsideration drive the policy shift (endogenous reinterpretation)? Analysis of timing, leadership writings, institutional debates, and counterfactual scenarios. The three readings will produce different constraint types and different ε values depending on which causal story is correct.',
    'Pragmatism reading (this one): Tangled Rope, ε=0.58, primary observable is the M-set gap (doctrine vs. practice). Exogenous override reading: Snare, higher ε (coercive compliance imposed externally). Endogenous reinterpretation reading: Rope, lower ε (institutional understanding evolved). The three readings are not all equally supported by the evidence; this omega identifies the empirical test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, empirical, 'Which reading of the plural marriage mandate kernel (pragmatism, exogenous override, endogenous reinterpretation) is historically accurate').

omega_variable(
    doctrine_consistency_recovery,
    'Does the institutional framework eventually repair the doctrinal inconsistency between the Manifesto''s official position and the continuations it secretly permitted, or does the inconsistency persist as a permanent feature requiring managed opacity?',
    'Longitudinal analysis of institutional doctrine post-1904 (after secret continuations formally ended): Did leadership explicitly acknowledge the 1890-1904 period as a transition with intentional doctrinal instrumentalism? Or did the institution continue to claim the Manifesto as a straightforward revelation, leaving the inconsistency unresolved?',
    'If repair occurs: the constraint''s theater_ratio should decline post-1904 — the institution moves from performance toward coherence. If inconsistency persists: theater_ratio remains high — the doctrine continues to be maintained through institutional inertia and managed opacity, supporting the Piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_consistency_recovery, empirical, 'Whether the institution eventually repairs the doctrine-practice inconsistency or sustains it through managed opacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_pragmatism_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, institutional_pragmatism_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(inst_tr_t2, institutional_pragmatism_reading, theater_ratio, 2, 0.68).
narrative_ontology:measurement(inst_tr_t4, institutional_pragmatism_reading, theater_ratio, 4, 0.72).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, institutional_pragmatism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(inst_be_t2, institutional_pragmatism_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(inst_be_t4, institutional_pragmatism_reading, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_pragmatism_reading, identity_coordination).
narrative_ontology:affects_constraint(institutional_pragmatism_reading, federal_coercive_override_reading).
narrative_ontology:affects_constraint(institutional_pragmatism_reading, doctrinal_evolution_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel admits three structurally distinct constraint stories with different ε values and different primary causal narratives. The institutional_pragmatism_reading (this file, ε=0.58) emphasizes strategic adaptation. The exogenous_override_reading (ε=0.72) emphasizes federal coercion as decisive. The endogenous_reinterpretation_reading (ε=0.35) emphasizes doctrinal evolution. All three are linked via affects_constraints; the choice of which reading to use depends on which historical evidence is primary. The pragmatism reading is appropriate when secret continuations and institutional strategy are emphasized; the override reading is appropriate when federal pressure and institutional powerlessness are emphasized; the reinterpretation reading is appropriate when doctrinal development and theological reasoning are emphasized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
