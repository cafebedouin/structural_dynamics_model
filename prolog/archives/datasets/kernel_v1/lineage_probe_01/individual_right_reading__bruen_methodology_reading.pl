% ============================================================================
% CONSTRAINT STORY: individual_right_reading__bruen_methodology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_individual_right_reading__bruen_methodology_reading, []).

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
 *   constraint_id: individual_right_reading__bruen_methodology_reading
 *   human_readable: Bruen's Historical Methodology: Suppression of Tiered Scrutiny in Gun Regulation
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   New York State Rifle & Pistol Association v. Bruen (2022) transformed
 *   Second Amendment doctrine by replacing 'interest balancing' with a
 *   historical methodology: modern gun regulations stand only if they have a
 *   historical analogue in the founding era's regulatory tradition. This
 *   constraint models one reading of the contested 'individual right' kernel
 *   — the methodological reading that defines how the right is enforced, not
 *   the right itself. The Bruen methodology creates a structural asymmetry:
 *   it benefits litigants armed with historical arguments but suppresses
 *   novel regulatory responses to novel weapons and novel harms.
 *   Extractiveness (0.62) reflects that the methodology systematically
 *   disadvantages regulatory innovation and public health experimentation.
 *   Suppression (0.72) reflects the elimination of interest balancing — state
 *   legislatures can no longer argue that a modest, carefully tailored
 *   regulation serves a compelling state interest. The historical test is
 *   binding; policy arguments are irrelevant. Theater ratio (0.58) reflects
 *   that the methodology performs a technical/jurisprudential function
 *   (tethering interpretation to the founding document) but increasingly
 *   functions as a performance of historical rigor that may mask
 *   outcome-driven judging.
 *
 * KEY AGENTS:
 *   - Second Amendment Litigants: Primary beneficiary (powerful/arbitrage) — armed with historical methodology as litigation lever; can challenge regulations by demanding founding-era analogues
 *   - Novel Regulatory Response (state legislatures, agencies): Primary victim (powerless/trapped) — cannot regulate novel weapons or harms without historical precedent; interest balancing suppressed
 *   - Public Health Practitioners: Secondary victim (moderate/constrained) — evidentiary approach systematically suppressed; expertise treated as irrelevant to constitutional analysis
 *   - Federal Courts: Mixed position (institutional/constrained) — benefit from clear decision rule; constrained by requirement to be historians-in-robes rather than policy adjudicators
 *   - Supreme Court Majority: Institutional beneficiary (institutional/arbitrage) — created and controls the methodology; maintains power to define what counts as 'analogous'
 *   - Gun Safety Advocates: Victim (moderate/constrained) — policy preferences delegitimized by methodology; no exit except constitutional amendment (generational timeline)
 *   - Analytical Observer: Sees both legitimate jurisprudential principle and potential false summit (analytical/analytical) — risks naturalizing contested methodological choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_right_reading__bruen_methodology_reading, 0.62).
domain_priors:suppression_score(individual_right_reading__bruen_methodology_reading, 0.72).
domain_priors:theater_ratio(individual_right_reading__bruen_methodology_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_right_reading__bruen_methodology_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(individual_right_reading__bruen_methodology_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(individual_right_reading__bruen_methodology_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_right_reading__bruen_methodology_reading, snare).
narrative_ontology:human_readable(individual_right_reading__bruen_methodology_reading, "Bruen's Historical Methodology: Suppression of Tiered Scrutiny in Gun Regulation").
narrative_ontology:topic_domain(individual_right_reading__bruen_methodology_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(individual_right_reading__bruen_methodology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(individual_right_reading__bruen_methodology_reading, '9aad37b1-ebe1-49ee-bbf1-57a4e18444a7').
narrative_ontology:cs_kernel_codification('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', formalized).
narrative_ontology:cs_authority_grounding('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', lineage).
narrative_ontology:cs_interpretation_layer_present('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7').
narrative_ontology:cs_reading_relation('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', individual_right_reading__heller_core_reading, coexists_with).
narrative_ontology:cs_reading_relation('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', individual_right_reading__sensitive_places_reading, influences).
narrative_ontology:cs_axiom('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', foundational, original_public_meaning_binding).
narrative_ontology:cs_axiom_status(original_public_meaning_binding, holdable).
narrative_ontology:cs_axiom_grounding('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', original_public_meaning_binding, deontological).
narrative_ontology:cs_axiom('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', foundational, historical_analogue_sufficiency).
narrative_ontology:cs_axiom_status(historical_analogue_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', historical_analogue_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', founding_era_interpretive_constraint).
narrative_ontology:cs_drift_state('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', post_bruen_application_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9aad37b1-ebe1-49ee-bbf1-57a4e18444a7', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(individual_right_reading__bruen_methodology_reading, individual_right_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_right_reading__bruen_methodology_reading, second_amendment_challengers).
narrative_ontology:constraint_victim(individual_right_reading__bruen_methodology_reading, novel_regulatory_responses).
narrative_ontology:constraint_victim(individual_right_reading__bruen_methodology_reading, public_safety_experimentation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVEL REGULATORY RESPONSE (SNARE) — State legislatures and federal agencies attempting to regulate newly emergent weapons technologies (3D-printed firearms, autonomous delivery systems, AI-assisted targeting) face structural impossibility: they must justify regulation by finding a historical analogue from 1787-1791. Modern harms (mass shooting logistics, child access via unserialized weapons, high-capacity magazine ammunition distribution) have no historical parallel. The regulatory agent is trapped — cannot exit the historical methodology requirement, cannot argue novel harms justify novel regulation, cannot conduct interest balancing. Maximum suppression of alternative approaches.
constraint_indexing:constraint_classification(individual_right_reading__bruen_methodology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH PRACTITIONER (SNARE) — Epidemiologists, trauma surgeons, and public health officials attempting to design evidence-based gun violence interventions face constrained but not zero options. They can fund research (though NIH gun research restriction was only formally lifted in 2018), they can document harms, they can petition legislatures. But their evidentiary approach — cost-benefit analysis, harm reduction, novel intervention testing — is systematically suppressed by the historical methodology. They bear extraction: their expertise is treated as irrelevant; their capacity to innovate is subordinated to the historical test.
constraint_indexing:constraint_classification(individual_right_reading__bruen_methodology_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SECOND AMENDMENT LITIGANTS (ROPE) — Armed with Bruen's historical methodology, litigants can challenge novel regulations by demanding historical analogues. They experience the constraint as pure coordination: the methodology gives them a lever, a language, a litigation strategy. No interest balancing means their claims need not overcome public safety arguments — only historical proof. High arbitrage: litigants can forum-shop, can select favorable historical periods, can frame 'bearing arms' to include technologies the founders never imagined. Net beneficiary.
constraint_indexing:constraint_classification(individual_right_reading__bruen_methodology_reading, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL COURTS (TANGLED ROPE) — Courts are both beneficiary and victim. Benefit: Bruen provides a clear decision rule (historical test), reducing indeterminacy from the prior 'interest balancing' framework. They avoid making policy judgments about whether regulations serve compelling state interests. Extraction: they are bound by historical methodology regardless of how poorly it fits novel circumstances. They cannot exercise judicial discretion; they become historians-in-robes rather than adjudicators. Mixed position — clear rule (benefit), but rigid rule (cost).
constraint_indexing:constraint_classification(individual_right_reading__bruen_methodology_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPREME COURT MAJORITY COALITION (ROPE) — The Court's Bruen majority experiences the historical methodology as a coordination mechanism: it unified diverse constitutional conservatives around a shared interpretive principle, solved the doctrinal instability of interest-balancing, and provided a clear rule that lower courts could apply. The Court maintains arbitrage: it can declare what counts as a 'historical analogue,' can select which founding-era documents to consult, can define 'analogous' capaciously or narrowly. The methodology is the Court's creation; it experiences the constraint as its coordinating tool.
constraint_indexing:constraint_classification(individual_right_reading__bruen_methodology_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: GUN SAFETY ADVOCATE (SNARE) — Advocates for reducing gun deaths face high suppression. Interest balancing is suppressed — they can no longer argue 'this modest regulation serves a compelling state interest in public safety.' The historical test locks them out: if they cannot find a 1791 precedent for red-flag laws, extreme risk protection orders, or universal background checks, the regulation is unconstitutional regardless of evidence. They experience systematic extraction: their policy preferences are delegitimized not by evidence or argument but by methodology. No exit path except finding historical analogues (usually impossible) or waiting for constitutional amendment (generational timeline).
constraint_indexing:constraint_classification(individual_right_reading__bruen_methodology_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an analytical position, one can see the historical methodology as reflecting an immutable constraint: constitutional law must be tethered to the founding document's original public meaning; deviating from that anchor produces interpretive chaos. The methodology appears as a natural law of sound jurisprudence — you cannot have a stable constitutional order if modern interpreters simply invent new rights or reimagine old ones. However, this perspective risks naturalizing what is actually a contested methodological choice. The engine's false summit detector will identify this as a false summit: the historical methodology is not a law of nature but a doctrinal invention by the Bruen majority, grounded in choices about which historical sources to privilege.
constraint_indexing:constraint_classification(individual_right_reading__bruen_methodology_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_right_reading__bruen_methodology_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_right_reading__bruen_methodology_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_right_reading__bruen_methodology_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(individual_right_reading__bruen_methodology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(individual_right_reading__bruen_methodology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High but not extreme. The methodology systematically disadvantages novel regulatory responses and public health experimentation, but it does so through a doctrinal rule rather than through brute coercion. The extraction flows through the suppression of alternative methods (interest balancing, cost-benefit analysis), not through direct enforcement. The trajectory shows rising extractiveness: t0 (0.45) at Bruen's announcement, rising through early applications as courts strike down modern regulations lacking historical analogues, reaching t4 (0.62) as the methodology hardens and courts develop consistent interpretive moves. Suppression (0.72): High. The elimination of interest balancing is structural suppression — state legislatures cannot argue 'this regulation is minimal, narrowly tailored, and serves a compelling state interest in public safety.' They must find a historical precedent. The founding era was not systematically documented in the way modern regulation is; finding exact analogues for 21st-century weapons is often impossible. Suppression increases over time (0.55 → 0.72) as courts clarify that interest balancing is foreclosed entirely. Theater ratio (0.58): Moderate-high. The methodology performs a legitimate jurisprudential function (constraining judicial discretion by tethering interpretation to text and original meaning). But it also functions as theater: courts can perform historical rigor while selectively choosing founding-era sources that support their outcomes. The trajectory shows rising theater (0.42 → 0.58) as district and appellate courts develop technique to find historical support for various regulations or to deny such support, depending on the court's view of Second Amendment scope.
 *
 * PERSPECTIVAL GAP:
 *   The methodology produces maximum perspectival divergence. Litigants and the Supreme Court majority see a coordination mechanism that enabled constitutional clarity and procedural fairness. Regulatory agencies, legislatures, and public health practitioners see suppression and extraction — a binding constraint that forecloses their preferred policy approaches. The gap is not empirical disagreement but structural: beneficiaries experience coordination; victims experience extraction. This is a hallmark of a Snare with institutional beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Second Amendment litigants derive low d (beneficiary + arbitrage exit = ~0.15) → low/negative f(d) → negative effective extraction (they benefit). Novel regulatory responses derive high d (victim + trapped exit = ~0.95) → high f(d) → high effective extraction (they bear costs). Federal courts occupy a mixed position: institutional power but constrained by the methodology (d ~ 0.55) → moderate f(d) → moderate extraction experience. The Supreme Court majority derives low d (beneficiary + arbitrage over the methodology itself = ~0.10) → very negative f(d) → they extract value from the constraint they created. Public health practitioners derive high d (victim + constrained exit = ~0.85) → high f(d) → high extraction experience. The perspectival gap reflects that the same methodology produces opposite directionalities depending on whether the agent created the rule (low d) or is subordinated by it (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Snare classification describes the constraint's effect on novel regulatory responses and public health experimentation, while the Rope classification describes the constraint's effect on Second Amendment litigants. Both are correct from their respective positions. The false summit risk arises when the analytical observer naturalizes the methodology as a law of sound jurisprudence (Mountain) — this risks treating a contested doctrinal choice as immutable. The engine's false summit detector will identify this constraint as a candidate FSM because: (a) the mountain perspective naturalizes the methodology as necessary for stable constitutional interpretation; (b) identifiable beneficiaries exist (litigants, the Supreme Court majority); (c) an alternative framing (Heller's interest-balancing approach) shows that the methodology is contingent, not necessary. The mandatrophy resolves by showing that the constraint's type depends entirely on perspective — it is a Snare for those it constrains, a Rope for those who benefit, and a false summit for those who naturalize it as necessary jurisprudence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_analogue_determinacy,
    'What counts as a sufficiently close ''historical analogue'' to 1791 regulations for a modern regulation to pass the Bruen test?',
    'Longitudinal analysis of district and appellate court applications of Bruen; documentation of which modern regulations courts hold are and are not supported by historical analogues; assessment of whether the same regulation is upheld or struck down in different circuits',
    'If determinacy is high: Bruen provides a clear limiting principle on gun regulation, and the extractiveness is justified by rule-of-law benefits. If determinacy is low: Bruen becomes a disguise for outcome-driven judging, and the suppression of novel regulation is pure extraction without coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_analogue_determinacy, empirical, 'Whether the historical analogue test produces consistent, predictable outcomes across courts').

omega_variable(
    founding_era_regulatory_scope,
    'Did the founding era regulate as broadly and intrusively as the Bruen methodology assumes when it accepts ''longstanding'' regulations as presumptively constitutional?',
    'Detailed historical scholarship on the actual scope of 1791-era gun regulation (licensing, registration, carry restrictions, militia training requirements); comparison with modern regulations claimed to be ''longstanding''',
    'If founding era was highly regulatory: the historical test constrains modern regulation less than Bruen suggests; modern regulations may find historical analogues more readily. If founding era was minimally regulatory: Bruen''s constraint is structurally more restrictive; novel regulations have fewer historical footings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_era_regulatory_scope, empirical, 'Historical scope of gun regulation in founding era').

omega_variable(
    novel_weapons_technological_lock,
    'Is the historical methodology''s requirement that new regulations find founding-era analogues fundamentally incompatible with regulating weapons that did not exist in 1791?',
    'Analysis of court responses to regulations addressing 3D-printed firearms, binary triggers, forced-reset triggers, and other technologies absent from founding era; determination of whether courts find analogues or declare modern weapons unprotected',
    'If courts find creative analogues: the technological lock is looser than the methodology suggests. If courts declare modern weapons unprotected by Second Amendment: the methodology enables perverse results (weapons more dangerous than those founders knew are less regulated). If courts strike down regulations of modern weapons: Bruen suppresses experimentation with novel regulatory approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(novel_weapons_technological_lock, empirical, 'Whether historical methodology can accommodate regulation of pre-1791 technologies').

omega_variable(
    methodology_vs_outcome_substitution,
    'Is the Bruen methodology a neutral application of historical interpretation, or a methodological choice designed to produce Second Amendment-protective outcomes regardless of historical evidence?',
    'Comparative analysis: do courts apply historical originalism as rigorously to Second Amendment as to other constitutional provisions (Fourth Amendment search doctrine, Fourteenth Amendment incorporation)? Evidence of selective historical cherry-picking or asymmetric methodological rigor.',
    'If methodology is neutral: Bruen reflects legitimate constitutional principle, and suppression is justified by the need for stable interpretation. If methodology is outcome-driven: Bruen is a doctrinal disguise for policy preferences, and suppression is pure extraction without rule-of-law legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_vs_outcome_substitution, conceptual, 'Whether Bruen methodology is neutral or outcome-driven').

omega_variable(
    reading_one_vs_all_kernels,
    'Does this reading (Bruen''s historical methodology) foreclose the other readings of the individual-right kernel, or do they coexist as live doctrinal positions?',
    'Analysis of the logical structure: does adopting Bruen''s methodology require rejecting Heller''s core holding (handguns in the home)? Does it require rejecting the sensitive places reading? Or can courts hold all three simultaneously?',
    'If Bruen forecloses the others: the kernel has collapsed to a single unified doctrine. If they coexist: the kernel remains contested despite Bruen''s majority rule. Coexistence would manifest as doctrinal instability and circuit splits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_one_vs_all_kernels, conceptual, 'Logical structure of kernel readings: foreclosure vs coexistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_right_reading__bruen_methodology_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bruen_meth_theater_t0, individual_right_reading__bruen_methodology_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bruen_meth_theater_t2, individual_right_reading__bruen_methodology_reading, theater_ratio, 2, 0.5).
narrative_ontology:measurement(bruen_meth_theater_t4, individual_right_reading__bruen_methodology_reading, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(bruen_meth_extract_t0, individual_right_reading__bruen_methodology_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bruen_meth_extract_t2, individual_right_reading__bruen_methodology_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(bruen_meth_extract_t4, individual_right_reading__bruen_methodology_reading, base_extractiveness, 4, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bruen_meth_supp_t0, individual_right_reading__bruen_methodology_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bruen_meth_supp_t2, individual_right_reading__bruen_methodology_reading, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(bruen_meth_supp_t4, individual_right_reading__bruen_methodology_reading, suppression_requirement, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_right_reading__bruen_methodology_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(individual_right_reading__bruen_methodology_reading, individual_right_reading__heller_core_reading).
narrative_ontology:affects_constraint(individual_right_reading__bruen_methodology_reading, individual_right_reading__sensitive_places_reading).

% DUAL FORMULATION NOTE:
% The Bruen methodology reading is one of three analytically distinct constraints that together constitute the post-Heller Second Amendment doctrine. Heller's core reading (handguns in the home) has lower extractiveness (ε ~ 0.25, Rope) because it established legitimate coordination without excessive suppression. The sensitive places reading (geographic carve-outs for certain locations) has moderate extractiveness (ε ~ 0.40, Tangled Rope) because it coordinates spatial access with some extraction. The Bruen methodology reading has higher extractiveness (ε = 0.62, Snare) because it systematically suppresses tiered scrutiny and novel regulation. All three readings are structurally necessary to the post-Heller doctrine; the constraint family as a whole exhibits higher total extractiveness than any single reading because the readings reinforce each other's suppressive effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(individual_right_reading__bruen_methodology_reading, institutional, 0.08).
constraint_indexing:directionality_override(individual_right_reading__bruen_methodology_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
