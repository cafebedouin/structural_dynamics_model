% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_accountability_void, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine: Accountability Void Reading
 *   domain: constitutional_law/civil_rights/law_enforcement_accountability
 *
 * SUMMARY:
 *   Qualified immunity doctrine is a judicial creation (not in § 1983
 *   statutory text) that shields state officers from personal § 1983 damages
 *   liability except in cases where the officer violated a 'clearly
 *   established' constitutional right. This reading, the
 *   accountability_void_reading, models qualified immunity as a systematic
 *   extraction mechanism that guarantees impunity for constitutional
 *   violations by making the remedy (damages suit) structurally unavailable.
 *   The constraint operates through a circularity: a right is 'clearly
 *   established' only if a prior case with materially identical facts has
 *   declared it; new violations are by definition not clearly established;
 *   therefore novel constitutional violations enjoy immunity. The victim
 *   faces a catch-22 — they cannot win a suit because no prior case
 *   establishes the violation, but no case law can be established until
 *   someone wins a suit. This reading is one of three structurally distinct
 *   interpretations of the QI kernel. The protective_scaffold_reading argues
 *   that QI is necessary to protect officer discretion and that 'clearly
 *   established law' is a coherent standard enabling genuine protection. The
 *   constitutional_fidelity_reading argues that QI is inconsistent with §
 *   1983 statutory text, which creates liability for violations of
 *   constitutional rights with no textual immunity. The
 *   accountability_void_reading argues that regardless of the doctrine's
 *   original intent, its current instantiation and doctrinal trajectory have
 *   created near-absolute immunity, making it an extraction mechanism. This
 *   reading's structural claim: the doctrine systematically shields from
 *   consequences those who violate rights, and shields governments from
 *   vicarious liability for officers' violations, while placing the full cost
 *   on rights-violation victims who have no remedy. The extractiveness
 *   trajectory (0.62 → 0.71 → 0.81 from 1971 to 2024) reflects the ratcheting
 *   effect of expanding 'clearly established law' requirement and
 *   increasingly stringent summary judgment standards for qualified immunity,
 *   making the immunity more comprehensive over time.
 *
 * KEY AGENTS:
 *   - Law Enforcement Officers: Primary beneficiary (institutional/arbitrage) — shielded from personal damages liability; experience doctrine as protective of discretion
 *   - Constitutional Rights Victims: Primary victim (powerless/trapped) — suffer constitutional violations with no damages remedy; caught in clearly-established-law circularity
 *   - Government Jurisdictions (cities, counties, states): Secondary beneficiary (institutional/arbitrage) — shielded from vicarious liability; experience cost-reduction from limited settlements
 *   - Civil Rights Litigation Infrastructure: Mixed (organized/mobile-constrained) — benefits from mobilization against QI but constrained by immunity from setting precedent; operates in a high-friction environment
 *   - Supreme Court / Doctrinal Institution: Piton actor (institutional/arbitrage) — maintains degraded doctrine through institutional inertia despite expressed concern from sitting justices
 *   - Alternative Accountability Systems (administrative discipline, state courts, criminal prosecution): Weak actors (moderate/constrained) — cannot fully substitute for § 1983 remedy despite potential coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.81).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.78).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine: Accountability Void Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement_accountability").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, 'b6d0eb89-0193-46f6-9f4a-3911ee20cfef').
narrative_ontology:cs_kernel_codification('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', fixed_text).
narrative_ontology:cs_authority_grounding('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', extraction).
narrative_ontology:cs_interpretation_layer_present('b6d0eb89-0193-46f6-9f4a-3911ee20cfef').
narrative_ontology:cs_reading_relation('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', qualified_immunity_doctrine__constitutional_fidelity_reading, influences).
narrative_ontology:cs_axiom('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', foundational, accountability_gap_is_rights_violation).
narrative_ontology:cs_axiom_status(accountability_gap_is_rights_violation, holdable).
narrative_ontology:cs_axiom_grounding('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', accountability_gap_is_rights_violation, deontological).
narrative_ontology:cs_axiom('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', foundational, clearly_established_law_circularity_empirical_absolute_immunity).
narrative_ontology:cs_axiom_status(clearly_established_law_circularity_empirical_absolute_immunity, holdable).
narrative_ontology:cs_axiom_grounding('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', clearly_established_law_circularity_empirical_absolute_immunity, empirically_contingent).
narrative_ontology:cs_reference_frame('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', full_constitutional_remedy_for_rights_violations).
narrative_ontology:cs_drift_state('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', contemporary_expanded_immunity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b6d0eb89-0193-46f6-9f4a-3911ee20cfef', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, government_jurisdictions).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, constitutional_rights_victims).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_remedial_capacity).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, equal_protection_guarantees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VICTIM OF CONSTITUTIONAL VIOLATION (SNARE) — Officer commits clear constitutional violation (unreasonable seizure, excessive force, warrantless arrest). Victim has no remedy: qualified immunity bars § 1983 suit unless victim can cite prior case with materially identical facts ('clearly established law'). This is structurally a catch-22 — no case law until a victim wins a suit, but no suit succeeds without prior case law. Victim is trapped with no exit and no recourse. Maximum extraction: the officer acted with constitutional knowledge they would face no consequence.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__accountability_void_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MUNICIPAL GOVERNMENT / JURISDICTION (TANGLED ROPE) — City faces liability pressure from civil rights lawsuits and negative publicity, but qualified immunity provides significant insulation. Municipality benefits from cost-reduction (fewer settlements, lower liability exposure) but also bears costs: reputational damage from high-profile violations, pressure to maintain officer discipline independently of litigation threat, tension between federal funding conditions (sometimes tied to compliance) and qualified immunity shields. Mixed extraction and coordination function — some genuine coordination (need to maintain government function without paralyzing officer discretion) alongside asymmetric extraction (cost-shifting to victims).
constraint_indexing:constraint_classification(qualified_immunity_doctrine__accountability_void_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LAW ENFORCEMENT OFFICER (ROPE) — Officer perceives qualified immunity as coordination mechanism solving a legitimate collective action problem: officers need discretion to act decisively in uncertain situations without fear of personal financial ruin from frivolous litigation. The doctrine provides this through the 'clearly established law' requirement. From the officer's immediate perspective, QI is not an extraction mechanism but a necessary protection enabling effective policing. Officers experience this as coordination that enables their function, not as extraction.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__accountability_void_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS ADVOCATES & LITIGATION INFRASTRUCTURE (TANGLED ROPE) — Civil rights attorneys and advocacy organizations (NAACP-LDF, ACLU, Center for Constitutional Rights, local legal aid) benefit from qualified immunity in one sense — it creates a clear 'enemy' for fundraising and mobilization, and generates the ongoing need for litigation infrastructure. But they face extreme extraction: the clearly-established-law bar makes almost every novel violation immune, gutting the litigation strategy. This is a constrained-to-mobile paradox: they have resources and expertise (mobile), but the doctrine's structure (requires pre-existing case law) constrains their ability to set precedent through litigation. Mixed extraction and coordination — both elements genuinely present.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__accountability_void_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPREME COURT DOCTRINE & PRECEDENT INSTITUTION (PITON) — The Supreme Court created and maintains qualified immunity doctrine through case law interpretation (not statutory text). The doctrine persists despite documented critique from sitting justices (Sotomayor), legal scholars (Baude, Sachs), and comparative constitutional systems. The SCOTUS institution sees the doctrine as degraded — multiple justices have signaled concern — but the precedent persists through institutional inertia. Reversal requires either SCOTUS majority change or Congressional intervention. The performative element: QI is maintained through formal reasoning about 'clearly established law' and 'objectively reasonable' officer conduct, but the practical effect is nearly absolute immunity. Theater ratio reflects this gap between doctrinal language and actual outcome.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__accountability_void_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURAL LAW VIEW / INHERENT LIMITS (MOUNTAIN) — From a civilizational perspective, some form of official immunity from personal liability is inevitable and natural: officers cannot function if every discretionary decision risks personal bankruptcy. The argument naturalizes: functional government inherently requires some shield against consequences. However, this is a false summit — the natural-law framing masks the contingent choice to place the shield at the 'clearly established law' bar rather than at alternative thresholds (e.g., 'in violation of a constitutional right,' 'reckless disregard for rights,' 'lack of probable cause,' standard tort negligence). The constraint is not natural law but a specific doctrinal allocation that benefits officers and governments at the cost of victim remedies.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__accountability_void_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qualified_immunity_doctrine__accountability_void_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qualified_immunity_doctrine__accountability_void_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, TR),
    TR >= 0.70.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.81): High and rising. The doctrine shields officers from consequences for constitutional violations. Base extractiveness reflects the structural reality that the 'clearly established law' requirement eliminates remedy for novel constitutional violations — this is systematic extraction from victims to officers. The rising trajectory (0.62 → 0.81 over 53 years) reflects doctrinal drift toward stricter immunity standards. Early QI (1970s) was a qualified immunity with some access to jury; current QI (2024) is near-absolute immunity for officials in discretionary roles. Suppression (0.78): High. The constraint operates through multiple suppressive mechanisms: (a) legal barrier — qualified immunity bars the suit on summary judgment; (b) epistemic barrier — victims and advocates cannot identify 'clearly established' standards in advance; (c) cost barrier — litigation costs are borne by victims while officers face no personal cost; (d) institutional barrier — alternative accountability mechanisms (administrative, criminal) are substantially weaker than § 1983 damages. Theater ratio (0.65): Moderate-high and rising. The performative element is the 'clearly established law' and 'objective reasonableness' standard, which operates as formal legal reasoning but produces outcomes (near-absolute immunity) that do not match the doctrine's stated protective intent. The rising trajectory reflects increasing gap between doctrinal language (protection of genuine discretion) and actual effect (systematic immunity).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Officers experience it as rope (necessary coordination mechanism protecting discretion). Victims experience it as snare (systematic extraction with no exit). Litigation advocates experience it as tangled rope (mixed benefit and constraint). Governments experience it as tangled rope (cost reduction alongside reputational pressure). The Supreme Court institutional perspective is piton (acknowledging degradation while maintaining through inertia). The civilizational analytical view risks naturalizing contingent doctrine as inherent limit. The gap between rope (officer perspective) and snare (victim perspective) is the core diagnostic signal — it reveals that the constraint's protective framing (rope) masks systematic immunity (snare).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status and exit options. Officers are beneficiaries with arbitrage-exit: they can comply with law, pay damages, and continue functioning (they exit the constraint by accepting accountability). But they choose not to — they benefit from immunity. Their d is low (0.05–0.15, around beneficiary + institutional canonical). Victims are pure victims with no exit: they cannot exit the constraint by complying with law (officers violated it), cannot exit by leaving jurisdiction (it's national), cannot exit by accepting damages (remedy is barred). Their d is maximal (0.95). This asymmetry is the structural description of extraction: officers benefit, victims bear cost, exit asymmetry is extreme. The f(d) sigmoid maps victim d=0.95 to maximum experienced extractiveness (~1.42 in legacy π scale). Scope modifier σ(national) = 1.0, so chi calculation for victims is χ = 0.81 × 1.42 × 1.0 ≈ 1.15, confirming snare-level experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing this as a snare reading of a contested kernel. The constraint is high-extraction (0.81) with substantial suppression (0.78) and performative elements (theater 0.65). The mandatrophy check: 'Is this pure extraction or genuine coordination?' The protective_scaffold_reading claims coordination — officers need discretion protection. The accountability_void_reading claims pure extraction — the 'protection' masks immunity. The mandatrophy resolves through the directionality analysis: if officers genuinely needed the specific 'clearly established law' standard to function (rather than alternative standards like 'reckless disregard' or 'lack of probable cause'), then some coordination function exists. But the empirical trajectory (rising extractiveness, expanding immunity, rising theater) suggests the doctrine has drifted from protection toward extraction. The reading's coherence lies in documenting this drift — QI may have started as protective scaffolding (coordinate protection + some openness to liability) and degraded into pure snare (near-absolute immunity). The classification as snare (not tangled rope) is justified because the coordination function (if it existed) has atrophied and the doctrine now operates primarily as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clearly_established_law_circularity,
    'Does the ''clearly established law'' requirement create an impossible circularity that amounts to absolute immunity in practice?',
    'Empirical analysis: (a) percentage of qualified immunity motions granted in practice; (b) success rate of cases that reach summary judgment on QI grounds; (c) comparison of constitutional violation frequency to damages awards; (d) analysis of ''novel facts'' determination by courts (how frequently are facts deemed insufficient precedent?)',
    'If the circularity is empirically real (>90% of QI motions granted, <5% of novel violations reach jury): this confirms snare classification from victim perspective and rules out protective_scaffold_reading (which requires genuine discretionary protection vs actual chilling effect on accountability). If circularity is overstated: snare classification weakens toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clearly_established_law_circularity, empirical, 'Whether clearly established law requirement creates empirical absolute immunity').

omega_variable(
    officer_chilling_effect_threshold,
    'What level of liability threat (what doctrinal standard) would actually chill officer discretion vs. what level merely prevents accountability?',
    'Comparative analysis: jurisdictions or periods with different QI standards (e.g., pre-Harlow absolute immunity, post-Harlow reasonable-officer standard, hypothetical strict liability); correlate with data on: officer-involved incidents, use-of-force rates, complaint and investigation patterns, settlements and verdicts, officer recruitment and retention',
    'If officer behavior is unaffected by moving from current QI to reasonable-care standard: current QI is not protective of discretion but merely shields extraction. If officer behavior significantly changes: some form of immunity may be structurally necessary. This omega determines whether protective_scaffold_reading has a coherent empirical basis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officer_chilling_effect_threshold, empirical, 'Causal link between liability threat and officer conduct change').

omega_variable(
    congressional_section_1983_intent,
    'What did Congress intend § 1983 to provide as a remedy for constitutional violations by state actors?',
    'Historical analysis: (a) legislative history of 1871 civil rights act (Reconstruction era intent); (b) 1871 common-law context (did officers have immunity?); (c) text of statute (''deprived of any rights, privileges, or immunities''); (d) contemporaneous state court and federal court interpretation',
    'If Congress intended full remedy without officer immunity: QI is ultra vires judicial doctrine inconsistent with statutory text, supporting constitutional_fidelity_reading and undermining this accountability_void_reading''s legitimacy claim. If Congress intended some immunity: both readings have doctrinal grounding. This is the definitional fork between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_section_1983_intent, empirical, 'Congressional intent regarding § 1983 officer liability and immunity').

omega_variable(
    alternative_accountability_mechanisms_effectiveness,
    'Do alternative accountability mechanisms (state tort law, administrative discipline, criminal prosecution, civil service procedures) adequately protect constitutional rights in absence of § 1983 damages remedy?',
    'Empirical comparison: jurisdictions with strong QI immunity paired with: (a) robust state tort remedies; (b) independent civilian oversight boards; (c) state attorney general enforcement; (d) federal criminal Civil Rights Act § 242 prosecution rates. Cross-correlate constitutional violations with accountability outcomes.',
    'If alternative mechanisms are effective: QI may represent a systematic choice to route accountability through different channels (administrative/criminal vs civil damages). If alternatives are documented failures: QI creates systematic immunity gap. This determines whether extractiveness should be adjusted downward or remains at 0.81.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_accountability_mechanisms_effectiveness, empirical, 'Efficacy of alternative accountability mechanisms absent § 1983 remedy').

omega_variable(
    qualified_immunity_reading_kernel_distinction,
    'Which reading of the qualified immunity doctrine kernel is empirically instantiated in current Supreme Court jurisprudence and lower court practice?',
    'Doctrinal analysis: (a) track Supreme Court language in recent QI decisions (Kisela 2018, McCoy 2020, Taylor v. Riojas 2020); (b) analyze summary judgment grant rates and reasoning patterns in lower courts; (c) examine how courts apply ''clearly established law'' and ''objective reasonableness'' in practice vs. doctrinal language; (d) compare SCOTUS majority rhetoric (protective scaffolding narrative) vs. observed outcomes (systematic immunity)',
    'If observed practice matches accountability_void_reading (near-absolute immunity in practice): this reading is the true structural description. If practice matches protective_scaffold_reading (genuine protection of reasonable discretion): the readings are both live and the snare classification should be reconsidered as tangled_rope. This omega documents which reading has structural coherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qualified_immunity_reading_kernel_distinction, empirical, 'Which QI reading is instantiated in actual judicial practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qiav_theater_1971, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(qiav_theater_1989, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1, 0.52).
narrative_ontology:measurement(qiav_theater_2024, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2, 0.65).

% Extraction over time
narrative_ontology:measurement(qiav_extractiveness_1971, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(qiav_extractiveness_1989, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1, 0.71).
narrative_ontology:measurement(qiav_extractiveness_2024, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(qiav_suppression_1971, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qiav_suppression_1989, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1, 0.68).
narrative_ontology:measurement(qiav_suppression_2024, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, police_union_contract_immunity).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, municipal_liability_doctrine).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, state_tort_immunity).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, civil_rights_remedial_gap).

% DUAL FORMULATION NOTE:
% The qualified_immunity_doctrine kernel has three structurally distinct readings, each with its own ε value and classification. This file instantiates accountability_void_reading (ε=0.81, snare). Sibling readings (protective_scaffold_reading and constitutional_fidelity_reading) are separate constraint stories with different ε values reflecting different empirical assessments. All three link through network.affects_constraints to show their interdependence within the kernel structure. The constraint family is: (1) accountability_void_reading (this file, ε=0.81, snare, empirical claim: immunity is nearly absolute); (2) protective_scaffold_reading (separate file, ε≈0.35–0.45, tangled rope or scaffold, empirical claim: immunity is necessary for discretion); (3) constitutional_fidelity_reading (separate file, ε≈0.55–0.65, tangled rope, empirical claim: QI is inconsistent with § 1983 text but operates as mixed mechanism). The epsilon difference reflects that the readings make genuinely different empirical predictions about what the doctrine does. Downstream constraints (police_union_contract_immunity, municipal_liability_doctrine) are affected by which reading of QI becomes institutionally dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, institutional, 0.1).
constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
