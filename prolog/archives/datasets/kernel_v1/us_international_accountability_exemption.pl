% ============================================================================
% CONSTRAINT STORY: us_international_accountability_exemption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_international_accountability_exemption, []).

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
 *   constraint_id: us_international_accountability_exemption
 *   human_readable: U.S. International Accountability Exemption
 *   domain: international_law/geopolitics
 *
 * SUMMARY:
 *   The U.S. International Accountability Exemption is a structural
 *   arrangement whereby the United States maintains systematic exclusion from
 *   international criminal jurisdiction while simultaneously participating
 *   in, funding, and legitimizing international law institutions that apply
 *   such jurisdiction to other nations. The exemption operates through
 *   multiple reinforcing mechanisms: non-ratification of the International
 *   Criminal Court statute, Security Council veto power over ICC
 *   investigations and prosecutions, bilateral immunity agreements ('Article
 *   98 agreements') that shield U.S. citizens from extradition, and
 *   rhetorical framing of accountability as a constraint on sovereignty
 *   rather than a universal principle. The constraint exhibits the full
 *   spectrum of DR classifications from different structural positions,
 *   making it a diagnostic exemplar for how power asymmetry manifests as
 *   perspectival disagreement. From the perspective of the U.S. military
 *   apparatus, the exemption is pure coordination (Rope) — enabling strategic
 *   freedom without international constraint. From the perspective of victim
 *   populations in non-aligned states, it is pure extraction (Snare) —
 *   systematic exclusion from justice. From the perspective of the ICC
 *   itself, it is a hybrid (Tangled Rope) — genuine coordination benefits
 *   from U.S. institutional support overlaid with severe extraction
 *   (inability to prosecute the most powerful actor, delegitimization). The
 *   constraint's theater ratio (0.68) reflects that extensive rhetoric about
 *   'international justice' and 'rule of law' masks the categorical exemption
 *   for the most powerful actor. The suppression value (0.72) reflects that
 *   structural barriers to accountability are reinforced by diplomatic and
 *   military power — nations cannot meaningfully challenge the exemption
 *   without bearing significant costs.
 *
 * KEY AGENTS:
 *   - U.S. Military and Executive Apparatus: Primary beneficiary (institutional/arbitrage) — captures strategic freedom to conduct military operations and interventions without fear of international prosecution
 *   - U.S. Defense Contractors: Secondary beneficiary (institutional/arbitrage) — profit from military interventions enabled by exemption; shield against liability for civilian harm
 *   - Victim Populations in Non-Aligned States: Primary victims (powerless/trapped) — bear the human costs of military interventions with no access to international justice mechanisms
 *   - Non-Aligned Nation-States: Secondary victims (moderate/constrained) — bound by ICC rules while U.S. operates outside framework; face asymmetric jurisdictional burden
 *   - International Criminal Court: Institutional actor experiencing mixed effects (institutional/constrained) — receives U.S. support and legitimacy while being rendered structurally impotent to prosecute the primary global military power
 *   - European Allies and Allied Nation-States: Tertiary actors (powerful/mobile) — benefit from security alliance with U.S. (coordination) while bearing cost of jurisdictional asymmetry (extraction)
 *   - International Legal Order: Systemic actor (institutional/arbitrage) — maintains theatrical facade of universal accountability while institutionalizing exception for hegemon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_international_accountability_exemption, 0.58).
domain_priors:suppression_score(us_international_accountability_exemption, 0.72).
domain_priors:theater_ratio(us_international_accountability_exemption, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_international_accountability_exemption, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_international_accountability_exemption, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_international_accountability_exemption, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_international_accountability_exemption, tangled_rope).
narrative_ontology:human_readable(us_international_accountability_exemption, "U.S. International Accountability Exemption").
narrative_ontology:topic_domain(us_international_accountability_exemption, "international_law/geopolitics").

domain_priors:requires_active_enforcement(us_international_accountability_exemption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_international_accountability_exemption, us_military_apparatus).
narrative_ontology:constraint_beneficiary(us_international_accountability_exemption, us_executive_branch).
narrative_ontology:constraint_beneficiary(us_international_accountability_exemption, us_defense_contractors).
narrative_ontology:constraint_victim(us_international_accountability_exemption, international_criminal_justice_system).
narrative_ontology:constraint_victim(us_international_accountability_exemption, non_aligned_nations).
narrative_ontology:constraint_victim(us_international_accountability_exemption, indigenous_populations_affected_by_us_interventions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VICTIM POPULATIONS (SNARE) — Populations affected by U.S. military interventions cannot pursue accountability through the ICC because the U.S. is exempted. They face structural impossibility of legal recourse. The constraint extracts from them (no access to justice) while conferring immunity on the enforcer. Maximum experienced extraction because exit requires changing the international legal architecture itself.
constraint_indexing:constraint_classification(us_international_accountability_exemption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ALIGNED NATION-STATES (SNARE) — These states are bound by ICC rules (many are signatories) while the U.S. operates outside that framework. They face asymmetric jurisdictional burden: their citizens and officials can be prosecuted; U.S. citizens and officials cannot. Exit options are constrained — leaving the ICC means abandoning access to international justice mechanisms, a costly move that signals weakness.
constraint_indexing:constraint_classification(us_international_accountability_exemption, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERNATIONAL CRIMINAL COURT (TANGLED ROPE) — The ICC experiences genuine coordination benefits from U.S. support (funding contributions, diplomatic backing, security cooperation) alongside severe extraction (structural inability to prosecute the most powerful military actor, delegitimization when the exemption is exposed, constrained by U.S. Security Council veto). The ICC's legitimacy is compromised by the exemption it cannot overcome, yet it depends on U.S. institutional support to function.
constraint_indexing:constraint_classification(us_international_accountability_exemption, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: U.S. MILITARY AND EXECUTIVE (ROPE) — Experiences the exemption as pure coordination: ability to conduct operations without fear of international prosecution enables strategic freedom. No perceived extraction — only benefits. The constraint solves a coordination problem from their perspective: 'How do we act in world affairs without international legal constraint?' Answer: maintain the exemption. Arbitrage exit because they can shift military operations across legal jurisdictions and maintain exemption through non-ratification and veto power.
constraint_indexing:constraint_classification(us_international_accountability_exemption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EUROPEAN ALLIES (TANGLED ROPE) — These states experience coordination benefits from security alliance with the U.S. alongside extraction: they are bound by ICC rules while their primary security guarantor is exempt, creating asymmetry. They have some mobility (could form alternative security arrangements or pressure the U.S. to ratify the ICC) but exercise it reluctantly due to dependence on U.S. security guarantees. Genuine coordination function (NATO, intelligence sharing) overlaid with asymmetric extraction (unequal ICC jurisdiction).
constraint_indexing:constraint_classification(us_international_accountability_exemption, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL ORDER (PITON) — The formal international legal architecture (UN Charter, ICC statute, human rights conventions) presents as a universal system of justice, but the U.S. exemption reveals this as theatrical. The system performs universality while structurally institutionalizing exception for the most powerful actor. The theater ratio is high: elaborate ICC procedures, Security Council debates, and diplomatic language about 'international justice' mask the fact that the most consequential actor is categorically exempt. The piton classification derives from degradation of the core function (universal accountability) through institutional inertia and theater.
constraint_indexing:constraint_classification(us_international_accountability_exemption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SYSTEMIC INEVITABILITY (MOUNTAIN) — From a civilizational perspective, this might appear as an inevitable feature of great power politics: no great power ever submits itself to jurisdiction by weaker actors; the exemption is a natural law of international relations. However, this perspective is a false summit that naturalizes what is actually a contingent political construction. The structural data reveals active mechanisms (non-ratification, veto power, bilateral immunity agreements) that could be changed through political will, not natural limits.
constraint_indexing:constraint_classification(us_international_accountability_exemption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_international_accountability_exemption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_international_accountability_exemption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_international_accountability_exemption, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_international_accountability_exemption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_international_accountability_exemption, TR),
    TR >= 0.70.

:- end_tests(us_international_accountability_exemption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The U.S. exemption from international criminal jurisdiction is one of the most consequential structural asymmetries in the contemporary international system. The beneficiaries (U.S. military apparatus, defense contractors) extract the ability to conduct military operations, interventions, and occupations without fear of prosecution for war crimes, crimes against humanity, or violations of the laws of war. The extraction is substantial because it applies to the global actor with the largest military apparatus and most frequent intervention capacity. However, it is not as severe as pure snare (χ ≥ 0.66) would be because: (1) the U.S. does experience some constraints from international opinion and alliance relationships, (2) the exemption is contested rather than passively accepted, and (3) some domestic legal accountability mechanisms (Congressional oversight, military justice systems) do exist for U.S. actors. The value of 0.58 reflects that the extraction is real and consequential but not absolute. Suppression (0.72): High. The suppression of accountability operates through multiple reinforcing mechanisms: (1) institutional — non-ratification and veto power make prosecution legally impossible; (2) diplomatic — bilateral immunity agreements extract concessions from other nations; (3) military — the capacity to coerce compliance through force; (4) rhetorical — sovereignty doctrine and exceptionalism narratives normalize the exemption. The measured value reflects that suppression is not total (some international pressure exists, some institutions resist), but the barriers to accountability are formidable. Theater ratio (0.68): High. The constraint exhibits substantial theater: elaborate ICC procedures, Security Council debates about accountability, declarations of commitment to international justice, and participation in international legal institutions create the appearance of universal justice while the structural exemption remains. The theater has increased over time (from 0.55 to 0.68) as international focus on accountability has intensified while the exemption persists, creating cognitive dissonance that the theatrical performance manages by emphasizing 'due process,' 'sovereignty concerns,' and 'strategic partnerships' rather than directly defending the exemption.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The gap between the U.S. institutional perspective (Rope: pure coordination, no extraction) and the victim population perspective (Snare: pure extraction, no exit) is the largest in the dataset. The ICC perspective (Tangled Rope) sits between these extremes — experiencing both coordination benefits and severe extraction. The analytical observer risks naturalizing the exemption as inevitable (Mountain: natural law of international relations) but the structural data reveals this as a false summit. The false summit detection is particularly important here because the 'inevitability' framing is actively deployed by beneficiaries to preclude change. If the analytical observer treats the exemption as a natural law, the analysis naturalizes what is actually a contingent power arrangement that could be changed through political will. The piton perspective is also critical — it reveals that the international legal order's performance of universality is increasingly theatrical as the exemption becomes more salient.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the structural position relative to the extraction flow. The U.S. military apparatus is a beneficiary with arbitrage exit options (can shift military operations across jurisdictions and maintain exemption through institutional control) → derived d ≈ 0.05 → f(d) ≈ -0.12 → negative χ (they experience the constraint as beneficial coordination). Victim populations are identified victims with trapped exit options (cannot pursue accountability without changing international architecture) → derived d ≈ 0.95 → f(d) ≈ 1.42 → high χ (they experience maximum extraction). The ICC is an institutional actor with constrained exit options (cannot prosecute without U.S. cooperation or cannot function without U.S. support) → derived d ≈ 0.60 → f(d) ≈ 0.85 → moderate-high χ (mixed coordination and extraction). Non-aligned nations are victims with constrained exit options (face costs of ICC participation asymmetry but cannot easily withdraw) → derived d ≈ 0.75 → f(d) ≈ 1.10 → high χ (significant extraction). The scope modifier σ(S) = 1.2 (global scope) amplifies the effective extraction by 20%, reflecting that the exemption's impact scales across all international military operations globally.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED THROUGH STRUCTURAL ASYMMETRY EXPOSURE: The constraint resolves the mandatrophy by revealing that the tangled rope classification for the ICC is not an equilibrium but an unstable hybrid exposed to contradiction. The ICC cannot perform its core function (universal criminal accountability) because the most powerful actor is exempted. This is not a hidden contradiction — it is the stated structure of the arrangement. The mandatrophy resolves when the analytical observer recognizes that the 'coordination' the ICC provides (investigating other nations, validating international justice processes) is systematically constrained by the institutional immunity of the hegemonic power. The false summit at the analytical context is crucial: if international relations scholars naturalize the exemption as inevitable, the analysis becomes complicit in the extraction. The resolution requires naming that this is a contingent power arrangement, not a law of nature. The victims' snare perspective (no exit, maximum extraction) and the beneficiary's rope perspective (pure coordination) are not symmetric divergences — they are evidence of the tangled rope's extraction component. A genuine tangled rope has both coordination and extraction visible from multiple perspectives. A false coordinate hidden behind false inevitability language is a snare for those excluded and a rope for those exempted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_identity_ambiguity,
    'Are the beneficiaries of the exemption the U.S. state as a whole, or specifically the military apparatus and executive branch that manage security policy?',
    'Analysis of domestic U.S. political dynamics: would ratifying the ICC create domestic political costs for different U.S. constituencies? Do defense contractors, veterans'' groups, or military leadership perceive direct threats from ICC jurisdiction distinct from broader U.S. interests?',
    'If the military apparatus specifically benefits while broader U.S. civilian population bears reputational costs, the directionality shifts: the constraint extracts from U.S. civil society (reputational cost, international isolation) while benefiting security-state actors. This would lower the institutional beneficiary''s derived d value and increase χ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_ambiguity, conceptual, 'Scope of beneficiary identity within U.S. state structure').

omega_variable(
    coordination_function_authenticity,
    'Does the exemption genuinely coordinate anything for its institutional beneficiaries, or is it pure extraction disguised as coordination?',
    'Historical analysis: has the U.S. military or executive demonstrated that ICC non-ratification is necessary for their operational effectiveness? Are there documented cases where ICC jurisdiction would have prevented legitimate U.S. actions? Counterfactual: what would U.S. security policy look like under ICC jurisdiction?',
    'If no genuine coordination function exists (i.e., the military could operate effectively under ICC jurisdiction with rare exceptions), the constraint reclassifies from Tangled Rope to Snare for the ICC perspective and downgrades the U.S. perspective from Rope to pure extraction. Theater ratio would increase, reflecting purely performative international justice language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Whether exemption serves coordination vs. pure extraction').

omega_variable(
    suppression_mechanism_structural_vs_cultural,
    'Is the suppression of accountability primarily structural (institutional architecture that makes prosecution impossible) or cultural (normalization of exceptionalism and sovereignty doctrine)?',
    'Comparative analysis: other great powers (Russia, China) do not ratify the ICC yet face different rhetorical treatment. U.S. exceptionalism rhetoric vs. explicit sovereignty claims. Survey of international legal professionals on whether suppression feels structural or normative.',
    'If primarily structural: suppression value is justified. If primarily cultural/normative: suppression is lower than measured, and the theater ratio increases (more performative than actually constraining). If mixed: decompose into separate stories tracking institutional (structural) and discursive (normative) suppression separately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_cultural, conceptual, 'Mechanism of suppression: structural vs. cultural/normative').

omega_variable(
    false_summit_verification,
    'Is the mountain classification (systemic inevitability) a genuine natural law of international relations, or a false summit naturalizing a contingent power arrangement?',
    'Historical analysis: have other great powers operated under international criminal jurisdiction? Are there credible institutional models for universal accountability? Does the exemption require active U.S. political choice (non-ratification, veto power) or would it emerge automatically from international anarchy?',
    'If false summit is confirmed: the mountain perspective is revealed as rationalization, and the constraint''s core function is exposure of power asymmetry, not immutable law. The clinical classification at the analytical context should reflect contestation, not universality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_verification, empirical, 'Verification that mountain perspective is false summit vs. genuine natural law').

omega_variable(
    mandatrophy_resolution_path,
    'What political or structural change would resolve the mandatrophy? Does the constraint require explicit renegotiation of international law, or could domestic U.S. political change alone shift it?',
    'Scenario analysis: What would trigger U.S. ratification of the ICC? Congressional action? Change of administration? International pressure campaign? Cost-benefit analysis of each pathway from different actor perspectives.',
    'If domestic change alone could shift it: the constraint is contingent on U.S. political will, supporting false-summit diagnosis. If international renegotiation is required: the constraint is more deeply embedded in the architecture. Mandatrophy resolution depends on identifying which level of change is feasible and acceptable to dominant beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_resolution_path, preference, 'Political pathway to resolving extractive mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_international_accountability_exemption, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usiae_tr_t0, us_international_accountability_exemption, theater_ratio, 0, 0.55).
narrative_ontology:measurement(usiae_tr_t10, us_international_accountability_exemption, theater_ratio, 10, 0.65).
narrative_ontology:measurement(usiae_tr_t20, us_international_accountability_exemption, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(usiae_be_t0, us_international_accountability_exemption, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(usiae_be_t10, us_international_accountability_exemption, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(usiae_be_t20, us_international_accountability_exemption, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(usiae_su_t0, us_international_accountability_exemption, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(usiae_su_t10, us_international_accountability_exemption, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(usiae_su_t20, us_international_accountability_exemption, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_international_accountability_exemption, enforcement_mechanism).
narrative_ontology:affects_constraint(us_international_accountability_exemption, international_criminal_court_legitimacy).
narrative_ontology:affects_constraint(us_international_accountability_exemption, non_aligned_nation_defense_autonomy).
narrative_ontology:affects_constraint(us_international_accountability_exemption, third_party_prosecution_mechanisms).
narrative_ontology:affects_constraint(us_international_accountability_exemption, us_domestic_military_accountability).

% DUAL FORMULATION NOTE:
% This constraint is a reading of how international law operates as both a coordination mechanism (establishing norms and institutions) and an extraction mechanism (concentrating enforcement power in hegemonic actors while exempting them). Sibling constraint stories could decompose this into: (1) international_law_coordination_function (ε ≈ 0.15, Rope) focusing on how international legal institutions genuinely enable cooperation, and (2) hegemonic_accountability_exemption (ε ≈ 0.68, Snare) focusing narrowly on the exemption mechanism. This story bridges both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_international_accountability_exemption, institutional, 0.05).
constraint_indexing:directionality_override(us_international_accountability_exemption, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
