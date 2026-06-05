% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Marriage Authority Fragmentation as Federalist Millet Mechanism
 *   domain: constitutional_law/legal_pluralism/consociational_democracy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of a contested constitutional
 *   kernel: the authority to govern marriage and family law. The
 *   federalist_millet_reading frames deliberately fragmented marriage
 *   authority (state controls secular aspects like property and inheritance;
 *   religious communities control personal law like marriage formation,
 *   divorce, spousal rights) as a consociational mechanism to prevent
 *   majoritarian domination of minorities. This reading competes with four
 *   sibling readings: communal_autonomy_reading (which emphasizes religious
 *   tradition as self-standing source of legitimacy rather than anti-tyranny
 *   logic), secularist_reading (which treats pluralism as transitional
 *   anomaly), gender_rights_reading (which prioritizes constitutional
 *   equality over federalist autonomy), and judicial_harmonization_reading
 *   (which sees courts gradually eroding pluralism through constitutional
 *   interpretation). The federalist_millet_reading occupies a distinct
 *   structural position: it accepts legal pluralism (like communal_autonomy)
 *   but grounds it in constitutional design logic (elite bargain to prevent
 *   tyranny) rather than religious authority. It expects legislative
 *   paralysis as a feature rather than a bug. It differs from
 *   gender_rights_reading by prioritizing collective minority protection over
 *   individual equality guarantees — a foundational normative tension that
 *   cannot be resolved within a single framework. The constraint's ε is low
 *   (0.28) because the core coordination function is genuine: preventing
 *   majoritarian homogenization of diverse populations. Extraction rises
 *   slightly over the interval (0.18→0.28) as gender minorities mobilize and
 *   experience the constraint as blocking their equality claims, and as
 *   theaters of constitutional debate proliferate (courts performing
 *   federalist deference while actually imposing minimal equality standards).
 *   Suppression is moderate (0.42) and stable: the constraint operates
 *   through both constitutional doctrine (courts defer to community law) and
 *   social enforcement (exit from community law triggers social
 *   consequences), but it does not reach snare-level suppression because exit
 *   pathways exist (albeit at identity cost, not just material cost).
 *
 * KEY AGENTS:
 *   - Religious Minority Communities: Beneficiary (organized/constrained at generational horizon) — experience the constraint as protecting cultural practice autonomy; core constituency of the federalist bargain
 *   - Consociational Elite Bargain: Beneficiary/architect (institutional/arbitrage at generational horizon) — constitutional founders and legislative power-sharers who designed fragmented authority; experience it as coordination mechanism preserving stability
 *   - Individual Subjects: Mixed position (moderate/constrained at biographical horizon) — face both coordination benefits (their family norms are recognized) and extraction (dual allegiance trap, cannot exit one regime without violating another)
 *   - Intra-Community Gender Minorities: Victim (powerless/identity_locked at biographical horizon) — women and LGBTQ+ persons whose community law denies rights that secular law would provide; identity_locked because exiting community to access secular protection means family dissolution
 *   - Judicial Reform Coalition: Organized reformer (organized/constrained at generational horizon) — civil rights organizations and courts gradually eroding federalist insulation via constitutional equality interpretation; see sunset path through judicial harmonization
 *   - Legislative System: Institutional actor (institutional/arbitrage at civilizational horizon) — unable to reform (any centralization triggers veto; any decentralized reform is contested); maintains constraint through inertia (piton perspective)
 *   - Analytical Observer: Position of analysis (analytical/analytical at civilizational scope) — risks naturalizing consociational pluralism as permanent necessity rather than contingent design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.28).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.42).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Marriage Authority Fragmentation as Federalist Millet Mechanism").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "constitutional_law/legal_pluralism/consociational_democracy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '77d899b7-11bd-4f48-a6d2-5053e2c33aaa').
narrative_ontology:cs_kernel_codification('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', fixed_text).
narrative_ontology:cs_authority_grounding('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', lineage).
narrative_ontology:cs_interpretation_layer_present('77d899b7-11bd-4f48-a6d2-5053e2c33aaa').
narrative_ontology:cs_reading_relation('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', marriage_authority__gender_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', foundational, deliberate_federalist_anti_tyranny_design).
narrative_ontology:cs_axiom_status(deliberate_federalist_anti_tyranny_design, holdable).
narrative_ontology:cs_axiom_grounding('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', deliberate_federalist_anti_tyranny_design, conventional).
narrative_ontology:cs_axiom('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', foundational, minority_collective_autonomy_legitimacy).
narrative_ontology:cs_axiom_status(minority_collective_autonomy_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', minority_collective_autonomy_legitimacy, deontological).
narrative_ontology:cs_reference_frame('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', federalist_consociational_plurality).
narrative_ontology:cs_drift_state('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', contemporary_gender_mobilization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('77d899b7-11bd-4f48-a6d2-5053e2c33aaa', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, religious_minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, sub_state_collective_identities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, consociational_elite_bargain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY RELIGIOUS COMMUNITY (ROPE) — Experiences marriage authority fragmentation as protection against majoritarian absorption. The deliberate constitutional division of authority (state property/inheritance law, community personal law authority) solves a coordination problem: how can minority family norms survive in a democratic majority without being overwritten? Extraction is minimal — the constraint provides genuine coordination benefit (preservation of cultural practice autonomy) with low coercive overhead. Exit options are constrained (joining majority legal regime would dissolve community identity), but this constraint does not prevent exit — it merely makes the cost identity-level rather than material.
constraint_indexing:constraint_classification(marriage_authority__federalist_millet_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSOCIATIONAL ELITE BARGAIN (ROPE) — Architects of federal/plural legal systems (legislative drafters, constitutional founders) designed fragmented marriage authority as a stability mechanism: by refusing to centralize authority, they prevented majoritarian domination while maintaining elite consensus through power-sharing across group boundaries. This perspective experiences the constraint as pure coordination with zero-sum benefit distribution: each elite faction preserves its own community's legal sphere in exchange for accepting others' autonomy. Low extraction; high coordination function.
constraint_indexing:constraint_classification(marriage_authority__federalist_millet_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: INDIVIDUAL SUBJECT / DUAL ALLEGIANCE TRAP (TANGLED ROPE) — A person living under plural marriage authority faces coordination benefits (their family norms are legally recognized and protected by their community) alongside extraction: they cannot easily exit one regime without violating another (e.g., a woman seeking divorce protection via secular law may face community excommunication; a man seeking enforcement of customary marriage may face secular constitutional challenge). Moderate extraction with genuine coordination; constrained exit because identity is bound up in both legal spheres.
constraint_indexing:constraint_classification(marriage_authority__federalist_millet_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTRA-COMMUNITY GENDER MINORITY / STRUCTURAL INVISIBILITY (SNARE) — Women and LGBTQ+ persons whose community law deprives them of rights that secular law would provide face high extraction with minimal exit. Identity_locked: exiting the community to access secular protections means family dissolution, social death within the only identity-granting structure they have. Suppression is enforced through both community social coercion and constitutional pluralism doctrine (courts refuse to harmonize personal laws to universal equality standards). The constraint benefits elite men (preserving patriarchal authority) and protects community collective identity (beneficiary) while extracting from gender minorities without alternative.
constraint_indexing:constraint_classification(marriage_authority__federalist_millet_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL REFORM COALITION / CONSTITUTIONAL REMEDIATION (SCAFFOLD) — Civil rights organizations, constitutional scholars, and reform-minded judges see fragmented marriage authority as a temporary institutional arrangement with a sunset: judicial imposition of constitutional equality floors across personal laws (without formal UCC legislation) is gradually harmonizing family law and creating exit paths for gender minorities. This perspective experiences the constraint as a problem with a definite solution pathway: incremental constitutional interpretation erodes the federalist insulation and replaces it with minimal-harm universal standards. Theater is moderate (courts perform constitutional interpretation without appearing to legislate), and the exit path is visible (escalating constitutional challenge doctrine).
constraint_indexing:constraint_classification(marriage_authority__federalist_millet_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGISLATIVE SYSTEM / STRUCTURAL PARALYSIS (PITON) — The deliberate fragmentation of marriage authority has created legislative paralysis: any attempt to pass a Uniform Civil Code triggers majoritarian-vs-minority backlash and coalition collapse. The legislature cannot centralize authority (reformers blocked by minority veto; minorities blocked by majoritarian override risk). The legislature also cannot reform family codes (each community code reform is contested, and centralizing reform violates federalist bargain). This perspective sees the constraint as degraded institutional infrastructure — maintained through inertia and occasional cosmetic reinterpretation, not because it solves the original coordination problem anymore. Theater ratio (0.55) reflects that the protective constitutional doctrine is now frequently invoked performatively: courts cite federalism and cultural autonomy while actually applying secular constitutional minimums anyway.
constraint_indexing:constraint_classification(marriage_authority__federalist_millet_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSOCIATIONAL STABILITY VIEW (ROPE) — From a civilizational perspective, fragmented marriage authority in diverse societies serves a genuine coordination function: it solves the tyranny-of-the-majority problem by refusing to impose a single family regime on internally diverse populations. This perspective sees low extraction and high coordination value. However, this view risks naturalizing the constraint as a permanent feature (it is not; consociational systems degrade over time as minority coalitions weaken or mobilize differently). The analytical observer should not mistake structural feature (consociational compromise) for necessity (inevitable way to organize plural societies).
constraint_indexing:constraint_classification(marriage_authority__federalist_millet_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_authority__federalist_millet_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_authority__federalist_millet_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28, rising to 0.28): LOW and STABLE at rope threshold. At the time of constitutional design (t=0, ε=0.18), the constraint genuinely solved a coordination problem: how to protect minority family practices without imposing majoritarian uniformity. Both minorities and majority elites benefited — minorities gained cultural autonomy, majority avoided costly homogenization warfare. As gender minorities mobilize (t=25-50), the constraint's extraction becomes visible: women and LGBTQ+ persons cannot access secular equality protections without community exit (identity-level cost). The extractiveness rises from 0.18 to 0.28 because the constraint now prevents rather than enables coordination — it blocks internal minority reform movements. However, extractiveness does not rise further because exit pathways exist (judicial challenge, secular law alternatives), so it does not reach snare threshold. Suppression (0.42, stable): MODERATE. The constraint operates through both constitutional doctrine (courts defer to community law, refusing to harmonize family codes) and social enforcement (community excommunication for those who exit). But it does not reach snare-level suppression (≥0.60) because: (a) secular law alternatives exist for those willing to exit community; (b) judicial doctrine is eroding (courts increasingly impose constitutional equality floors); (c) gender minorities, while suppressed, are not wholly trapped (organized reform movements are mobilizing). Theater ratio (0.55, rising from 0.40): MODERATE and RISING. At the time of constitutional design, the constraint had low theater — it genuinely solved the tyranny-of-majority problem. Contemporary theater has risen because courts now frequently cite federalist deference and cultural autonomy (the original justification) while actually imposing secular constitutional standards (contradicting the original purpose). The constitutional doctrine performatively invokes federalism while eroding it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The minority religious community experiences rope (genuine coordination of cultural autonomy protection). The consociational elite bargain experiences rope (power-sharing stability mechanism). But the individual subject experiences tangled_rope (cultural protection plus extraction via dual allegiance). Intra-community gender minorities experience snare (high extraction with identity-locked exit). The judicial reform coalition experiences scaffold (temporary institution with visible sunset via constitutional harmonization). The legislative system experiences piton (maintained through inertia, not function). The analytical observer experiences rope but risks naturalizing it as permanent (should recognize it as contingent design choice). The perspectival gap reveals that 'federalist pluralism as anti-tyranny' is an elite beneficiary reading — it captures what the institutional architects experienced and what minority community leaders experience, but it does NOT capture what gender minorities experience (snare) or how the system actually operates now (piton, not rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each agent are derived from their structural relationship to the extraction flow. Religious minorities as beneficiaries of authority autonomy: d ≈ 0.25 (low, beneficiary position, though constrained by identity cost of exit → derived d modulates upward from canonical beneficiary baseline). Consociational elites as beneficiaries of power-sharing stability: d ≈ 0.10 (very low, pure beneficiary with arbitrage options — can exit by changing constitutional design). Individual subjects as both beneficiaries (cultural protection) and victims (dual legal obligation): d ≈ 0.50 (symmetric position). Gender minorities as victims with identity-locked exit: d ≈ 0.89 (very high, victim position with constrained-to-trapped exit). Judicial reformers as constrained agents working to narrow the constraint: d ≈ 0.55 (organized victim position with visible exit path). Legislature as trapped institutional actor unable to move in either direction: d ≈ 0.85 (high, institutional position but constrained by coalition geometry). Analytical observer as external analyst: d ≈ 0.72 (canonical analytical position). The engine derives these automatically from beneficiary/victim declarations and exit options; they are provided here for transparency.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that 'federalist pluralism' has fundamentally different extraction profiles depending on the observer. For elite minority leaders and constitutional architects, it is rope (pure coordination). For gender minorities, it is snare (pure extraction with identity-locked exit). For individual subjects, it is tangled_rope (mixed). The mandatrophy resolves by recognizing that the constraint is NOT a single unified mechanism but a presheaf of mechanisms layered on the same formal structure. The constitutional doctrine of 'federalist deference to personal laws' coordinates one set of actors (preserving minority community autonomy) while extracting from another (preventing gender minorities from escaping patriarchal family law). The classical mandatrophy question — 'Is this coordination or extraction?' — has the answer: 'Both, for different actors.' The constraint's legitimacy claim rests on whether one accepts the foundational axiom that minority collective autonomy justifies individual gender subordination. This axiom is not logically necessary (gender-protective pluralism is theoretically possible) but empirically common in jurisdictions that have adopted consociational pluralism. The constraint's terminal attractor is unstable: as gender minorities mobilize, the extraction becomes visible, triggering judicial and political pressure to harmonize. The federalist reading's survival depends on either (a) accepting gender subordination as permanent cost, or (b) redesigning pluralism to protect both collective identity and individual equality. The constraint as currently instantiated cannot do both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consociational_stability_vs_gender_equality_tradeoff,
    'Is the extraction of gender minorities an inevitable cost of consociational stability, or a contingent design choice within federalist pluralism?',
    'Comparative analysis: Do non-patriarchal personal law codes exist within federalist frameworks? Do hybrid systems (federalist+constitutional equality floor) maintain stability? Historical counterfactual: What gender protection levels were achievable at the moment of constitutional design?',
    'If inevitable tradeoff: the constraint''s legitimacy rests on accepting structural gender subordination as the price of minority autonomy protection (deep ethical tension for the reading). If contingent: the constraint should be redesigned to protect both collective identity AND gender equality (reframes from rope/snare mix to pure rope). Affects whether gender_rights_reading genuinely forecloses federalist_millet_reading or merely influences its form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consociational_stability_vs_gender_equality_tradeoff, conceptual, 'Whether gender extraction is inherent to consociational pluralism or a contingent design choice').

omega_variable(
    elite_bargain_fragility,
    'Does the consociational elite bargain that produces fragmented marriage authority remain stable as minority demographics and political coalitions shift?',
    'Historical analysis of coalition durability in other federalist plural systems (Belgium, Lebanon, Bosnia); demographic modeling of minority political power; coalition formation dynamics in contemporary legislatures',
    'If bargain is durable: the constraint remains a genuine coordination mechanism across generations (rope classification holds). If bargain is fragile: the constraint degrades into a Piton as the original bargain''s enforcing coalition weakens, and legislative paralysis becomes a side effect rather than a feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_bargain_fragility, empirical, 'Stability of the elite bargain underlying consociational pluralism').

omega_variable(
    federalist_reading_vs_judicial_harmonization_reading_boundary,
    'At what point does incremental judicial harmonization (judicial_harmonization_reading) foreclose the federalist pluralism reading by eroding the authority fragmentation that the federalist reading depends on?',
    'Empirical tracking of Supreme Court doctrine: Does judicial imposition of constitutional equality floors preserve federalism while narrowing its scope (coexists), or does it systematically dismantle federalist authority division (forecloses)? Case law analysis showing whether courts treat federalist and equality constraints as compatible or contradictory.',
    'If compatible: the readings coexist and the reading_relations should be ''coexists_with''. If contradictory: judicial_harmonization_reading forecloses federalist_millet_reading (the Supreme Court is abolishing the authority fragmentation the federalist reading depends on). This affects terminal attractor analysis — does the system converge on secular unified law or stabilize at federalist+constitutional_floor hybrid?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federalist_reading_vs_judicial_harmonization_reading_boundary, empirical, 'Whether judicial harmonization erodes or preserves federalist authority fragmentation').

omega_variable(
    reading_provenance_and_elite_consensus,
    'Is this reading (federalist pluralism as deliberate anti-tyranny mechanism) the actual historical justification for legal pluralism in this jurisdiction, or a post-hoc rationalization constructed by contemporary constitutionalists?',
    'Textual analysis: Do founding documents, constitutional debates, or legislative records explicitly invoke anti-tyranny consociational reasoning? Or do they invoke religious freedom, cultural autonomy, or administrative convenience? Genealogical analysis: Which constituencies mobilized around federalist pluralism reasoning vs. other justifications?',
    'If reading reflects actual design intent: it has higher epistemic weight and the constraint''s legitimacy claim is grounded in genuine constitutional commitment. If post-hoc rationalization: the reading is an interpretation overlaid on unstated design choices, and its authority rests on whether contemporary power holders accept the retroactive framing. Affects whether the axiom ''deliberate_federalist_anti_tyranny_design'' is foundational to the reading or secondary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_provenance_and_elite_consensus, conceptual, 'Historical provenance of federalist reading vs. post-hoc rationalization').

omega_variable(
    minority_communities_as_beneficiaries_ambiguity,
    'Does the federalist pluralism constraint actually benefit religious/cultural minority communities, or does it primarily benefit conservative elites within those communities who use ''community autonomy'' as a shield for patriarchal authority?',
    'Survey and interview data from minority community members: Do women, LGBTQ+ persons, and reform-minded individuals experience the constraint as protective of their identity or as coercive? Comparative analysis: In federalist systems, do minorities report higher autonomy satisfaction than in assimilationist systems? Do reform movements within minority communities mobilize FOR or AGAINST legal pluralism?',
    'If communities genuinely benefit: beneficiary declaration is accurate and the rope classification from minority perspective is correct. If benefit accrues only to conservative elites: the beneficiary is ''conservative_male_elites_within_minority_communities'', not ''minority_communities'' broadly, and the extraction from gender minorities is more severe than modeled (Snare for gender minorities, not Tangled Rope; lower overall ε).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_communities_as_beneficiaries_ambiguity, empirical, 'Whether legal pluralism benefits minority communities or primarily conservative elites within them').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mar_fed_theater_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(mar_fed_theater_t25, marriage_authority__federalist_millet_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(mar_fed_theater_t50, marriage_authority__federalist_millet_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(mar_fed_extractiveness_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mar_fed_extractiveness_t25, marriage_authority__federalist_millet_reading, base_extractiveness, 25, 0.24).
narrative_ontology:measurement(mar_fed_extractiveness_t50, marriage_authority__federalist_millet_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(mar_fed_suppression_t0, marriage_authority__federalist_millet_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(mar_fed_suppression_t25, marriage_authority__federalist_millet_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(mar_fed_suppression_t50, marriage_authority__federalist_millet_reading, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority kernel decomposes into five structurally distinct constraints (five readings, each with its own ε, beneficiary/victim structure, and perspectival profile). This story represents the federalist_millet_reading (ε≈0.28, rope classification for elite beneficiaries, snare for gender minorities). The sibling readings have different ε values and different beneficiary/victim structures: communal_autonomy_reading emphasizes religious tradition legitimacy (potentially lower suppression, different beneficiary class); secularist_reading models pluralism as degraded/transitional (piton perspective); gender_rights_reading prioritizes constitutional equality (higher suppression of patriarchal norms, victim class = intra-community gender minorities); judicial_harmonization_reading models gradual erosion of pluralism via constitutional interpretation (scaffold perspective). Each reading must be authored as a separate constraint story linked via network.affects_constraints. All five readings operate on the same formal institutional structure (fragmented authority) but frame it differently and extract different structural predictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, organized, 0.25).
constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, powerless, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
