% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Promise of Eretz Yisrael as Inalienable Territorial Claim
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story models the religious-zionist reading of the
 *   contested kernel 'jewish_sovereignty_palestine' — the claim that divine
 *   promise to the Jewish people establishes an inalienable, non-negotiable
 *   title to Eretz Yisrael, and that Jewish statehood constitutes theological
 *   fulfillment of that covenant. The reading operates as a snare: it
 *   coordinates Jewish collective identity and territorial presence (a
 *   genuine coordination function for the covenant community) while
 *   extracting land, rights, and security from the Palestinian population
 *   through active enforcement (military occupation, settlement expansion,
 *   legal regimes). The claimed_type is snare because the coordination story
 *   (divine covenant) functions as cover for a territorial maximalism that
 *   categorically excludes Palestinian self-determination and requires
 *   continuous coercion to maintain. The constraint has hardened since 1967:
 *   extractiveness rose from 0.55 to 0.92 as the settlement enterprise
 *   expanded and the Oslo process collapsed; suppression requirement rose
 *   from 0.6 to 0.88 as the enforcement architecture matured. Theater ratio
 *   remains low (0.15) — the constraint's theological core is genuinely
 *   believed by its agenda-setters, not merely performed.
 *
 * KEY AGENTS:
 *   - jewish_covenant_community: Primary beneficiary (organized/identity_locked/universal) — receives theological validation and territorial rights
 *   - religious_zionist_institutions: Agenda setter (institutional/constrained/national) — administers the theological-legal framework
 *   - settlement_enterprise: Beneficiary/agenda setter (organized/constrained/regional) — materializes the claim on the ground
 *   - palestinian_population: Primary payer (powerless/trapped/local) — bears extraction via occupation and dispossession
 *   - palestinian_refugees: Excluded (powerless/trapped/universal) — categorically foreclosed from return
 *   - israeli_liberal_dissenters: Payer/excluded (moderate/constrained/national) — bears civic costs of a constraint they reject
 *   - international_legal_order: Observer (institutional/analytical/universal) — documents violations without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.92).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.88).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Promise of Eretz Yisrael as Inalienable Territorial Claim").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '41986ff2-93e6-45fc-a8aa-29fb5337887e').
narrative_ontology:cs_kernel_codification('41986ff2-93e6-45fc-a8aa-29fb5337887e', fixed_text).
narrative_ontology:cs_authority_grounding('41986ff2-93e6-45fc-a8aa-29fb5337887e', lineage).
narrative_ontology:cs_interpretation_layer_present('41986ff2-93e6-45fc-a8aa-29fb5337887e').
narrative_ontology:cs_reading_relation('41986ff2-93e6-45fc-a8aa-29fb5337887e', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('41986ff2-93e6-45fc-a8aa-29fb5337887e', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('41986ff2-93e6-45fc-a8aa-29fb5337887e', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('41986ff2-93e6-45fc-a8aa-29fb5337887e', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_axiom('41986ff2-93e6-45fc-a8aa-29fb5337887e', foundational, divine_title_to_eretz_yisrael_non_negotiable).
narrative_ontology:cs_axiom_status(divine_title_to_eretz_yisrael_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('41986ff2-93e6-45fc-a8aa-29fb5337887e', divine_title_to_eretz_yisrael_non_negotiable, theological).
narrative_ontology:cs_axiom('41986ff2-93e6-45fc-a8aa-29fb5337887e', foundational, statehood_as_theological_fulfillment).
narrative_ontology:cs_axiom_status(statehood_as_theological_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('41986ff2-93e6-45fc-a8aa-29fb5337887e', statehood_as_theological_fulfillment, theological).
narrative_ontology:cs_axiom('41986ff2-93e6-45fc-a8aa-29fb5337887e', secondary, palestinian_claims_categorically_foreclosed).
narrative_ontology:cs_axiom_status(palestinian_claims_categorically_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('41986ff2-93e6-45fc-a8aa-29fb5337887e', palestinian_claims_categorically_foreclosed, theological).
narrative_ontology:cs_reference_frame('41986ff2-93e6-45fc-a8aa-29fb5337887e', biblical_covenantal_promise).
narrative_ontology:cs_drift_state('41986ff2-93e6-45fc-a8aa-29fb5337887e', contemporary_statehood_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('41986ff2-93e6-45fc-a8aa-29fb5337887e', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenant_community).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_institutions).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugees).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, israeli_liberal_dissenters).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, divine_title_to_eretz_yisrael).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, theological_fulfillment_via_statehood).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, covenant_inheritance_non_negotiable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The covenant community understands itself as the divinely designated inheritor of Eretz Yisrael. The constraint's operation validates their theological self-understanding and provides the legitimating framework for territorial maximalism. Exit from this frame would require abandoning core religious identity; the constraint is experienced as the ground of that identity, not an imposition upon it.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenant_community, beneficiary,
    organized, civilizational, identity_locked, universal).

% Yeshivas, rabbinical courts, settlement organizations, and political parties (e.g., Religious Zionist Party) that administer the theological-legal framework. They interpret divine promise into halakhic rulings on territory, authorize settlement activity, and mobilize political support. Their authority depends on the constraint's continued recognition; they are constrained by the need to maintain theological coherence while exercising state power.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Network of settlements, outposts, and supporting infrastructure (regional councils, security coordination, state funding channels) that materializes the theological claim on the ground. They receive state resources, legal protection, and ideological validation. Their existence creates facts that make territorial compromise structurally difficult; they are constrained by dependence on state patronage and international illegitimacy.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, settlement_enterprise, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, settlement_enterprise, agenda_setter).

% Subject to military occupation, land expropriation, movement restrictions, and demographic engineering that enact the theological claim. They bear the material costs of the constraint's enforcement — displacement, fragmentation, denial of self-determination — without recognition in the constraint's internal logic. Exit is structurally blocked by enclosure, permit regimes, and the absence of any recognized alternative sovereignty.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_population, payer,
    powerless, generational, trapped, local).

% Descendants of those displaced in 1948 and 1967, whose right of return is rendered theologically impossible by the constraint's logic. They are not merely absent from the beneficiary calculus; their very claim is categorically foreclosed as a challenge to divine title. They remain in permanent exile, stateless, with no structural pathway to re-entry.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugees, excluded,
    powerless, generational, trapped, universal).

% Israeli citizens who oppose the theological maximalism but remain subject to its political consequences — military service in occupied territories, taxation funding settlements, erosion of democratic norms. They pay the civic and moral costs of a constraint they reject, with limited exit (emigration) and constrained voice (marginalized in hegemonic discourse).
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_liberal_dissenters, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, israeli_liberal_dissenters, excluded).

% UN bodies, ICJ, ICC, and human rights treaty systems that document the constraint's violation of international law (occupation, settlements, apartheid findings). They possess analytical authority but lack enforcement power against the constraint's primary enforcers. Their judgments are systematically rejected by the agenda-setting institutions as category errors — legalistic readings of a theological claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_legal_order, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish collective identity, historical continuity, and territorial presence around a divine covenant narrative that renders the land non-negotiable and the return theologically necessary. Solves the existential coordination problem of maintaining a peoplehood across exile by anchoring it in an immutable divine promise.
% TRANSFER_FUNCTION: Transfers land, water, mobility, political rights, and physical security from the Palestinian population to the Jewish covenant community and its institutional embodiments (settlement enterprise, state apparatus). Transfers legitimacy and theological validation from the international legal order to the religious-zionist framework.
% ABSENT_VOICES: Palestinian refugees (permanently excluded from the territorial calculus), Palestinian citizens of Israel (whose equality is structurally subordinated to the Jewish character of the state), and Mizrahi Jewish communities whose indigenous Middle Eastern histories are erased by the European-origin theological framework. They are absent because the constraint's logic recognizes only the covenant community as a subject of divine promise.
% DISAPPEARANCE_RATIONALE: If the divine promise constraint vanished overnight, the legal and moral architecture justifying settlements, occupation, and refusal of Palestinian return would collapse. The settlement enterprise would lose its legitimating core; the Israeli state would face an existential legitimacy crisis; the international legal order's judgments would become enforceable rather than contestable. The material geography of control would remain temporarily, but the ideological engine driving maximalism would cease.
% FOUNDING_PROBLEM: The existential vulnerability of Jewish peoplehood in exile — the recurring cycle of persecution, expulsion, and assimilation that threatened collective survival — required a divine guarantee of return that no secular nationalism could provide.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist authorities (Rav Kook, Gush Emunim lineage) attest the founding problem remains live — exile continues spiritually even with statehood, and the divine promise is not yet fully realized. Historians of Zionism (Shapira, Engel) and Palestinian scholars (Said, Khalidi) corroborate that the theological framework was constructed in response to 19th-20th century European conditions, not as an immutable ancient dictate. The 'exile' framing is contested by Jewish communities who never experienced exile as theological catastrophe.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.92) is very high because the constraint transfers nearly all territorial control, resource access, and political rights from Palestinians to the Jewish covenant community, with no reciprocal benefit to the extracted population. Suppression (0.88) is high because the constraint's persistence depends on military enforcement, legal exclusion, and the systematic foreclosure of alternatives (partition, binationalism, return). Theater ratio (0.15) is low because the theological conviction of the agenda-setters is genuine — the coordination function is not a cynical cover but a sincerely held framework that happens to operate extractively. Accessibility collapse (0.85) is high because the divine-title framework renders alternatives (partition, shared sovereignty) theologically illegitimate, not merely politically difficult. Resistance (0.75) is substantial — Palestinian sumud, international legal challenges, Israeli dissent — but has not altered the constraint's trajectory.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (covenant community, religious institutions), the constraint is experienced as rope — a genuine coordination mechanism solving the existential problem of Jewish continuity. From the payer/excluded seats (Palestinians, refugees), it is experienced as snare — pure extraction enforced by military power. From the analytical seat (international legal order), it reads as snare with a coordination cover. The engine computes this seat divergence from the structural data; the claim/metric independence rule means we author the claimed_type as snare (the structural reality) while the agenda-setters would claim rope (their self-understanding). This divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish covenant community sits at d ≈ 0.05 (full beneficiary — the constraint subsidizes their theological self-understanding and territorial claims). Religious Zionist institutions sit at d ≈ 0.15 (agenda-setters who benefit from administering the constraint but are constrained by its theological logic). Settlement enterprise sits at d ≈ 0.2 (beneficiaries of state resources but constrained by dependence). Palestinian population sits at d ≈ 0.95 (full targets — trapped, bearing maximal extraction). Palestinian refugees sit at d ≈ 1.0 (excluded, foreclosed entirely). Israeli liberal dissenters sit at d ≈ 0.7 (payers with constrained exit). International legal order sits at d = 0.5 (analytical — symmetric observer). The engine will derive these from beneficiary/victim declarations + power + exit; the override array is not needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential vulnerability in exile) is contested: the reading's internal logic treats it as permanently live until messianic completion; external scholarship treats statehood as having substantially resolved the existential condition. The mandate has not been resolved within the reading's own framework — hence mandatrophy_resolved is false. But the six_questions.founding_problem_status = contested captures the structural ambiguity: the arrangement persists with maximal extraction despite the original existential condition having changed. This contested status is itself diagnostic — a constraint whose founding problem is contested but whose extraction intensifies is a strong snare signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the contested kernel ''jewish_sovereignty_palestine'', and does its ε referent remain the standing arrangement under contest (the theological maximalist framework) rather than the reading''s endorsed alternative?',
    'Cross-reading comparison of ε referents: if all readings of the kernel assess the same standing arrangement (the actually-existing regime of control) rather than their own preferred arrangements, the ε-invariance principle holds. If this reading''s ε describes a hypothetical post-redemption state, it violates the fixed referent rule.',
    'Violation would mean this constraint story does not model the kernel reading it claims to model — it would be a different constraint (the reading''s telos) mislabeled as a reading of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this story correctly instantiates the kernel reading frame per the ε-invariance principle.').

omega_variable(
    foreclosure_of_palestinian_subjectivity,
    'Does the religious-zionist reading structurally foreclose the settler_colonial_reading and post_zionist_reading, or does it merely coexist with them as a competing framework?',
    'Test whether a single party could simultaneously hold the religious-zionist reading''s foundational axiom (divine title is non-negotiable) and the settler_colonial_reading''s foundational axiom (the project instantiates European colonial displacement). If the axioms are logically incompatible within one framework, foreclosure holds.',
    'If foreclosure holds, the kernel contains a genuine logical split — no single commitment framework can contain both readings. If coexistence, the readings are held by different factions and the kernel''s dispute is political, not logical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_of_palestinian_subjectivity, conceptual, 'Structural relationship between this reading and its siblings — foreclosure vs coexistence.').

omega_variable(
    theological_coercion_mechanism,
    'Is the constraint''s suppression primarily structural (state power, military enforcement) or does it operate significantly through internalized theological coercion (identity fusion, fear of divine consequence, communal shunning)?',
    'Compare suppression trajectories of agents who exit the religious-zionist framework vs those who remain. If suppression persists after structural exit (psychological, communal, identity-based), internalized coercion is operative. Survey data on former settlers, dati leumi dropouts, and Haredi anti-Zionists who reject the framework.',
    'If internalized coercion is significant, the effective suppression is higher than structural measures suggest — the constraint continues to extract compliance after formal exit. This would increase the constraint''s extractiveness from the analytical seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_coercion_mechanism, empirical, 'Structural vs internalized suppression mechanism in a theologically grounded constraint.').

omega_variable(
    mandatrophy_of_divine_promise,
    'Has the founding problem (existential vulnerability in exile) been resolved by statehood, rendering the theological maximalism a mandatrophic remnant, or does the reading''s own logic define the founding problem as permanently live until full redemption?',
    'Internal textual analysis: does the reading''s authoritative corpus (Rav Kook, R. Zvi Yehuda, Gush Emunim, contemporary Religious Zionist poskim) treat statehood as partial fulfillment requiring completion, or as sufficient realization? External corroboration: do non-beneficiary scholars identify a shift from existential necessity to expansionist ideology?',
    'If mandatrophic (founding problem dead but constraint persists), the constraint reclassifies toward piton or snare with dead mandate. If the reading internally defines the problem as permanently live, mandatrophy does not apply — the constraint remains theologically necessary by its own lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_of_divine_promise, conceptual, 'Whether the constraint''s persistence reflects genuine ongoing theological necessity or mandatrophic drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsp_rzr_tr_t1967, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(jsp_rzr_tr_t1977, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(jsp_rzr_tr_t1987, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1987, 0.12).
narrative_ontology:measurement(jsp_rzr_tr_t1993, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1993, 0.14).
narrative_ontology:measurement(jsp_rzr_tr_t2000, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(jsp_rzr_tr_t2010, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(jsp_rzr_tr_t2024, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(jsp_rzr_be_t1967, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(jsp_rzr_be_t1977, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1977, 0.68).
narrative_ontology:measurement(jsp_rzr_be_t1987, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1987, 0.72).
narrative_ontology:measurement(jsp_rzr_be_t1993, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1993, 0.78).
narrative_ontology:measurement(jsp_rzr_be_t2000, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(jsp_rzr_be_t2010, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2010, 0.89).
narrative_ontology:measurement(jsp_rzr_be_t2024, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jsp_rzr_su_t1967, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1967, 0.6).
narrative_ontology:measurement(jsp_rzr_su_t1977, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1977, 0.72).
narrative_ontology:measurement(jsp_rzr_su_t1987, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1987, 0.78).
narrative_ontology:measurement(jsp_rzr_su_t1993, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1993, 0.82).
narrative_ontology:measurement(jsp_rzr_su_t2000, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(jsp_rzr_su_t2010, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(jsp_rzr_su_t2024, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__religious_zionist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint and its four siblings form the jewish_sovereignty_palestine constraint family. All five stories share the kernel_id but instantiate distinct constraints with different ε values, beneficiary/victim structures, and claimed types. This reading (religious_zionist) has the highest ε (0.92) and claims snare; liberal_nationalist_reading likely claims rope/tangled_rope with lower ε; settler_colonial_reading claims snare with different victim/beneficiary assignment; cultural_zionist_reading claims rope/scaffold with minimal extraction; post_zionist_reading claims piton/tangled_rope with high theater. The network edges represent structural influence: this reading's maximalism constrains the operational space of the others (e.g., liberal_nationalist partition proposals are delegitimized by the divine-title claim).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
