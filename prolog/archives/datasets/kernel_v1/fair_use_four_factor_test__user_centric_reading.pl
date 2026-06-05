% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use Four-Factor Test (User-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   Fair use is a contested legal doctrine grounding copyright's legitimacy
 *   in the copyright statute (17 U.S.C. § 107) and constitutional copyright
 *   clause. The four-factor test — (1) purpose and character of use, (2)
 *   nature of the work, (3) amount used, (4) market effect — is the operative
 *   doctrine for determining whether unlicensed use of copyrighted material
 *   is lawful. This constraint story instantiates the USER-CENTRIC READING of
 *   that four-factor test: a reading that prioritizes public access, cultural
 *   participation, educational use, and transformative creation over creator
 *   compensation maximization. The reading affirms that fair use is an
 *   affirmative user right, not merely a defense to infringement. Under this
 *   reading, the four factors are weighted to presume that educational uses,
 *   critical commentary, transformative innovation, and public-interest
 *   creation are protected unless the creator can demonstrate substantial
 *   commercial harm. The beneficiaries are public users, educators, cultural
 *   producers, and remix creators. The victims are original rights holders
 *   and licensing intermediaries whose revenue streams are reduced by fair
 *   use protection. The constraint exhibits tangled coordination (fair use
 *   enables cultural participation that benefits both creators and public)
 *   alongside asymmetric extraction (rights holders bear reduced compensation
 *   and market control). The 20-year measurement interval reflects the
 *   gradual erosion of theater in fair use doctrine: early periods
 *   (1990s–2000s) saw more performative four-factor analysis with predictable
 *   outcomes in favor of creators; later periods (2010s–2020s) show more
 *   genuine user-protective jurisprudence, but at the cost of institutional
 *   controversy and litigation uncertainty.
 *
 * KEY AGENTS:
 *   - Public Users (Educational): Primary beneficiary (powerless/mobile) — educators and students access copyrighted materials for teaching and research without licensing friction
 *   - Original Rights Holders: Primary victim (institutional/constrained) — creators and copyright owners face reduced licensing revenue and derivative market control under user-centric reading
 *   - Cultural Producers / Remix Creators: Secondary beneficiary (moderate/constrained) — independent artists, musicians, visual artists, writers benefit from ability to build on existing work without permission or payment, but face litigation risk and uncertainty
 *   - Licensing Intermediaries: Secondary victim (institutional/constrained) — performing rights organizations, mechanical licensing agencies, and collect societies lose revenue as unlicensed fair uses expand
 *   - Copyright Industry Enforcement System: Institutional actor (institutional/arbitrage) — DMCA and takedown notice apparatus sees fair use as a ritualistic exception, maintains theatrical four-factor appearance while operational reality is binary
 *   - Open Culture Movement: Organized reformist (organized/mobile) — Creative Commons, public libraries, open-access advocates see user-centric fair use as temporary scaffold being replaced by open licensing infrastructure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating user-centric reading as a natural property of copyright law rather than a contested legal construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.35).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.48).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '0eebf40a-cbac-47d3-a182-c11160c3dde9').
narrative_ontology:cs_kernel_codification('0eebf40a-cbac-47d3-a182-c11160c3dde9', fixed_text).
narrative_ontology:cs_authority_grounding('0eebf40a-cbac-47d3-a182-c11160c3dde9', lineage).
narrative_ontology:cs_interpretation_layer_present('0eebf40a-cbac-47d3-a182-c11160c3dde9').
narrative_ontology:cs_reading_relation('0eebf40a-cbac-47d3-a182-c11160c3dde9', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('0eebf40a-cbac-47d3-a182-c11160c3dde9', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('0eebf40a-cbac-47d3-a182-c11160c3dde9', foundational, fair_use_is_affirmative_user_right).
narrative_ontology:cs_axiom_status(fair_use_is_affirmative_user_right, holdable).
narrative_ontology:cs_axiom_grounding('0eebf40a-cbac-47d3-a182-c11160c3dde9', fair_use_is_affirmative_user_right, deontological).
narrative_ontology:cs_axiom('0eebf40a-cbac-47d3-a182-c11160c3dde9', foundational, public_access_overrides_licensing_revenue).
narrative_ontology:cs_axiom_status(public_access_overrides_licensing_revenue, holdable).
narrative_ontology:cs_axiom_grounding('0eebf40a-cbac-47d3-a182-c11160c3dde9', public_access_overrides_licensing_revenue, instrumental).
narrative_ontology:cs_axiom('0eebf40a-cbac-47d3-a182-c11160c3dde9', secondary, transformative_use_presumptively_fair).
narrative_ontology:cs_axiom_status(transformative_use_presumptively_fair, holdable).
narrative_ontology:cs_axiom_grounding('0eebf40a-cbac-47d3-a182-c11160c3dde9', transformative_use_presumptively_fair, empirically_contingent).
narrative_ontology:cs_reference_frame('0eebf40a-cbac-47d3-a182-c11160c3dde9', statutory_fair_use_as_affirmative_user_right).
narrative_ontology:cs_drift_state('0eebf40a-cbac-47d3-a182-c11160c3dde9', contemporary_creator_centric_jurisprudence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0eebf40a-cbac-47d3-a182-c11160c3dde9', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, cultural_producers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, remix_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, original_rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, licensing_intermediaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EDUCATIONAL USER (ROPE) — Universities and educators see fair use as genuine coordination: it enables teaching, research, commentary, and criticism without requiring costly licensing negotiations. The four-factor test, applied user-centrically, permits educational use at minimal friction. Extraction is low because the coordination function (access to materials for teaching) is real and benefits the user directly. No suppression — legal right is exercisable.
constraint_indexing:constraint_classification(fair_use_four_factor_test__user_centric_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT CREATOR / REMIX ARTIST (TANGLED ROPE) — Cultural producers (musicians remixing samples, visual artists referencing copyrighted imagery, writers quoting extensively) experience fair use as both enabler and constraint. The coordination function is real: fair use permits cultural participation and derivative innovation without permission costs. But the suppression is significant: uncertainty about which uses qualify, chilling effect from litigation risk, and asymmetric litigation cost. The four-factor test under user-centric reading reduces suppression by presuming transformative educational use is protected, but litigation risk remains.
constraint_indexing:constraint_classification(fair_use_four_factor_test__user_centric_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RIGHTS HOLDER FACING DERIVATIVE MARKET LOSS (SNARE) — Original creators and copyright holders experience the user-centric four-factor test as pure extraction: fair use permits others to use their work without compensation, reducing licensing revenues and derivative work control. The user-centric reading minimizes these losses by design — it prioritizes public access over creator compensation. The rights holder is trapped: cannot exit the constraint without abandoning copyright claims in the market. This perspective reveals the distributional cost of the user-centric reading.
constraint_indexing:constraint_classification(fair_use_four_factor_test__user_centric_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: LICENSING INTERMEDIARY (TANGLED ROPE) — Rights-licensing agencies (performing rights organizations, mechanical licensing collectives) experience fair use as both coordination and extraction. Fair use coordination function: reduces transaction costs by eliminating need for licenses on certain educational and critical uses. Fair use extraction: reduces licensing revenue streams by defining categories of use that must be free. Suppression is moderate: licensing intermediaries can and do operate at scale, but the boundary between licensed and unlicensed use is contested, creating enforcement uncertainty.
constraint_indexing:constraint_classification(fair_use_four_factor_test__user_centric_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COPYRIGHT INDUSTRY ENFORCEMENT SYSTEM (PITON) — The institutional apparatus of copyright enforcement (Digital Millennium Copyright Act anti-circumvention provisions, takedown notice systems, litigation-based policing) sees fair use as a ritualistic exception: the four-factor test produces performative decision-making in practice. Most use disputes are resolved through pre-litigation negotiation or chilling effect rather than through genuine fair use adjudication. Theater ratio is high because the enforcement system maintains the appearance of nuanced four-factor balancing while the operational reality is binary: fight or capitulate. The system persists through institutional inertia even as the user-centric reading has eroded the suppression function it was designed to maintain.
constraint_indexing:constraint_classification(fair_use_four_factor_test__user_centric_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: OPEN CULTURE MOVEMENT (SCAFFOLD) — Organized actors (Creative Commons advocates, open-access organizations, public libraries, arts education networks) see the user-centric four-factor test as a temporary scaffolding for cultural participation. The sunset logic: as Creative Commons licensing, open-access publishing, and digitally-native culture mature, fair use as a doctrine becomes less necessary — the infrastructure shift replaces fair use negotiation with standardized open licensing. This perspective sees fair use as holding a space while the underlying institution (copyright's scope) transforms. Theater is moderate: the movement's success depends on building genuine alternatives, not merely performing them.
constraint_indexing:constraint_classification(fair_use_four_factor_test__user_centric_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the user-centric reading risks being framed as a natural property of copyright doctrine: that the four-factor test necessarily protects public access as a fundamental right, that cultural participation is inherent to healthy societies, and that balancing copyright against public benefit is a mathematical law of cultural economics. This perspective classifies the constraint as mountain — treating the user-centric reading's axioms as discovered truths rather than contested legal commitments. However, this is a false summit candidate: the reading's beneficiaries (public users, educators, cultural producers) are identifiable, and the reading's operation requires active enforcement of specific interpretive rules. The mountain classification masks that this is a contingent legal construction, not a natural law.
constraint_indexing:constraint_classification(fair_use_four_factor_test__user_centric_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fair_use_four_factor_test__user_centric_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fair_use_four_factor_test__user_centric_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, TR),
    TR >= 0.70.

:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The user-centric reading significantly reduces creator compensation and market control compared to a strict creator-centric baseline, but does not eliminate either. Licensing for commercial uses remains common; creators retain control over attribution and derivative licensing. The moderate value reflects that fair use protection is substantial but not total — most uses are still licensed or negotiated, and original creators retain exclusive rights for non-fair-use purposes. The measurement trajectory (0.28 → 0.35 over 20 years) reflects gradual institutionalization of user-protective jurisprudence. Suppression (0.48): Moderate. Significant barriers exist to exercising fair use: litigation risk is asymmetric (corporations can afford to defend; individuals cannot), uncertainty about which uses qualify deters many potential fair uses, and DMCA anti-circumvention provisions create chilling effects even for uses that would qualify as fair under the four-factor test. However, suppression is not total — major educational and news institutions regularly assert fair use; case law provides increasingly clear guidance on transformative use; and public awareness of fair use rights has increased. Theater ratio (0.62): Moderate-high. The four-factor test in practice involves significant performative analysis: courts often go through the motions of factor-by-factor balancing while the outcome is largely predetermined by case law on transformative use and commercial harm. The rise from 0.48 to 0.68 reflects increasing institutional formalization of fair use doctrine: as courts have established clearer rules (transformative use is strongly presumed fair), the actual balancing process becomes more theatrical — judges narrate a four-factor analysis that largely rubber-stamps the transformative-use heuristic. This is not a pure negative: clearer rules reduce uncertainty. But it means the doctrine's legitimacy rests increasingly on whether the judge agrees with the heuristic rule, not on genuine application of the statutory factors.
 *
 * PERSPECTIVAL GAP:
 *   The user-centric reading creates maximum perspectival divergence. The public user sees coordination (Rope) — fair use genuinely enables education without licensing friction. The independent creator sees mixed coordination and extraction (Tangled Rope) — fair use enables cultural participation but uncertainty creates chilling effect. The rights holder sees pure extraction (Snare) — licensing revenues are lost with no corresponding benefit. The licensing intermediary sees coordination with revenue loss (Tangled Rope) — transaction costs are reduced but licensing market shrinks. The enforcement apparatus sees a performative ritual (Piton) — the four-factor test is maintained theatrically but operational outcomes are predetermined. The open culture movement sees temporary scaffolding (Scaffold) — fair use is holding space while open licensing infrastructure replaces it. The civilizational observer risks naturalizing the reading (Mountain) — but the structural data reveals this as a false summit. The perspectival gap measures the reading's distributional conflict: the same doctrine that enables one group's cultural participation reduces another group's compensation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the constraint. Public users (beneficiaries with mobile exit options) experience low directionality — they benefit from fair use protection and are not heavily suppressed. Rights holders (victims with constrained exit) experience high directionality — they are targets of the extractive mechanism (licensing revenue loss) and cannot easily exit by switching to non-copyrightable alternatives. The analytical observer risks directionality error: treating the user-centric reading as a natural law (mountain) rather than a constructed legal reading. The engine's false summit detector will flag this if beneficiaries are declared (which they are), revealing that the 'fair use is a natural right' framing naturalizes what is actually a contingent reading of contested doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that fair use is neither pure coordination (rope) nor pure extraction (snare), but genuinely hybrid. The user-centric reading privileges coordination and public interest at the expense of creator compensation — it is ideologically committed to reading the doctrine in the user's favor when the four factors are genuinely balanced. The creator-centric reading privileges creator rights and licensing markets at the expense of public access — it reads the same four factors in the creator's favor. Both readings are defensible applications of the statutory language. The mandatrophy reflects a constitutional choice: whether copyright's primary purpose is to incentivize creation (creator-centric) or to promote the progress of science and useful arts (user-centric). This is not a factual question about which reading is correct. The tangled rope classification is appropriate because the constraint genuinely serves coordination (cultural participation, educational access, critical discourse) AND extraction (licensing revenue, derivative market control). The user-centric reading has simply chosen which half to emphasize.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_use_criterion_operationalization,
    'What operationalizes ''transformative use'' in the user-centric reading? Does adding commentary, criticism, or educational value suffice, or must the use substantially alter the work''s meaning or commercial function?',
    'Case law evolution and appellate decision patterns; analysis of what kinds of uses courts classify as transformative under the user-centric reading vs. creator-centric reading; empirical comparison of licensing denial rates pre/post-transformative doctrine clarification',
    'Broad operationalization: more uses qualify as fair use, higher extractiveness for rights holders (lower effective extraction for users). Narrow operationalization: fewer uses qualify, reduced user protection, increased licensing market.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_criterion_operationalization, empirical, 'Operationalization of ''transformative use'' standard').

omega_variable(
    commercial_use_harm_measurement,
    'How does the four-factor test weigh commercial harm to the original creator under the user-centric reading? Is any commercial use of protected material per se non-fair, or does derivative innovation with minimal market cannibalization remain fair?',
    'Analysis of settled litigation outcomes; comparative impact studies on creator revenue in jurisdictions with different fair use interpretations; measurement of licensing market size and pricing elasticity under different fair use regimes',
    'Strict commercial exclusion: licensing market expands, creator compensation increases, user access decreases. Permissive commercial inclusion: licensing market contracts, user access increases, creator compensation decreases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_use_harm_measurement, empirical, 'Treatment of commercial derivative use under fair use test').

omega_variable(
    public_interest_vs_private_right,
    'Does the user-centric reading treat fair use as a public-interest exception to copyright (copyright exists to serve the public good, and fair use is how that obligation is met), or as a private right granted to users (individuals have a right to use copyrighted material subject to four-factor balancing)?',
    'Legislative history analysis; interpretation of copyright''s preamble (''promote the progress of science and useful arts''); case law distinguishing public interest framing from individual rights framing; constitutional analysis of copyright''s relationship to First Amendment',
    'Public interest framing: broader fair use, stronger copyright limitation doctrine, higher extractiveness for rights holders. Private right framing: narrower fair use, weaker copyright limitations, lower extractiveness for rights holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_interest_vs_private_right, conceptual, 'Conceptual grounding of fair use: public interest vs. private right').

omega_variable(
    market_substitution_vs_market_expansion,
    'In the user-centric reading, does the four-factor test presume that unlicensed transformative uses expand the total market for the original work (through discoverability, cultural diffusion, derivative demand) or substitute for licensing revenues?',
    'Empirical studies of remix, fan fiction, and commentary markets; measurement of correlation between fair use prevalence and original work sales; qualitative analysis of user survey data on whether fair use use would have become licensed use absent fair use protection',
    'Market expansion presumption: higher fair use finding, lower measured harm, higher extractiveness for rights holders. Market substitution presumption: lower fair use finding, higher measured harm, lower extractiveness for rights holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_substitution_vs_market_expansion, empirical, 'Market impact assumption in fair use analysis').

omega_variable(
    nature_of_work_and_purpose_weighting,
    'Does the user-centric reading weight the first two factors (nature of work, purpose of use) as determinative or merely as initial framing for the analysis of market harm?',
    'Case law pattern analysis: frequencies of fair use findings when first two factors favor fair use but last two favor copyright holder; jurisprudential commentary on factor hierarchy',
    'Determinative weighting: educational and transformative purposes nearly guarantee fair use. Mere framing: market harm analysis can override purpose considerations, reducing user protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nature_of_work_and_purpose_weighting, empirical, 'Weighting of purpose and nature factors in fair use test').

omega_variable(
    contested_reading_arbitration,
    'When the user-centric reading and creator-centric reading apply the same four-factor test to the same use and reach opposite conclusions, what settles the disagreement?',
    'Appellate court decision; legislative clarification; scholarly consensus shift; community practice evolution',
    'If appellate courts systematically favor creator-centric framing: user-centric reading loses institutional authority, extractiveness decreases for users (increases for rights holders). If courts systematically favor user-centric framing: creator-centric reading loses authority, extractiveness increases for users.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_reading_arbitration, conceptual, 'Arbitration mechanism for competing fair use readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fusr_tr_t0, fair_use_four_factor_test__user_centric_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(fusr_tr_t10, fair_use_four_factor_test__user_centric_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement(fusr_tr_t20, fair_use_four_factor_test__user_centric_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(fusr_be_t0, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fusr_be_t10, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(fusr_be_t20, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, copyright_licensing_market_extraction).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, cultural_commons_access_constraint).

% DUAL FORMULATION NOTE:
% Fair use is one kernel with three structurally distinct readings, each instantiating a different constraint. The user-centric reading (this file) has extractiveness 0.35, prioritizing public access. The creator-centric reading (separate file) has extractiveness ~0.18, prioritizing licensing revenue. The transformative-use reading (separate file) has extractiveness ~0.28, balancing transformation doctrine. Each reading applies the same four-factor test but reaches systematically different conclusions due to divergent weighting of factors. These are not the same constraint viewed from different perspectives — they are different legal instantiations of an ambiguous kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
