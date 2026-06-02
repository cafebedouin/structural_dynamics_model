% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Doctrine Under Market-Licensing Interpretation
 *   domain: intellectual_property_law/copyright_doctrine/information_economics
 *
 * SUMMARY:
 *   The fair use doctrine, formalized in Section 107 of the Copyright Act,
 *   provides a statutory exception to copyright infringement for uses that
 *   are transformative, educational, critical, or otherwise socially valuable
 *   despite copying. The four-factor test (purpose, nature, amount, and
 *   effect on the market for the original) was designed to balance copyright
 *   holders' economic interests against the public's interest in access and
 *   speech. The market-licensing reading interprets the fourth factor (effect
 *   on the market) to mean that fair use ceases to exist wherever a licensing
 *   mechanism could theoretically exist. Under this reading, fair use is not
 *   an independent defense but rather a gap-filling exception for uses where
 *   no licensing market exists. Any use that could be monetized through
 *   licensing loses its fair use protection. This reading transforms the
 *   doctrine from a protective carve-out for certain categories of valuable
 *   copying into a residual category covering only economically unmonetizable
 *   uses. The kernel (fair use statutory exception) accommodates multiple
 *   readings: the market-licensing reading (this story), the
 *   transformative-right reading (fair use is an independent doctrine
 *   protecting transformative expression regardless of licensing
 *   possibility), and the narrow-defense reading (fair use applies only to
 *   narrow historical categories like news reporting and scholarly citation).
 *   This story instantiates the market-licensing reading exclusively,
 *   examining how it collapses fair use doctrine into licensing economics,
 *   creating a snare constraint where the statutory exception becomes null in
 *   practice.
 *
 * KEY AGENTS:
 *   - Copyright Holders with Licensing Capacity (institutional/arbitrage): Primary beneficiaries — control licensing markets, capture revenue from all monetizable uses, need no suppression infrastructure because legal doctrine itself forecloses fair use.
 *   - Potential Fair Users (powerless/trapped): Primary victims — students, teachers, researchers, critics, parodists, documentary filmmakers who would previously invoke fair use; now face licensing gatekeeping for any conceivable use.
 *   - Transformative Creators (moderate/constrained): Secondary victims — artists, musicians, filmmakers whose work is transformative (parody, remix, commentary) but still licensable; face licensing gatekeeping for transformative use that doctrine nominally protects.
 *   - Educational Institutions (institutional/constrained): Secondary victims with some benefit — gain legal clarity through licensing, but face extraction costs as fair use collapse forces licensing expenditures on previously-free educational uses.
 *   - Open-Access Movement (organized/mobile): Organized agents building exit routes — CC-licensed content, open-access publishing, deliberately-restricted copyright create alternative domains where fair use questions become moot.
 *   - Judicial Community (institutional/arbitrage): Institutional maintainers of piton — keep fair use doctrine formally operative while the market-licensing reading empties it of function.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing a contingent legal construction as an economic law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.78).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.72).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Doctrine Under Market-Licensing Interpretation").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property_law/copyright_doctrine/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '73f8507c-c3d3-462c-9322-4bbd71693b0f').
narrative_ontology:cs_kernel_codification('73f8507c-c3d3-462c-9322-4bbd71693b0f', formalized).
narrative_ontology:cs_authority_grounding('73f8507c-c3d3-462c-9322-4bbd71693b0f', extraction).
narrative_ontology:cs_interpretation_layer_present('73f8507c-c3d3-462c-9322-4bbd71693b0f').
narrative_ontology:cs_reading_relation('73f8507c-c3d3-462c-9322-4bbd71693b0f', fair_use_transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('73f8507c-c3d3-462c-9322-4bbd71693b0f', fair_use_narrow_defense_reading, influences).
narrative_ontology:cs_axiom('73f8507c-c3d3-462c-9322-4bbd71693b0f', foundational, fair_use_exists_only_where_no_licensing_market).
narrative_ontology:cs_axiom_status(fair_use_exists_only_where_no_licensing_market, holdable).
narrative_ontology:cs_axiom_grounding('73f8507c-c3d3-462c-9322-4bbd71693b0f', fair_use_exists_only_where_no_licensing_market, instrumental).
narrative_ontology:cs_axiom('73f8507c-c3d3-462c-9322-4bbd71693b0f', foundational, licensing_availability_overrides_transformativeness).
narrative_ontology:cs_axiom_status(licensing_availability_overrides_transformativeness, holdable).
narrative_ontology:cs_axiom_grounding('73f8507c-c3d3-462c-9322-4bbd71693b0f', licensing_availability_overrides_transformativeness, instrumental).
narrative_ontology:cs_reference_frame('73f8507c-c3d3-462c-9322-4bbd71693b0f', licensing_market_comprehensive_coverage).
narrative_ontology:cs_drift_state('73f8507c-c3d3-462c-9322-4bbd71693b0f', contemporary_licensing_infrastructure_maturity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('73f8507c-c3d3-462c-9322-4bbd71693b0f', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, copyright_holders_with_licensing_capacity).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, publishing_enterprises).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, potential_fair_users).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, public_domain_commons).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, transformative_creators_without_licensing_budget).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POTENTIAL FAIR USER (SNARE) — No exit. Any conceivable fair use (quotation, criticism, educational reproduction, research) that could theoretically be licensed is foreclosed. Exit from the constraint requires abandoning the use entirely, not merely paying a cost. The market-licensing reading eliminates fair use defense by definition wherever a licensing market exists.
constraint_indexing:constraint_classification(fair_use_statutory_exception__market_licensing_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRANSFORMATIVE CREATOR (SNARE) — Constrained by licensing costs and licensing gatekeeping. Under the market-licensing reading, transformative use itself is not a defense — only uses for which no licensing mechanism exists remain free. Since licensing markets can theoretically exist for any use, the market reading effectively forecloses the transformative use doctrine. High suppression from licensing infrastructure itself.
constraint_indexing:constraint_classification(fair_use_statutory_exception__market_licensing_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COPYRIGHT HOLDER WITH LICENSING APPARATUS (ROPE) — Benefits substantially from the market-licensing reading. Coordinates rights distribution and revenue capture through licensing markets. Experiences the constraint as legitimate coordination: fair use carve-outs are unnecessary because all legitimate uses can be licensed. The copyright holder has arbitrage options — can refuse licenses, demand high prices, or differentiate by licensing tier. Net beneficiary with no suppression pressure.
constraint_indexing:constraint_classification(fair_use_statutory_exception__market_licensing_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EDUCATIONAL INSTITUTION (TANGLED ROPE) — Partially benefits from licensing arrangements (can negotiate volume discounts, obtain legal certainty) but also bears significant extraction costs. Fair use collapse forces licensing expenditures on uses previously considered defensive. Constrained because institutional mission (public education) creates ongoing demand for the licensed material. Some coordination function (licensing markets reduce uncertainty about permissible uses), but asymmetric extraction (costs borne by institutions, benefits accrue to copyright holders).
constraint_indexing:constraint_classification(fair_use_statutory_exception__market_licensing_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN-ACCESS MOVEMENT (SCAFFOLD) — Organized actors (open-access publishers, CC-licensed creators, library coalitions) see the market-licensing reading as a temporary constraint that their own licensing models are superseding. By deliberately restricting their own copyright and using alternative licensing (CC0, CC-BY), they create domains where fair use questions become moot — the content is pre-licensed for use. This is a sunset mechanism: as CC and open-access content accumulate, the market-licensing constraint's functional domain shrinks. Effective extraction low because this coalition has exit routes and can migrate to alternative regimes.
constraint_indexing:constraint_classification(fair_use_statutory_exception__market_licensing_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: JUDICIAL INTERPRETIVE COMMUNITY (PITON) — The four-factor fair use test remains formally operative but has become substantially performative under the market-licensing reading. Courts acknowledge fair use doctrine while structurally reducing it via the licensing market factor (factor 4). The judicial system maintains the interpretive apparatus (fair use hearing, four-factor analysis) as a vestigial ritual despite the market-licensing reading having largely predetermined outcomes. Theater ratio high: the appearance of fair use adjudication persists while the functional space for fair use has contracted.
constraint_indexing:constraint_classification(fair_use_statutory_exception__market_licensing_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MARKET EFFICIENCY VIEW (MOUNTAIN) — From a civilizational perspective, the market-licensing reading represents an optimization principle: if a use can be licensed, it should be licensed; efficiency demands monetization of all monetizable value. Under this view, fair use doctrine is an incomplete market correction now rendered unnecessary by sophisticated licensing infrastructure. The reading appears as a natural law of information economics. However, the structural data (specific beneficiaries, institutional enforcement, victims identified) contradicts the mountain classification — this is a false summit.
constraint_indexing:constraint_classification(fair_use_statutory_exception__market_licensing_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fair_use_statutory_exception__market_licensing_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fair_use_statutory_exception__market_licensing_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, TR),
    TR >= 0.70.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High, reflecting near-total gatekeeping power over uses that would previously be fair-use-free. The market-licensing reading eliminates the doctrine's protective function by subordinating transformativeness and educational value to licensing market existence. As licensing infrastructure matures (music licensing via Harry Fox and Performing Rights Organization models, e-book licensing, image licensing through Getty and Corbis systems), the practical domain of fair use shrinks toward zero. The 0.78 value reflects that while a few economically nonviable uses remain unmonetizable (criticism of obscure works, educational use where licensing revenue would be trivial), nearly all significant uses can be licensed. Suppression (0.72): High. The mechanism is not legal prohibition per se but rather licensing gatekeeping backed by copyright law enforcement. Potential fair users face institutional barriers (licensing transaction costs, licensing availability uncertainty, licensing price discrimination) and legal barriers (fair use doctrine's collapse under the market-licensing reading). The suppression is enforced through copyright litigation and licensing negotiations, both capital-intensive. Theater ratio (0.68): Moderately high. The four-factor fair use test remains formally operative in judicial proceedings, but the market-licensing reading has made the test substantially performative. Courts hear fair use arguments while the licensing market factor overwhelmingly predetermines outcomes. The appearance of fair use adjudication persists while the functional doctrine has contracted. The theater ratio reflects that this is not yet fully piton (judicial system still produces genuine uncertainty in borderline cases), but the trajectory is toward piton as licensing markets mature and courts internalize the market-licensing logic.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The copyright holder sees legitimate coordination (Rope) — fair use is an incomplete market now corrected by licensing infrastructure. The potential fair user sees pure extraction (Snare) — any conceivable use that could theoretically be licensed is barred, with no defense. The transformative creator sees snare with no recognition of transformativeness (Snare) — the doctrine that nominally protects parodists and critics is collapsed by the licensing market factor. The educational institution sees mixed coordination and extraction (Tangled Rope) — licensing provides certainty and coordination of rights distribution, but forces expenditures on previously-free uses. The open-access movement sees a temporary problem being solved (Scaffold) — CC licensing and open-access publishing create alternative domains where the constraint's logic becomes irrelevant. The judicial system sees its own degraded ritual (Piton) — the fair use test persists as theater while outcomes are largely predetermined by the licensing market factor. The civilizational observer risks seeing natural law (Mountain) — that information economics necessarily requires monetization of all monetizable value — but the structural data reveals beneficiaries and victims, revealing this as a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position and exit options. Copyright holders with licensing apparatus occupy d ≈ 0.05 (full beneficiaries with arbitrage exit) — low d produces low or negative chi, confirming their rope experience. Potential fair users occupy d ≈ 0.95 (full targets with no exit) — high d produces high chi via f(d) ≈ 1.42, confirming snare. Transformative creators occupy d ≈ 0.70 (victims constrained by licensing cost) — moderate-high d produces moderate chi, confirming snare with some agency. Educational institutions occupy d ≈ 0.55 (both benefit from coordination and bear extraction) — this symmetric positioning produces tangled rope. Open-access coalition occupies d ≈ 0.40 (victims with exit routes) — constrained-but-mobile exit options produce scaffold. The falsehood of the mountain classification at the analytical perspective is revealed by computing d for the 'market efficiency' view: an observer who treats market-licensing as natural law must assign d based on whether they benefit from licensing regimes, which varies by institutional position. An analytical observer cannot be structurally neutral — their professional identity (academic, technologist, publisher) determines d, making the mountain classification false.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The market-licensing reading resolves mandatrophy by collapsing fair use into licensing economics. There is no ambiguity about whether this is extraction or coordination — the reading explicitly eliminates coordination (fair use as independent doctrine) and replaces it with pure extraction (licensing gatekeeping). The mandatrophy for the kernel (fair use doctrine broadly) remains unresolved — the three readings produce contradictory classifications of the same statutory text, and the engine sees all three, producing a presheaf that cannot be flattened to a single type. But for the market-licensing reading instantiated in this story, mandatrophy is resolved: this reading produces consistent snare classification for all victims (fair users, transformative creators, students, teachers) and rope classification for all beneficiaries (copyright holders with licensing capacity). The high extractiveness (0.78) is justified by the doctrine's near-total collapse and the rising suppression trajectory (0.48 → 0.72) reflects licensing infrastructure maturation over the past 40 years.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    licensing_market_existence_threshold,
    'What constitutes ''existence'' of a licensing market for fair use collapsibility? Technical feasibility vs. actual commercial operation vs. theoretical possibility?',
    'Case law analysis: does copyright holder''s theoretical ability to license (they could set up a licensing scheme) collapse fair use, or does an actual functioning market need to exist? Survey of licensing market prevalence across use categories (educational excerpts, criticism, parody, research quotation).',
    'If threshold = theoretical possibility: fair use collapses entirely — nearly all uses can be theoretically licensed. If threshold = functioning market: fair use survives where licensing markets are economically nonviable (criticism of obscure works, educational use of long-tail content). This is the axiomatic question that determines the reading''s applicability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_market_existence_threshold, conceptual, 'Whether licensing market existence means theoretical possibility or actual functioning market').

omega_variable(
    transformative_use_independence,
    'Can transformative use doctrine survive the market-licensing reading, or does transformativeness become irrelevant if a licensing market can exist?',
    'Jurisprudential analysis of Harper & Row v. Nation (licensing factor precedent) and post-Campbell (Acuff-Rose) cases; examination of whether courts treat transformativeness as a standalone defense or as an input to the licensing market factor. Corpus analysis of fair use cases where transformativeness was decisive despite licensing availability.',
    'If transformativeness overrides licensing market availability: snare classification weakens, fair use survives in parodic and critical contexts even if licensing is possible. If licensing availability overrides transformativeness: snare classification confirmed, transformative doctrine becomes vestigial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformative_use_independence, empirical, 'Whether transformative use doctrine survives market-licensing collapsibility').

omega_variable(
    reading_foreclosure_scope,
    'Does the market-licensing reading logically foreclose the transformative-right reading, or do they coexist in contemporary doctrine?',
    'Analysis of Supreme Court language in Campbell, Cariou, and Andy Warhol Foundation cases; examination of holdings that explicitly protect transformative uses despite licensing possibility vs. holdings that prioritize licensing market factor. Identification of cases that adopt the market-licensing logic vs. transformative-right logic; assessment of whether courts apply both simultaneously or as competing frameworks.',
    'If forecloses: the two readings cannot coexist in a single legal framework — a holding for one rules out the other. If coexists: both readings remain live in contemporary case law, producing inconsistent outcomes. Coexistence would indicate the kernel (fair use doctrine) accommodates contradictory readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_scope, conceptual, 'Whether market-licensing reading forecloses transformative-right reading or coexists with it').

omega_variable(
    licensing_administration_cost_feasibility,
    'For what categories of potential uses is licensing market administration economically feasible? At what transaction cost does licensing become infeasible despite theoretical availability?',
    'Cost-benefit analysis of licensing administration for different use categories: educational quotations, research excerpts, criticism, parody, remix. Comparison of licensing revenue vs. administration cost (licensing agency overhead, rights clearance complexity, enforcement cost). Identification of use categories where licensing revenue < administration cost.',
    'If many uses become economically non-licensable: the market-licensing reading cannot practically cover all fair use scenarios, and fair use survives in the economically non-licensable domain. If licensing is administratively feasible for nearly all uses: snare classification confirmed across all use types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_administration_cost_feasibility, empirical, 'Economic feasibility threshold for licensing market administration across use categories').

omega_variable(
    natural_law_status_of_reading,
    'Is the market-licensing reading a natural law of copyright optimization, or a contingent legal construction that benefits specific institutional actors?',
    'Historical analysis: did copyright law always embody market-licensing logic, or was the licensing factor (factor 4) introduced later and subsequently expanded? Comparison with alternative reading frameworks (transformative-right, narrow-defense) and their historical pedigree. Identification of which beneficiary institutions gained power and revenue as the market-licensing reading became dominant in case law.',
    'If natural law: the reading reflects optimal information economics and cannot be superseded without efficiency loss. If contingent construction: the reading is a beneficiary-favorable doctrinal choice that can be revised without structural economic harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_status_of_reading, conceptual, 'Whether market-licensing reading is natural law or contingent legal construction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fuslr_theater_ratio_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fuslr_theater_ratio_t20, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(fuslr_theater_ratio_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(fuslr_base_extractiveness_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fuslr_base_extractiveness_t20, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(fuslr_base_extractiveness_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fuslr_suppression_requirement_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(fuslr_suppression_requirement_t20, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(fuslr_suppression_requirement_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_licensing_infrastructure_maturity).

% DUAL FORMULATION NOTE:
% The fair use doctrine kernel (fair_use_statutory_exception) decomposes into three structurally distinct constraint stories, each instantiating a different reading of Section 107 and producing different classifications. The market-licensing reading (this story, ε=0.78) treats fair use as collapsed into licensing economics — a snare for users, rope for holders. The transformative-right reading (sibling, ε estimated ~0.40) treats fair use as an independent doctrine protecting transformative expression, producing tangled rope. The narrow-defense reading (sibling, ε estimated ~0.30) treats fair use as a narrow historical category, producing rope with limited scope. The three readings coexist in contemporary case law, producing inconsistent jurisprudence. Each reading structures its own constraint story with its own perspectives, beneficiary/victim declarations, and measurements. This story links to both siblings via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
