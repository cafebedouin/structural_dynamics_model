% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal_theory/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   Fair use doctrine under US copyright law is a contested kernel with at
 *   least three live readings. This constraint models the CREATOR-CENTRIC
 *   READING: a jurisprudential interpretation that treats fair use as a
 *   narrow exception to copyright property rights, weighted heavily toward
 *   preserving creator incentives and market protection. Under this reading,
 *   the four factors (purpose/character, nature of work,
 *   amount/substantiality, market effect) are applied with a presumption that
 *   fair use must not undermine the copyright holder's market opportunity —
 *   particularly factor 4 (market harm), which is often determinative. This
 *   reading has been dominant in US appellate jurisprudence since the 1970s
 *   (Harper & Row v. Nation, Sony v. Universal, Campbell v. Acuff-Rose,
 *   Cariou v. Prince) but faces increasing pressure from transformative-use
 *   and user-centric readings that prioritize cultural remix, educational
 *   access, and public-domain extension. The structural data reveals this
 *   reading as a tangled rope: it genuinely coordinates creator incentives
 *   and licensing markets (coordination function) while simultaneously
 *   extracting from transformative users through legal uncertainty, chilling
 *   effects, and suppressed alternatives (extraction function). The
 *   suppression values reflect both legal doctrine (courts apply the
 *   framework to restrict fair use) and structural barriers (litigation
 *   costs, evidentiary burden on defendants, commercial licensing
 *   requirements).
 *
 * KEY AGENTS:
 *   - Copyright Holders (Rights Holders): Primary institutional beneficiary (institutional/arbitrage) — benefit from narrow fair use exceptions, predictable licensing markets, enforcement rights, market control.
 *   - Transformative Users (Artists, Remixers, Educators): Primary victim (powerless/trapped or moderate/constrained) — face legal uncertainty, litigation risk, licensing costs, and narrowed fair use scope that restricts derivative creation.
 *   - Derivative Work Creators (Fan Works, Remix Artists, Critical Adaptations): Secondary victim (moderate/constrained) — entrepreneurial creators who build on existing works without licenses; chilled by legal exposure under this reading.
 *   - Public Domain Advocates: Tertiary victim (powerful/mobile) — argue for narrower copyright terms and broader public domain; systematically disadvantaged by a reading that privileges creator protection over public access.
 *   - Courts (Appellate Judiciary): Institutional actor (institutional/arbitrage) — apply the framework as case-by-case adjudication; perform a ritual of individualized fair use analysis while doctrine has crystallized around creator-protective outcomes (piton perspective).
 *   - Analytical Observer: Sees hybrid structure (tangled_rope) — the reading coordinates creator incentives while extracting from transformers; both functions necessary to the reading's operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.58).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.62).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal_theory/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '3d22c7fe-1271-4a6d-9545-8a107bd98389').
narrative_ontology:cs_kernel_codification('3d22c7fe-1271-4a6d-9545-8a107bd98389', formalized).
narrative_ontology:cs_authority_grounding('3d22c7fe-1271-4a6d-9545-8a107bd98389', extraction).
narrative_ontology:cs_interpretation_layer_present('3d22c7fe-1271-4a6d-9545-8a107bd98389').
narrative_ontology:cs_reading_relation('3d22c7fe-1271-4a6d-9545-8a107bd98389', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d22c7fe-1271-4a6d-9545-8a107bd98389', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('3d22c7fe-1271-4a6d-9545-8a107bd98389', foundational, copyright_protection_primary_coordination_objective).
narrative_ontology:cs_axiom_status(copyright_protection_primary_coordination_objective, holdable).
narrative_ontology:cs_axiom_grounding('3d22c7fe-1271-4a6d-9545-8a107bd98389', copyright_protection_primary_coordination_objective, instrumental).
narrative_ontology:cs_axiom('3d22c7fe-1271-4a6d-9545-8a107bd98389', foundational, market_harm_presumption_for_licensing_competing_uses).
narrative_ontology:cs_axiom_status(market_harm_presumption_for_licensing_competing_uses, holdable).
narrative_ontology:cs_axiom_grounding('3d22c7fe-1271-4a6d-9545-8a107bd98389', market_harm_presumption_for_licensing_competing_uses, empirically_contingent).
narrative_ontology:cs_reference_frame('3d22c7fe-1271-4a6d-9545-8a107bd98389', copyright_property_protection_as_default).
narrative_ontology:cs_drift_state('3d22c7fe-1271-4a6d-9545-8a107bd98389', transformative_use_doctrine_pressure, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('3d22c7fe-1271-4a6d-9545-8a107bd98389', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, original_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, derivative_work_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_extension_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANSFORMATIVE USER (SNARE) — Artists, educators, and remixers operating under legal uncertainty. The four-factor test as applied under this reading creates maximum extraction: courts weigh market harm (factor 4) heavily, transformativeness (factor 2) narrowly, and hold defendants liable for proving fair use rather than placing burden on rights holders. The suppression is structural — copyright law itself creates the constraint; alternatives (compulsory licensing, explicit exemptions) are foreclosed by the reading's core commitment to property protection. No meaningful exit: cease derivative work or face litigation.
constraint_indexing:constraint_classification(fair_use_four_factor_test__creator_centric_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL EDUCATOR (TANGLED ROPE) — Universities and libraries benefit from fair use exceptions for classroom use and digital archiving (coordination function), but face significant constraints. The creator-centric reading narrows fair use for transformative educational purposes; educators must either license materials (market extraction), narrow curriculum (censorship cost), or litigate defensively (transaction cost). Mixed experience: genuine benefit from the fair use framework existing at all, but severe constraint on scope relative to perceived educational need.
constraint_indexing:constraint_classification(fair_use_four_factor_test__creator_centric_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COPYRIGHT HOLDER (ROPE) — Rights holders experience the four-factor test as a coordination mechanism: it establishes predictable rules for licensing, derivative work permissions, and market protection. The creator-centric reading is their native frame — it privileges their interests as the coordination center. Extraction runs toward this agent. They can arbitrage between licensing revenue, enforcement, and selective permission-granting. The framework's existence benefits them by establishing property rights; the creator-centric reading narrows exceptions, increasing their control.
constraint_indexing:constraint_classification(fair_use_four_factor_test__creator_centric_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COURTS / APPELLATE JURISPRUDENCE (PITON) — The judicial machinery that applies the four-factor test has largely become performative under this reading. Judge-made doctrine has crystallized around creator-protective interpretations (Harper & Row v. Nation, Sony v. Universal, Cariou v. Prince); the 'fair use analysis' ritual persists as case-by-case adjudication despite the fact that the creator-centric reading has effectively narrowed the outcome space. Theater ratio reflects that extensive litigation procedures yield predictable protection for rights holders — the appearance of individualized judgment obscures settled doctrine favoring creators.
constraint_indexing:constraint_classification(fair_use_four_factor_test__creator_centric_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RIGHTS HOLDER / NATURAL LAW READING (MOUNTAIN) — From the institutionalized perspective of copyright law itself, this reading naturalizes creator protection as inherent to copyright doctrine. The creator-centric reading presents the four-factor test as discovering and protecting what copyright 'essentially' does — reward original creators and ensure market incentives. This perspective risks false summitry by treating a contestable legal interpretation as a natural law. However, the structural data (identified beneficiaries, enforcement costs, suppression of alternatives) contradicts genuine naturality.
constraint_indexing:constraint_classification(fair_use_four_factor_test__creator_centric_reading, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — This reading instantiates a genuine hybrid: it coordinates creator incentives and property protection (coordination function: stable authorship markets, predictable licensing ecosystem) while simultaneously extracting from transformative users through legal uncertainty, litigation costs, and narrowed fair use scope (extraction function: chilling effect on derivative works, public domain restriction). Both functions are active and structurally necessary to the reading's operation. The effectiveness of creator incentives relies on the extraction from potential transformers.
constraint_indexing:constraint_classification(fair_use_four_factor_test__creator_centric_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fair_use_four_factor_test__creator_centric_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fair_use_four_factor_test__creator_centric_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, TR),
    TR >= 0.70.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. Under the creator-centric reading, transformative users face structural extraction: (1) legal uncertainty about fair use scope forces costly licensing or litigation, (2) four-factor test presumes market harm if the user's work substitutes for or competes with the original's market, (3) burden of proof falls on defendants, (4) courts apply factor 4 (market effect) with presumption against uses that could be licensed. The measurement trajectory (0.35 → 0.52 → 0.58) reflects intensifying doctrine: appellate cases from the 1980s-2000s progressively narrowed fair use; more recent cases (particularly Cariou v. Prince, Andy Warhol Foundation v. Goldsmith) have further restricted transformative-use defenses. Extractiveness rises as case law accumulates constraints. Suppression (0.62): Moderate-high, increasing. Suppression reflects: (1) copyright law itself as legal barrier, (2) litigation costs creating de facto barriers, (3) evidentiary burden on defendants to prove fair use, (4) institutional resistance from copyright industries to broader exemptions, (5) foreclosed alternatives (compulsory licensing, expanded exemptions for educational/nonprofit use) that would reduce extraction. The trajectory (0.45 → 0.58 → 0.62) shows suppression intensifying as enforcement infrastructure matures and industry litigation becomes routine. Theater ratio (0.48): Moderate, stable. Fair use analysis includes genuine substantive law (the four factors are real, not purely performative) but also performative elements: courts treat each case as a novel adjudication despite doctrine having crystallized around predictable outcomes favoring rights holders; the appearance of flexible individualized judgment obscures settled doctrine. Theater is lower than in constraints where ritual dominates (e.g., peer review), but non-trivial. The stable trajectory suggests theater has plateaued — the courts have settled into performative fair-use analysis that yields predictable pro-rights-holder outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between copyright holders and transformative users is maximal. Copyright holders see rope (predictable coordination through licensing, property protection, market incentives). Transformative users see snare (legal traps, suppressed alternatives, extraction with no exit). The four-factor test exists in both perspectives but serves opposite functions: for rights holders, it is a protection mechanism; for users, it is a control mechanism. The piton perspective (courts) reveals that the ritual of case-by-case fair use analysis obscures crystallized pro-rights-holder doctrine. The analytical observer sees the tangled rope structure: the reading genuinely coordinates creator incentives (without copyright protection, individual creators lose investment incentives) while simultaneously extracting from transformers (the narrowing of fair use beyond what incentive theory requires). The false-summit risk appears in perspective 5: treating creator-centric reading as a natural law of copyright doctrine rather than a contestable interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from the structural position of each agent relative to extraction. Copyright holders (beneficiaries with arbitrage options) occupy d ≈ 0.15 — they can choose to license, enforce, or permit uses; their exit capacity is maximal. Transformative users (victims with trapped or constrained options) occupy d ≈ 0.88–0.95 — they face legal barriers, litigation risk, and foreclosed alternatives; their exit capacity is minimal. The creator-centric reading establishes doctrine that maximizes the d gap: rights holders enjoy arbitrage, users face traps. The sigmoid f(d) converts these d values into experienced extraction (chi): beneficiaries with d ≈ 0.15 experience f(d) ≈ -0.01 (negative chi, institutional subsidy); victims with d ≈ 0.90 experience f(d) ≈ 1.30 (high chi, powerless extraction). The scope modifier σ(S) applies globally (σ=1.2), amplifying extraction at large scale: fair use doctrine operates uniformly across all US courts and internationally, making the extraction effect global in reach.
 *
 * MANDATROPHY ANALYSIS:
 *   The creator-centric reading resolves mandatrophy by explicitly identifying this constraint as a hybrid: it is NOT pure coordination (rope) because it extracts from transformative users, narrowing fair use beyond what incentive theory requires. It is NOT pure extraction (snare) because it genuinely coordinates creator incentives and maintains licensing markets that enable cultural production. The mandatrophy is resolved by recognizing that the coordination function (creator incentives) is partially achieved through the extraction function (user suppression). Narrowing fair use suppresses transformers, which increases the market value of legitimate licensing — the extraction feeds the coordination. This is the defining feature of tangled rope: both functions are structural necessities. Alternative readings (transformative-use, user-centric) would resolve the mandate differently by widening fair use and reducing suppression, thereby rebalancing the coordination/extraction ratio. This reading maximizes creator-side coordination at the cost of user-side extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_threshold_ambiguity,
    'What degree of transformation suffices for fair use under the creator-centric reading? Is incremental transformation (colorization, format-shifting) sufficient, or only substantial conceptual reconfiguration (parody, commentary)?',
    'Case law analysis: track which factual circumstances courts classify as transformative under this reading vs. sibling readings; identify whether threshold has drifted over time',
    'If threshold = incremental: many derivative works are protected (rope behavior dominates). If threshold = substantial: most derivatives are excluded (snare behavior dominates, extraction increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformativeness_threshold_ambiguity, conceptual, 'Definition of transformativeness threshold under creator-centric reading').

omega_variable(
    market_harm_causation_standard,
    'Under the creator-centric reading, does factor 4 (market harm) require direct substitution harm (the use substitutes for the original), or does it include opportunity cost (the rights holder could license what users are doing for free)?',
    'Comparative jurisprudence: identify how US courts weight substitution vs. opportunity cost; contrast with international copyright regimes; track whether this reading''s doctrine shifts burden to defendants',
    'If substitution only: fair use scope is moderate. If opportunity cost included: fair use scope collapses — nearly all uses that could be licensed are infringing (higher ε, higher suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_causation_standard, conceptual, 'Whether market harm includes opportunity cost or only direct substitution').

omega_variable(
    reading_vs_sibling_contradiction,
    'Does the creator-centric reading logically foreclose the transformative-use reading, or can both coexist within different jurisprudential frameworks?',
    'Formal analysis: identify core premises of each reading; determine whether one''s core premise directly contradicts the other''s (foreclosure) or whether they differ in weight/emphasis (coexistence)',
    'If foreclosure: US copyright law is in a state where only one reading can be canonical (implies reading_relations: forecloses). If coexistence: circuit splits and doctrinal evolution reflect simultaneous live readings (implies reading_relations: coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_contradiction, conceptual, 'Logical relationship between creator-centric and transformative-use readings').

omega_variable(
    derivative_work_chilling_effect_magnitude,
    'How severe is the actual chilling effect on derivative works and transformative creation under this reading? Is the effect empirically measurable (reduction in transformative creations filed/registered post-doctrine shift) or primarily theoretical?',
    'Empirical: registration data for derivative works, transformation-heavy genres (remix, fan works, remix music, critical adaptations); interviews with creators about licensing vs. fear of litigation',
    'If empirically severe: suppression is high, victim set (transformative users) is large. If minor: suppression may be overstated, and the reading''s actual extraction is lower than structural analysis suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivative_work_chilling_effect_magnitude, empirical, 'Magnitude of chilling effect on transformative creation').

omega_variable(
    creator_incentive_necessity,
    'Is copyright-holder protection under this reading empirically necessary to maintain creator incentives, or would alternative mechanisms (moral rights, attribution, compulsory licensing, public funding) achieve the same incentive function at lower extraction cost?',
    'Comparative law and economic evidence: jurisdictions with weaker copyright regimes; empirical studies of creator behavior under different IP regimes; international variation in cultural production',
    'If protection necessary: the reading''s extraction is justified as coordination cost. If alternatives exist: the suppression and extraction reveal themselves as protecting incumbent rights holders beyond what incentive theory requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_incentive_necessity, empirical, 'Whether copyright protection is empirically necessary for creator incentives').

omega_variable(
    sibling_reading_judicial_adoption,
    'Is the creator-centric reading the dominant jurisprudential interpretation in US courts, or have transformative-use and user-centric readings gained judicial adoption in recent decades?',
    'Case law analysis: count citations, reversal rates, and adoption timeline for creator-centric vs. sibling readings in appellate decisions over the past 20 years',
    'If creator-centric dominates: ε and suppression values reflect current doctrine accurately. If sibling readings have gained ground: ε may be declining over time (measurement should show drift), indicating this reading''s influence is weakening.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_judicial_adoption, empirical, 'Judicial dominance of creator-centric reading vs. sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fairuse_cc_theater_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fairuse_cc_theater_t15, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement(fairuse_cc_theater_t30, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(fairuse_cc_extract_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fairuse_cc_extract_t15, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(fairuse_cc_extract_t30, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fairuse_cc_suppress_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fairuse_cc_suppress_t15, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(fairuse_cc_suppress_t30, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, copyright_term_extension__sonny_bono_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, trademark_dilution__brand_protection_reading).

% DUAL FORMULATION NOTE:
% The fair use doctrine kernel decomposes into three structurally distinct constraints corresponding to three readings: creator-centric (this story, ε=0.58, tangled_rope), user-centric (downstream, ε=0.28, rope/scaffold), transformative-use (intermediate, ε=0.42, tangled_rope). Each reading applies the same legal text (four-factor test) but produces different ε values and classifications because the readings weight factors differently and presume different distributions of burden and risk. The readings are live alternatives in contemporary jurisprudence; no single reading is canonical (hence 'distributed' kernel_codification). Extractiveness differs by a factor of ~2× between creator-centric (0.58) and user-centric (0.28) readings because they presume opposite directionality: creator-centric presumes rights holders are the coordinative center; user-centric presumes access is the coordinative center. This is the canonical case of ε-invariance triggering decomposition: if the same observable (fair use doctrine) yields different ε values under different readings, the readings are different constraints, not alternative framings of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
