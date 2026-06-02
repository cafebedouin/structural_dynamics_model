% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_narrow_defense, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Statutory Exception (Restrictive Interpretation Reading)
 *   domain: intellectual_property_law/copyright/legal_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates one specific reading of the contested fair
 *   use kernel: fair use is interpreted as a narrow statutory exception to
 *   copyright's property protection rather than as an independent right.
 *   Under this reading, the burden of proof falls on the user to demonstrate
 *   that their use qualifies as fair — commercial nature, market harm to the
 *   original, and amount of taking used are weighted heavily in copyright
 *   holder's favor. The reading privileges the axiom that 'copyright is
 *   property, copyright holders' control over their works is primary, and
 *   fair use is the exception whose scope must be constrained to preserve
 *   market value.' This is in tension with an alternative reading
 *   (transformative_right_reading) that treats fair use as a recognized
 *   independent right whose burden is on the copyright holder to prove market
 *   harm, and with a third reading (market_licensing_reading) that treats
 *   fair use primarily as a mechanism for clearing rights through market
 *   channels. The narrow defense reading has dominated US copyright doctrine
 *   since the 1970s, particularly sharpened after the Campbell v. Acuff-Rose
 *   and Harper & Row cases, and has been reinforced through the Sony Bono
 *   Copyright Term Extension Act (1998) and digital copyright enforcement
 *   (DMCA). The extractiveness value (0.58) reflects the reading's asymmetric
 *   burden allocation and the expanding scope of what counts as commercial
 *   exploitation (social media posts, educational use with institutional
 *   revenue, remix communities generating attention/influence). Suppression
 *   has increased over the measurement interval (0.50 → 0.65) as litigation
 *   costs rise, intermediary licensing requirements multiply, and
 *   circumvention liability (DMCA § 1201) creates secondary layers of legal
 *   risk independent of fair use doctrine itself.
 *
 * KEY AGENTS:
 *   - Copyright Holders (Publishers, Studios, Rights Owners): Primary beneficiary (institutional/arbitrage) — the narrow reading preserves their licensing revenue by constraining fair use scope and placing burden of proof on users. Benefits from market-substitution presumptions and commercial-character weighting.
 *   - Remix and Criticism Communities: Primary victim (powerless/trapped) — face chilling effect, expensive litigation risk even with plausible defenses, inability to use cultural materials for transformative purposes without licensing. Structurally immobilized by the narrow reading's burden allocation.
 *   - Educational Institutions: Secondary victim (moderate/constrained) — high licensing costs, restricted use of copyrighted materials in curricula, but also benefit from market coordination of authorized educational materials. Cannot freely adapt or create derivative educational works.
 *   - Non-Commercial Reusers: Victim (powerless/constrained) — the narrow reading does not create a commercial-use exception; even non-profit reusers face extraction through licensing mandates and legal uncertainty.
 *   - Licensing Intermediaries (ASCAP, BMI, HarperCollins Rights): Beneficiary (institutional/arbitrage) — profit from licensing bottleneck; maintain institutional infrastructure around the narrow reading. Have interest in defending the reading against policy reform.
 *   - Open Culture Coalition (EFF, Wikimedia, Creative Commons): Organized opposition (organized/constrained) — building alternative frameworks; challenging narrow reading through test-case litigation, policy advocacy, institutional defection (Creative Commons licensing, open-source models). Constrained but not trapped.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.58).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.65).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Statutory Exception (Restrictive Interpretation Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property_law/copyright/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, 'e0ed55a6-982f-48d8-9bb7-f40da3eba05a').
narrative_ontology:cs_kernel_codification('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', formalized).
narrative_ontology:cs_authority_grounding('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', extraction).
narrative_ontology:cs_interpretation_layer_present('e0ed55a6-982f-48d8-9bb7-f40da3eba05a').
narrative_ontology:cs_reading_relation('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', foundational, copyright_is_property_right_primary_entitlement).
narrative_ontology:cs_axiom_status(copyright_is_property_right_primary_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', copyright_is_property_right_primary_entitlement, conventional).
narrative_ontology:cs_axiom('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', foundational, fair_use_is_narrow_affirmative_defense_not_independent_right).
narrative_ontology:cs_axiom_status(fair_use_is_narrow_affirmative_defense_not_independent_right, holdable).
narrative_ontology:cs_axiom_grounding('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', fair_use_is_narrow_affirmative_defense_not_independent_right, conventional).
narrative_ontology:cs_reference_frame('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', copyright_as_property_protection_primary).
narrative_ontology:cs_drift_state('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', contemporary_digital_commerce_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e0ed55a6-982f-48d8-9bb7-f40da3eba05a', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, commercial_licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, non_commercial_reusers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, remix_and_criticism_communities).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, derivative_work_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REMIX & CRITICISM COMMUNITY (SNARE) — Structurally immobilized. Cannot exit copyright governance; cannot freely use cultural material for commentary, parody, or derivative works without legal risk. Narrow fair use reading maximizes extraction: commercial character of new work becomes determinative (even transformative criticism is 'commercial' if it generates attention or modest revenue); transformative purpose is underweighted. Suppression is high — chilling effect on creation, expensive litigation risk even for plausible defenses, resource asymmetry vs corporate copyright holders.
constraint_indexing:constraint_classification(fair_use_statutory_exception__narrow_defense_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EDUCATIONAL INSTITUTIONS (TANGLED ROPE) — High-cost exit (licensing fees, restricted curricula, circumvention liability); also derive genuine benefit from copyright (purchasing textbooks, licensed materials). Mixed experience: the narrow reading extracts via licensing mandates while also enabling the institutional coordination of knowledge access through market channels. Constrained rather than trapped — institutions have bargaining power and can negotiate licenses, but costs are substantial and asymmetric.
constraint_indexing:constraint_classification(fair_use_statutory_exception__narrow_defense_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COPYRIGHT HOLDER (ROPE) — Benefits from narrow fair use reading; experiences it as coordination of the licensing market. Fair use as 'narrow exception to preserve property value' translates to: market value is preserved, licensing revenue is maximized, derivative works and secondary uses flow through licensing channels rather than unauthorized use. Arbitrage exit (multiple revenue streams, territorial arbitrage, format licensing). The constraint solves a coordination problem: how to monetize secondary uses and derivative works across jurisdictions.
constraint_indexing:constraint_classification(fair_use_statutory_exception__narrow_defense_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN CULTURE COALITION (TANGLED ROPE) — Organized agents (EFF, Wikimedia, academic fair-use advocacy) see the narrow reading as extractive but are actively building alternative frameworks (Creative Commons, open-source licensing, policy reform). The constraint does coordinate something real (channeling derivative works through market), but the extraction is asymmetric (powerless creators subsidize copyright holder gatekeeping). Not a snare because organized resistance exists; not a rope because the distribution is unfair. Constrained by current legal regime but not trapped — coalition has legal strategies (policy advocacy, test case litigation, institutional defection).
constraint_indexing:constraint_classification(fair_use_statutory_exception__narrow_defense_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LICENSING INTERMEDIARY SYSTEM (PITON) — Rights clearance agencies (ASCAP, BMI, HarperCollins rights departments) maintain this constraint through institutional inertia. The narrow reading supports the licensing bottleneck: secondary uses must be cleared through intermediaries rather than claimed as fair use. Theater is moderate (0.48) because licensing does serve a coordination function — it IS the mechanism for tracking secondary rights — but the mechanism persists partly because it's institutionally entrenched. Exit is arbitrage (intermediaries profit either way, via licensing revenue or via defending narrow fair use in court). Piton classification: the functional verification of this constraint (does licensing actually protect authors better than open fair use would?) is contested, yet the institutional structure persists.
constraint_indexing:constraint_classification(fair_use_statutory_exception__narrow_defense_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, this reading naturalizes copyright as a property right grounded in fundamental principles (incentive theory, natural rights, authorial control) such that fair use becomes not a right but an exception whose burden is on the defendant. The classification treats intellectual property as an immutable feature of culture and commerce. However, this is a false summit: the structural data reveals the constraint as constructed institutional arrangement (copyright terms have expanded, fair use scope has contracted, transformativeness doctrine was invented in the 1990s). The analytical view risks naturalizing what is actually a reading choice with identifiable beneficiaries and victims.
constraint_indexing:constraint_classification(fair_use_statutory_exception__narrow_defense_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fair_use_statutory_exception__narrow_defense_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fair_use_statutory_exception__narrow_defense_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, TR),
    TR >= 0.70.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): This reading creates moderate-to-high extraction through asymmetric burden allocation and expansive commercial-character interpretation. The four-factor test (purpose, nature, amount, market effect) is weighted toward copyright holders: commercial nature becomes nearly determinative (even transformative works are 'commercial' if they generate influence or modest revenue); market substitution is presumed without empirical evidence; factor 4 (harm to market) is applied broadly to foreclose licensing revenue, not just direct sales substitution. The value reflects that the reading does preserve a coordination function — the licensing market for secondary rights — but the distribution is unfair. Not a pure snare (0.66+) because legitimate copyright protection purposes exist; not a pure rope (≤0.35) because the burden allocation is asymmetric. Suppression (0.65): High structural suppression. The narrow reading eliminates safe harbor for many uses and places litigation risk on the user (defendant must prove fair use affirmatively). Chilling effect is substantial: creators self-censor, educational institutions restrict use, remix communities operate in legal grey zones. The measurement interval shows increasing suppression (0.50 → 0.65) as: (a) copyright terms expand (Bono Act), (b) digital enforcement tools (DMCA circumvention liability) create secondary legal barriers, (c) litigation costs escalate, (d) intermediary licensing requirements proliferate. Theater (0.48): Moderate. The constraint does serve a genuine coordination function — licensing mechanism for tracking and compensating secondary rights — but the mechanism's scope has expanded beyond its functional necessity. Licensing covers uses that would not directly substitute for original-work sales (educational use, criticism, parody). The theater reflects the gap between the stated purpose (protecting author incentives) and the actual scope (controlling all secondary revenue streams). Not high theater (piton range) because the licensing mechanism does function and generate legitimate market data; not low theater because the scope expansion is not functionally justified. Claimed type (tangled_rope): Requires active enforcement (court litigation, DMCA circumvention cases), has clear beneficiaries (copyright holders), clear victims (reusers), and mixes genuine coordination (licensing market) with asymmetric extraction (burden allocation).
 *
 * PERSPECTIVAL GAP:
 *   The narrow defense reading produces a stark perspectival gap. The copyright holder sees coordination (rope) — they experience the constraint as solving the problem of monetizing secondary uses. The open culture coalition sees tangled rope — they see genuine coordination value but recognize asymmetric extraction. The remix community sees snare — structurally trapped by the burden allocation. Educational institutions see tangled rope — forced licensing alongside real benefit from market-coordinated materials. The licensing intermediary system sees piton — the institutional infrastructure persists partly through inertia, and the functional verification of licensing (does it actually protect authors better than open fair use?) is contested. The analytical observer at civilizational scope risks seeing mountain — treating fair use as an exception to property rights as if it were a natural law. The engine's false summit detector identifies this as naturalization: copyright scope, fair use exception, and burden allocation are all legislatively contingent, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the extraction flow. Copyright holders are beneficiaries with arbitrage options (multiple revenue streams, territorial flexibility, format licensing) — they experience low d (0.10-0.20) and thus low or negative effective extraction chi. Remix communities are victims with no exit (trapped) — they experience high d (0.90-0.95) and maximum effective extraction. Educational institutions are victims with some exit options (licensing negotiation, curriculum redesign) but high cost — they experience moderate d (0.60-0.70). The open culture coalition is organized with real exit options (policy reform, Creative Commons adoption, institutional defection) — they experience moderate d (0.50-0.60). The analytical observer sees the whole structure — d ≈ 0.72 per canonical mapping — and risks naturalizing it. The narrow reading's specific contribution to directionality is the burden allocation: by placing proof burden on the user, it shifts d upward for potential fair-use claimants (increasing their experienced extraction) and lowers d for copyright holders (by making their position the default). This directional shift is independent of the underlying market facts about substitution or complementarity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by explicit recognition that this is ONE reading of a contested kernel, not the objective truth about fair use. The constraint's classification as tangled rope reflects the specific axiom choice: copyright-as-property-primary + narrow-exception-doctrine = moderate-to-high extraction with coordination. Under the transformative_right_reading axiom (fair use as independent right), the same statutory text produces rope classification (pure coordination with minimal coercive overhead). Under the market_licensing_reading, it produces tangled rope with a different victim set. The mandate resolves by clarifying that fair use doctrine is not immanent in the statute but chosen through interpretive readings, each with different distributional consequences. The false summit concern is acute: if the narrow reading naturalizes its interpretation as 'what copyright law inherently requires' rather than 'one choice among legal-interpretive possibilities,' it becomes a false summit (mountain classification at analytical context). The engine should flag this as FSM because structural beneficiaries are identifiable and the natural-law claim is contestable. The resolution path: recognize that fair use scope is legislatively contingent (Congress has repeatedly narrowed or broadened it), that burden allocation is a doctrinal choice (not inherent in statute), and that the reading's persistence reflects institutional entrenchment (copyright industries have organized influence over doctrine) rather than inexorability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_transformativeness_weighting,
    'When a use is both transformative AND commercial (criticism with modest revenue, educational derivative work, parody used in advertising), does commercial character override transformativeness, or should the four-factor test weight them equally?',
    'Case law trajectory analysis (Sony v. Bleistein, Harper & Row v. Nation, Google Books, Andy Warhol Foundation cases) showing evolution of commercial-factor weighting vs transformativeness weighting. Court opinions explicitly discussing relative weight.',
    'If commercial overrides: narrow reading confirmed, extraction ε remains 0.58+. If four factors weighted equally: fair use scope expands substantially, extraction drops to 0.35-0.40 (shifts toward rope), classification becomes transformative_right_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_transformativeness_weighting, empirical, 'Weight of commercial character vs transformativeness in four-factor test').

omega_variable(
    statutory_language_original_intent,
    'Does the statutory language of Section 107 (''fair use is not infringement'') establish fair use as a right or as an affirmative defense whose burden is on the user?',
    'Legislative history (1976 Copyright Act revision committee reports, Congressional testimony, statutory text analysis). Compare statutory framing to tort law conventions re affirmative defenses vs rights. Analyze pre-1976 common-law fair use doctrine (broader) vs post-1976 statutory codification (narrower).',
    'If statutory language treats fair use as a right: reading collapses, fair use becomes primary entitlement not exception. If affirmative defense interpretation is correct: narrow reading structural. Affects how courts frame burden of proof and scope of inquiry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_language_original_intent, conceptual, 'Fair use as right vs affirmative defense in statutory interpretation').

omega_variable(
    market_substitution_vs_complementarity,
    'Do secondary uses (criticism, remix, fan works, educational derivatives) primarily substitute for original-work market sales, or do they generate complementary demand (readers discover works through criticism, fan works drive franchise value)?',
    'Empirical study of consumer behavior: do criticism articles and reviews increase or decrease book/film sales? Do fan communities increase or decrease merchandise revenue? Do derivative works in public domain increase interest in original-era works? Historical analysis of pre-copyright cultural production (folk songs, literary remixes) vs post-copyright.',
    'If substitution dominant: narrow reading economically justified, ε remains high. If complementarity significant: market-harm factor is weaker than narrow reading assumes, extraction drops, reading shifts toward transformative_right_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_substitution_vs_complementarity, empirical, 'Whether secondary uses substitute for or complement original markets').

omega_variable(
    reading_foreclosure_and_axiom_status,
    'Does the axiom ''copyright_is_property_right_primary_entitlement'' (foundational to narrow reading) logically foreclose the transformative_right_reading''s axiom ''fair_use_is_recognized_right_not_exception'', or can both readings coexist within different institutional frameworks?',
    'Jurisprudential analysis: can a legal system hold both (a) copyright as strong property protection AND (b) fair use as a robust independent right? Examine comparative law (US copyright vs European copyright/resale right systems, TRIPS framework, jurisdictional variation). Identify whether the readings are logically contradictory or occupy different interpretive traditions.',
    'If foreclosure holds: reading relation is forecloses (one framework cannot hold both). If coexistence: reading relation is coexists_with (different jurisdictions/traditions maintain different axioms). Affects omega axiom status classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_and_axiom_status, conceptual, 'Logical relationship between property-rights-primary and fair-use-as-right axioms').

omega_variable(
    narrow_defense_reading_vs_natural_law_ambiguity,
    'Is the narrow defense reading (''fair use is exception, burden on defendant'') a contingent legal interpretation of statutory language, or does it reflect a natural law principle about property rights and authorial control?',
    'Historical analysis of copyright doctrine: (a) pre-Statute of Anne (1710) common-law fair use practices, (b) early US copyright doctrine evolution, (c) 1976 statutory codification narrative, (d) comparative legal systems. Identify whether the ''exception'' framing is inherent to copyright or introduced as a doctrinal choice in specific historical moment.',
    'If contingent interpretation: mountain (false summit) classification is correct engine output. If natural law principle: mountain classification is legitimate. The constraint can only be a genuine mountain if property rights are immutable; if copyright scope is legislatively contingent, the analytical perspective mistakes institutional arrangement for natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrow_defense_reading_vs_natural_law_ambiguity, conceptual, 'Whether narrow fair use reading reflects contingent interpretation or natural law principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fusd_tr_t0, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fusd_tr_t15, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(fusd_tr_t30, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(fusd_be_t0, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fusd_be_t15, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(fusd_be_t30, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fusd_su_t0, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fusd_su_t15, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(fusd_su_t30, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, copyright_term_extension__perpetual_exclusion_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, dmca_circumvention_liability__anticircumvention_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, digital_rights_management__vendor_lock_in_reading).

% DUAL FORMULATION NOTE:
% Fair use doctrine in US copyright law decomposes into three structurally distinct constraints with different ε values: (1) narrow_defense_reading (this file, ε=0.58, tangled_rope), (2) transformative_right_reading (ε=0.30-0.35, rope), (3) market_licensing_reading (ε=0.40-0.45, tangled rope with different victim set). The statutory text (Section 107) is the kernel; the readings are different interpretations of the same text. This narrow reading is upstream of copyright-term-extension and downstream of digital-rights-management doctrines — it is reinforced by DMCA circumvention liability (creates secondary barriers to fair use) and reinforces copyright-term extension (longer terms make the licensing bottleneck more extractive). Temporal note: the narrow reading has dominated since the 1990s (Sony Bono Act, digital copyright era); prior to 1976, common-law fair use was broader.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, institutional, 0.15).
constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
