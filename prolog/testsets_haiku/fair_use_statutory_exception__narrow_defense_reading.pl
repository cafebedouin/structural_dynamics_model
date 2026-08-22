% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Property-Preservation Reading)
 *   domain: intellectual_property_law/legal_interpretation
 *
 * SUMMARY:
 *   Fair use under 17 U.S.C. § 107 is a statutory doctrine permitting
 *   unauthorized uses of copyrighted works under limited circumstances. The
 *   constraint story here examines ONE reading of this contested kernel: the
 *   narrow-defense reading, which interprets fair use as a narrowly construed
 *   affirmative exception to the copyright owner's default property right,
 *   where the defendant bears a high burden of proof and commercial use is
 *   disfavored per se. This reading is instantiated by major copyright
 *   holders, licensing organizations, and appellate precedent from
 *   high-profile cases (Harper & Row v. Nation Enterprises; Sony v. Betamax;
 *   Harper & Row v. Skidmore College). The sibling readings —
 *   transformative-right and market-licensing readings — instantiate the same
 *   statutory kernel but interpret fair use's scope and foundation
 *   differently. This story focuses exclusively on the narrow reading's
 *   structural logic and extractive consequences.
 *
 * KEY AGENTS:
 *   - copyright_holders (institutional beneficiary) — collect licensing fees and define the boundaries of fair use through litigation strategy
 *   - licensing_markets (structural beneficiary) — the institutional arrangement that monetizes every potential use
 *   - fair_use_defendants (primary target, moderate power) — individuals and organizations accused of infringement, burdened with affirmative defense
 *   - secondary_creators (primary target, powerless, identity-locked) — remix artists, fan creators, transformative reusers whose creative identity is fused with reuse practices
 *   - educational_institutions (secondary target, organized but constrained) — schools, libraries, universities that rely on fair use for teaching and research
 *   - courts_and_judges (agenda setter, institutional) — interpret and enforce the narrow reading through precedent
 *   - transformative reuse advocates (excluded) — scholars, civil-society organizations, creators arguing for broader fair-use doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.81).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.64).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Property-Preservation Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property_law/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '9a77e432-11db-4a73-838d-17de5016951a').
narrative_ontology:cs_kernel_codification('9a77e432-11db-4a73-838d-17de5016951a', fixed_text).
narrative_ontology:cs_authority_grounding('9a77e432-11db-4a73-838d-17de5016951a', lineage).
narrative_ontology:cs_interpretation_layer_present('9a77e432-11db-4a73-838d-17de5016951a').
narrative_ontology:cs_reading_relation('9a77e432-11db-4a73-838d-17de5016951a', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a77e432-11db-4a73-838d-17de5016951a', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('9a77e432-11db-4a73-838d-17de5016951a', foundational, copyright_is_property_default).
narrative_ontology:cs_axiom_status(copyright_is_property_default, holdable).
narrative_ontology:cs_axiom_grounding('9a77e432-11db-4a73-838d-17de5016951a', copyright_is_property_default, deontological).
narrative_ontology:cs_axiom('9a77e432-11db-4a73-838d-17de5016951a', foundational, fair_use_is_narrow_exception_not_right).
narrative_ontology:cs_axiom_status(fair_use_is_narrow_exception_not_right, holdable).
narrative_ontology:cs_axiom_grounding('9a77e432-11db-4a73-838d-17de5016951a', fair_use_is_narrow_exception_not_right, deontological).
narrative_ontology:cs_axiom('9a77e432-11db-4a73-838d-17de5016951a', secondary, licensing_market_viability_measure_of_copyright_value).
narrative_ontology:cs_axiom_status(licensing_market_viability_measure_of_copyright_value, holdable).
narrative_ontology:cs_axiom_grounding('9a77e432-11db-4a73-838d-17de5016951a', licensing_market_viability_measure_of_copyright_value, instrumental).
narrative_ontology:cs_reference_frame('9a77e432-11db-4a73-838d-17de5016951a', copyright_as_property_default_right).
narrative_ontology:cs_drift_state('9a77e432-11db-4a73-838d-17de5016951a', contemporary_digital_abundance_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9a77e432-11db-4a73-838d-17de5016951a', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, licensing_markets).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, fair_use_defendants).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, secondary_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, copyright_defendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive right to authorize or deny uses of protected works. Under this reading, every unauthorized use — commercial or not — is prima facie infringement unless the defendant can affirmatively prove fair use meets a narrow statutory exception. They collect licensing fees from uses the reading would otherwise prohibit; they define the licensing market and defend its value through litigation and licensing demands.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, beneficiary,
    institutional, generational, analytical, global).

% The institutional arrangement permitting copyright holders to monetize every potential use through market licensing. Under the narrow-defense reading, any use that COULD be licensed — even if it is educational, transformative, or culturally valuable — competes with the licensing market and therefore presumptively harms it. This reading privileges the licensing market's expansion as a measure of copyright value.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, licensing_markets, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(fair_use_statutory_exception__narrow_defense_reading, licensing_markets).

% Face infringement claims for unauthorized uses and must bear the burden of proving fair use affirmatively in court. The narrow reading makes this burden heavier: they must show that their use does not harm the licensing market and that commercial uses are disfavored per se. Legal costs are substantial; most small defendants cannot afford defense and must settle or cease. Defendants include educators, researchers, remix artists, and digital archivists.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_defendants, payer,
    moderate, biographical, trapped, global).

% Major copyright holders and licensing organizations that enforce the reading through litigation and licensing demands. They set the terms under which fair use is recognized and police the boundaries. Their financial incentive aligns with restricting fair use scope — every successful defense narrows their licensing revenue. They fund test cases that establish narrow fair-use precedent; they pursue strategic litigation against secondary creators and educational users to establish the reading's interpretation as legal doctrine.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, fair_use_plaintiffs, agenda_setter,
    powerful, generational, arbitrage, global).

% Remix artists, fan creators, mashup producers, and transformative reusers who build on existing works. Under the narrow reading, their creative practices are presumptively infringing unless they can prove non-commercial motivation and zero market harm — a high bar. Many are identity-fused with creative practice (their self-concept is constituted through transformation and reuse); exit would mean abandoning their creative identity. They face cease-and-desist letters, takedowns, and litigation threat; most self-censor rather than risk exposure.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, secondary_creators, payer,
    powerless, biographical, identity_locked, global).

% Schools, libraries, and universities that use copyrighted works for teaching and research. Under the narrow reading, even face-to-face classroom copying is scrutinized for market harm; digitized course reserves and digital libraries are presumed to harm licensing markets. They must license heavily or restrict access; they negotiate blanket licenses with copyright collectives that capture revenue for every use, including teaching uses that are traditionally considered fair use in doctrine.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, educational_institutions, payer,
    organized, generational, constrained, national).

% Interpret the fair use statute (17 U.S.C. § 107) and decide infringement cases. Under the narrow-defense reading, courts frame fair use as an exception to the copyright owner's default right and place a high evidentiary burden on defendants. Judges trained in property-law reasoning tend to favor the narrow reading; appellate precedent from major copyright holders' litigation establishes it as authoritative doctrine despite the statute's language suggesting fair use is a broader equitable principle.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% Scholars, artists, and civil-society organizations arguing for a broader fair use doctrine centered on transformativeness and cultural production. They would argue fair use should be a right, not a narrow exception; that transformative uses should receive presumptive protection; that the licensing market should not expand into educational and cultural uses. Their voice is excluded from the enforcement machinery and from the licensing negotiations that define boundaries in practice.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, transformative_reuse_advocates, excluded,
    moderate, biographical, constrained, global).

% The 1976 Copyright Act's fair use statute (17 U.S.C. § 107) lists four non-exclusive factors for courts to consider in determining fair use: purpose and character of the use; nature of the copyrighted work; amount used; market effect. The statute's text does not declare fair use an exception narrowly construed; it provides factors for equitable determination. The narrow-defense reading privileges the market-effect factor and interprets it to preserve the licensing market's expansion, rather than a more balanced reading of all four factors.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, statutory_fair_use_text, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(fair_use_statutory_exception__narrow_defense_reading, statutory_fair_use_text).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The copyright-as-property regime provides incentive for creative production by ensuring authors can monetize their works through exclusive control and licensing arrangements. Fair use, under this reading, protects only those uses that fall outside the licensing market (e.g., private scholarship with zero commercial potential). This coordinates author incentive with market revenue capture.
% TRANSFER_FUNCTION: Moves licensing fees from users and secondary creators to copyright holders and licensing collectives; also transfers litigation risk and self-censorship costs from rights-holders to defendants and smaller creators. The narrow reading amplifies the transfer by making every use presumptively infringing unless the defendant can affirmatively meet a high bar for fair-use defense.
% ABSENT_VOICES: Transformative reuse advocates, remix communities, digital archivists, and open-culture movements would argue that fair use should protect innovation and cultural production; they are excluded from the enforcement machinery and from licensing-market negotiations. Statutory fair use's four-factor test is reinterpreted to center market harm, deprioritizing the transformativeness factor — alternative readings of the same statute are structurally absent from enforcement mechanisms.
% DISAPPEARANCE_RATIONALE: If the narrow-defense reading evaporated and fair use reverted to a broader, transformativeness-centered interpretation, licensing revenues would contract, litigation costs would drop for secondary creators, digital reuse would accelerate, and secondary creative markets (remix, fan art, digital archives) would expand. The licensing market depends on the narrow reading's enforcement; it would not survive broad fair use.
% FOUNDING_PROBLEM: Copyright was established to incentivize original creative production by providing exclusive economic rights to authors. The fair-use doctrine emerged to balance author incentives with public access and scholarly use. The narrow-defense reading interprets this balance as: every use belongs to the copyright holder until proven to fall outside licensability.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and licensing organizations attest the founding problem is live and the narrow reading protects author incentives and licensing market viability. Scholars, digital archivists, and secondary creators outside the benefiting parties attest the founding problem has shifted: creative incentive is no longer at risk from scholarly and transformative reuse; the constraint now primarily protects licensing revenue expansion rather than baseline authorship. Empirical evidence (patent-licensing-field economics, declining licensing-model viability in digital markets) corroborates the shifted-function reading from outside the benefiting parties.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because the narrow reading presumes every unauthorized use is prima facie infringing, shifting the burden to defendants to prove an exception from narrow statutory categories. The commercial-use disfavor rule means even educational and scholarly uses face litigation threat unless non-commercial motivation is clear and market harm is zero — a high bar. Suppression is moderate-high (0.64) because enforcement is active (litigation, cease-and-desist letters, licensing demands, takedown notices) and the burden structure itself suppresses secondary creative activity — many creators self-censor rather than risk litigation. Theater is low-to-moderate (0.28) because the constraint's real function (preserving licensing market value) is distinct from its stated function (protecting author incentive); as digital markets mature and licensing becomes less viable, the theatrical gap widens — courts recite author-incentive rationales while data shows licensing revenue concentration. The measurement series shows extraction accumulating over the interval (0.68 to 0.81) as digital uses multiply, commercial potential for every use expands, and licensing demand intensifies. Suppression and theater remain relatively stable because the burden structure is fixed; the rise in extractiveness comes from the expanding scope of uses deemed to have commercial potential and therefore to implicate market harm.
 *
 * PERSPECTIVAL GAP:
 *   The copyright-holder seat and the secondary-creator seat compute radically different types. From the copyright holder's position, the arrangement is genuine coordination with a legitimate exception (fair use) protecting unprofitable uses — a Rope or light Tangled Rope. From the secondary-creator seat, the same structure is enforced extraction disguised as property doctrine — a Snare with identity-lock suppression. The agenda-setter seat (courts) splits the difference: judges author honest-seeming balancing-test language while precedent heavily favors copyright holders because the litigation landscape is asymmetric (well-funded repeat players vs. individual defendants). The engine's per-seat computation should show this divergence clearly.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders are full beneficiaries (d near 0.0): they collect from licensing, set the enforcement agenda, define fair use boundaries. Secondary creators are full targets (d near 1.0): they bear litigation risk, self-censor, and pay licensing fees or cease their practices. Fair-use defendants (moderate power, trapped exit) sit high on the target end because the burden of affirmative defense is structural: every use is infringing unless proven otherwise, and proof requires showing zero market harm — a fact not in defendants' control. Educational institutions are secondary targets (d around 0.75–0.85) because they have organizational power and can negotiate blanket licenses, but they cannot escape the licensing regime; teaching is identity-constitutive for educators, so exit is constrained. Courts sit near agenda-setter (d around 0.1) because they interpret the statute and set precedent, but precedent is heavily influenced by repeat-player litigation strategy from copyright holders. Transformative-reuse advocates are excluded, not seated — their directionality cannot be computed from within the constraint's operative structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (author incentive to create) has shifted over 25 years. Copyright protection is no longer at risk from scholarly and transformative reuse; the core incentive mechanism (exclusive distribution rights, media sales) is alive. What the narrow-defense reading now protects is licensing revenue expansion into secondary markets (educational use, fan culture, digital reuse). The constraint persists because major copyright holders have embedded the narrow reading into appellate precedent and licensing practice, but the reading's connection to the founding problem has attenuated. The measurement series showing rising extractiveness (0.68→0.81) while suppression remains stable and theater rises suggests the constraint is accumulating extraction without corresponding coordination gain — a mandatrophy candidate. The narrow reading has become self-vindicating: it preserves the licensing market by narrowing fair use, then cites licensing-market value as justification for the narrow reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_harm_measurement_ambiguity,
    'What counts as market harm under the narrow reading? Does harm require proven lost sales, or is mere potential licensing revenue (market opportunity cost) sufficient?',
    'Empirical study of licensing damages vs. lost-sale damages in fair-use cases; analysis of licensing-demand letters and settlements to determine what courts actually treat as market harm.',
    'If potential licensing revenue is sufficient, the market-harm factor becomes expansionist and every use with commercial potential is harmed; if only proven lost sales count, fair use remains viable for many educational and transformative uses. The current doctrine is ambiguous, which favors the copyright holder.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_measurement_ambiguity, empirical, 'Whether market harm means lost sales or foregone licensing opportunity.').

omega_variable(
    transformativeness_weight_variance,
    'Is transformativeness a separate, co-equal fair-use factor, or merely one aspect of the first factor (purpose and character)? Does the narrow reading systematically underweight it?',
    'Meta-analysis of appellate fair-use decisions coding how heavily transformativeness is weighted relative to market harm; comparison across readings (transformative-right vs. narrow-defense) in the same fact patterns.',
    'If transformativeness is systematically underweighted in the narrow reading, the reading is reconstructing fair use around market preservation rather than the statute''s four-factor test. This would move the narrow reading from plausible interpretation toward doctrine-capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformativeness_weight_variance, empirical, 'Whether the narrow reading privileges market harm over transformativeness.').

omega_variable(
    commercial_use_per_se_rule,
    'The narrow reading disfavors commercial uses per se; does this disfavor follow from the statute, or is it an interpretive add-on that shifts burden to defendants?',
    'Statutory text analysis (17 U.S.C. § 107 does not contain a per-se rule against commercial use) combined with legislative history; comparison with how other common-law defenses are structured (good-faith belief, reasonable mistake) vs. per-se rules.',
    'If per-se commercial disfavor is interpretive add-on, the narrow reading is more constraining than the statute authorizes; this would place the reading outside the kernel''s legitimate interpretation range and support the transformative-right reading''s claim that courts have narrowed fair use beyond statutory warrant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_use_per_se_rule, conceptual, 'Whether commercial-use disfavor is statutory or interpretive doctrine.').

omega_variable(
    identity_lock_mechanism_in_suppression,
    'For secondary creators (identity_locked exit), is suppression primarily structural (legal risk, litigation costs) or internalized (the creator''s sense of legitimacy, self-censorship from guilt over infringement)?',
    'Qualitative research: interviews with remix artists and fan creators before and after cease-and-desist orders; study of behavior change when legal risk is removed (e.g., in jurisdictions with broader fair-use doctrine or after license acquisition).',
    'If suppression is primarily structural, broadening fair use would release creators immediately; if internalized, broader fair use would leave residual self-censorship because the creator''s identity-fusion with copyright violation persists. Identity-internalized suppression is higher-cost to remedy and suggests the constraint''s extractive power runs deeper than legal doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_suppression, empirical, 'Structural vs. internalized suppression for identity-locked creators.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the narrow-defense reading and the transformative-right reading logically foreclose each other, or do they coexist as different parties'' competing interpretations?',
    'Analysis of whether the foundational axioms (narrow-defense: copyright-as-property-default; transformative-right: fair-use-as-right-to-innovate) can both be held within a single legal framework, or whether accepting one requires rejecting the other.',
    'If they foreclose each other, the relationship should be ''forecloses'' in cs_structure.reading_relations; if they coexist (different judges, different circuits, different parties hold them simultaneously), the relationship is ''coexists_with''. The answer determines whether the kernel is in foundational contest or in inter-institutional power struggle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether narrow-defense and transformative-right readings logically foreclose each other.').

omega_variable(
    licensing_market_expansion_necessity,
    'Is the expansion of licensing into educational, scholarly, and transformative-reuse markets necessary for author incentive, or is it secondary revenue capture after the baseline incentive is satisfied?',
    'Economic analysis comparing creative output and author income under different fair-use regimes (broad vs. narrow); cross-national comparison of countries with narrower fair-use doctrine and their creative-output metrics.',
    'If baseline-incentive is satisfied and licensing expansion is secondary revenue, the narrow reading is protecting rent-seeking rather than author incentive, which would reclassify the constraint from Tangled Rope (coordination + extraction) toward Snare (pure extraction). If baseline-incentive truly depends on licensing expansion, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_market_expansion_necessity, empirical, 'Whether licensing-market expansion is necessary for author incentive or secondary revenue capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fair_tr_t5, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(fair_tr_t10, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(fair_tr_t15, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(fair_tr_t20, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(fair_tr_t25, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(fair_be_t5, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(fair_be_t10, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(fair_be_t15, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(fair_be_t20, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(fair_be_t25, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 25, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0, 0.56).
narrative_ontology:measurement(fair_su_t5, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(fair_su_t10, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(fair_su_t15, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(fair_su_t20, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(fair_su_t25, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 25, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__narrow_defense_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, copyright_enforcement_litigation_asymmetry).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, licensing_market_expansion_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the fair-use statutory kernel (17 U.S.C. § 107). The narrow-defense reading interprets fair use as a narrow exception to copyright's default property right; the transformative-right reading interprets it as a right to enable cultural innovation; the market-licensing reading extends the narrow reading to treat licensing-market viability as dispositive. All three share the statutory text and structure; they diverge in how they weight factors, distribute burden, and connect fair use to copyright's founding justification. The three constraints form a family linked by network.affects_constraints, with the narrow-defense reading occupying a middle position between pure property (market-licensing) and pure right (transformative). Ε values diverge substantially because the readings frame the referent (the standing arrangement under contest) differently: narrow-defense frames it as copyright-as-property; transformative-right frames it as copyright-as-coordinated-innovation-right. The three stories are mutually dependent — each reading's interpretation changes the structural valence of the statute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
