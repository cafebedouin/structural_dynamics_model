% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__transformative_use_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use Four-Factor Test: Transformative Use Dominance Reading
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   The transformative-use reading of the fair-use four-factor test
 *   establishes that when a use adds new meaning, purpose, or expression to
 *   an existing copyrighted work, that transformation becomes the dominant
 *   factor in fair-use analysis, often overriding market harm to the
 *   original. This reading emerged from Campbell v. Acuff-Rose Music (1994)
 *   and has become the leading interpretive frame in federal courts. The
 *   reading benefits remix practitioners and UGC platforms (who can now host
 *   vast libraries of derivative work), while imposing extraction on
 *   copyright holders whose licensing revenue erodes and licensing
 *   intermediaries whose transaction volume shrinks. Transformation itself is
 *   context-dependent and contestable, creating a boundary-definition problem
 *   the reading was meant to solve. The claim/metric independence principle
 *   applies: this reading is CLAIMED as tangled_rope (genuine coordination of
 *   copyright incentives with cultural production + asymmetric extraction
 *   from copyright holders whose licensing control is subordinated), and the
 *   authored metrics describe that mixed structure honestly—moderate
 *   extraction, context-dependent suppression (suppression of copyright
 *   claims, not of speech), rising theater ratio as claims about
 *   'transformation' become more expansive than the doctrinal core.
 *
 * KEY AGENTS:
 *   - remix_culture_practitioners: beneficiaries who gain exemption from licensing requirements when their reuse adds new meaning
 *   - user_generated_content_platforms: institutional beneficiaries and agenda-setters whose business models depend on hosting transformative work
 *   - original_copyright_holders_without_transformation_defense: payers whose licensing revenue is subordinated to the transformation threshold
 *   - courts_adjudicating_fair_use: institutional agenda-setters who establish what counts as transformation in the first instance
 *   - licensing_industry_intermediaries: payers whose market shrinks as transformation-as-exemption expands
 *   - low_resourced_independent_creators: excluded from the transformation conversation; harmed when their work is remixed without compensation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.58).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.42).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test: Transformative Use Dominance Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, 'a25e3316-3161-4415-b6ff-5e84bc9ba2e0').
narrative_ontology:cs_kernel_codification('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', fixed_text).
narrative_ontology:cs_authority_grounding('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', lineage).
narrative_ontology:cs_interpretation_layer_present('a25e3316-3161-4415-b6ff-5e84bc9ba2e0').
narrative_ontology:cs_reading_relation('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', foundational, transformation_as_dominance_threshold).
narrative_ontology:cs_axiom_status(transformation_as_dominance_threshold, holdable).
narrative_ontology:cs_axiom_grounding('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', transformation_as_dominance_threshold, instrumental).
narrative_ontology:cs_axiom('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', foundational, market_harm_subordinated_to_new_meaning).
narrative_ontology:cs_axiom_status(market_harm_subordinated_to_new_meaning, holdable).
narrative_ontology:cs_axiom_grounding('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', market_harm_subordinated_to_new_meaning, deontological).
narrative_ontology:cs_reference_frame('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', statutory_four_factor_balance).
narrative_ontology:cs_drift_state('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', contemporary_ugc_platform_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a25e3316-3161-4415-b6ff-5e84bc9ba2e0', '2026-06-11T14:23:00Z').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_culture_practitioners).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, transformative_artists).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders_without_transformation_defense).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, licensing_industry_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, musicians, filmmakers, and creators who build on existing works by adding new meaning, context, or expression. The transformative-use reading privileges their practice by elevating transformation above raw market harm in the four-factor test. They gain legal protection for work that would fail under a creator-centric framing that prioritizes original author compensation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_culture_practitioners, beneficiary,
    organized, biographical, mobile, global).

% YouTube, TikTok, Twitch, remix sites, and fan-creation ecosystems whose business models depend on user-created content that incorporates existing works. The transformative-use reading enables them to host and monetize vast libraries of transformative work (mashups, reaction videos, parodies, covers, remixes) that would face higher infringement liability under the creator-centric reading. They set the de facto policy by deciding what content to host and promote.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms, agenda_setter).

% Music labels, film studios, publishers, and individual authors whose works are used without permission in ways the copyright holder views as non-transformative or inadequately transformative. Under this reading, their ability to restrict derivative use and collect licensing fees is subordinated to a transformation threshold they did not set. They bear enforcement costs (takedown notices, litigation) and face revenue loss where uses they would charge for are declared transformative and therefore exempt.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders_without_transformation_defense, payer,
    powerful, generational, constrained, global).

% Federal courts, particularly the Second Circuit and Supreme Court, that interpret the fair-use doctrine and apply the four-factor test. The transformative-use reading has become the dominant lens in precedent (Campbell v. Acuff-Rose, Harper & Row v. Nation, Andy Warhol Foundation v. Goldsmith); courts now front-load transformation in their analysis, often treating factors 2–4 as secondary. Their decisions establish what qualifies as transformation, what market harm counts as 'superseded' by transformation, and who bears the burden of justifying non-transformation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, courts_adjudicating_fair_use, agenda_setter,
    institutional, generational, analytical, national).

% Self-published authors, indie musicians, and solo creators whose work is sampled, remixed, or incorporated without permission in uses that may or may not be transformative. They lack the legal resources to pursue infringement claims and are often excluded from the transformation conversation—their work is treated as raw material, not authored creation deserving of protection. They benefit from fair use when they remix others' work but are harmed when their own work is remixed without license and no one advocates for their interests.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, independent_creators_low_resourced, excluded,
    powerless, biographical, identity_locked, global).

% Rights agencies, collective licensing societies (ASCAP, BMI, SESAC), and licensing-management platforms that operate on the assumption that use of copyrighted works requires negotiated permission and compensation. The transformative-use reading erodes their market by expanding the exemption space—uses that would previously require licenses are now declared fair use. They lose transaction volume and must navigate a growing gap between what the reading treats as license-exempt (transformative) and what copyright holders believe should require compensation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, licensing_industry_intermediaries, payer,
    powerful, generational, constrained, global).

% Legal scholars, fair-use commentators, and the broader interpretive community that develops case law doctrine and establishes what 'counts' as sufficiently transformative. They serve an analytical function, clarifying and testing the reading's boundaries without direct stake in outcomes. Their analysis shapes how courts apply the reading and therefore indirectly influences what uses are protected.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, transformative_threshold_interpreters, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal framework that permits creative reuse of existing works when the use adds new meaning, context, or expression—enabling remix, parody, commentary, and cultural evolution without requiring negotiated permission from every original creator. Solves the collective-action problem that transformative cultural production would face if every reuse required individual licensing.
% TRANSFER_FUNCTION: Shifts control over licensing decisions from original copyright holders (who would otherwise authorize or deny reuse) to an emergent category of transformative uses (whose exemption is determined by courts rather than negotiated). The constraint transfers some of the original author's exclusive right to license derivative works toward users/remixers who can now reuse work without payment or permission, conditional on meeting the transformation threshold.
% ABSENT_VOICES: Low-resourced independent creators whose work is remixed without their participation or compensation are structurally absent from the fair-use conversation. They lack standing to object when their work is transformed by others and cannot afford litigation to clarify whether the use of their work was fair. Licensing intermediaries excluded from profitable licensing transactions do not appear in canonical fair-use doctrine. Audiences/consumers who benefit from access to transformative work are not named seats in the legal framework.
% DISAPPEARANCE_RATIONALE: If the transformative-use reading disappeared and fair use reverted to a more creator-centric balancing (transformation as one factor among equals, not the dominant frame), licensing transaction volume would rise sharply, UGC platforms would face heightened liability exposure, and remix/parody/mashup culture would contract or move to licensed/permission-based models. The digital creative economy built on transformation-as-exemption would reorganize toward clearing houses or would decentralize to platforms operating outside U.S. jurisdiction.
% FOUNDING_PROBLEM: Copyright law's four-factor fair-use test (17 U.S.C. § 107) needed a coherent interpretive frame to prevent it from collapsing into case-by-case unpredictability. The transformative-use reading emerged from Campbell v. Acuff-Rose Music (1994) as a doctrinal solution: transformation of purpose/meaning became the primary threshold question, unifying and simplifying analysis of what uses serve copyright policy without requiring individual negotiation.
% FOUNDING_PROBLEM_CORROBORATION: The transformative-use reading is attested by federal case law, particularly the Supreme Court's Campbell opinion and subsequent Second Circuit precedent (Harper & Row, Andy Warhol Foundation). Independent legal scholarship (e.g., Tushnet, Lemley, Nimmer) documents the reading's doctrinal emergence and its effects on licensing markets. However, copyright holders and licensing industries contest whether the reading has solved or merely displaced the predictability problem, and whether transformation has overgrown its proper role. The foundational problem's status remains contested because courts still face difficult line-drawing questions about what counts as transformation.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The transformative-use reading is extractive (ε = 0.58 at interval end) because it systematically shifts licensing control from copyright holders to an emergent category (transformative reuse) that copyright holders do not set. The reading solves a real coordination problem—copyright's four-factor test was unpredictable—but the solution privileges one party's interests (remix practitioners, platforms) over another's (copyright holders). Extraction grows over time (from 0.35 to 0.58) as courts apply the reading more expansively, treating more marginal transformations as exemptive. Theater ratio rises (0.10 to 0.28) because enforcement increasingly focuses on claims about 'transformation' that diverge from the doctrinal core—platforms claim transformation for content that adds marginal novelty (reaction videos, algorithm-selected clips) not deliberate new meaning. Suppression is moderate (0.42 at end) because the reading actively suppresses copyright holders' licensing claims via fair-use exemption, but does not suppress free speech or artistic production directly; the suppression operates on one dimension (copyright control) while enabling another (remix production). The constraint is tangled_rope, not snare, because it genuinely coordinates fair-use predictability and incentives for cultural production—both real problems—while also extracting licensing value from copyright holders who bear enforcement costs and revenue loss. The asymmetry is built into the doctrine: transformation dominates factor analysis, so copyright holders cannot easily argue that market harm should override exemption.
 *
 * PERSPECTIVAL GAP:
 *   From the remix practitioner and platform perspective, this reading is close to rope: it coordinates cultural production, solves the licensing bottleneck, and treats transformation-as-exemption as the natural state. From the copyright holder's perspective, it is closer to snare: the transformation threshold is contested and boundary-stretching, enforcement burden is high (must dispute whether a use is truly transformative), and licensing revenue erodes without compensation. From the low-resourced independent creator's perspective, the reading offers no protection: their own work is remixed without license, they lack standing in fair-use doctrine, and the reading treats all creators' work as equally available for transformation. Courts apply the reading as doctrinal, neutral principle; copyright holders experience it as extraction; platforms experience it as business enablement. The engine should compute different types per seat based on directionality: platforms near full beneficiary (d near 0.0), copyright holders near target (d near 1.0), practitioners symmetric (d near 0.5).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from the beneficiary/victim declarations and exit options. Remix practitioners and UGC platforms are beneficiaries with arbitrage/mobile exit (they could license, but the reading exempts them). Original copyright holders are victims with constrained exit (they must accept transformation claims, cannot easily litigate away the doctrine, and licensing volume shrinks regardless). The transformation threshold is the pivot: where transformation is clearly established, exit is more constrained for copyright holders (they cannot argue non-transformation successfully). Where transformation is marginal or disputed, suppression burden is higher (copyright holders must litigate to establish non-transformation). Licensing intermediaries are payers in constrained positions (they lose market share as transformation expands the exemption space). Courts are agenda-setters who establish what counts as transformation, thereby controlling access to the beneficiary status. The spatial scope (global) amplifies effective extraction because the reading's precedent effects flow across borders where U.S. copyright law's interpretation shapes platform behavior worldwide.
 *
 * MANDATROPHY ANALYSIS:
 *   The transformative-use reading resolves a specific mandatrophy: the four-factor test created unpredictability that harmed both remix creators (uncertain whether their use would be protected) and copyright holders (uncertain whether their claims would succeed). The reading solved this by elevating transformation to a dominant-threshold question: if a use is transformative, proceed with fair-use analysis; if not, copyright holders' interests are foregrounded. This was mandatrophy resolution: the founding problem (unpredictability) was genuinely addressed. However, the reading created a new problem it does not address: the transformation threshold itself is now the unpredictability site. Courts struggle to define transformation, leading to divergent outcomes and requiring litigation-heavy fact patterns. The reading did not make the doctrine simpler—it relocated the complexity from 'how to balance four factors' to 'what counts as transformation.' The theater ratio rising over time (0.10 to 0.28) reflects this: the reading's doctrinal coherence is performing maintenance work (courts must continually reestablish what 'transformation' means) rather than resolving the underlying problem. The constraint avoids pure snare territory because the transformation doctrine is genuinely alive in case law and courts do make distinctions; however, the mandatrophy resolution is partial: the reading solved one problem but created another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_threshold_ambiguity,
    'What degree and kind of transformation suffices to subordinate market harm? Is the threshold quantitative (% of original), qualitative (new meaning/purpose), or context-dependent (varying by medium)?',
    'Systematic analysis of court decisions on transformation across media (visual art, music, literary works, video); development of empirical taxonomy of what courts have found transformative vs. non-transformative.',
    'If the threshold is genuinely indeterminate, the reading creates unpredictability it claimed to solve (pushing the problem from case-by-case balancing to case-by-case transformation assessment). If the threshold is discoverable, it is context-dependent, which means the reading''s advantage over the creator-centric reading (simplicity, clarity) is overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformation_threshold_ambiguity, empirical, 'Whether transformation is a coherent, discoverable threshold or a relocatable balance point.').

omega_variable(
    beneficiary_scope_drift,
    'Does the transformative-use reading primarily benefit remix practitioners (who add deliberate new meaning) or does it extend to any use that claims transformation (including algorithmic sampling, data mining, incremental modifications)? Are UGC platforms the intended beneficiary or an unintended capture of the reading?',
    'Examination of how courts apply transformation doctrine to platform-hosted content (reaction videos, algorithmic recommendations, automated remixes) vs. self-authored remixes; analysis of platform incentive structures and whether they encourage genuine transformation or exploit transformation cover.',
    'If the reading''s scope has drifted to encompass minimal or incentive-distorted ''transformation,'' the extracted value and suppression of licensing revenue are higher than intended, shifting the reading toward snare territory. If the reading remains disciplined to genuine transformation-as-new-meaning, the extraction is coordinated and bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_scope_drift, empirical, 'Whether the transformative-use reading has extended to uses beyond its original scope.').

omega_variable(
    creator_centric_vs_transformative_foreclosure,
    'Is the transformative-use reading logically incompatible with the creator-centric reading, or are they two emphases within a single doctrine that could be balanced differently?',
    'Doctrinal analysis: if transformation can be demoted back to one factor among four (as the creator-centric reading would do), the readings coexist; if transformation was structurally necessary to fix problems the creator-centric reading created (unpredictability, absolute liability for licensing failures), the readings are mutually dependent rather than foreclosing.',
    'If foreclosing, the reading is a genuine alternative grounding of the kernel, and the divergence is between incompatible readings. If coexisting, the divergence is emphatic: the readings agree on the doctrine but differ on which factor should weigh more—a different kind of contest within a shared framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creator_centric_vs_transformative_foreclosure, conceptual, 'Whether transformation and creator-centric framings are logically incompatible or differently weighted within one doctrine.').

omega_variable(
    market_harm_subordination_mechanism,
    'When transformation subordinates market harm (factor 4), what is the engine of that subordination? Is it (a) transformation creates enough social value that copyright law''s core policy (incentivizing creation) is already met, (b) transformation''s new meaning renders the original''s market irrelevant, or (c) transformation is simply weighted more heavily as policy preference?',
    'Textual analysis of court opinions on why transformation overrides market harm; doctrinal comparison with how other legal systems handle transformation in copyright (EU doctrine, Commonwealth law); economic analysis of whether transformative uses materially affect original work''s market or create new separate markets.',
    'If (a), the reading is coordinating copyright incentives with cultural production incentives. If (b), the reading is based on a contingent empirical claim about market separability. If (c), the reading is explicit preference without doctrinal anchoring. The classification and mandatrophy status depend on which mechanism is true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_subordination_mechanism, empirical, 'The theoretical basis for subordinating market harm to transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(fair_tr_t0, observed).
narrative_ontology:measurement(fair_tr_t8, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(fair_tr_t8, observed).
narrative_ontology:measurement(fair_tr_t16, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(fair_tr_t16, observed).
narrative_ontology:measurement(fair_tr_t24, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement_basis(fair_tr_t24, observed).
narrative_ontology:measurement(fair_tr_t32, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement_basis(fair_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(fair_be_t0, observed).
narrative_ontology:measurement(fair_be_t8, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(fair_be_t8, observed).
narrative_ontology:measurement(fair_be_t16, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement_basis(fair_be_t16, observed).
narrative_ontology:measurement(fair_be_t24, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(fair_be_t24, observed).
narrative_ontology:measurement(fair_be_t32, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement_basis(fair_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(fair_su_t0, observed).
narrative_ontology:measurement(fair_su_t8, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement_basis(fair_su_t8, observed).
narrative_ontology:measurement(fair_su_t16, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement_basis(fair_su_t16, observed).
narrative_ontology:measurement(fair_su_t24, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement_basis(fair_su_t24, observed).
narrative_ontology:measurement(fair_su_t32, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement_basis(fair_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__transformative_use_reading, 0.18).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, copyright_licensing_market_structure).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, ugc_platform_liability_exposure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fair-use four-factor test kernel. The creator-centric and user-centric readings are separate constraints (different ε, different beneficiary/victim sets, different types) linked via affects_constraints. All three readings interpret the same statutory doctrine (17 U.S.C. § 107) but produce structurally distinct constraint stories. The transformative-use reading (this story) treats transformation as dominance threshold; the creator-centric reading treats it as one balanced factor; the user-centric reading treats it as important but not dispositive. Decomposition follows from ε-invariance: measuring fair use through the transformation lens produces a different extractiveness score (0.58 here) than measuring through the creator-centric lens (would be lower, ~0.45) or user-centric lens (would be different mix of extraction/coordination). They are not the same constraint viewed from different perspectives; they are different constraints instantiated by different interpretations of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__transformative_use_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
