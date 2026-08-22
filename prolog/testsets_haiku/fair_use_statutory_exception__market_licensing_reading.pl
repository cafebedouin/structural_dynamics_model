% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Statutory Exception — Market Licensing Reading
 *   domain: intellectual_property/legal_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the fair use statutory
 *   exception: the market-licensing reading. Under this reading, fair use
 *   exists only where no licensing market exists for the use. Any use that
 *   could be licensed—because someone would pay for it or because a copyright
 *   holder could establish a market for it—falls outside fair use protection.
 *   This reading was explicitly advocated in judicial opinions (Harper & Row
 *   v. Nation, Sony v. Universal) and remains influential in
 *   licensing-dependent industries. The reading treats fair use as a residual
 *   category, narrowed to uses where no revenue stream is possible. The
 *   constraint's expected structural delta is extremely high ε: fair use
 *   doctrine becomes null in practice for most transformative uses, because
 *   most transformative uses have potential licensing markets. This story
 *   instantiates the reading and measures its operational extractiveness
 *   under the assumption that the reading's legal premise is correct—the
 *   constraint's ε referent is the standing arrangement (this reading, as
 *   applied) assessed by the reading's own lights, not by a competing
 *   reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.89).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.76).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Statutory Exception — Market Licensing Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '353c0d6d-be56-49eb-9e49-18ebb1b3bb2d').
narrative_ontology:cs_kernel_codification('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', fixed_text).
narrative_ontology:cs_authority_grounding('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', extraction).
narrative_ontology:cs_interpretation_layer_present('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d').
narrative_ontology:cs_reading_relation('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', foundational, licensing_market_harm_is_dispositive).
narrative_ontology:cs_axiom_status(licensing_market_harm_is_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', licensing_market_harm_is_dispositive, instrumental).
narrative_ontology:cs_axiom('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', secondary, fair_use_secondary_to_copyright_property_rights).
narrative_ontology:cs_axiom_status(fair_use_secondary_to_copyright_property_rights, holdable).
narrative_ontology:cs_axiom_grounding('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', fair_use_secondary_to_copyright_property_rights, deontological).
narrative_ontology:cs_reference_frame('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', copyright_as_exclusive_economic_right).
narrative_ontology:cs_drift_state('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', contemporary_digital_creativity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('353c0d6d-be56-49eb-9e49-18ebb1b3bb2d', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, copyright_holder_licensors).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, research_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, nonprofit_educational_entities).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, independent_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entities that control copyright registrations and license works for compensation. They argue that fair use doctrine must not undermine licensing markets; any use that could generate licensing revenue should require a license. They set licensing rates, determine whom to license, and can withhold licenses entirely. They frame the market-licensing test as the proper boundary of fair use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, copyright_holder_licensors, agenda_setter,
    institutional, generational, arbitrage, global).

% Artists, scholars, filmmakers, and software developers who reuse copyrighted material in new works—parody, commentary, remix, derivative innovation. Under this reading, their use is fair use only if no licensing market exists for it. If a licensor could monetize the use, they must license or refrain. They bear the cost of obtaining licenses, renegotiating creative strategies to avoid triggering licensing requirements, or abandoning derivative works entirely.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, transformative_users, payer,
    moderate, biographical, constrained, global).

% Universities, think tanks, and non-profit research centers that rely on fair use to quote, analyze, and reproduce copyrighted material for scholarship and teaching. Under this reading, their fair use protection evaporates if the copyright holder has established (or could establish) a licensing market. They face institutional licensing costs, licensing denial for critical scholarship, and restrictions on academic freedom.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, research_institutions, payer,
    organized, generational, constrained, global).

% Schools, libraries, and educational nonprofits that use copyrighted works for teaching and preservation. This reading subjects them to licensing requirements even when the use is clearly educational and non-competing. They face budget pressures from licensing costs and face denial of fair use claims based on the existence of licensing schemes.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, nonprofit_educational_entities, payer,
    moderate, generational, constrained, national).

% Individual artists and creators (musicians sampling prior work, videographers using copyrighted footage, writers quoting existing narratives) whose creative identity is fused with the practice of transformation and reuse. This reading makes their practice legally precarious—licensing fees may be unaffordable, licenses may be denied, and fair use protection disappears once a licensor claims market harm. Identity-locked because their creative practice is defined by reuse and transformation; exit means abandoning the creative identity itself.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, independent_creators, payer,
    powerless, biographical, identity_locked, global).

% Judiciary interpreting and applying the fair use doctrine. Under this reading, courts are instructed to foreclose fair use whenever a licensing market could exist, transforming judicial discretion into a formulaic 'market harm = no fair use' rule that collapses the multi-factor test into a single determinant.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts_applying_fair_use, observer,
    institutional, generational, analytical, national).

% The body of copyright jurisprudence and statutory interpretation recognizing that fair use is an affirmative defense that should preserve non-commercial, transformative, and criticism-based uses. This reading excludes the tradition's broader framing of fair use as a balancing doctrine; it forecloses competing interpretations that emphasize transformation, criticism, and cultural production.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, copyright_law_tradition, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(fair_use_statutory_exception__market_licensing_reading, copyright_law_tradition).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint appears to coordinate the licensing market by preventing fair use from fragmenting it into unlicensed reuse that bypasses revenue collection. But the coordination is asymmetric: licensors coordinate their collective interest in maximum licensing revenue; users are not coordinated—they are individually constrained. A pure coordination reading would require participants to jointly benefit; here, the benefit flows one direction only.
% TRANSFER_FUNCTION: Moves the right to reuse copyrighted material from users to copyright holders, requiring users who would formerly rely on fair use to instead pay licensing fees to the copyright holder. The transfer is enforced by collapsing fair use doctrine to apply only where licensing markets do not exist—rendering fair use inapplicable wherever a monetizable use can be imagined.
% ABSENT_VOICES: The creative and scholarly communities most affected by this reading are excluded from the reading's authoritative circle. Copyright law is interpreted by a small set of institutional actors (major publishers, music labels, licensing administrators, a subset of judges). The millions of independent creators, educators, and researchers who depend on fair use to practice their disciplines are systematically absent from the adjudication process, despite bearing the full cost of this reading's application.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and a competing reading took its place—one that preserved fair use as a genuine defense for transformative, educational, and critical uses—the entire infrastructure of licensed works would persist, but the licensing market would contract to those uses that genuinely benefit from it. Independent creators, researchers, and educators would regain the ability to reuse works without obtaining licenses for every transformation. The reading itself exists to maximize licensing revenue; removing it would reduce that revenue while leaving copyright protection intact for non-fair-use scenarios.
% FOUNDING_PROBLEM: The founding problem is the perceived threat that fair use doctrine, broadly interpreted, would erode the licensing market for copyrighted works. If users could freely claim fair use for any reuse that did not directly compete with the original, licensors would lose licensing revenue. The reading frames the founding problem as: 'Fair use must be constrained to protect the licensing market.'
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and licensing-dependent institutions attest that the founding problem is live: fair use doctrine does threaten licensing revenue. But independent economic analysis (Gervais, Netanel) and scholarly testimony from outside the benefiting parties challenge whether the threat is real or whether licensing markets are more resilient and nuanced than a simple market-harm test assumes. Courts themselves remain divided on whether 'potential market harm' should be the dominant fair use factor. The problem's status is actively disputed in appellate litigation and academic jurisprudence.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.89 at interval end) because the constraint applies a single test ('would a licensing market exist for this use?') to collapse four factors of the statutory fair use test into a near-determinant. Once a copyright holder can claim a market for licensing exists, fair use protection disappears entirely for that use. Suppression is high (0.76) because the constraint requires active enforcement: courts must be trained to apply the market-harm test; licensing administrators must claim markets exist and deny fair use to users claiming the markets don't; users must be constrained from claiming fair use absent proof that no licensing mechanism exists. Theater rises from 0.28 to 0.42 (and plateaus) because the initial rise reflects increasing rhetorical emphasis on 'protecting licensing markets' in judicial opinions, but the plateau suggests that once the interpretive frame is stable, performance and function align—the market-harm test is what courts actually do, not what they claim to do instead. Measurements are authored on a single shared time grid: every metric is valued at every time point. The trajectory models a reading that strengthened from approximate (early opinions with mixed reasoning) to institutionalized (recent opinions treating market harm as dispositive).
 *
 * PERSPECTIVAL GAP:
 *   From the copyright holder's seat, the reading is correct: licensing markets exist for most uses, so fair use should be limited to uses where no market can be established. The doctrine preserves licensing revenue. From the transformative user's seat, the reading is a capture mechanism: it collapses fair use to near-nothing by declaring that any conceivable market for licensing means no fair use. The perspectival gap is extreme—the same constraint appears as either legitimate market protection or pure extraction depending on where you stand. The engine computes both seats' perceptions from the structural data; this story's role is to supply the structural facts, not to adjudicate the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The copyright holder licensors are the structural beneficiary (d near 0.0, full beneficiary end): they collect licensing fees, can deny licenses to suppress competitive reuse, and set rates without constraint. Transformative users and independent creators are the structural targets (d near 1.0, full target end): they pay licensing fees, face denial of fair use claims, and are suppressed from derivative work. Research institutions and educational entities are also targets, though with slightly higher power and organizational backing. Courts are the analytical observer seat (d at analytical). The directionality derivation from beneficiary/victim declarations flows naturally: beneficiaries get low d, victims get high d. The suppression mechanism is both structural (legal doctrine that forecloses fair use as a defense) and internalized (users internalize the reading and self-censor, avoiding reuses that might trigger licensing claims even where fair use would defensibly apply). The suppression is not lifted by exit: even if a user leaves the copyright system (creates original work only), the identity-locked creators have left their creative practice itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The fair use doctrine has undergone mandate drift. The statutory mandate (17 U.S.C. § 107) calls for courts to balance four factors and preserve fair use as a genuine exception to copyright exclusivity. The market-licensing reading narrows this mandate to a single test (potential market harm) that collapses the exception into near-nonexistence. This is mandatrophy: the doctrine persists, courts still recite the four factors, but the binding rule is the market-harm test, rendering the original statutory mandate functionally obsolete. The theater ratio climb from 0.28 to 0.42 marks the acceleration of this mandate drift—increasing rhetorical emphasis on licensing-market protection in judicial opinions, while the statutory multi-factor balance is displaced. The plateau suggests institutional stabilization: the mandate is now thoroughly inverted, and performance has caught up to the fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    licensing_market_existence_determination,
    'What constitutes ''a licensing market exists'' for the purpose of fair use analysis? Is the threshold: (a) a licensor HAS established an actual licensing scheme for this use category, (b) a licensor COULD establish one if they chose to, or (c) any use for which someone might hypothetically pay?',
    'Appellate decision clarifying what evidence suffices to establish market existence; empirical study of licensing scheme proliferation across content categories.',
    'Interpretation (a) leaves fair use space for uses where no actual scheme exists. Interpretation (b) or (c) renders fair use nearly extinct, as most uses could theoretically be licensed. This omega locates the reading''s operative hinge: the reading''s extractiveness depends entirely on how broadly ''market'' is defined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_market_existence_determination, conceptual, 'The threshold for establishing licensing market existence').

omega_variable(
    licensing_market_vs_transformative_function,
    'Can a use be both licensing-marketable AND transformative in a way that fair use doctrine should protect? Or does transformativeness become irrelevant once licensing markets exist?',
    'Comparative study of jurisdictions and statutory regimes (EU fair dealing vs. US fair use): do other systems preserve transformative use even when licensing markets exist?',
    'If transformativeness can override market-harm analysis, fair use survives for certain uses despite licensing markets. If market existence is dispositive, transformativeness becomes a hollow factor. This omega locates whether the reading is truly logically necessary or just an institutional choice about weighting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_market_vs_transformative_function, conceptual, 'Whether transformativeness survives once licensing markets are identified').

omega_variable(
    internalized_suppression_post_exit,
    'For identity-locked creators who internalize this reading and self-censor their derivative work, how much suppression persists if the reading is reversed? Do they return to derivative creation, or does the internalized prohibition persist?',
    'Post-reversal observation: if a different reading were adopted (transformative_right_reading), track whether identity-locked creators resume derivative work or remain suppressed.',
    'If suppression persists post-reversal, the constraint has produced internalized identity-fusion that cannot be easily undone. The measured suppression (0.76) understates the constraint''s true cost. If suppression reverses quickly, the constraint''s power is purely structural enforcement, not internalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_post_exit, empirical, 'Persistence of internalized suppression among identity-locked creators after doctrine change').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the market-licensing reading logically foreclose the transformative_right_reading, or do they merely compete across institutional seats?',
    'Formal analysis of axioms: if market-harm is dispositive, then transformative value becomes irrelevant — this is logical foreclosure within a single framework. If market-harm is one factor among four, both readings can coexist in the same framework.',
    'If foreclosure is real, the engine''s signature_reading_foreclosure will detect it. If the readings merely compete, they coexist. This omega documents the committer-frame uncertainty: the reading''s presentation suggests foreclosure, but statutory language (four-factor test) suggests coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether this reading logically forecloses competing fair-use readings').

omega_variable(
    copyright_holder_monopoly_assumption,
    'Does the reading assume that copyright holders can reliably determine and control what licensing markets exist? Or does it assume licensing markets are natural and objectively discoverable?',
    'Study of actual licensing scheme behavior: do copyright holders establish schemes where demand exists, or only where they choose to, creating artificial gaps?',
    'If markets are endogenously chosen by copyright holders, the reading creates a feedback loop: licensors suppress fair use by declaring markets exist, then establish schemes to monetize the suppressed uses. If markets are exogenous, the reading is merely tracking economic reality. This omega locates whether the reading is a descriptor or a value-maximizing strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyright_holder_monopoly_assumption, empirical, 'Whether licensing markets are objectively determined or strategically chosen by copyright holders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fair_tr_t8, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(fair_tr_t16, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(fair_tr_t24, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(fair_tr_t32, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(fair_tr_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(fair_be_t8, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 8, 0.76).
narrative_ontology:measurement(fair_be_t16, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 16, 0.81).
narrative_ontology:measurement(fair_be_t24, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 24, 0.86).
narrative_ontology:measurement(fair_be_t32, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 32, 0.88).
narrative_ontology:measurement(fair_be_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(fair_su_t8, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 8, 0.71).
narrative_ontology:measurement(fair_su_t16, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(fair_su_t24, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(fair_su_t32, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(fair_su_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__market_licensing_reading, 0.22).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, licensing_market_efficiency_hypothesis).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_absolutism_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fair use statutory exception kernel. Three distinct constraints inhabit this kernel: market_licensing_reading (THIS), transformative_right_reading (fair use as cultural production right), and narrow_defense_reading (fair use as narrow affirmative defense to property). Each reading has structurally different ε, beneficiary/victim sets, and classifications. The readings coexist across institutional factions and no single framework currently adjudicates them. All three are linked in network.affects_constraints to signal the constraint family decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
