% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Exception (Market Licensing Reading)
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint represents the 'market licensing' reading of the fair use
 *   statutory exception in intellectual property law. Under this
 *   interpretation, any use of copyrighted material that could potentially be
 *   licensed is deemed to harm the market for licensed uses, thereby
 *   precluding a fair use defense. Fair use is thus restricted to only those
 *   situations where no existing or potential market for licensing can be
 *   identified. This reading effectively expands the scope of copyright
 *   holders' control and significantly narrows the public's ability to reuse
 *   and transform copyrighted works.
 *
 * KEY AGENTS:
 *   - copyright_holders: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - licensing_agencies: Beneficiary (organized/mobile)
 *   - creators_of_derivative_works: Primary target (moderate/constrained)
 *   - educators: Target (powerless/constrained)
 *   - researchers: Target (moderate/constrained)
 *   - public_domain_advocates: Excluded (organized/trapped)
 *   - courts: Agenda_setter (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.9).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.85).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Exception (Market Licensing Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, 'a070c459-56d2-4f68-9524-3ea203f1c8d6').
narrative_ontology:cs_kernel_codification('a070c459-56d2-4f68-9524-3ea203f1c8d6', fixed_text).
narrative_ontology:cs_authority_grounding('a070c459-56d2-4f68-9524-3ea203f1c8d6', lineage).
narrative_ontology:cs_interpretation_layer_present('a070c459-56d2-4f68-9524-3ea203f1c8d6').
narrative_ontology:cs_reading_relation('a070c459-56d2-4f68-9524-3ea203f1c8d6', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('a070c459-56d2-4f68-9524-3ea203f1c8d6', fair_use_statutory_exception__narrow_defense_reading, influences).
narrative_ontology:cs_axiom('a070c459-56d2-4f68-9524-3ea203f1c8d6', foundational, any_potential_market_harm_precludes_fair_use).
narrative_ontology:cs_axiom_status(any_potential_market_harm_precludes_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('a070c459-56d2-4f68-9524-3ea203f1c8d6', any_potential_market_harm_precludes_fair_use, conventional).
narrative_ontology:cs_axiom('a070c459-56d2-4f68-9524-3ea203f1c8d6', foundational, copyright_holder_monetization_is_paramount).
narrative_ontology:cs_axiom_status(copyright_holder_monetization_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('a070c459-56d2-4f68-9524-3ea203f1c8d6', copyright_holder_monetization_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('a070c459-56d2-4f68-9524-3ea203f1c8d6', expansive_copyright_monetization).
narrative_ontology:cs_drift_state('a070c459-56d2-4f68-9524-3ea203f1c8d6', contemporary_digital_economy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a070c459-56d2-4f68-9524-3ea203f1c8d6', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_agencies).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, creators_of_derivative_works).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, researchers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and benefit from the broadest possible interpretation of copyright protection, asserting a right to license any potential use. They actively enforce this interpretation through litigation and lobbying, seeking to expand the scope of monetizable uses and minimize fair use exceptions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Profit from facilitating licensing transactions. Their business model thrives when more uses require licenses, aligning their interests with the market licensing reading of fair use. They provide the infrastructure for copyright holders to monetize potential uses.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_agencies, beneficiary,
    organized, biographical, mobile, global).

% Bear the costs of licensing or the risk of infringement lawsuits under this reading. Their ability to create new works based on existing material is severely curtailed, as almost any use could be deemed to harm a potential market. Exit means abandoning creative projects or operating in niche, unmonetizable areas.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, creators_of_derivative_works, payer,
    moderate, biographical, constrained, global).

% Face significant barriers to using copyrighted materials for teaching and scholarship. The expansive market harm interpretation forces them to seek licenses for uses previously considered fair, increasing costs and administrative burden. Exit means reducing access to diverse materials or risking legal action.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educators, payer,
    powerless, biographical, constrained, national).

% Are restricted in their ability to analyze and build upon existing data, texts, and media. The market licensing reading makes data mining, text analysis, and other computational uses potentially infringing, hindering academic progress. Exit means limiting research scope or relying on less comprehensive, licensed datasets.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, researchers, payer,
    moderate, biographical, constrained, global).

% Argue for a robust public domain and broad fair use exceptions to foster creativity and access to knowledge. This reading of fair use directly undermines their goals by expanding private control over information and shrinking the commons. They are excluded from the interpretive process that shapes this reading.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates, excluded,
    organized, generational, trapped, global).

% Are tasked with interpreting fair use statute. Under this reading, they are pressured to prioritize the potential market for licensed uses, often leading to rulings that narrow the scope of fair use and favor copyright holders. Their interpretive power is a key mechanism for the constraint's persistence.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__market_licensing_reading, copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit restrictive, framework for copyright holders to monetize their works by establishing a strong presumption against unlicensed uses that could be licensed.
% TRANSFER_FUNCTION: Transfers potential revenue from creators of derivative works, educators, and researchers to copyright holders and licensing agencies by requiring licenses for a broad range of uses.
% ABSENT_VOICES: Public domain advocates, open access proponents, and many independent creators are largely excluded from the legal and lobbying processes that entrench this reading. They would argue for a more balanced approach that prioritizes public benefit and transformative use.
% DISAPPEARANCE_RATIONALE: If this interpretation of fair use vanished, the market for licensed uses would contract significantly, leading to a surge in transformative and educational uses without prior permission. Copyright holders would need to adapt their business models, and the creative economy would rebalance towards greater reuse and innovation.
% FOUNDING_PROBLEM: The original fair use doctrine aimed to balance copyright protection with public interest in promoting science and the useful arts, preventing copyright from stifling creativity.
% FOUNDING_PROBLEM_CORROBORATION: Many legal scholars, public interest groups, and creators attest that the original problem of balancing interests is now distorted, with copyright holders' interests overwhelmingly prioritized. Independent economic analyses often show that this reading stifles innovation and cultural production, contradicting the founding intent. Copyright holders, however, maintain that the problem of protecting their investments remains live.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is extremely high (0.9) because this reading effectively converts nearly all potential uses into monetizable opportunities for copyright holders, extracting value from a vast array of creative and educational activities. Suppression is also very high (0.85) as it actively suppresses alternative interpretations of fair use and chills creative reuse through the threat of litigation. The theater ratio is low (0.1) because the enforcement is direct and functional, aimed at securing licensing revenue rather than merely performing a function. Accessibility collapse is high (0.95) as this reading makes it nearly impossible to find a use that doesn't 'harm' a potential market, effectively collapsing alternatives to licensing. Resistance is high (0.7) due to ongoing legal challenges and advocacy from affected parties.
 *
 * PERSPECTIVAL GAP:
 *   Copyright holders and licensing agencies perceive this as a legitimate and necessary framework for protecting intellectual property and fostering creative industries. Conversely, creators, educators, and researchers experience it as an extractive snare that stifles innovation and access to knowledge. Courts, as agenda-setters, mediate these conflicting perspectives, often leaning towards the market-centric view due to lobbying and established legal precedents.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and licensing agencies are clear beneficiaries, as the constraint directly enables their revenue streams and expands their market. Creators, educators, and researchers are clear targets, bearing the costs of licensing or the chilling effect on their work. Public domain advocates are excluded, as their arguments for broader fair use are systematically marginalized by this interpretation. Courts, while technically neutral arbiters, act as agenda-setters by upholding and reinforcing this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of fair use has arguably undergone mandatrophy, where its original mandate to balance copyright with public interest has atrophied, replaced by a de facto mandate to maximize market control for copyright holders. The classification as a snare highlights this shift, preventing it from being mislabeled as a legitimate coordination mechanism (rope) or a temporary support (scaffold) for the creative economy. The high extractiveness and suppression, coupled with the 'dead' status of the founding problem, indicate a system that persists primarily through coercion and rent-seeking, rather than genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_market_definition,
    'How broadly should ''potential market for licensed uses'' be defined? Does it include hypothetical markets that do not yet exist?',
    'Legislative clarification or Supreme Court ruling establishing clear boundaries for ''potential market'' in fair use analysis.',
    'A narrow definition would expand fair use, reducing extractiveness and suppression. A broad definition (as currently interpreted) maintains high extractiveness and suppression, reinforcing the snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_market_definition, conceptual, 'Ambiguity in defining the scope of ''potential market'' for fair use.').

omega_variable(
    transformative_use_precedence,
    'Should transformative use be given greater weight in fair use analysis, even if a market for licensing exists?',
    'Judicial precedent that explicitly prioritizes transformative use over market harm in certain contexts, or legislative amendment to the fair use statute.',
    'If transformative use gains precedence, the constraint''s extractiveness and suppression would decrease, potentially shifting its classification towards a tangled rope or even a rope. If market harm continues to dominate, the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_precedence, preference, 'The relative weight of transformative use versus market harm in fair use analysis.').

omega_variable(
    chilling_effect_quantification,
    'What is the quantifiable chilling effect of this reading on new creative works, education, and research?',
    'Empirical studies measuring the decline in derivative works, educational resource diversity, and research output due to licensing costs or legal fears.',
    'Strong empirical evidence of a significant chilling effect would strengthen arguments for legislative reform or judicial reinterpretation, potentially reducing the constraint''s suppression and extractiveness. Lack of such evidence would allow the current interpretation to persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'Quantifying the negative impact of restrictive fair use on creative and academic output.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fair_be_t1980, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1980, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, digital_rights_management_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'fair_use_statutory_exception' kernel. Its high extractiveness and suppression contrast sharply with the 'transformative_right_reading' (a claimed rope) and the 'narrow_defense_reading' (a claimed tangled_rope), which emphasize public benefit and limited exceptions, respectively. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
