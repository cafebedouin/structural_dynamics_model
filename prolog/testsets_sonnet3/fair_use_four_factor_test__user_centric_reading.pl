% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use as Affirmative User Right (User-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story instantiates the user-centric reading of the fair use
 *   four-factor test kernel: fair use as an affirmative right belonging to
 *   the public, weighed to preserve access and cultural production rather
 *   than as a narrow carve-out from an otherwise absolute property right. On
 *   this reading, courts applying 17 U.S.C. §107 treat purpose-and-character,
 *   nature-of-the-work, amount-used, and market-effect as factors to be read
 *   generously toward educators, libraries, critics, and follow-on creators,
 *   with market harm to the rights holder subordinated to the public-access
 *   rationale. Extraction from rights holders is low but non-zero: some
 *   licensing revenue that would otherwise flow does not, and some control
 *   over derivative and educational uses is lost. This is a distinct
 *   constraint from the creator_centric_reading (which authors fair use as a
 *   narrow, disfavored exception with high deference to creator incentives
 *   and correspondingly higher creator-protective epsilon on the same
 *   underlying doctrine) and from the transformative_use_reading (which makes
 *   transformativeness itself the load-bearing factor, subordinating market
 *   harm specifically to novelty of meaning rather than to public access per
 *   se). Each reading is its own constraint with its own epsilon; this file
 *   authors only the user-centric one.
 *
 * KEY AGENTS:
 *   - educators_and_students: primary beneficiary (powerless/constrained) — gains access without licensing cost
 *   - libraries_and_archives: primary beneficiary (organized/constrained) — preservation and access mission protected
 *   - rights_holders: primary target (powerful/constrained) — bears foregone licensing revenue and lost control
 *   - licensing_intermediaries: secondary target (organized/constrained) — bears shrunk transaction volume
 *   - courts_and_doctrine_administrators: agenda-setter (institutional/analytical) — administers the weighting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.18).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.22).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use as Affirmative User Right (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '34034aa4-e625-4941-bca2-caf355ef7708').
narrative_ontology:cs_kernel_codification('34034aa4-e625-4941-bca2-caf355ef7708', formalized).
narrative_ontology:cs_authority_grounding('34034aa4-e625-4941-bca2-caf355ef7708', practice).
narrative_ontology:cs_interpretation_layer_present('34034aa4-e625-4941-bca2-caf355ef7708').
narrative_ontology:cs_reading_relation('34034aa4-e625-4941-bca2-caf355ef7708', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('34034aa4-e625-4941-bca2-caf355ef7708', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('34034aa4-e625-4941-bca2-caf355ef7708', foundational, public_access_is_the_governing_value).
narrative_ontology:cs_axiom_status(public_access_is_the_governing_value, holdable).
narrative_ontology:cs_axiom_grounding('34034aa4-e625-4941-bca2-caf355ef7708', public_access_is_the_governing_value, instrumental).
narrative_ontology:cs_axiom('34034aa4-e625-4941-bca2-caf355ef7708', secondary, market_harm_subordinate_to_access_rationale).
narrative_ontology:cs_axiom_status(market_harm_subordinate_to_access_rationale, holdable).
narrative_ontology:cs_axiom_grounding('34034aa4-e625-4941-bca2-caf355ef7708', market_harm_subordinate_to_access_rationale, conventional).
narrative_ontology:cs_reference_frame('34034aa4-e625-4941-bca2-caf355ef7708', equitable_public_purpose_doctrine).
narrative_ontology:cs_drift_state('34034aa4-e625-4941-bca2-caf355ef7708', post_campbell_transformative_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34034aa4-e625-4941-bca2-caf355ef7708', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educators_and_students).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, libraries_and_archives).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, documentarians_and_critics).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, follow_on_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, the_public_domain_of_discourse).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, licensing_intermediaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, commercial_publishers_and_content_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Quote, excerpt, and reproduce copyrighted material for teaching and scholarship. Under this reading, fair use is not a grudging exception but an affirmative right that presumptively favors their access; without it they would need to license every excerpt, which most classrooms and students cannot afford. Their exit from the copyright system entirely is not available — they need the material itself, not a substitute.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educators_and_students, beneficiary,
    powerless, generational, constrained, national).

% Preserve, digitize, and lend copyrighted works, including orphan works and out-of-print material rights holders no longer service. This reading treats their preservation and access mission as a primary purpose the four factors exist to protect, not an incidental byproduct of a property right.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, libraries_and_archives, beneficiary,
    organized, civilizational, constrained, national).

% Use clips, images, and quotations to criticize, comment, and document. They generally lack the capital to clear rights on every included fragment; under this reading the four-factor test is weighted to preserve their capacity to speak about culture using culture, treating denial of access as a public harm, not merely a private licensing failure.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, documentarians_and_critics, beneficiary,
    moderate, biographical, constrained, global).

% Build new works — parody, remix, fan work, sampling — using existing material as raw input. This reading treats their downstream production as part of the cultural commons fair use exists to keep open, independent of whether their specific use is judged sufficiently transformative.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, follow_on_creators, beneficiary,
    moderate, generational, constrained, global).

% Hold the underlying copyright and would otherwise license or refuse each use. Under this reading, courts weigh the four factors with a thumb on the scale toward the user's access claim, which means rights holders lose licensing revenue and the ability to control derivative uses even where the use is commercially substitutive at the margins. Their exit is litigation, which is expensive and outcome-uncertain under a user-favoring doctrine.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, rights_holders, payer,
    powerful, biographical, constrained, global).

% Collect and administer licensing fees (collecting societies, stock-image and clip licensors, textbook permission services). A user-centric fair use doctrine shrinks the transactions they exist to broker; every use classified as fair is a transaction that does not happen and revenue that does not flow through them.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, licensing_intermediaries, payer,
    organized, biographical, constrained, national).

% Apply the four statutory factors case by case. Under this reading they are instructed (by a body of user-protective precedent and scholarship) to weigh purpose, nature, amount, and market effect with public access and cultural production as the governing value, not merely as one factor among four neutral considerations.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts_and_doctrine_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Both license content and rely on fair use defenses for their own aggregation, indexing, and hosting activities. They observe how the doctrine's center of gravity shifts and adjust licensing terms and platform policy accordingly; unlike individual rights holders, they can absorb doctrinal risk through diversified revenue and contract design.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, commercial_publishers_and_content_platforms, observer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, commercial_publishers_and_content_platforms, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__user_centric_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__user_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fair use, on this reading, coordinates access to copyrighted material for teaching, criticism, preservation, and follow-on creation without requiring every user to negotiate a license for every use — solving the transaction-cost problem that would otherwise make broad swaths of education, scholarship, and commentary practically impossible.
% TRANSFER_FUNCTION: Moves the economic value of certain uses from rights holders and licensing intermediaries (who forgo fees they would otherwise collect) to public and educational users (who obtain access without payment), justified by the claim that unpriced access to culture is itself the public good the doctrine protects.
% ABSENT_VOICES: Individual authors and smaller rights holders who cannot afford to litigate a fair-use defense against a well-resourced user (a university, a platform, a documentary studio) are functionally absent from the case law that sets the doctrine's user-favoring center of gravity — the precedents are disproportionately set by disputes involving well-funded parties on both sides, or well-funded users against under-resourced creators.
% DISAPPEARANCE_RATIONALE: If this reading of fair use disappeared and courts reverted to a narrow, creator-protective default, licensing markets for excerpts, clips, and quotations would thicken substantially, transaction costs for education and criticism would rise sharply, and much current teaching, scholarship, and commentary practice would need to either pay, seek permission, or cease — the public-access infrastructure this reading protects would visibly need rebuilding through negotiated licenses or legislative carve-outs.
% FOUNDING_PROBLEM: Copyright's exclusive rights, applied literally, would make ordinary acts of teaching, criticism, scholarship, and preservation infringing unless separately licensed — an outcome understood since the doctrine's equitable origins to be inconsistent with copyright's own constitutional purpose of promoting knowledge and the arts.
% FOUNDING_PROBLEM_CORROBORATION: Library associations, educational institutions, and public-interest legal scholars outside the beneficiary classes' direct economic interest (First Amendment and information-access scholars, some judges in published opinions) attest the founding problem remains live — transaction costs still make case-by-case licensing infeasible at scale. Rights holder organizations and licensing collecting societies attest the problem has been substantially addressed by modern licensing infrastructure (collective licensing, digital rights management, streaming deals) and that the user-centric reading now operates well beyond the doctrine's founding rationale, functioning as a de facto compulsory license without compensation.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because the user-centric reading's own account of itself treats the constraint as coordination — solving a genuine transaction-cost problem that would otherwise foreclose teaching, criticism, and preservation — with the cost to rights holders framed as the acceptable price of that public good, not as extraction from them. Suppression is low-moderate (0.22): the doctrine does not suppress rights holders' underlying copyright, only their ability to fully monetize every marginal use, and rights holders retain litigation as a real (if costly) recourse. Accessibility collapse is moderate-low (0.25): rights holders still have licensing markets, statutory remedies, and legislative recourse — alternatives have not collapsed, they have been narrowed. Resistance is moderate-high (0.55) because rights holder organizations and licensing intermediaries actively litigate and lobby against the user-favoring weighting; this is a live, contested doctrine, not settled law. Theater ratio stays low across the interval (0.08→0.15) because the four-factor analysis, on this reading, does substantive work case by case rather than performing a predetermined outcome — though the modest rise reflects growing scholarly critique that some transformative-use findings have become formulaic recitations rather than genuine factor-weighing.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (educators, libraries, critics, follow-on creators), this reading looks like genuine coordination — a right that lets cultural and educational work proceed without transaction-cost paralysis. From the rights holder and licensing intermediary seats, the identical doctrinal weighting looks like an enforced transfer dressed in public-interest language, especially where the 'use' in question is functionally substitutive for a licensable product. The engine computes these as different effective extraction levels from the same structural data (beneficiary/victim declarations + exit options), not from any claim this story makes about which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (educators, libraries, documentarians, follow-on creators) are declared as such because the user-centric reading exists specifically to protect their access; they get low d and correspondingly low/negative effective extraction — the constraint subsidizes their activity. Rights holders and licensing intermediaries are declared victims because the same weighting that protects users' access directly reduces what rights holders can license and collect; they get high d. Courts are the agenda-setter/administrator, not a beneficiary or victim — they run the balancing test but do not collect from it. Commercial publishers and platforms are given a dual observer/payer role because they experience the doctrine from both sides depending on transaction: they license out and also invoke fair use defensively, and their mobile exit options (diversified revenue, ability to negotiate around doctrine through contract) distinguish them structurally from individual rights holders with constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — literal copyright infringement liability for ordinary teaching, criticism, and preservation — is contested rather than resolved: licensing infrastructure has matured substantially since fair use's equitable origins, which is exactly the fact pattern that could support a claim the doctrine has drifted from solving a real problem into functioning as an uncompensated transfer. Classifying this as rope rather than tangled_rope or snare reflects the reading's own assessment that the coordination function (enabling access at viable transaction cost) remains real and dominant, not merely a cover story — but the founding_problem_status of 'contested' and the corroboration split (public-interest scholars vs. rights holder organizations) is exactly the structural marker that should keep this classification open to revision rather than settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    user_right_vs_narrow_exception_framing,
    'Is fair use structurally an affirmative user right that copyright''s exclusivity must yield to, or a narrow judicially-tolerated exception to a property right that remains the default rule?',
    'Doctrinal and historical analysis of whether courts, post-Campbell v. Acuff-Rose and its progeny, have shifted the presumptive burden of the four-factor analysis toward users; comparative analysis of case outcomes before and after the transformative-use era; legislative history of the 1976 Act''s codification of the equitable doctrine.',
    'If the user-right framing is correct, this reading''s low epsilon accurately describes the doctrine''s actual operation. If the narrow-exception framing is correct, this story is describing an aspirational or minority position rather than the doctrine''s actual center of gravity, and the creator_centric_reading''s higher epsilon is closer to the operative reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_right_vs_narrow_exception_framing, conceptual, 'Whether fair use is structurally a right or an exception — the core interpretive fork this reading takes one side of.').

omega_variable(
    licensing_market_maturation_ambiguity,
    'Has licensing infrastructure (collective licensing societies, digital permissions services, streaming and clip-licensing platforms) matured enough that the original transaction-cost justification for user-favoring fair use has substantially weakened, making the founding problem partially dead?',
    'Empirical study of licensing transaction costs and availability across educational, documentary, and archival use cases before and after 2000; comparison of markets where licensing infrastructure exists (music sampling clearance) versus markets where it remains thin (individual scholarly excerpting).',
    'If licensing markets have matured broadly, continued user-favoring weighting looks less like solving a live transaction-cost problem and more like an entrenched transfer — supporting reclassification toward tangled_rope or piton. If licensing markets remain thin outside a few well-organized sectors, the founding problem remains substantially live and the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_market_maturation_ambiguity, empirical, 'Whether licensing market maturation has eroded the transaction-cost rationale underlying the user-centric reading.').

omega_variable(
    sibling_reading_convergence_pressure,
    'Does sustained doctrinal dominance of the transformative_use_reading (which this reading treats as a subordinate consideration but which has independently become load-bearing in circuit case law) functionally absorb or hollow out the distinct public-access rationale this reading claims, such that the two readings are converging in practice even though they remain conceptually distinct?',
    'Track whether case outcomes classified as ''fair'' increasingly cite transformativeness as the dispositive factor even in cases with strong independent public-access rationales (education, archives), which would suggest the transformative_use_reading is displacing this reading''s distinct doctrinal logic rather than coexisting with it.',
    'If convergence is occurring, this reading''s coordination_function description may be describing a rationale that courts invoke rhetorically while actually deciding on transformativeness grounds — a mismatch between stated and operative doctrine that would not change this story''s epsilon but would affect how much independent structural weight the user-centric reading actually carries in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_convergence_pressure, empirical, 'Whether the user-centric and transformative-use readings are converging in practice despite remaining analytically distinct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__user_centric_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(fair_tr_t8, fair_use_four_factor_test__user_centric_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(fair_tr_t16, fair_use_four_factor_test__user_centric_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(fair_tr_t24, fair_use_four_factor_test__user_centric_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement(fair_tr_t32, fair_use_four_factor_test__user_centric_reading, theater_ratio, 32, 0.13).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__user_centric_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(fair_be_t8, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement(fair_be_t16, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 16, 0.14).
narrative_ontology:measurement(fair_be_t24, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement(fair_be_t32, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fair_use_four_factor_test__user_centric_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__user_centric_reading, 0.08).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, transformative_use_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the fair_use_four_factor_test kernel, each with its own epsilon, beneficiary/victim structure, and classification: user_centric_reading (this file, rope, epsilon=0.18, beneficiaries=public/educational users), creator_centric_reading (expected higher epsilon on unauthorized use, rights holders as primary protected party), and transformative_use_reading (transformativeness as dispositive factor, market harm subordinated specifically to novelty rather than to public access broadly). The three are not the same constraint measured three ways — they are three structurally distinct doctrinal postures toward the same statutory text, linked here so contamination/coupling analysis can trace how a shift in one reading's dominance in case law creates pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
