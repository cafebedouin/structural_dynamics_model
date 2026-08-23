% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Instrumentalist Orthographic Legitimacy Criterion (Literacy-Efficiency Reading)
 *   domain: political linguistics / state formation
 *
 * SUMMARY:
 *   A state adopts a Latin-derived alphabet adapted to its language's
 *   phonology, replaces public text within a statutory deadline, and
 *   thereafter adjudicates all orthographic policy by one test: which
 *   arrangement maximizes literacy rates and administrative efficiency. This
 *   file authors ONLY the instrumentalist reading of that legitimacy question
 *   as a single epsilon-invariant constraint. The epsilon referent is the
 *   standing arrangement under contest — the script regime selected and
 *   maintained under the efficiency criterion — assessed by this reading's
 *   own lights; it is NOT the continuity reading's tradition-preserving
 *   alternative or the modernist reading's civilizational-rupture
 *   alternative, which instantiate separate constraints with their own
 *   epsilon, victim sets, and classifications. The three readings form a
 *   constraint family linked through network.affects_constraints; their
 *   epsilon values differ because they license different arrangements from
 *   the same kernel, not because one observable is being measured two ways.
 *
 * KEY AGENTS:
 *   - republican_state_authorities: agenda setter (institutional/arbitrage) — selects and certifies the script by statistical test
 *   - newly_literate_citizens: primary beneficiary (moderate/constrained) — receives the acquisition-speed dividend
 *   - state_administrative_apparatus: beneficiary and enforcement executor (institutional/mobile) — realizes and administers the efficiency dividend
 *   - arabic_script_literate_elite: primary target (organized/identity_locked) — vocational and religious capital stranded by decree
 *   - pre_reform_educated_generation: secondary target (moderate/constrained) — bears one-time retraining cost
 *   - arabic_calligraphy_manuscript_artisans: paying excluded party (organized/trapped) — craft market extinguished without a seat
 *   - rural_women_learners: excluded party (powerless/trapped) — distributional failure hidden inside the aggregate statistic
 *   - literacy_policy_analysts: analytical observer — sees dividend, confiscation, and aggregation choices together
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.46).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.28).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Instrumentalist Orthographic Legitimacy Criterion (Literacy-Efficiency Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political linguistics / state formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, '05eada46-ec53-438b-be69-0af473559d98').
narrative_ontology:cs_kernel_codification('05eada46-ec53-438b-be69-0af473559d98', formalized).
narrative_ontology:cs_authority_grounding('05eada46-ec53-438b-be69-0af473559d98', expertise).
narrative_ontology:cs_interpretation_layer_present('05eada46-ec53-438b-be69-0af473559d98').
narrative_ontology:cs_reading_relation('05eada46-ec53-438b-be69-0af473559d98', orthographic_legitimacy_kernel__modernist_reading, influences).
narrative_ontology:cs_reading_relation('05eada46-ec53-438b-be69-0af473559d98', orthographic_legitimacy_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_axiom('05eada46-ec53-438b-be69-0af473559d98', foundational, orthographic_value_is_measurable_outcome).
narrative_ontology:cs_axiom_status(orthographic_value_is_measurable_outcome, holdable).
narrative_ontology:cs_axiom_grounding('05eada46-ec53-438b-be69-0af473559d98', orthographic_value_is_measurable_outcome, empirically_contingent).
narrative_ontology:cs_axiom('05eada46-ec53-438b-be69-0af473559d98', secondary, tradition_carries_no_legitimacy_weight).
narrative_ontology:cs_axiom_status(tradition_carries_no_legitimacy_weight, holdable).
narrative_ontology:cs_axiom_grounding('05eada46-ec53-438b-be69-0af473559d98', tradition_carries_no_legitimacy_weight, deontological).
narrative_ontology:cs_reference_frame('05eada46-ec53-438b-be69-0af473559d98', measured_utility_script_selection).
narrative_ontology:cs_drift_state('05eada46-ec53-438b-be69-0af473559d98', post_universal_literacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('05eada46-ec53-438b-be69-0af473559d98', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_citizens).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_script_literate_elite).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, pre_reform_educated_generation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_calligraphy_manuscript_artisans).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and enacted the alphabet law, set the compliance deadline for public signage and publishing, commissioned the literacy campaigns, and control the statistics by which the script's performance is judged. They chose the script and retain the ability to amend or extend orthographic policy; no external actor can impose a script on them.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, republican_state_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Cohorts schooled after the reform acquired reading and writing in months rather than the years the old system demanded. They receive schooling, newspapers, bureaucratic forms, and civic participation in a script matched to their spoken language. They cannot opt out of the literacy regime, but few have reason to want to.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_citizens, beneficiary,
    moderate, generational, constrained, national).

% Runs the censuses, tax rolls, conscription records, and provincial school systems through which the efficiency dividend is realized, and staffed the compulsory evening literacy courses. It collects cheaper, faster, more uniform documentation and also executes the enforcement that produced it.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, agenda_setter).

% Ulema, Ottoman-trained officials, poets, jurists, and teachers whose decades of training in Arabic-script composition and religious text constituted their professional standing and, for many, their religious vocation. Within a single season their skill ceased to function in public life; retraining meant starting over as beginners in middle age. Leaving means abandoning the identity the skill anchored.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_script_literate_elite, payer,
    organized, biographical, identity_locked, national).

% Adults literate in the old script at the moment of the switch, outside the elite. They attended mandatory night classes after working days or lapsed into functional illiteracy vis-a-vis public text. Their loss was transitional rather than vocational, but it fell on precisely the years they had already paid to become literate once.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, pre_reform_educated_generation, payer,
    moderate, biographical, constrained, national).

% Calligraphers, illuminators, and manuscript copyists whose guild economy rested on demand for the old script. Their craft market contracted to private devotion and museum preservation within a decade. They held no seat in the assembly debate and their objection was aesthetic and livelihood-based rather than statistical, which the efficiency criterion had no slot for.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_calligraphy_manuscript_artisans, payer,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_calligraphy_manuscript_artisans, excluded).

% Village women, the group aggregate literacy statistics showed lagging by decades after urban men. The campaigns reached them last and thinnest. Had the efficiency criterion been applied distributionally rather than in aggregate, their exclusion from early rounds would have registered as a failure of the very metric doing the legitimating; nobody asked them.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, rural_women_learners, excluded,
    powerless, generational, trapped, national).

% Demographers, education historians, and comparative linguists who reconstruct acquisition timelines, audit the literacy series, and compare the Turkish transition with other states' script choices. They see the full structure: the dividend, the confiscated capital, and the aggregation choices inside the headline numbers.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, literacy_policy_analysts, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single standardized, phonetically adapted script lets the state publish laws, schoolbooks, forms, and newspapers in one system that most speakers of the language can learn in months instead of years, replacing a script inheritance split between an Arabic literary line and a thin administrative literacy.
% TRANSFER_FUNCTION: Moves usable cultural capital and institutional authority away from holders of Arabic-script literacy (clergy, Ottoman-educated officials, calligraphers) toward the state's new administrative class and each successive school cohort; moves the one-time retraining burden onto existing adult readers; moves the recurring efficiency dividend to the administration.
% ABSENT_VOICES: The calligrapher and manuscript-artisan guilds and the madrasa teaching corps objected but were never seated in the deliberation that fixed the criterion; rural women, whom the aggregate literacy number long concealed, were not consulted on whose literacy was being maximized or in what order.
% DISAPPEARANCE_RATIONALE: Archives, school curricula, print trades, signage conventions, and the state's documentary infrastructure are all organized around the script the criterion selected and continues to certify. Remove the criterion-governance arrangement overnight and script policy loses its adjudicator: revision proposals, dual-script demands, and heritage-access programs would reopen the question the arrangement currently settles by default.
% FOUNDING_PROBLEM: Mass illiteracy (roughly nine in ten adults) under a script whose Arabic consonantal skeleton represented Turkish vowels poorly, making administration, conscription, taxation, and civic communication slow, error-prone, and dependent on a small scribal intermediary class.
% FOUNDING_PROBLEM_CORROBORATION: Foreign diplomatic and League-of-Nations-era education reporting independently corroborates the severity of the original literacy baseline, so the founding crisis is not self-asserted. Whether the problem is still live is disputed from outside the benefiting parties: literacy historiography holds the acquisition problem substantially solved, while comparative politics of later Latinization programs treats the criterion as reusable; the surviving Arabic-script scholarly community attests that the criterion answered a real problem by dispossessing them, and attests nothing in favor of its continuing force.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate and decaying (0.58 at enactment falling to 0.46): the arrangement's harm concentrates in a one-time confiscation of accumulated human capital plus a persistent heritage-access barrier, while the coordination dividend is real and compounding. Suppression at interval end is 0.28 because the enforcement arc is genuinely falling — signage deadlines, compulsory evening courses, and restrictions on old-script publishing were intensive early and decayed as the new script became self-sustaining habit; the suppression_requirement series tracks that enforcement-capacity decline explicitly. Theater ratio rises slowly from 0.12 to 0.24: the campaigns taught real people to read, but as the function completed, commemorative activity (anniversary rites, museums of the old letters, ceremonial invocations of the literacy statistics) grew as a share of activity — mild proxy drift worth watching, not yet Goodhart dominance. Accessibility collapse is 0.55: public and print alternatives to the new regime closed almost completely, but private correspondence and devotional use of the old script survived, so exit into purely private practice persisted. Resistance is 0.42: elite protest existed but was structurally weakened — the abolition of the caliphate two years earlier had removed the elite's strongest coalition anchor — while popular resistance was thin because the dividend was broadly felt. All three tracked series run on one shared time grid (points 0, 6, 12, 18, 24, 30 of a 30-year interval mapping 1928-1958), with end-state base_properties scalars matching the terminal measurements.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the agenda-setter seat the arrangement is a coordination triumph that its own statistics vindicate; from the identity-locked payer seat the identical structure is expropriation — a decree converted thirty years of training into zero public value within a season; from the excluded rural-women seat the celebrated aggregate literacy curve is itself part of the harm, since averaging is what hid who was left out. Same nominal event, three different constraint experiences; the engine derives this divergence from the declared roles, power atoms, and exit options, not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Newly literate citizens sit near the beneficiary pole: the constraint subsidizes their acquisition speed, though their constrained exit keeps them from the arbitrage end. The administrative apparatus sits near-beneficiary with a twist — it also executes enforcement, which is why it carries a secondary agenda-setter role; the derivation handles this from the dual declaration without an override. The Arabic-script elite is the full-target seat: payer role, identity-locked exit (their fused vocational-religious identity is exactly what was devalued), organized but coalition-poor. The pre-reform generation is a high-d payer with constrained rather than locked exit — their loss was transitional. No directionality overrides are needed: beneficiary/victim declarations plus exit atoms reproduce the structural relationships accurately, so the override chain is left untouched.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against both symmetrical mislabels. Calling this a rope erases the identifiable victims — an elite expropriated mid-career and a craft tradition extinguished — which the canonical classifier refuses to permit without declared payers and active enforcement. Calling it a snare erases the genuine, large, durable coordination dividend that made compliance broadly attractive and enforcement progressively unnecessary. The temporal series matters here: falling extraction and falling suppression with rising theater is the signature of an arrangement whose extraction component is a fading transition cost riding on a persisting coordination core, not an accumulating rent. On the R5 seam: the founding problem's status is authored contested rather than dead, because whether the problem (settling script legitimacy for a mass polity) recurs with each technology shift is precisely what the parties dispute — a dead-status authorship here would manufacture a capture flag the structure does not support, since no seat captures gains covertly; the apparatus's collection is open, declared, and was the arrangement's announced purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_omega,
    'This constraint is one reading (instrumentalist) of the orthographic_legitimacy_kernel; which reading governs, and what structurally changes if a sibling reading is instantiated instead?',
    'Not resolvable by data inside this story: the disagreement is located in the legitimacy test itself. A continuity_reading instantiation flips the Arabic-literate elite from victim to beneficiary and converts heritage-access loss into the primary harm; a modernist_reading instantiation reframes the same adoption as liberation and shifts extraction assessment onto what the rupture narrative suppresses.',
    'Sibling instantiation changes the beneficiary/victim sets and hence every seat''s derived directionality and the computed type; the efficiency-tested arrangement could compute as anything from rope-flavored tangled_rope to a differently-shaped hybrid depending on which reading''s arrangement is the standing one under evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_omega, conceptual, 'Committer structure: this story is the instrumentalist reading of a contested orthographic-legitimacy kernel; sibling readings are separate constraints, not alternative observables of this one.').

omega_variable(
    aggregate_literacy_masking,
    'Does maximizing the aggregate literacy rate — the reading''s own legitimacy metric — conceal distributional failure among subpopulations (rural women especially) such that the coordination claim is weaker than the headline statistic asserts?',
    'Disaggregate the historical literacy series by region, gender, and cohort; compare campaign resource allocation against population distribution.',
    'If disaggregation reveals systematically deferred groups, the arrangement extracted attention and resources from them under a criterion that certified itself with their absence; effective extraction for excluded seats rises and the coordination-function gate weakens for the story''s later interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_literacy_masking, empirical, 'Whether the legitimating statistic aggregates away the populations it failed.').

omega_variable(
    counterfactual_script_comparison,
    'Was the adopted Latin-derived alphabet actually efficiency-maximal against the live alternatives (a vowel-marked reformed Arabic script, Cyrillic adaptation, or deeper Turkic reform), or did the criterion ratify a decision already taken?',
    'Historical-linguistic reconstruction of contemporaneous commission proposals and acquisition-time estimates, cross-checked against other states'' script transitions with matched baseline literacy.',
    'If comparable alternatives existed, the efficiency criterion functioned as retrospective warrant for a predetermined outcome, moving this reading''s operation closer to the modernist reading in practice and raising measured extraction (warrant manufactured, not measured); if the adopted script was genuinely optimal, the coordination claim stands on its own evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_script_comparison, empirical, 'Counterfactual validity of the efficiency test that legitimates the standing arrangement.').

omega_variable(
    consent_vs_internalization,
    'Did the population acquire positive attachment to the new script (consent consolidating into preference), or does the falling suppression series measure mere habituation — suppression internalized rather than released?',
    'Oral-history and private-correspondence studies tracking old-script private use across generations; persistence of Ottoman-letter devotional and familial writing after enforcement ended.',
    'If consent is real, the falling suppression_requirement trajectory reflects genuine enforcement decay and the arrangement trends toward stable rope-like coordination; if habituation, current suppression is understated — targets carry the constraint internally after enforcement machinery dissolved, and the end-state type leans back toward tangled_rope with higher latent suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_internalization, empirical, 'Structural versus internalized component of the measured suppression decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(orth_tr_t6, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(orth_tr_t12, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(orth_tr_t18, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement(orth_tr_t24, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(orth_tr_t30, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 30, 0.24).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(orth_be_t6, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(orth_be_t12, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(orth_be_t18, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(orth_be_t24, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(orth_be_t30, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 30, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(orth_su_t6, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(orth_su_t12, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(orth_su_t18, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 18, 0.47).
narrative_ontology:measurement(orth_su_t24, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 24, 0.37).
narrative_ontology:measurement(orth_su_t30, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, information_standard).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of 'orthographic legitimacy': the colloquial concept covers three structurally distinct legitimacy tests that yield different arrangements, different beneficiary/victim sets, and different epsilon values from the same historical event. This member (instrumentalist) licenses the efficiency-tested script regime: beneficiaries are new literates and the administration, victims are the Arabic-script elite and transition generation, moderate decaying epsilon. The modernist reading licenses rupture-framed adoption with civilizational narrative doing the legitimating work; the continuity reading licenses tradition-preserving script policy in which the Arabic-literate elite flips from victim to beneficiary and mass acquisition slows. The upstream/downstream pressure runs from this reading outward: its literacy statistics supplied the empirical warrant the modernist program cited and the standard of proof the continuity defense had to answer. Each file links the others; none averages over them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
