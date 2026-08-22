% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Protection Yields to Demonstrable Unconsented Harm (Harm-Limited Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the harm-limited reading of the First Amendment
 *   speech-protection kernel: protection yields once speech causes
 *   demonstrable, unconsented-to harm to an identifiable party. This is a
 *   single reading among three declared readings of the same kernel
 *   (absolutist, categorical-balancing, harm-limited); each reading is
 *   authored as its own constraint with its own epsilon per the
 *   disambiguation rule — this file does not describe the contest between
 *   readings, only the structure of this one. Under this reading the
 *   protected-speech set contracts around a harm boundary that courts and
 *   legislatures administer case-by-case as evidence of harm accumulates.
 *
 * KEY AGENTS:
 *   - targeted_minority_groups: primary beneficiary (moderate/constrained) — gains a redress path absent under absolutist doctrine
 *   - harassment_victims: primary beneficiary (powerless/trapped) — bears the harm the standard exists to remedy
 *   - controversial_speakers: primary target (moderate/constrained) — bears contracted protection and after-the-fact liability exposure
 *   - dissident_political_organizers: secondary target (powerless/trapped) — least resourced to litigate a harm defense
 *   - courts_and_legislatures: agenda_setter (institutional/analytical) — administers and sets the harm threshold
 *   - civil_liberties_organizations: excluded analytical voice — objects to standard's expansibility but not treated as case evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.52).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.58).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Protection Yields to Demonstrable Unconsented Harm (Harm-Limited Reading)").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '384dab30-9c1a-42ba-964e-5b1a2c83f66c').
narrative_ontology:cs_kernel_codification('384dab30-9c1a-42ba-964e-5b1a2c83f66c', fixed_text).
narrative_ontology:cs_authority_grounding('384dab30-9c1a-42ba-964e-5b1a2c83f66c', lineage).
narrative_ontology:cs_interpretation_layer_present('384dab30-9c1a-42ba-964e-5b1a2c83f66c').
narrative_ontology:cs_reading_relation('384dab30-9c1a-42ba-964e-5b1a2c83f66c', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('384dab30-9c1a-42ba-964e-5b1a2c83f66c', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('384dab30-9c1a-42ba-964e-5b1a2c83f66c', foundational, demonstrable_harm_defeats_categorical_immunity).
narrative_ontology:cs_axiom_status(demonstrable_harm_defeats_categorical_immunity, holdable).
narrative_ontology:cs_axiom_grounding('384dab30-9c1a-42ba-964e-5b1a2c83f66c', demonstrable_harm_defeats_categorical_immunity, instrumental).
narrative_ontology:cs_axiom('384dab30-9c1a-42ba-964e-5b1a2c83f66c', secondary, unconsented_injury_generates_regulable_interest).
narrative_ontology:cs_axiom_status(unconsented_injury_generates_regulable_interest, holdable).
narrative_ontology:cs_axiom_grounding('384dab30-9c1a-42ba-964e-5b1a2c83f66c', unconsented_injury_generates_regulable_interest, deontological).
narrative_ontology:cs_reference_frame('384dab30-9c1a-42ba-964e-5b1a2c83f66c', text_as_categorical_command).
narrative_ontology:cs_drift_state('384dab30-9c1a-42ba-964e-5b1a2c83f66c', contemporary_harm_jurisprudence_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('384dab30-9c1a-42ba-964e-5b1a2c83f66c', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, targeted_minority_groups).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, harassment_victims).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, defamation_plaintiffs).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, controversial_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, dissident_political_organizers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, satirists_and_provocateurs).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, harm_principle_as_speech_limit).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, equal_dignity_as_constitutional_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the brunt of speech that the absolutist reading would fully protect — targeted harassment campaigns, group-directed slurs tied to real-world violence, doxxing. Under the harm-limited reading, courts and legislatures can act once harm is demonstrated, giving this group a path to relief that the absolutist framework forecloses entirely.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, targeted_minority_groups, beneficiary,
    moderate, generational, constrained, national).

% Individuals subjected to sustained targeted speech (stalking-adjacent speech, revenge content, coordinated pile-ons) with no practical way to exit the harm short of leaving public life. The harm-limited reading is the only framework under which their injury, once proven, outweighs the speaker's claim.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, harassment_victims, beneficiary,
    powerless, biographical, trapped, local).

% People whose reputations are damaged by demonstrably false statements. They rely on courts applying the harm-limited standard rather than a categorical immunity that would require proving speech falls into a narrow historical exception.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, defamation_plaintiffs, beneficiary,
    moderate, biographical, constrained, national).

% Speakers whose provocative, offensive, or politically extreme statements are chilled or penalized once a harm-showing is possible. They must now calibrate speech against a harm standard whose boundary is set case-by-case after the fact, rather than knowing in advance that only narrow historical categories are unprotected.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% Activists whose inflammatory rhetoric against powerful institutions can be recast as causing psychological or reputational harm to those institutions or their allies. They have the fewest resources to litigate a harm-showing and the most to lose if the standard is applied asymmetrically against dissent rather than incumbents.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, dissident_political_organizers, payer,
    powerless, biographical, trapped, national).

% Comedians, parodists, and cultural critics whose work trades on causing discomfort or offense. The harm-limited reading exposes them to liability whenever offense is reframed as demonstrable psychological or dignitary harm, narrowing the space in which satire has historically operated.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, satirists_and_provocateurs, payer,
    moderate, biographical, constrained, national).

% Administers the harm-limited standard: sets evidentiary thresholds for 'demonstrable' harm, decides which harms count as unconsented-to, and enforces the resulting speech restrictions. Controls the boundary that determines who counts as beneficiary and who as payer in any given case.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Would argue the harm-limited standard is a slow-motion abandonment of categorical protection that will inevitably expand to cover offense-as-harm, but their objections are treated as slippery-slope speculation rather than admitted evidence in individual harm-showing proceedings.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, civil_liberties_organizations, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__harm_limited_reading, diffuse).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for redress when speech causes concrete, provable injury to identifiable people — allowing legal response to harassment, targeted defamation, and incitement-adjacent speech that the absolutist reading would leave entirely unaddressed.
% TRANSFER_FUNCTION: Moves protective legal standing from speakers whose expression is found to cause demonstrable unconsented harm to the people who suffer that harm — shifting liability exposure and chilling effects onto the speaker side of the line once harm is shown.
% ABSENT_VOICES: Civil liberties organizations that view the harm standard as inherently expansible are structurally present in litigation but their systemic slippery-slope argument is treated as speculative rather than as evidence in any single harm-showing case; the aggregate chilling effect across many cases is nobody's burden of proof.
% DISAPPEARANCE_RATIONALE: If the harm-limited standard vanished and only the absolutist reading governed, harassment victims and targeted minorities would lose most current avenues for legal relief against non-defamatory, non-incitement speech; conversely, if it expanded unchecked, controversial and dissident speech would face a case-by-case harm gauntlet with no categorical floor — either direction visibly reorganizes who can speak safely and who can seek redress.
% FOUNDING_PROBLEM: Categorical or absolutist free-speech doctrine left demonstrably injured people — victims of targeted harassment, provable defamation, group-directed intimidation — with no doctrinal path to relief because their harm did not fit pre-existing narrow historical exceptions.
% FOUNDING_PROBLEM_CORROBORATION: Documented in tort law scholarship and harassment-law reform literature written by legal scholars outside the advocacy organizations that benefit from expanded harm standards; also corroborated by absolutist-reading critics who, while opposing the doctrine, do not dispute that some harms fall through the categorical framework's cracks — they dispute the remedy, not the gap's existence.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) reflects that the standard does redistribute real protection away from a genuine victim class of speakers toward a genuine victim class of harm-bearers — this is neither pure coordination nor pure extraction, hence tangled_rope rather than snare or rope. Suppression (0.58) is substantial because the standard requires active case-by-case enforcement (courts weighing harm claims) and produces real chilling effects on speakers who cannot predict in advance where the harm line falls. Theater ratio is comparatively low and rising slowly (0.12 to 0.28) — most of the machinery does real adjudicative work, though a growing share of harm-showing litigation increasingly performs vindication rather than remedying provable injury. Accessibility collapse is moderate (0.42): speakers retain substantial protected space, but the space contracts as harm doctrine accretes precedent. Resistance is high (0.71) because controversial speakers, satirists, and civil liberties organizations actively litigate and organize against harm-standard expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (harassment victims, targeted minorities) the standard reads as overdue coordination — finally providing a remedy path. From the payer seats (controversial speakers, dissident organizers, satirists) the identical doctrinal structure reads as an ever-expanding liability exposure with no predictable boundary. The engine computes these divergent per-seat classifications from the same structural facts; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (targeted minority groups, harassment victims, defamation plaintiffs) sit near the full-beneficiary end of directionality: the constraint exists structurally to give them standing they otherwise lack. Victims (controversial speakers, dissident organizers, satirists) sit near the full-target end: the same doctrinal machinery that grants relief to the beneficiary class extracts protected space from them. Powerless dissident organizers are pushed further toward the target end than moderate-power controversial speakers because they lack litigation resources to contest a harm-showing, despite occupying a structurally similar payer role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — real, uncompensated harm falling through categorical gaps — remains live by the corroboration record, which prevents this from being classified as inertial mandatrophy (a piton). But the accumulating extraction trend (rising base_extractiveness and suppression_requirement over the interval) is exactly the pattern that would eventually justify reclassification toward snare if the harm threshold keeps loosening without a corresponding tightening of evidentiary rigor — the tangled_rope classification is a live diagnosis, not a permanent one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_standard_expansibility,
    'Does the demonstrable-harm threshold have a principled stopping point, or does ''harm'' inevitably expand from physical/reputational injury to include psychological distress and eventually mere offense?',
    'Longitudinal doctrinal tracking of what courts accept as ''demonstrable harm'' across successive cases; a widening evidentiary bar over time (weaker showings accepted) would indicate expansibility rather than a stable principled boundary.',
    'If the standard expands without limit, this reading converges functionally toward the categorical_balancing_reading and eventually toward outcomes the absolutist_reading was designed to prevent — reclassification toward snare becomes likely as suppression rises without a corresponding coordination gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_standard_expansibility, conceptual, 'Whether the harm threshold is a stable boundary or a one-way ratchet.').

omega_variable(
    asymmetric_enforcement_risk,
    'Is the harm-limited standard applied symmetrically across politically powerful and powerless speakers, or does litigation capacity mean only well-resourced harm claimants can invoke it while powerless speakers cannot mount an adequate defense?',
    'Comparative analysis of harm-claim outcomes by claimant and defendant resource level; systematic asymmetry in win rates controlling for harm-showing strength would indicate capture by resourced actors rather than neutral harm remediation.',
    'If enforcement is asymmetric, the beneficiary class functionally narrows to well-resourced claimants (including institutions using harm claims strategically against dissidents), pushing the constraint toward tangled_rope''s more extractive pole or toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_enforcement_risk, empirical, 'Whether harm-standard litigation access is symmetric across claimant power levels.').

omega_variable(
    kernel_reading_boundary_location,
    'Is the disagreement between this reading and categorical_balancing_reading a genuine structural difference (injury-triggered vs. category-triggered regulation) or merely two descriptions of the same adjudicative practice viewed at different grain?',
    'Examine whether courts applying ''harm-limited'' doctrine actually reason from individual injury-showings or from pre-established categories dressed in harm language; divergent case outcomes under the same facts would indicate genuine structural difference.',
    'If the readings collapse into one practice, network linkage and epsilon values across the two sibling stories should be revisited; if genuinely distinct, the current three-way decomposition is warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether harm-limited and categorical-balancing readings are structurally distinct or notational variants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(firs_tr_t8, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(firs_tr_t16, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(firs_tr_t24, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(firs_tr_t32, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(firs_be_t8, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(firs_be_t16, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(firs_be_t24, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(firs_be_t32, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(firs_su_t8, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(firs_su_t16, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(firs_su_t24, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(firs_su_t32, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% Part of a three-member constraint family decomposing the colloquial 'First Amendment protection' concept: absolutist_reading (categorical, near-zero extraction, protection is nearly Mountain-like within its own frame), categorical_balancing_reading (moderate extraction via case-by-case category construction), and this harm_limited_reading (moderate-to-substantial extraction via individualized harm-showing). Each reading is authored as its own constraint with its own stable epsilon per the eps-invariance principle; they are linked here rather than merged because measuring 'First Amendment protection' under each reading yields materially different extraction profiles and different victim/beneficiary sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
