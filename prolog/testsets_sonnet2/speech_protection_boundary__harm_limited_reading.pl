% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Harm-Limited Reading of the Speech Protection Boundary
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the harm-limited reading of the contested
 *   speech-protection kernel: speech protection is conditional on the absence
 *   of significant harm to dignity, equality, and freedom from harassment.
 *   Under this reading the protected set narrows relative to the absolutist
 *   Brandenburg standard, unprotected speech now includes hate speech,
 *   harassment, and coded dog-whistle rhetoric, and the state (via tribunals
 *   or courts) becomes an active gatekeeper determining, case by case,
 *   whether a given utterance crosses the harm threshold. This gatekeeping
 *   function is itself a locus of abuse risk — the same discretion that
 *   protects targeted minorities from dignitary harm can be turned against
 *   unpopular political speech under an expansive reading of 'harm.' This
 *   story authors ONLY the harm-limited reading; the absolutist and balancing
 *   readings are separate constraints with their own ε and stakeholder
 *   structures, linked here via network edges, per the ε-invariance
 *   principle.
 *
 * KEY AGENTS:
 *   - historically_targeted_minority_groups: primary beneficiary — gains actionable protection against dignitary harm
 *   - state_speech_tribunals: agenda_setter — administers the harm threshold with wide discretion
 *   - controversial_speakers, dissident_political_movements, religious_traditionalist_speakers: primary targets — bear liability/suppression risk under an unpredictable standard
 *   - coded_dog_whistle_users: paradigm target of the rationale but structurally absent from clean adjudication
 *   - civil_liberties_organizations: analytical observer tracking doctrinal drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.58).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.62).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Harm-Limited Reading of the Speech Protection Boundary").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, 'e8f80255-7aab-45d3-9f89-e7ed77371a9a').
narrative_ontology:cs_kernel_codification('e8f80255-7aab-45d3-9f89-e7ed77371a9a', distributed).
narrative_ontology:cs_authority_grounding('e8f80255-7aab-45d3-9f89-e7ed77371a9a', distributed).
narrative_ontology:cs_reading_relation('e8f80255-7aab-45d3-9f89-e7ed77371a9a', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('e8f80255-7aab-45d3-9f89-e7ed77371a9a', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('e8f80255-7aab-45d3-9f89-e7ed77371a9a', foundational, dignitary_equality_is_a_speech_limiting_constitutional_value).
narrative_ontology:cs_axiom_status(dignitary_equality_is_a_speech_limiting_constitutional_value, holdable).
narrative_ontology:cs_axiom_grounding('e8f80255-7aab-45d3-9f89-e7ed77371a9a', dignitary_equality_is_a_speech_limiting_constitutional_value, deontological).
narrative_ontology:cs_axiom('e8f80255-7aab-45d3-9f89-e7ed77371a9a', secondary, state_gatekeeping_of_harmful_categories_is_legitimate_despite_discretion_risk).
narrative_ontology:cs_axiom_status(state_gatekeeping_of_harmful_categories_is_legitimate_despite_discretion_risk, holdable).
narrative_ontology:cs_axiom_grounding('e8f80255-7aab-45d3-9f89-e7ed77371a9a', state_gatekeeping_of_harmful_categories_is_legitimate_despite_discretion_risk, instrumental).
narrative_ontology:cs_reference_frame('e8f80255-7aab-45d3-9f89-e7ed77371a9a', content_neutral_speech_primacy).
narrative_ontology:cs_drift_state('e8f80255-7aab-45d3-9f89-e7ed77371a9a', contemporary_equality_jurisprudence_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e8f80255-7aab-45d3-9f89-e7ed77371a9a', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, historically_targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, equality_rights_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_speech_tribunals).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, controversial_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, dissident_political_movements).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, religious_traditionalist_speakers).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, dignitary_harm_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, substantive_equality_as_constitutional_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have historically borne the brunt of hate speech, coded harassment, and dignitary attacks that this reading makes actionable or unprotected. Gain a legal mechanism to seek redress or removal of speech that degrades their equal standing, but remain dependent on state tribunals correctly identifying harm rather than merely offense.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, historically_targeted_minority_groups, beneficiary,
    organized, generational, constrained, national).

% Litigate, lobby, and draft the doctrinal standards that operationalize 'significant harm to dignity, equality, and freedom from harassment.' Benefit from the doctrine's existence as a tool and shape its expanding scope through test cases and amicus advocacy.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, equality_rights_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, equality_rights_advocates, agenda_setter).

% Administer and enforce the harm threshold — determining case by case whether speech crosses from protected controversy into actionable dignitary harm. Hold discretionary gatekeeping power over which speech survives; their rulings define the doctrine's actual reach far more than the abstract standard does.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_speech_tribunals, agenda_setter,
    institutional, civilizational, analytical, national).

% Political commentators, satirists, and provocateurs whose speech may be found to inflict 'significant harm to dignity' even absent incitement or violence. Face sanction, deplatforming, or liability under a standard whose boundaries are set case-by-case by tribunals they cannot predict in advance. Exit means self-censorship.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% Fringe or radical political movements whose rhetoric — sometimes genuinely harmful, sometimes merely unpopular — gets swept into the harm category by a state apparatus with discretion over what counts as 'significant.' Lack resources to litigate the boundary and are most exposed to expansive or politically convenient application.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, dissident_political_movements, payer,
    powerless, biographical, trapped, national).

% Speakers articulating traditional religious positions on sexuality, gender, or family that clash with equality norms. Their doctrinal speech risks classification as dignitary harm or harassment, converting theological expression into legally cognizable injury. See exit only through public silence on contested doctrine.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, religious_traditionalist_speakers, payer,
    moderate, generational, constrained, national).

% Speakers using ambiguous or coded language deliberately calibrated to evade direct hate-speech findings while still causing dignitary harm. The doctrine's own logic targets them but its enforcement machinery struggles to reach coded speech reliably — they are simultaneously the doctrine's paradigm target and its practical evasion case, and are not represented as a coherent voice in litigation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, coded_dog_whistle_users, excluded,
    powerless, immediate, trapped, national).

% Monitor tribunal rulings and doctrinal drift for expansion beyond the stated harm categories into viewpoint suppression. Produce independent analysis of case outcomes, file amicus briefs on both sides, and serve as the primary outside check on gatekeeper discretion.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_liberties_organizations, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for protecting the equal standing and psychological security of groups historically subject to speech-based degradation, coordinating around a shared value that unrestricted speech can itself undermine the conditions for other groups' full civic participation.
% TRANSFER_FUNCTION: Moves protection away from speakers whose expression is found to inflict significant dignitary or equality harm, and toward the targets of that expression — converting what would otherwise be legally costless speech into a liability or removal risk, with tribunals as the transfer's administrative point.
% ABSENT_VOICES: Coded dog-whistle users are targeted by the doctrine's rationale but effectively absent from the adjudicative record because their speech is calibrated to avoid clean findings; dissident movements lack litigation resources to shape the doctrine's boundaries even though they are disproportionately exposed to its discretionary application.
% DISAPPEARANCE_RATIONALE: Equality advocates and targeted groups would say the world rearranges sharply — dignitary harms would go unremedied and hate speech would proliferate unchecked. Free-speech absolutists and dissident speakers would say enforcement volume would drop and self-censorship would ease, but underlying social harm dynamics (harassment, discrimination) would persist through non-speech channels; whether the doctrine's removal changes the substantive world or merely its legal remedy is itself the live dispute.
% FOUNDING_PROBLEM: Legal systems recognized that formally content-neutral speech protection permitted sustained, targeted degradation of historically subordinated groups — harassment and hate speech that functioned to exclude those groups from equal civic and economic participation, a harm the traditional imminent-lawless-action standard did not reach.
% FOUNDING_PROBLEM_CORROBORATION: Equality rights advocates and some constitutional scholars (writing outside direct case advocacy) attest the founding problem remains live, citing ongoing documented harassment campaigns with material effects on targets' civic participation. Civil liberties organizations and dissident-movement representatives — outside the beneficiary set — attest the doctrine has drifted from its founding harm rationale toward viewpoint-sensitive suppression of unpopular but non-harassing speech, corroborated by pattern analysis of tribunal rulings showing asymmetric application across ideological lines.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, contested).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 at interval end — moderate-to-substantial, reflecting that the doctrine does perform genuine coordination work (protecting historically subordinated groups from a real and documented harm) while simultaneously extracting from a widening circle of speakers whose expression is reclassified as harmful under discretionary tribunal judgment. Suppression (0.62) exceeds extraction because the doctrine's chilling effect operates prospectively — speakers self-censor to avoid tribunal exposure even where no finding would ultimately issue. Theater ratio is modest (0.28) but rising, tracking the drift from harm-focused adjudication toward performative viewpoint-signaling in some tribunal rulings. Accessibility collapse (0.45) is moderate: alternative speech channels and appeal mechanisms exist, but predictability collapses once a speaker cannot forecast whether their utterance clears the discretionary threshold. Resistance is high (0.72) — this reading is the most actively contested of the three siblings, drawing sustained free-speech advocacy pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (targeted minority groups, equality advocates), the doctrine reads as protective coordination filling a genuine gap in the absolutist standard. From the payer seat (controversial speakers, dissidents, religious traditionalists), the identical structure reads as state-administered viewpoint suppression dressed in dignitary-harm language. The state tribunal seat experiences neither pure benefit nor pure cost but wields the discretion that determines which experience prevails in any given case — this is the seat divergence the engine is expected to compute from the structural data, not a claim this story resolves in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically targeted groups and equality advocates sit near the beneficiary end: the doctrine subsidizes their standing by converting previously costless speech into legal risk for the speaker. State tribunals are agenda-setters with analytical exit — they administer the boundary but do not personally bear its costs or benefits in the way advocacy groups or speakers do. Controversial speakers, dissidents, and religious traditionalists sit near the target end: constrained or trapped exit, biographical time horizon, and direct exposure to sanction. Coded dog-whistle users are structurally the doctrine's intended target but practically evade clean classification, producing an odd directionality: nominally targeted but functionally under-enforced against.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unremedied dignitary harm under a purely content-neutral speech regime) remains genuinely live by the corroboration of equality advocates and some independent scholarship, which is why founding_problem_status is authored as contested rather than dead — this blocks a premature 'mandatrophy resolved' or pure-snare verdict. But the mismatch flagged by disappearance_verdict=contested against founding_problem_status=contested is itself the diagnostic: outside corroborators (civil liberties organizations, dissident representatives) attest the doctrine has drifted toward suppressing unpopular-but-non-harassing speech, which is exactly the tangled-rope signature — genuine coordination function (protecting equality/dignity) coexisting with asymmetric extraction (discretionary suppression of disfavored viewpoints) requiring active tribunal enforcement to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_definition_boundary_stability,
    'Can ''significant harm to dignity, equality, and freedom from harassment'' be given a stable, predictable legal content, or does it necessarily collapse into ad hoc discretionary judgment by whichever tribunal or court applies it?',
    'Longitudinal analysis of tribunal/court rulings under this standard across multiple jurisdictions: measure inter-decision consistency, reversal rates, and correlation between outcomes and the political valence of the speech at issue.',
    'If the standard proves stable and consistently applied regardless of the speaker''s viewpoint, the doctrine functions closer to genuine coordination (rope-leaning tangled rope). If outcomes correlate strongly with the political salience or unpopularity of the speaker''s viewpoint, the doctrine functions closer to viewpoint-based suppression (snare-leaning tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_boundary_stability, empirical, 'Whether the harm standard is administrable without collapsing into viewpoint discrimination.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (harm_limited_reading) of the speech_protection_boundary kernel. The sibling readings — absolutist_reading and balancing_reading — instantiate structurally different protected/unprotected sets and different gatekeeper arrangements. Which reading a given jurisdiction''s courts actually adopt is not settled by this story and is itself a live, high-stakes constitutional dispute.',
    'Track which reading a jurisdiction''s highest court formally adopts over time, and whether it migrates between readings (e.g., from something closer to absolutist toward balancing or harm-limited) in response to social or political pressure.',
    'The reading a jurisdiction adopts determines the entire protected/unprotected boundary and the identity of the doctrine''s victim class. Migration between readings would itself be a significant constitutional event, not merely doctrinal drift within one reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'This story is located within a three-reading kernel contest; the reading adopted is jurisdiction-dependent and contested.').

omega_variable(
    coded_speech_enforcement_gap,
    'Does the doctrine''s practical inability to reliably reach coded dog-whistle speech (the paradigm harm case) while more readily reaching plainly unpopular but less coded speech represent a temporary enforcement-technology gap, or a structural feature that channels the doctrine''s actual force toward less-coded, more-vulnerable speakers?',
    'Compare enforcement/sanction rates for coded versus overt speech carrying comparable documented harm, controlling for the speaker''s institutional resources and legal representation.',
    'If enforcement systematically falls harder on less-resourced, less-coded speakers while sophisticated coded harassment evades sanction, the doctrine''s actual distributive effect diverges sharply from its stated rationale — evidence for the tangled-rope reading over a pure-rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coded_speech_enforcement_gap, empirical, 'Whether enforcement asymmetry between coded and overt harmful speech undermines the doctrine''s stated purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t8, speech_protection_boundary__harm_limited_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(spee_tr_t16, speech_protection_boundary__harm_limited_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(spee_tr_t24, speech_protection_boundary__harm_limited_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(spee_tr_t32, speech_protection_boundary__harm_limited_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__harm_limited_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spee_be_t8, speech_protection_boundary__harm_limited_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(spee_be_t16, speech_protection_boundary__harm_limited_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(spee_be_t24, speech_protection_boundary__harm_limited_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(spee_be_t32, speech_protection_boundary__harm_limited_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__harm_limited_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(spee_su_t8, speech_protection_boundary__harm_limited_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(spee_su_t16, speech_protection_boundary__harm_limited_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(spee_su_t24, speech_protection_boundary__harm_limited_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(spee_su_t32, speech_protection_boundary__harm_limited_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__harm_limited_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__harm_limited_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language 'speech protection boundary' kernel per the ε-invariance principle. absolutist_reading authors near-zero state gatekeeper discretion and a narrow harm exception (imminent lawless action only) — its ε and victim set differ sharply from this story's. balancing_reading authors a case-by-case weighing standard with intermediate discretion. harm_limited_reading (this story) authors the narrowest protected set and the widest gatekeeper discretion, with correspondingly higher suppression and extraction directed at speakers whose expression is reclassified as dignitary harm. Each story carries its own claimed_type, stakeholders, and metrics; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
