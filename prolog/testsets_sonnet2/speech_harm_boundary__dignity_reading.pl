% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Dignity-Subordinated Speech Protection (Categorical Exclusion Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the dignity-reading of the
 *   speech-harm-boundary kernel: constitutional orders (paradigmatically
 *   post-WWII Germany and other post-atrocity states, extended through EU
 *   framework decisions on racism and xenophobia) that treat human dignity as
 *   a supra-positive constitutional value which speech protection cannot
 *   override. Under this reading, certain categories of speech — Holocaust
 *   denial, group defamation, incitement targeting protected classes — are
 *   categorically excluded from protection ex ante, without requiring
 *   case-by-case proof of concrete harm. This is a distinct constraint from
 *   the absolutist_reading (near-absolute protection, extremely high
 *   harm-override threshold) and the harm_balancing_reading (presumptive
 *   protection yielding to proportionality-tested harm showings) — the three
 *   readings produce different victim sets, different burdens of proof, and
 *   different epsilon values from the same underlying kernel: what does
 *   'freedom of speech' mean when it collides with claims of group
 *   personhood. This story authors ONLY the dignity reading; the siblings are
 *   separate constraint files linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - constitutional_courts: agenda_setter (institutional/analytical) — draws and enforces the categorical dignity line
 *   - targeted_dignity_groups: primary beneficiary (organized/constrained) — protected without needing case-specific harm proof
 *   - denialist_speakers and hate_speech_defendants: primary targets (powerless/trapped) — lose context, intent, and harm-absence defenses categorically
 *   - post_atrocity_states: institutional beneficiary/agenda_setter — the rule also serves state legitimacy narrative
 *   - provocative_political_dissidents: collateral payer (powerless/constrained) — swept into the categorical zone without a forum to distinguish protest from persecution
 *   - comparative_legal_scholars: analytical observer — documents cross-jurisdictional drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.58).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.72).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Dignity-Subordinated Speech Protection (Categorical Exclusion Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, 'bcaf5d36-222b-40c3-a32e-b41a0a10115b').
narrative_ontology:cs_kernel_codification('bcaf5d36-222b-40c3-a32e-b41a0a10115b', formalized).
narrative_ontology:cs_authority_grounding('bcaf5d36-222b-40c3-a32e-b41a0a10115b', lineage).
narrative_ontology:cs_interpretation_layer_present('bcaf5d36-222b-40c3-a32e-b41a0a10115b').
narrative_ontology:cs_reading_relation('bcaf5d36-222b-40c3-a32e-b41a0a10115b', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('bcaf5d36-222b-40c3-a32e-b41a0a10115b', speech_harm_boundary__harm_balancing_reading, influences).
narrative_ontology:cs_axiom('bcaf5d36-222b-40c3-a32e-b41a0a10115b', foundational, dignity_as_trumping_constitutional_value).
narrative_ontology:cs_axiom_status(dignity_as_trumping_constitutional_value, holdable).
narrative_ontology:cs_axiom_grounding('bcaf5d36-222b-40c3-a32e-b41a0a10115b', dignity_as_trumping_constitutional_value, deontological).
narrative_ontology:cs_axiom('bcaf5d36-222b-40c3-a32e-b41a0a10115b', foundational, categorical_exclusion_requires_no_case_specific_harm_proof).
narrative_ontology:cs_axiom_status(categorical_exclusion_requires_no_case_specific_harm_proof, holdable).
narrative_ontology:cs_axiom_grounding('bcaf5d36-222b-40c3-a32e-b41a0a10115b', categorical_exclusion_requires_no_case_specific_harm_proof, conventional).
narrative_ontology:cs_reference_frame('bcaf5d36-222b-40c3-a32e-b41a0a10115b', post_atrocity_constitutional_dignity_supremacy).
narrative_ontology:cs_drift_state('bcaf5d36-222b-40c3-a32e-b41a0a10115b', contemporary_digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bcaf5d36-222b-40c3-a32e-b41a0a10115b', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, targeted_dignity_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, holocaust_survivor_communities).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, post_atrocity_states).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, denialist_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, hate_speech_defendants).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, provocative_political_dissidents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates which speech categories fall outside protection by applying a dignity threshold rather than a harm-magnitude test. Draws the categorical line (Holocaust denial, group defamation, incitement against protected classes) and enforces it through criminal and civil sanction, framing the line as flowing from the constitutional commitment to human dignity as a supra-positive value rather than from a balancing test.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Historically targeted minority and post-atrocity communities whose personhood claims the categorical exclusion is built to protect. Gain a legal floor beneath which speech about their humanity cannot fall regardless of the speaker's intent or the audience's sophistication; they cannot litigate every instance themselves and rely on the categorical rule doing the work case-by-case adjudication would not reliably do.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, targeted_dignity_groups, beneficiary,
    organized, generational, constrained, national).

% A specifically named beneficiary class in many dignity-reading jurisdictions (Germany, Austria, France); denial statutes exist substantially because of direct survivor and descendant advocacy. Their exit from the harm is not available — they cannot opt out of being denied — so the categorical rule substitutes for an exit they structurally lack.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, holocaust_survivor_communities, beneficiary,
    moderate, biographical, constrained, national).

% Individuals prosecuted or sanctioned for statements categorically excluded from protection regardless of context, sincerity of belief, or academic framing. Cannot argue harm was absent, minimal, or outweighed by other values — the categorical structure forecloses that argument by design. Exit means self-censorship or exile to jurisdictions with the absolutist or harm-balancing reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, denialist_speakers, payer,
    powerless, biographical, trapped, national).

% Face criminal or civil liability for group-defamatory speech without the state needing to prove concrete incitement or measurable harm to a specific victim — the dignity violation is treated as the harm. Some are genuinely engaged in targeted intimidation; others are provocateurs, satirists, or academics whose speech is swept into the same categorical bucket.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, hate_speech_defendants, payer,
    powerless, biographical, trapped, national).

% Speakers using deliberately offensive or dehumanizing rhetoric as political critique (of religion, of state ideology, of historical narratives) find their speech collapsed into the same categorical exclusion as genuine dignity-violation, without a forum to argue their speech served a different function. Their objection — that categorical rules cannot distinguish protest from persecution — rarely reaches the court that matters.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, provocative_political_dissidents, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, provocative_political_dissidents, excluded).

% States with a specific historical catastrophe (genocide, totalitarian collapse) embed the dignity-subordination rule in their constitutional order as a structural commitment never to permit re-legitimation of the ideology that produced the catastrophe. The rule serves the state's own legitimacy narrative as much as it serves living victims.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, post_atrocity_states, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, post_atrocity_states, agenda_setter).

% Civil liberties organizations and free-expression scholars who would argue the categorical exclusion is unadministrable and viewpoint-discriminatory. They participate in academic and international debate but rarely have standing or a receptive forum within the domestic court system that has already constitutionalized the dignity threshold.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, absolutist_free_speech_advocates, excluded,
    organized, generational, mobile, national).

% Study how differently the same act of speech is treated under dignity, absolutist, and harm-balancing regimes; document doctrinal drift, chilling effects, and cross-jurisdictional forum-shopping by speakers and litigants.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__dignity_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, litigation-resistant floor of protection for historically targeted groups' basic claim to be recognized as full persons, without requiring each victim to individually prove concrete harm from each utterance.
% TRANSFER_FUNCTION: Moves the burden of proof and the risk of erroneous restriction from targeted dignity groups (who no longer must demonstrate case-specific harm) to speakers whose speech falls within the categorical zone (who lose the ability to argue context, intent, or absence of harm as a defense).
% ABSENT_VOICES: Absolutist free-speech advocates and many provocative political dissidents are structurally outside the room once the dignity threshold is constitutionalized — their argument that categorical rules cannot distinguish persecution from protest, satire, or academic inquiry rarely gets a domestic hearing because the categorical exclusion is precisely designed to foreclose that inquiry.
% DISAPPEARANCE_RATIONALE: If the dignity-subordination reading vanished and jurisdictions reverted to an absolutist or pure harm-balancing regime, categorical denial and group-defamation statutes would fall, prosecutions would require case-specific harm showings, and post-atrocity states would lose a central pillar of their constitutional self-definition — a substantial rearrangement of both individual case outcomes and state legitimacy narratives.
% FOUNDING_PROBLEM: Post-WWII constitutional orders (and later transitional and post-genocide states) confronted the fact that formally neutral, harm-balancing speech protection had not prevented — and by some accounts had legally enabled — the rhetorical preconditions for genocide and totalitarian capture. The founding problem was: how to prevent speech that denies a group's personhood from ever again accumulating into the political precondition for their destruction.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and survivor-descendant organizations attest the problem remains live, citing continued Holocaust denial, genocide-denial movements, and resurgent ethnonationalist rhetoric. Independent free-expression scholars and international human rights bodies (including some UN Special Rapporteurs on freedom of expression) attest that the categorical mechanism has, in numerous documented cases, been used well beyond its founding rationale — against journalists, academics, and political dissidents whose speech bore no plausible relationship to atrocity-enabling rhetoric — suggesting the mechanism's scope has drifted from its founding justification even where the founding problem itself persists.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects genuine, substantial costs imposed on speakers whose speech is categorically barred without individualized harm-showing — this is real restriction, not merely nominal. It is not scored as extreme (0.8+) because the coordination function (protecting a genuinely vulnerable class's personhood claim without requiring perpetual re-litigation) is real and non-trivial, distinguishing this from a pure snare. Suppression (0.72) is high and rising over the measured interval because dignity-reading jurisdictions have progressively broadened categorical coverage (from Holocaust denial specifically to broader group-defamation and hate-speech statutes), which requires correspondingly more active enforcement machinery — prosecutorial units, platform-takedown mandates, cross-border extradition requests for online denial. Theater ratio is comparatively low (0.22) because enforcement is substantive, not performative — people are actually prosecuted and convicted under these statutes, not merely nominally covered. Accessibility collapse (0.6) is moderate-high: once a court accepts the dignity-subordination framework, the categorical structure by design forecloses most speaker-side arguments (context, intent, absence of harm), but the framework itself remains genuinely contested across jurisdictions and among scholars, so full collapse (mountain-level ~0.85+) is not appropriate. Resistance (0.68) is substantial and reflects ongoing academic, civil-liberties, and cross-jurisdictional pushback against the categorical approach.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted dignity groups and post-atrocity states sit near the beneficiary end: the former receive protection without needing to individually prove harm; the latter receive a durable constitutional legitimacy narrative. Denialist speakers, hate speech defendants, and provocative political dissidents sit near the target end: the categorical structure removes exactly the defenses (context, intent, proportionality) that would otherwise let a court find no harm. Constitutional courts and post-atrocity states are institutional, with analytical/institutional exit options — they set and administer the boundary rather than living inside it. Absolutist free-speech advocates are excluded rather than coordinated: they are mobile (can advocate, publish, litigate in other fora) but structurally outside the domestic forum that matters once the dignity threshold is constitutionalized.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing speech from re-accumulating into atrocity-enabling political conditions) retains partial vitality — genocide denial and ethnonationalist rhetoric are not historical curiosities. But the founding_problem_status is authored as contested rather than clearly live, because independent corroboration (international human rights bodies, comparative scholars) documents substantial mechanism creep: the categorical tool, built for a narrow and historically specific danger, has been extended to cover political dissidents, academics, and satirists whose speech bears no clear relationship to the founding danger. This is the textbook mandatrophy signature — a mandate whose original function has partially atrophied into broader discretionary suppression while retaining its original legitimating narrative. Classifying this as tangled_rope (rather than snare) is deliberate: the coordination function for the core beneficiary class is real and not merely cover, but the same structure now extracts, through the same categorical mechanism, from a payer class (political dissidents, provocateurs) whose speech was never the founding target. That is precisely the tangled_rope signature — genuine coordination and asymmetric extraction riding the same enforcement machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_versus_balancing_administrability,
    'Can a categorical dignity-exclusion rule be administered without eventually sweeping in political dissent, satire, and academic inquiry that bear no genuine relationship to the founding atrocity-prevention rationale?',
    'Longitudinal comparative study of prosecution and conviction patterns under dignity-reading statutes (Germany, France, Austria, Rwanda) tracking the proportion of cases matching the founding rationale (genocide denial, direct incitement) versus cases involving political critique, satire, or academic speech.',
    'If the categorical mechanism reliably confines itself to the founding rationale, the tangled_rope classification''s extraction component would be overstated and the constraint would sit closer to a rope. If mechanism creep is substantial and accelerating, this supports the tangled_rope reading and potentially a drift toward snare over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_versus_balancing_administrability, empirical, 'Whether categorical dignity exclusions can be cabined to their founding rationale or inevitably expand.').

omega_variable(
    dignity_as_trumping_value_versus_state_interest,
    'Is the dignity-subordination framework genuinely about protecting individual and group personhood, or does it substantially serve post-atrocity states'' own institutional legitimacy needs (never appearing to relativize the founding catastrophe)?',
    'Compare enforcement intensity and case selection where the beneficiary group''s dignity interest is clear but the state''s legitimacy interest is absent (e.g., dignity claims by groups unconnected to the state''s founding atrocity) against cases where both align.',
    'If enforcement concentrates heavily on cases implicating state legitimacy narratives rather than the full range of dignity claims, this supports reading post_atrocity_states as a primary rather than incidental beneficiary, strengthening the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_as_trumping_value_versus_state_interest, conceptual, 'Whether the beneficiary structure is genuinely victim-centered or substantially state-legitimacy-centered.').

omega_variable(
    kernel_framing_committer_structure,
    'Is the dignity_reading a structurally distinct constraint from harm_balancing_reading, or merely a stricter point on the same proportionality continuum?',
    'Examine whether dignity-reading courts treat the categorical exclusion as admitting NO proportionality inquiry once triggered (structurally distinct), versus applying an unusually low harm threshold within a still-proportionality-based framework (same continuum, different calibration).',
    'If dignity-reading courts genuinely admit no proportionality inquiry once the category is triggered, the two readings are structurally distinct constraints (as authored here). If courts quietly smuggle proportionality back in through category definition, the dignity_reading and harm_balancing_reading may converge in practice despite differing in doctrine, which would argue for tighter coupling between the two stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_committer_structure, conceptual, 'Whether the dignity and harm-balancing readings are genuinely structurally distinct or converge in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__dignity_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__dignity_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__dignity_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__dignity_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__dignity_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__dignity_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__dignity_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__dignity_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__dignity_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__dignity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__dignity_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__dignity_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__dignity_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__dignity_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__dignity_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language 'freedom of speech vs. dignity/harm' kernel per the ε-invariance principle. dignity_reading (this file) authors categorical ex ante exclusion with ε=0.58 and a tangled_rope structure. absolutist_reading authors near-total protection with a very high override threshold and correspondingly low ε for speaker-side restriction. harm_balancing_reading authors presumptive protection with case-by-case proportionality testing, an intermediate ε between the other two. All three share the same underlying contested kernel (how personhood claims interact with speech protection) but are structurally distinct constraints with different beneficiary/victim sets, different burdens of proof, and different persistence mechanisms — they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
