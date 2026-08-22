% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection (Brandenburg Standard)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Brandenburg v. Ohio standard (1969) set the constitutional boundary
 *   for unprotected speech at direct incitement to imminent lawless action.
 *   This absolutist reading maximizes the protected set: nearly all speech,
 *   including hate speech, harassment, dehumanizing rhetoric, and false
 *   claims, receives First Amendment protection unless it explicitly incites
 *   immediate violence. The constraint is presented as a neutral legal
 *   boundary that protects dissent and prevents censorship. The reading
 *   instantiates one interpretation of the speech-protection kernel; sibling
 *   readings (harm-limited and balancing) interpret the same constitutional
 *   text differently, grounding protection in different values and drawing
 *   the boundary at different points. This story models the absolutist
 *   reading specifically — its beneficiaries, its victims, and the structural
 *   extraction from marginalized communities that the boundary produces.
 *
 * KEY AGENTS:
 *   - political_speakers: beneficiary from near-absolute protection; moderate power, mobile exit
 *   - dissidents: powerless beneficiary; protection enables mobilization against power imbalances
 *   - mass_media: institutional beneficiary; editorial independence and market advantage ride on protection
 *   - marginalized_communities: powerless victim; bear coordinated harassment without legal recourse
 *   - harassment_targets: powerless victim; trapped between participating and absorbing cumulative harm
 *   - judicial_system: agenda-setter; administers Brandenburg through constitutional interpretation
 *   - legislators_proposing_harm_restrictions: excluded; any restriction they pass faces invalidation
 *   - international_human_rights_bodies: excluded; recognize hate speech as human rights violation but have no enforcement authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.68).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.24).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist Speech Protection (Brandenburg Standard)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '6df9103c-8ed6-47d3-95a7-f3fd5670fcbd').
narrative_ontology:cs_kernel_codification('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', fixed_text).
narrative_ontology:cs_authority_grounding('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', lineage).
narrative_ontology:cs_interpretation_layer_present('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd').
narrative_ontology:cs_reading_relation('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', foundational, imminent_violence_only_gate).
narrative_ontology:cs_axiom_status(imminent_violence_only_gate, holdable).
narrative_ontology:cs_axiom_grounding('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', imminent_violence_only_gate, deontological).
narrative_ontology:cs_axiom('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', foundational, text_original_meaning_primacy).
narrative_ontology:cs_axiom_status(text_original_meaning_primacy, holdable).
narrative_ontology:cs_axiom_grounding('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', text_original_meaning_primacy, deontological).
narrative_ontology:cs_reference_frame('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', founders_censorship_prevention).
narrative_ontology:cs_drift_state('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', contemporary_digital_age, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6df9103c-8ed6-47d3-95a7-f3fd5670fcbd', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, political_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, dissidents).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, mass_media).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, marginalized_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, harassment_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can articulate political views, criticism of government, and controversial claims with near-absolute immunity from legal restraint. Risk of consequences is limited to counter-speech, social disapproval, and civil liability for specific harms (libel, contract breach). Their ability to shape public discourse is protected even when their speech is caustic, offensive, or factually contested.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, political_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Can voice opposition to established institutions, authority, and dominant narratives without facing criminal prosecution for the expression itself. This protection is structurally essential to their capacity to mobilize, organize, and challenge power imbalances. The constraint shields their ability to speak despite having no institutional resources.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, dissidents, beneficiary,
    powerless, biographical, mobile, national).

% Publishes reporting, opinion, and investigative journalism with minimal prior restraint or post-publication legal jeopardy for editorial content. Can cover controversial topics, publish leaked information, and criticize powerful actors without requirement to pre-screen for harm. The near-absolute protection enables their market position and editorial independence.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, mass_media, beneficiary,
    institutional, generational, arbitrage, national).

% Encounter hate speech, dehumanizing rhetoric, and coordinated harassment campaigns that the Brandenburg standard does not restrict unless the speech explicitly incites imminent lawless action. The constraint protects speakers who demonize, stereotype, or advocate exclusion of their group, leaving no legal recourse for dignitary harm or cumulative psychological injury. They cannot exit the speech environment.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, marginalized_communities, payer,
    powerless, generational, trapped, national).

% Face coordinated harassment, doxing, slur campaigns, and threats that fall outside the imminent-lawless-action gate. Individual statements targeting them are protected speech; the aggregate effect is psychological coercion and exclusion from public participation. Their exit is constrained: leaving social media means ceding voice; engaging means absorbing harm.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, harassment_targets, payer,
    powerless, biographical, constrained, national).

% Administers the Brandenburg standard through First Amendment doctrine, applying imminent-lawless-action test to speech-restriction laws and rejecting most content-based regulation. Enforces the boundary through case law and constitutional interpretation. Decides when speech crosses from protected to unprotected, determining what counts as imminent, what counts as directed to lawless action.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, judicial_system, agenda_setter,
    institutional, generational, analytical, national).

% Would enact broader restrictions on hate speech, harassment, and dignitary harm (as many democracies have done) but are blocked by the Brandenburg boundary. Their excluded position is structural: any speech restriction they propose is subject to judicial invalidation if it fails the imminent-lawless-action test. They see the boundary as leaving real harms unaddressed.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, legislators_proposing_harm_restrictions, excluded,
    institutional, generational, constrained, national).

% Recognize hate speech and harassment as human rights violations (ICCPR Article 20, European Convention on Human Rights Article 10(2)) but have no enforcement authority over U.S. speech law. Their position is that dignitary harm and community safety justify speech restrictions within proportionality limits; they are excluded from the U.S. constitutional conversation but would object if present.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, international_human_rights_bodies, excluded,
    organized, generational, analytical, global).

% Interpret the Brandenburg standard as grounded in the original public meaning of the First Amendment's text and founding-era understanding. From their seat, the absolutist reading is the only coherent reading consistent with constitutional structure. Other readings, in their analysis, are judicial overreach.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, originalist_judges, observer,
    institutional, generational, analytical, national).

% Analyze the absolutist reading as allowing structural harm to continue and treating dignitary harm as an acceptable cost of individual liberty. They see alternative readings as better reconciling free expression with equal protection and freedom from harassment. Their analysis treats the Brandenburg boundary as a contested choice, not a natural law.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, progressive_jurists, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, mass_media).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform legal standard for what speech the state can restrict, preventing censorship through ad hoc harm judgments. Provides speakers with clear notice of legal boundaries, enabling speech planning without chilling legitimate expression. Coordinates judicial review across jurisdictions around a single high-burden test for speech restriction.
% TRANSFER_FUNCTION: Moves dignitary safety and harassment protection away from marginalized communities and harassment targets (who cannot restrict the speech that harms them) toward speakers and media institutions (who gain near-absolute freedom to publish). The constraint transfers from the powerless (who bear aggregate harm as externality) to the beneficiary seats (who accrue speech protection).
% ABSENT_VOICES: Harassment targets and marginalized communities whose dignitary harm falls outside Brandenburg protection are structurally excluded from the legal calculus that shaped the boundary. Their objection — that the standard leaves them defenseless against coordinated hate — is not heard in constitutional doctrine. International human rights frameworks that recognize hate speech as a human rights violation are also excluded from U.S. discourse.
% DISAPPEARANCE_RATIONALE: Absolutist reading: if the Brandenburg boundary disappeared and were replaced by a standard that restricted speech causing significant dignitary or equality harm, speakers and media would face heightened legal jeopardy; the beneficiaries would reorganize around risk mitigation. Harm-limited reading: if it disappeared, marginalized communities would gain legal recourse against harassment; the constraint itself would not be missed because the coordination value (notice to speakers about boundaries) is preserved under alternative standards. The parties dispute whether the world rearranges or whether better arrangements emerge.
% FOUNDING_PROBLEM: The First Amendment was designed to prevent government censorship and protect political dissent. The Brandenburg standard operationalizes this by barring the government from criminalizing speech merely because it is offensive, controversial, or wrong. The standard exists to protect speakers (especially dissidents and vulnerable minorities) from being silenced by those in power.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists and First Amendment absolutists attest that the founding problem (censorship of dissent) remains live and that Brandenburg solves it. Progressive jurists and marginalized-community advocates attest that the founding problem is substantially solved (the U.S. does not systematically censor for political reasons) but that applying Brandenburg to hate speech and harassment creates a new problem the founders did not contemplate (concentrated harm on the powerless). International human rights experts attest that democracies can protect dissent AND restrict hate speech simultaneously. No corroboration exists from outside all three seats — the problem-status itself is the contested core.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, contested).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.51 to 0.68 over the interval because the constraint's protective function (preventing government censorship) remains stable, but its extractive effect (enabling hate speech and harassment) compounds as social media scale amplifies coordinated harassment. The harm accumulates on the same targets over time; the legal boundary does not shift, but its distributional consequences intensify. Theater is very low (0.12 at end): the Brandenburg standard is primarily functional, not performative — judicial review genuinely applies the imminent-lawless-action test. Suppression is low (0.24) because the absolutist reading does not rely on coercion to persist; it relies on judicial doctrine, constitutional text interpretation, and the perception that it protects important values. Accessibility collapse is high (0.72) because once the Brandenburg boundary is accepted as law, marginalized communities cannot appeal to legal protection for dignitary harm — alternatives (civil rights law, tort law, legislative action) are foreclosed by the constitutional constraint. Resistance is high (0.78) because progressive jurists, marginalized-community advocates, and international human rights bodies actively contest the boundary and call for alternative readings.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (especially judicial originalists and First Amendment scholars), the absolutist reading protects essential liberties and prevents tyranny. From the victim seats (marginalized communities, harassment targets), the same boundary is a mechanism that protects the powerful's right to harm the powerless. The judicial_system's perspective is procedural: they apply the rule as law. The excluded seats would see a fundamentally different constraint if they were heard — one that balances speech freedom against dignity and equality. The engine computes these different types from the structural data: a beneficiary seat will compute differently from a victim seat, even though they are governed by the same legal rule, because their directionality, exit options, and power differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats (political_speakers, dissidents, mass_media) have low directionality: the constraint subsidizes their speech. Victim seats (marginalized_communities, harassment_targets) have high directionality: they pay the cost in unaddressed dignitary harm. The judicial_system sits as agenda-setter but has moderate directionality because they enforce a rule they did not author and cannot easily change — their position is structural administrator, not primary beneficiary or victim. The excluded seats (legislators, international bodies) have inverted directionality from their own perspective: they would benefit from a different boundary and are harmed by the absolutist standard, but they are outside the constraint's direct operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing government censorship of dissent) is live for political speakers and dissidents but contested for marginalized communities and harassment targets. Their objection is that the founding problem was solved decades ago — the government does not systematically suppress political speech — and that maintaining Brandenburg protects speech that is not dissent but hate. The mandatrophy resolution depends on whether the audience reads the constraint as solving the founding problem or as outdated. For dissidents, the constraint is essential (mandatrophy = low). For marginalized communities, the constraint's function is obsolete but its structure persists (mandatrophy = high). The 'contested' disappearance verdict captures this: the parties dispute whether the world would rearrange or improve if the boundary changed, which is the core mandatrophy question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminent_lawless_action_definition,
    'What counts as ''imminent'' in the Brandenburg standard, and how do algorithmic amplification and coordinated online campaigns change the structural imminence of violent action?',
    'Cases testing Brandenburg application to online radicalization, coordinated harassment campaigns, and algorithmic amplification; empirical analysis of correlation between online speech and violent action timing.',
    'If ''imminence'' expands to include algorithmically accelerated or coordinated campaigns, the unprotected set grows and marginalized communities gain legal recourse. If it remains tied to direct temporal proximity, the boundary holds and extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminent_lawless_action_definition, conceptual, 'The temporal and technological definition of ''imminence'' is contested and evolving.').

omega_variable(
    dignitary_harm_cumulative_vs_singular,
    'Does dignitary harm from hate speech and harassment accumulate (such that sustained campaigns cause measurable harm even if no singular statement incites violence), or is it appropriately disaggregated to per-statement analysis under Brandenburg?',
    'Psychological and sociological evidence on cumulative harm from harassment; comparative analysis of jurisdictions that recognize cumulative dignitary harm as legally relevant.',
    'If accumulation is recognized, harm-limited and balancing readings gain structural ground; the absolutist boundary becomes more clearly extractive. If per-statement analysis is maintained, the boundary holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignitary_harm_cumulative_vs_singular, empirical, 'Whether dignitary harm is properly analyzed as cumulative or singular per-statement.').

omega_variable(
    reading_foreclosure_originalist_vs_progressive,
    'Does the absolutist reading logically foreclose the harm-limited and balancing readings, or do they coexist as different interpretive frameworks that different judicial and political communities can adopt?',
    'Examination of whether accepting the absolutist grounding (original public meaning, text-only analysis) logically commits one to rejecting harm-limited axioms, or whether the readings are compatible within different constitutional theories.',
    'If foreclosure exists, the engine classifies the relation as forecloses; if coexistence is structural, the relation is coexists_with. This affects how the constraint family is modeled and whether readings are alternatives or contradictions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_originalist_vs_progressive, conceptual, 'Whether the absolutist reading logically forecloses sibling readings or merely disagrees with them.').

omega_variable(
    beneficiary_identity_lock_vulnerability,
    'To what extent is the speech-protection benefit the absolutist reading provides dependent on the beneficiary''s identity as a political speaker or dissident, versus universally available to any speaker?',
    'Analysis of Brandenburg application across speaker types (marginalized vs. powerful speakers; dissidents vs. state actors) to determine whether the benefit distributes equally or concentrates.',
    'If the benefit is identity-locked to certain speaker types (e.g., powerful speakers disproportionately benefit from the protection while marginalized speakers face coordinated harm), the constraint''s extractiveness increases and the beneficiary designation becomes more precise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_lock_vulnerability, empirical, 'Whether Brandenburg''s protection benefits are equally available to all speakers or concentrated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__absolutist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(spee_tr_t5, speech_protection_boundary__absolutist_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__absolutist_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(spee_tr_t15, speech_protection_boundary__absolutist_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__absolutist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(spee_tr_t25, speech_protection_boundary__absolutist_reading, theater_ratio, 25, 0.11).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__absolutist_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__absolutist_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__absolutist_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(spee_be_t5, speech_protection_boundary__absolutist_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__absolutist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(spee_be_t15, speech_protection_boundary__absolutist_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__absolutist_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(spee_be_t25, speech_protection_boundary__absolutist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__absolutist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__absolutist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__absolutist_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(spee_su_t5, speech_protection_boundary__absolutist_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__absolutist_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(spee_su_t15, speech_protection_boundary__absolutist_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__absolutist_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(spee_su_t25, speech_protection_boundary__absolutist_reading, suppression_requirement, 25, 0.23).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__absolutist_reading, suppression_requirement, 30, 0.24).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__absolutist_reading, suppression_requirement, 40, 0.24).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel constrains three structurally distinct constraint stories: absolutist_reading (this file), harm_limited_reading, and balancing_reading. Each instantiates a different interpretation of the First Amendment and produces a different ε-value, different beneficiary/victim structure, and different type. The readings coexist in contemporary U.S. constitutional discourse but differ in what counts as justified speech restriction. Absolutism (imminent violence only) maximizes protected speech and extraction from the powerless. Harm-limited (restriction for significant dignitary/equality harm) distributes protection more broadly. Balancing (case-by-case weight of interests) creates context-dependent protection. The three readings are NOT observational variants of one constraint; they are different constraints with different kernels and axioms, linked by network affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__absolutist_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
