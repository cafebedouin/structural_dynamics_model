% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Categorical Balancing — Judicial Control of Speech Categories
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The First Amendment's categorical balancing reading instantiates a
 *   doctrine where the judiciary decides speech protection via case-by-case
 *   assessment of speech value against governmental interest. This reading
 *   produces discrete categories of protected speech (political speech,
 *   artistic speech, news) and unprotected or weakly-protected speech
 *   (obscenity, incitement, commercial speech, true threats). The doctrine
 *   emerged after mid-20th century Supreme Court cases rejected both absolute
 *   First Amendment protection and pure political-branch discretion, settling
 *   on judicial gatekeeping. The constraint is CLAIMED as tangled_rope
 *   (genuine coordination problem: allocating First Amendment protection
 *   across heterogeneous speech; asymmetric extraction: judiciary collects
 *   interpretive authority, speakers in marginal categories bear
 *   predictability costs). The reading/constraint distinction is crucial:
 *   this is ONE READING of the first_amendment_speech_protection kernel; the
 *   absolutist and harm-limited readings are structurally different
 *   constraints with different ε values and beneficiary/victim sets.
 *
 * KEY AGENTS:
 *   - federal_judiciary: institutional agenda-setter, maintains interpretive control over category boundaries, collects authority through balancing doctrine
 *   - speakers_in_contested_categories: powerless payer, cannot predict protection in advance, identity-locked (cannot exit speech itself)
 *   - established_speech_norms: organized beneficiary, protected speech categories align with mainstream institutional voices
 *   - legal_predictability: abstract victim, compromised by case-by-case balancing retroactivity
 *   - marginalized_speech_communities: excluded payer, excluded from core-protection categories
 *   - competing_institutional_voices: excluded (executive, legislative, platform governance prefers alternate authority allocation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.62).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.58).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing — Judicial Control of Speech Categories").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional/political").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '39c507d9-a64b-4b8a-a460-10cc195e1695').
narrative_ontology:cs_kernel_codification('39c507d9-a64b-4b8a-a460-10cc195e1695', fixed_text).
narrative_ontology:cs_authority_grounding('39c507d9-a64b-4b8a-a460-10cc195e1695', extraction).
narrative_ontology:cs_interpretation_layer_present('39c507d9-a64b-4b8a-a460-10cc195e1695').
narrative_ontology:cs_reading_relation('39c507d9-a64b-4b8a-a460-10cc195e1695', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('39c507d9-a64b-4b8a-a460-10cc195e1695', first_amendment_speech_protection__harm_limited_reading, influences).
narrative_ontology:cs_axiom('39c507d9-a64b-4b8a-a460-10cc195e1695', foundational, judicial_categorical_balancing_is_legitimate).
narrative_ontology:cs_axiom_status(judicial_categorical_balancing_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('39c507d9-a64b-4b8a-a460-10cc195e1695', judicial_categorical_balancing_is_legitimate, deontological).
narrative_ontology:cs_axiom('39c507d9-a64b-4b8a-a460-10cc195e1695', secondary, speech_value_harm_incommensurability_resolvable).
narrative_ontology:cs_axiom_status(speech_value_harm_incommensurability_resolvable, holdable).
narrative_ontology:cs_axiom_grounding('39c507d9-a64b-4b8a-a460-10cc195e1695', speech_value_harm_incommensurability_resolvable, empirically_contingent).
narrative_ontology:cs_reference_frame('39c507d9-a64b-4b8a-a460-10cc195e1695', categorical_balancing_authority_of_judiciary).
narrative_ontology:cs_drift_state('39c507d9-a64b-4b8a-a460-10cc195e1695', contemporary_digital_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('39c507d9-a64b-4b8a-a460-10cc195e1695', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, established_speech_norms).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, speakers_in_contested_categories).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, marginalized_speech_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the First Amendment via case-by-case balancing doctrine, adjudicating which speech categories receive protection and which do not. The Court maintains discretion to weigh speech value against governmental interests (compelling state interest, narrow tailoring). Collects institutional authority and interpretive control over constitutional meaning through this balancing authority. Creates binding precedent that shapes the boundary between protected and unprotected speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Speakers whose expression falls in categories the judiciary deems unprotected or weakly protected (commercial speech, speech by public employees, speech inciting imminent lawless action, obscenity, true threats) bear the cost of judicial gatekeeping. They cannot predict in advance whether their speech will be protected or suppressed; protection depends on the Court's assessment of speech value versus harm. Exit options are constrained by the identity-fused nature of speech: one cannot be a political activist, artist, or public employee without engaging in categories that may be judged unprotected.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, speakers_in_contested_categories, payer,
    powerless, biographical, identity_locked, national).

% Dominant viewpoints, institutional voices, and speech aligned with established social hierarchies benefit from a balancing doctrine that protects 'core' political speech while carving out exceptions for marginalized categories. The balancing framework grants stable protection to mainstream speech while leaving contested or challenging speech (radical critique, obscenity, protest incitement) vulnerable to category-by-category limitation. Benefits from judicial gatekeeping that filters which speech demands full protection.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, established_speech_norms, beneficiary,
    organized, generational, mobile, national).

% The abstract good of legal predictability — the ability to know in advance what speech is permissible — is compromised by case-by-case balancing. The doctrine requires courts to weigh incommensurate values (speech value, governmental interest) without algorithmic rules, producing inconsistent outcomes and leaving speakers and lower courts uncertain about category boundaries. Balancing doctrine is inherently retroactive: speakers cannot know if their expression is protected until a court decides.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

% Speech communities whose expression (racial justice rhetoric framed as incitement, sex work advocacy in 'obscene' form, radical political speech, prisoner speech, immigrant speech) falls into categories the balancing doctrine has historically limited bear heightened suppression risk. They are often excluded from the categories deemed 'core political speech' and thus receive less protection. Their participation in the constitutional conversation is structurally curtailed by categories that don't protect their forms of expression.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, marginalized_speech_communities, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, marginalized_speech_communities, excluded).

% Executive and legislative actors, police enforcement, school administrators, and private platforms all interpret the First Amendment differently and would prefer different boundaries between protected and unprotected speech. The judiciary's categorical balancing locks in a particular institutional answer to what speech merits protection, excluding competing readings and enforcement preferences from the authoritative framing.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, competing_institutional_voices, excluded,
    institutional, generational, constrained, national).

% Analyze whether the balancing doctrine is coherent, whether categories track First Amendment values, and whether the doctrine produces just or unjust outcomes. Provide expert testimony and academic support for alternative readings (absolutist, harm-limited). Their role is descriptive and critical, not determinate of the constraint's structure.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, constitutional_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of how to allocate First Amendment protection across different speech categories: establishes a stable institutional process (judicial review, case-by-case balancing) for determining which speech merits constitutional protection and which may be regulated by government.
% TRANSFER_FUNCTION: Transfers interpretive authority over the meaning of the First Amendment from the political branches and decentralized actors to the federal judiciary. Moves legal stability and predictability from individual speakers (who cannot know in advance if their speech is protected) to institutional doctrine (where the judiciary's categories are stable across time). Moves enforcement discretion from direct government suppression (which would face facial First Amendment scrutiny) to categorical carve-outs that government can cite.
% ABSENT_VOICES: Speakers in categories the doctrine excludes from 'core' protection (obscenity speakers, commercial speakers, incitement speakers) would argue for their own participation in the constitutional conversation. Absolutists arguing the First Amendment permits no categorical exceptions are structurally excluded by the balancing framework itself. Harm-limited advocates arguing protection yields only when direct unconsented harm occurs are outside the interpretive authority the judiciary claims. Alternative institutional actors (legislatures, agencies, platforms) would advocate for their own authority over category boundaries.
% DISAPPEARANCE_RATIONALE: If this categorical balancing doctrine disappeared overnight and the First Amendment reverted to either absolutist or harm-limited interpretation, constitutional law would reorganize: the set of protected speech would expand (absolutist reading) or contract dynamically case-by-case (harm-limited reading). The judiciary would lose its institutional gatekeeping function. Speakers would have either stronger predictability (under absolutism) or would navigate a different category system (under harm-limited). Government regulation of speech would shift to whichever alternative framework displaced balancing.
% FOUNDING_PROBLEM: The First Amendment's text ('Congress shall make no law...abridging the freedom of speech') was interpreted by the mid-20th century as not absolutely categorical but as permitting some classes of speech to be excluded from protection (obscenity, incitement, true threats, etc.). The balancing doctrine emerged to resolve the question of how courts would decide which speech fell outside protection: by establishing categories and weighing speech value against governmental interest on a case-by-case basis, rather than either treating the First Amendment as absolute or leaving regulation entirely to political judgment.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court itself attests the founding problem is live, citing ongoing cases where speech protection must be adjudicated (Brandenburg v. Ohio incitement test, obscenity balancing, true-threats doctrine). Absolutist constitutional scholars (Hugo Black's tradition) attest the founding problem arises from a mistaken premise—that the First Amendment permits exceptions at all. Harm-limited advocates attest the problem is that balancing conflates harm-triggering speech with speech-qua-speech. Legal predictability experts and speakers challenging their own categorization attest that the balancing doctrine leaves the boundary unstable and speaker-harming. The founding problem is corroborated from outside the judiciary by scholars and speakers the doctrine affects.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the judiciary extracts interpretive authority over constitutional meaning; balancing doctrine is presented as neutral adjudication but in practice creates discretionary gatekeeping that allows regulation of disfavored speech categories. Suppression (0.58) reflects that the doctrine's mechanism for handling speech the judiciary disfavors is to exclude it from protection via categorical carve-outs—this is sustained by active judicial enforcement of category boundaries and by government reliance on those boundaries to regulate speech. Theater (0.41) reflects that judicial balancing rhetoric invokes neutral principles (weighing values, narrow tailoring) while operating as institutional boundary maintenance—the performance of neutral adjudication masks gatekeeping. Accessibility_collapse (0.71) reflects that once the categorical balancing framework is institutionalized, alternative understandings (absolute protection, harm-only limitation, legislative authority) become structurally inaccessible—the categories themselves are the installed constraint and reorganizing around them requires constitutional amendment or Supreme Court reversal. Resistance (0.68) reflects substantial ongoing resistance from speakers challenging categorization, from absolutist scholars, from harm-limited theorists, and from marginalized communities arguing their speech is wrongly excluded from core protection. The measurement series track rising extractiveness (judiciary's gatekeeping authority hardens from 1960–2000, then plateaus) and rising theater ratio (balancing language becomes more elaborate and performative as doctrine faces critique), reflecting institutional entrenchment of the categorical system. Suppression_requirement rises steadily, reflecting that maintaining category boundaries requires increasing judicial effort as speakers develop new forms of expression and resist categorization.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat: balancing doctrine is coordination—it solved the problem of how to determine constitutional protection in a heterogeneous speech landscape; it is fair, neutral, and necessary. From the speaker in a marginalized category's seat: the same structure is gatekeeping—it creates a predefined set of protected categories and leaves their speech vulnerable to case-by-case suppression justified by 'balancing.' From the constitutional scholar's seat (observer position): the doctrine is theoretically incoherent—it claims to weigh incommensurate values (speech value, state interest) with no principled method. From competing institutional actors' seats: it is usurpation—the judiciary has monopolized First Amendment interpretation and excluded legislative and executive input. The engine computes classification from the stakeholder's power, exit, and benefit/cost structure; these perspectives are rooted in structural position, not mere opinion.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits at d ≈ 0.1–0.2 (beneficiary: collects interpretive authority, no obligation to defend the constraint, can modify categories). Speakers_in_contested_categories sit at d ≈ 0.85–0.95 (targets: constrained by categories they cannot control, cannot predict outcomes, identity-locked, bear suppression risk). Established_speech_norms sit at d ≈ 0.15–0.25 (beneficiary: their speech is protected; they do not bear gatekeeping costs). Legal_predictability is a non-agent (abstract good) and does not receive a d. Marginalized_speech_communities sit at d ≈ 0.88–0.98 (targets: worst-positioned speakers, excluded from core categories, highest suppression risk). The seat divergence is extreme: a speaker in an unprotected category experiences this constraint as snare-like (suppression, no escape, no coordination benefit); the judiciary experiences it as rope-like (coordination problem solved, institutional control collected). The engine computes per-seat types from power, exit, beneficiary/victim position; this divergence is the analytical product of the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (allocating First Amendment protection, deciding which categories merit full protection) is live and contested. The categorical balancing doctrine is the mechanism the judiciary chose to solve it. The constraint is NOT mandatrophic: the founding problem remains unsolved in a deeper sense (balancing doctrine is contested, not settled), and the beneficiary (judiciary) actively maintains the constraint because it solves the coordination problem for the judiciary—no institutional inertia or theater-maintenance is driving it. The resistance measurement (0.68) reflects that the constraint is not decaying; it is actively contested and defended. A mandatrophic constraint would show rising theater_ratio without rising active resistance; this constraint shows both rising theater and sustained resistance, indicating the doctrine is being reproduced because it serves institutional interests, not because it persists by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_versus_absolute_interpretation,
    'Is the First Amendment''s text ''no law'' read literally (categorical absolutism) or as permitting categories and balancing?',
    'Constitutional convention or formal amendment. In practice, the Supreme Court has chosen balancing since ~1960, but the literal text could be reinstated if the Court reversed doctrine or if the amendment process intervened.',
    'If absolutist interpretation is adopted, this constraint dissolves: the judiciary loses gatekeeping authority, speaker protection expands dramatically, and legal predictability shifts from categorical balancing to bright-line rules. If balancing doctrine is retained, this constraint persists as institutional feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_versus_absolute_interpretation, conceptual, 'The interpretive frame determines whether categorical exceptions are permissible under the First Amendment.').

omega_variable(
    balancing_coherence,
    'Is the balancing test (weighing speech value against state interest) theoretically coherent, or does it rest on incommensurable value comparisons?',
    'Philosophical and jurisprudential analysis of whether the test can be applied consistently. Empirical observation of whether courts apply the same weights across different speaker categories.',
    'If incoherent, the constraint''s legitimacy is undermined; if coherent, the balancing framework is defensible as neutral adjudication. Incoherence would support either absolutism or harm-limitation as alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balancing_coherence, conceptual, 'Whether the balancing doctrine rests on a coherent theory or performs neutrality while enabling gatekeeping.').

omega_variable(
    internalization_of_category_exclusions,
    'Do speakers in marginalized categories eventually internalize the balancing doctrine''s exclusion as legitimate, or does resistance persist?',
    'Longitudinal study of speaker attitudes and behavior; observation of whether resistance movements maintain or attenuate over time; comparison of speaker self-censorship rates (internalized suppression) vs. external enforcement rates.',
    'If speakers internalize exclusions, suppression is partly sustained by internalized constraint (more efficient for the regime, more complete). If resistance persists, suppression requires continuous active enforcement. The internalization level modulates the effective suppression above the structural base.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_of_category_exclusions, empirical, 'Whether marginalized speakers accept category exclusions as legitimate or maintain resistance.').

omega_variable(
    judicial_authority_sources,
    'Does the judiciary''s gatekeeping authority derive from the First Amendment text itself, from historical practice, from institutional competence, or from power consolidation?',
    'Originalist analysis of the Framers'' intent; historical study of judicial First Amendment interpretation pre-1960; comparison to legislative and executive First Amendment authority claims; analysis of whether the balancing authority would survive if assigned to coordinate branches.',
    'If authority derives from the text or history, the constraint is well-grounded. If from institutional competence, it depends on the Court demonstrating neutrality. If from power consolidation, the constraint is extractive in its authority structure itself, independent of how categories are drawn.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_authority_sources, conceptual, 'What grounds the judiciary''s authority to define protected speech categories.').

omega_variable(
    category_boundary_drift,
    'Do category boundaries (obscenity, incitement, commercial speech, true threats) remain stable across time and context, or do they drift in ways that expand/contract protection for particular speaker groups?',
    'Doctrinal and empirical analysis of category definitions over time; measurement of whether the same speech is protected or unprotected across different time periods or speaker types; comparison of rates at which different speaker categories are found to fall within protected vs. unprotected bounds.',
    'If boundaries are stable, the balancing doctrine provides predictability within its categorical structure. If boundaries drift, predictability is undermined and patterns of drift would reveal whether marginalized categories are selectively contracted. Differential category stability across speaker groups would evidence extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_boundary_drift, empirical, 'Whether category boundaries are stable or drift in ways that differentially protect some speakers.').

omega_variable(
    reading_kernel_distinction,
    'Is the distinction between readings of the first_amendment_speech_protection kernel (absolutist, categorical_balancing, harm_limited) a real structural difference or a reframing of the same underlying constraint?',
    'Structural analysis of whether each reading produces a different beneficiary/victim set, different ε value, and different institutional outcome. Test whether switching readings would materially change speech regulation outcomes.',
    'If readings are truly distinct constraints, they should be separately analyzed. If they are observationally equivalent reframings of the same structure, they should be collapsed. The kernel decomposition is justified only if readings produce different empirical predictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether the kernel contains genuinely distinct constraint structures or reframings of a single structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1960, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(firs_tr_t1985, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(firs_tr_t2000, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(firs_tr_t2012, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2012, 0.39).
narrative_ontology:measurement(firs_tr_t2018, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(firs_be_t1960, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement(firs_be_t1985, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1985, 0.54).
narrative_ontology:measurement(firs_be_t2000, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2000, 0.59).
narrative_ontology:measurement(firs_be_t2012, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2012, 0.61).
narrative_ontology:measurement(firs_be_t2018, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1960, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement(firs_su_t1985, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(firs_su_t2000, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(firs_su_t2012, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2012, 0.56).
narrative_ontology:measurement(firs_su_t2018, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2018, 0.57).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__categorical_balancing_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% The first_amendment_speech_protection kernel supports three structurally distinct constraint readings: (1) absolutist_reading—no categorical exceptions permitted, protection is bright-line; (2) categorical_balancing_reading (this story)—judiciary balances speech value vs. harm, creating discrete categories; (3) harm_limited_reading—protection yields only when speech causes demonstrable unconsented harm, no categorical pre-exclusions. Each reading produces a different beneficiary/victim structure and different ε. The readings coexist across different institutional and scholarly communities. This story instantiates only the categorical_balancing reading, treating the balancing doctrine as the standing arrangement under contest, assessed from the balancing reading's own lights (ε is the reading's assessment, not a neutral observer's). Sibling readings author their own ε and beneficiary/victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__categorical_balancing_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
