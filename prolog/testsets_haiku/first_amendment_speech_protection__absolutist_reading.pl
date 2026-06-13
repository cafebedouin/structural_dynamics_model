% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Reading: Categorical Speech Protection
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The absolutist reading of the First Amendment holds that 'no law' means
 *   no law—protection of speech is categorical and admits no cost-benefit
 *   analysis except for a narrow, historically defined set of exceptions
 *   (true threats, incitement to imminent lawless action). The reading is
 *   authored as a mountain (natural constitutional law, not a choice) while
 *   measuring substantially extractive operation (0.68 extractiveness at
 *   interval end, rising monotonically from 0.38). This is intentional: the
 *   constraint claims naturality while empirically operating to externalize
 *   costs to targeted minorities. The measurement series tracks how
 *   suppression of counter-speech (resistance to the reading's application,
 *   legal and scholarly challenges) has intensified from 0.55 to 0.71 over 50
 *   years as the costs to targeted groups became empirically documented and
 *   the reading's foundational problem (government censorship of political
 *   dissent) shifted from live to dead. Theater has risen from 0.25 to 0.42
 *   as courts maintain the absolutist reading through increasingly narrow
 *   interpretations of the historical exceptions while declaring harm to
 *   minorities irrelevant—the functional shift from 'prevent state
 *   censorship' to 'protect hate speech' is masked by categorical language.
 *
 * KEY AGENTS:
 *   - majority_speakers: institutional and political voices that benefit from unrestricted expressive latitude without liability
 *   - targeted_minorities: historically marginalized groups bearing externalized harm from hate speech and coordinated abuse
 *   - judiciary: administers and enforces the absolutist reading by narrowly interpreting exceptions and striking down harm-based restrictions
 *   - legislative_bodies: attempt to restrict harmful speech in response to constituent harm but are overridden by courts
 *   - international_human_rights_bodies: observe the absolutist reading as a deviant outlier, documenting its costs to dignity and equality
 *   - academic_critics: produce empirical evidence of hate-speech harms but are excluded from doctrinal input via the rule that harm is constitutionally irrelevant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.68).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.71).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, mountain).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Reading: Categorical Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional/political").

domain_priors:emerges_naturally(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '91a7543e-27b8-4285-9ff6-d03584392cf3').
narrative_ontology:cs_kernel_codification('91a7543e-27b8-4285-9ff6-d03584392cf3', fixed_text).
narrative_ontology:cs_authority_grounding('91a7543e-27b8-4285-9ff6-d03584392cf3', extraction).
narrative_ontology:cs_interpretation_layer_present('91a7543e-27b8-4285-9ff6-d03584392cf3').
narrative_ontology:cs_reading_relation('91a7543e-27b8-4285-9ff6-d03584392cf3', first_amendment_speech_protection__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('91a7543e-27b8-4285-9ff6-d03584392cf3', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('91a7543e-27b8-4285-9ff6-d03584392cf3', foundational, textual_absolutism).
narrative_ontology:cs_axiom_status(textual_absolutism, holdable).
narrative_ontology:cs_axiom_grounding('91a7543e-27b8-4285-9ff6-d03584392cf3', textual_absolutism, deontological).
narrative_ontology:cs_axiom('91a7543e-27b8-4285-9ff6-d03584392cf3', foundational, majoritarian_liberty_as_foundational).
narrative_ontology:cs_axiom_status(majoritarian_liberty_as_foundational, holdable).
narrative_ontology:cs_axiom_grounding('91a7543e-27b8-4285-9ff6-d03584392cf3', majoritarian_liberty_as_foundational, instrumental).
narrative_ontology:cs_reference_frame('91a7543e-27b8-4285-9ff6-d03584392cf3', originalist_constitutional_textualism).
narrative_ontology:cs_drift_state('91a7543e-27b8-4285-9ff6-d03584392cf3', contemporary_digital_harassment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91a7543e-27b8-4285-9ff6-d03584392cf3', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_speakers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, institutional_speech_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, constitutional_textual_supremacy).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, original_public_meaning_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political majorities, institutional voices, and speakers aligned with dominant narratives gain maximal latitude to express views without legal liability. Speech restriction is categorically off the table except in a narrow doctrinal set (true threats, incitement to imminent lawless action, narrow historical carve-outs). No cost-benefit analysis applies to their speech; the constraint operates as an absolute floor.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_speakers, beneficiary,
    organized, generational, arbitrage, national).

% Historically marginalized groups (racial, religious, sexual minorities) bear the systemic cost when absolutist protection permits hate speech, slurs, and coordinated harassment campaigns targeting their identities and dignity. Under this reading, the harm they absorb is the price of categorical liberty—not an exception to the rule, but a declared externality. Exit from exposure requires physical relocation or complete social withdrawal from public discourse.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minorities, payer,
    powerless, generational, trapped, national).

% Media institutions, corporations, and state actors with resources to amplify speech (or silence it through platform control) operate under the same categorical protection as individual speakers. They collect the benefit of unrestricted expression capacity while their institutional power to distribute or suppress speech compounds the asymmetry: they can maximize their own speech reach while deciding what others' speech reaches.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, institutional_speech_actors, beneficiary,
    institutional, generational, arbitrage, national).

% Courts apply and maintain the absolutist reading by: (1) narrowly interpreting the historical exceptions (true threats, incitement) with high evidentiary bars, (2) rejecting new categorical restrictions (harassment, emotional distress, coordinated abuse) as inconsistent with the 'no law' principle, (3) striking down legislative attempts to add harm-based carve-outs. The judicial branch is both the administrator and the enforcer—it defines the boundaries of the categorical protection.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Legislatures attempt to restrict speech in response to constituent harm (hate-speech codes, harassment statutes, disinformation regulations) and are repeatedly overridden by courts applying the absolutist reading. They have formal authority to pass laws but not the power to make them stick—their exclusion is not from the conversation but from efficacy.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, legislative_bodies, excluded,
    institutional, generational, constrained, national).

% UN mechanisms, international courts, and foreign legal systems recognize hate-speech and harassment restrictions as consistent with human rights law and dignity protections. They observe the U.S. absolutist reading as an outlier—a reading that treats categorical speech protection as overriding other fundamental rights (dignity, equality, freedom from targeted violence). Their analysis is unheeded in domestic U.S. constitutional doctrine.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Constitutional scholars, empirical researchers on hate-speech effects, and interdisciplinary critics produce evidence that absolute speech protection correlates with measurable harms to targeted groups (health disparities, participation withdrawal from civic space, coordinated violence). Their work is available to courts but systematically rejected as inconsistent with the absolutist reading—harm data is not a legitimate input to the categoricality judgment.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, academic_critics, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, majority_speakers).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, transparent constitutional rule that prevents government from selectively silencing disfavored speakers or viewpoints. The categorical protection solves a genuine coordination problem: without a bright-line rule, governments (and courts) would inevitably weaponize 'harm' and 'offense' to suppress dissent. The function is genuine—it prevents arbitrary power.
% TRANSFER_FUNCTION: Transfers the cost of defending against harmful speech from speakers to the targets of that speech. Speakers retain maximal expressive capacity; targets absorb the psychic, social, and sometimes physical costs of exposure without legal recourse. The arrangement moves protection to one side of the speech dyad and leaves the other side exposed.
% ABSENT_VOICES: Targeted minorities most harmed by hate speech and harassment are structurally marginalized in doctrinal formation—their testimony about effects is treated as irrelevant to the constitutional question ('harm is not a First Amendment input'), and their interests are explicitly sacrificed as the price of categorical liberty. Legislators responding to constituent harm are also excluded from efficacy: they can testify, but courts override their judgments. International human-rights bodies and foreign legal systems offer competing frameworks but are excluded from U.S. constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If the absolutist reading disappeared—if courts adopted the harm_limited_reading and permitted narrow speech restrictions on proven hate-speech or coordinated harassment—government power to silence dissent would remain unchecked but would shift its tools. The coordination benefit (preventing arbitrary censorship via categorical rule) would vanish; legislatures and courts would have discretionary authority to restrict speech based on harm claims, and the political incentive to suppress disfavored voices would remain. The constraint's removal would not restore balance between speakers and targets; it would relocate power to judicial/legislative discretion. Institutional arrangements would reorganize around new restriction doctrines.
% FOUNDING_PROBLEM: The Framers' concern: government had historically weaponized sedition laws, blasphemy statutes, and licensing regimes to suppress political dissent and religious minorities. The founding problem was state censorship of inconvenient political speech, not private hate speech or coordinated abuse—those categories did not exist in the Framers' doctrinal universe.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars attest the founding problem is about preventing government censorship of political speech and cite Sedition Act prosecutions as evidence. Civil rights scholars and international human-rights bodies attest the founding problem is solved (modern democracies rarely prosecute political dissent) but the absolutist reading persists anyway, now functioning primarily to protect hate speech and coordinated harassment rather than political dissent. Historical analysts outside the originalist school note that hate-speech restrictions and harassment laws operate differently than sedition prosecutions—they target protected classes, not political power-holders—and thus do not recreate the founding problem. The corroboration splits: originalist and free-speech-maximalist seats attest the founding problem remains live; marginalized groups and international bodies attest it is dead but the reading persists theatrically.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(first_amendment_speech_protection__absolutist_reading),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as mountain because the absolutist reading appeals to constitutional text ('no law'), original public meaning, and categorical logic—all presented as natural law rather than policy choice. The metrics measure something different: how the constraint actually operates to extract costs from targeted minorities. Extractiveness (0.68) is high because the constraint systematically externalizes harm to a powerless, trapped group without their consent and without legal recourse. Suppression (0.71) is high because the reading survives by actively suppressing counter-readings (harm_limited_reading, categorical_balancing_reading) and by treating evidence of harm as categorically irrelevant to constitutional analysis—suppression of dissenting doctrine, not physical coercion. Theater (0.42) is moderate and rising because the reading justifies itself in categorical constitutional language while functioning to protect hate speech specifically—the gap between the stated function ('prevent government censorship') and the actual operation ('allow unfettered harm to minorities') is what theater measures. Accessibility (0.79) is high because once the absolutist reading is understood as protecting hate speech without remedy, targeted minorities recognize they have no legal alternative—the constraint collapses alternatives for them even though it preserves alternatives for speakers. Resistance (0.72) is high because the reading faces sustained empirical and moral challenge from scholars, international bodies, and harmed communities, yet persists via judicial enforcement. The measurement trajectory shows extraction accumulating (0.38→0.68) as digital communication enabled coordinated harassment at scale; suppression and theater both rising as the reading's gap from its founding function widened; the constraint is not eroding but intensifying its extractive operation while maintaining categorical justification.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (majority speakers, institutional actors, originalist judges), the constraint is a mountain—a necessary constitutional bedrock preventing government tyranny. From the payer seat (targeted minorities, marginalized groups), it is a snare—extraction masked as natural law, enforced suppression of remedy. From the excluded legislative seat, it is a rope they cannot enforce. The engine computes this divergence from the stakeholder structure: beneficiaries (organized, arbitrage exit) and payers (powerless, trapped) sit at opposite ends of the directionality spectrum, so their computed type classifications should diverge sharply. The constraint's persistence depends on the judicial seat's power to define what counts as 'speech' worthy of protection and what counts as 'harm' worthy of dismissal—the judiciary's control of interpretation is the enforcement mechanism that makes the asymmetry stick.
 *
 * DIRECTIONALITY LOGIC:
 *   The absolutist reading benefits majority speakers and institutional actors (low d toward target end) by granting them maximal expressive latitude without liability. It imposes costs on targeted minorities (high d toward target end) by externalizing harm they bear without legal remedy. Directionality derives from beneficiary/victim declarations: majority_speakers listed as beneficiaries (collect protection without bearing costs); targeted_minorities listed as victims (bear costs without legal recourse). The judiciary and institutional speech actors (powerful, arbitrage exit) sit near the beneficiary end despite their institutional power—they control the constraint's interpretation and can modify it if it served their interests. Targeted minorities (powerless, trapped, identity_locked via repeated exposure to the same harassers in public space) sit at the full-target end. The suppression metric is elevated because the reading's persistence depends on active judicial suppression of competing doctrines (harm_limited_reading) and suppression of harm-based evidence as a category of legitimate constitutional input.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading exhibits mandatrophy: the founding problem (government censorship of political dissent) is dead or substantially solved in modern democracies. Courts rarely prosecute political speech; sedition laws are defunct. Yet the absolutist reading persists, now functioning primarily to protect hate speech and coordinated abuse—a downstream problem the Framers did not contemplate because mass communication and coordinated digital harassment were not possible in 1791. The reading's mandate (prevent tyranny via government censorship) no longer matches its function (prevent remedy for hate-speech harm to minorities). The theater ratio's rise (0.25→0.42) reflects this mandate drift: courts maintain the absolutist reading via increasingly narrow interpretations of exceptions (true threats, incitement) while declaring harm to minorities irrelevant—they perform adherence to the original function while the actual operation has shifted. The reading could be reformed via doctrinal revision (harm_limited_reading) to address the actual problem, but the institutional incentive to maintain categorical language (simpliciter, appears neutral) and the beneficiary power to resist revision keeps the old mandate alive theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_choice,
    'Is the absolutist reading a natural law consequence of constitutional text and original meaning, or a constructed interpretive choice that benefits identifiable actors?',
    'Comparative constitutional analysis: if democracies with structurally similar free-speech protections (Canada, Germany, UK) consistently adopt harm_limited_reading or categorical_balancing_reading without legal instability, the absolutist reading is revealed as a choice, not a necessity. Alternatively, examine whether the reading''s beneficiaries (majority speakers, institutional actors) have institutional power to maintain it despite contrary evidence about founding-problem status.',
    'If constructed, the constraint reclassifies from mountain to tangled_rope (genuine coordination function of preventing arbitrary censorship, but asymmetric extraction of hate-speech harms). If natural, the constraint remains mountain but requires explaining why targeted minorities'' harm is not input to constitutional analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_choice, empirical, 'Whether the absolutist reading is constitutional necessity or beneficiary-maintained choice.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structural (legal barriers to counter-speech, judicial exclusion of harm evidence) or internalized (targeted minorities internalize the reading''s logic that harm is irrelevant, silence themselves preemptively)?',
    'Post-reading-reform trajectory: if the harm_limited_reading were adopted and narrowly permitted hate-speech restrictions, would suppression-measured resistance persist at high levels, or would it decline? Persistence would indicate internalization; decline would indicate structural suppression via judicial gatekeeping.',
    'If internalized, targeted minorities carry suppression with them even if the reading changed—the constraint''s effective suppression is higher than structural measures suggest. If structural, the suppression is localized to the judicial/doctrinal layer and could be rapidly unwound by doctrinal revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of counter-speech is structural or internalized in target populations.').

omega_variable(
    founding_problem_as_cover_story,
    'Is the appeal to the Framers'' concern about government censorship (founding_problem) a genuine constitutional principle or a cover story that legitimizes the reading without addressing its contemporary operation?',
    'Examine whether courts apply the founding-problem logic consistently: do they permit harm-based restrictions on speech from powerless groups (hate speech, harassment) while protecting speech from powerful groups (corporate speech, government speech)? If application is asymmetric (protecting some speakers more than others), the founding-problem justification is ex-post rationalization of a power-asymmetric outcome.',
    'If cover story, the constraint functions as a snare (pure extraction of hate-speech harm to minorities, masked by neutral constitutional language). If genuine principle, it remains mountain or tangled_rope depending on whether alternatives exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_as_cover_story, conceptual, 'Whether the founding-problem justification is principle or post-hoc rationalization.').

omega_variable(
    identity_lock_in_harmed_populations,
    'For targeted minorities (racial, religious, sexual minorities), is the identity_locked exit_option a mechanism of the constraint itself—does the constraint fuse their social identity to their status as harassment targets—or is it an external social fact?',
    'Examine whether identity-minority members who exit public discourse altogether (withdraw from social media, civic participation, journalism) experience suppression reduction. If suppression persists (internalized shame, erosion of civic sense of belonging), the constraint itself has constructed the identity-lock. If suppression drops, the lock was pre-existing and the constraint merely exacerbates exit.',
    'If the constraint constructs identity-lock, the effective extraction is higher than static measures suggest—it binds targets to the constraint via their most salient identity. If pre-existing, the constraint exploits but does not construct the vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_harmed_populations, empirical, 'Whether the constraint constructs identity-lock for harmed populations or exploits pre-existing vulnerability.').

omega_variable(
    coordinate_international_legal_divergence,
    'Why do other constitutional democracies (Canada, Germany, France, UK) permit narrower hate-speech and harassment restrictions without experiencing the state-censorship problems the absolutist reading claims to prevent?',
    'Institutional comparative analysis: examine whether jurisdictions with harm_limited_reading show higher rates of political-speech suppression, sedition prosecutions, or government censorship than the U.S. Results would falsify the claim that categorical protection is necessary to prevent government tyranny.',
    'If other democracies maintain rule of law and political freedom with harm_limited_reading, the absolutist reading is revealed as a doctrinal choice, not a constitutional necessity. The constraint would reclassify from mountain to tangled_rope or snare depending on whether the choice serves legitimate coordination or primarily benefits speakers at targets'' expense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinate_international_legal_divergence, empirical, 'Whether categorical absolutism is necessary for constitutional protection or a doctrinal choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(firs_tr_t0, observed).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__absolutist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(firs_tr_t10, observed).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__absolutist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(firs_tr_t20, observed).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__absolutist_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement_basis(firs_tr_t30, observed).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__absolutist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(firs_tr_t40, observed).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__absolutist_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(firs_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(firs_be_t0, observed).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(firs_be_t10, observed).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(firs_be_t20, observed).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement_basis(firs_be_t30, observed).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(firs_be_t40, observed).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(firs_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(firs_su_t0, observed).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(firs_su_t10, observed).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(firs_su_t20, observed).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(firs_su_t30, observed).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(firs_su_t40, observed).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(firs_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% The first_amendment_speech_protection kernel decomposes into three constraint stories: absolutist_reading (this file), categorical_balancing_reading, and harm_limited_reading. Each instantiates a different reading of the same constitutional text with substantially different ε values and beneficiary/victim structures. The absolutist reading claims naturality (mountain) while measuring extractive operation (0.68); it benefits majority speakers and externalizes harm to targeted minorities. The categorical_balancing_reading (downstream) balances speech value against competing interests case-by-case. The harm_limited_reading (downstream) permits narrow restrictions on demonstrably harmful speech. The three readings coexist across different judicial coalitions and doctrinal schools; the absolutist reading influences the other two by setting the presumptive doctrinal baseline (reading away from which must be justified). ε divergence: absolutist (0.68, extractive operation masking as natural law) vs. categorical_balancing (~0.45, genuine coordination with partial recognition of harm) vs. harm_limited (~0.35, coordination with harm-based remedy available). Beneficiary structures: absolutist concentrates benefits on majority speakers and institutional actors; categorical_balancing distributes asymmetrically to speakers with stronger competing interests; harm_limited balances speaker and target interests more symmetrically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__absolutist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
