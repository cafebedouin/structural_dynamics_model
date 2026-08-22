% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: First Amendment Speech Protection - Categorical Balancing Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The standing arrangement under contest: federal courts administer speech
 *   protection by sorting expression into protected and unprotected
 *   categories whose boundaries are drawn and redrawn case-by-case through
 *   weighing expressive value against asserted harms. Exclusionary categories
 *   (obscenity, incitement, true threats, and their successors) are
 *   judicially maintained artifacts; nominally protected speech remains
 *   subject to losing the weighing when a sufficiently weighty countervailing
 *   interest appears. The arrangement concentrates interpretive authority in
 *   the judiciary, hands regulators a workable vocabulary for surviving
 *   challenge, and places outcome risk on speakers, with the heaviest
 *   incidence on dissidents and minorities whose expression sits nearest the
 *   boundaries. Family note (decomposition documented per the
 *   epsilon-invariance principle): this file is one of three linked readings
 *   of the same ratified text; the sibling stories carry their own epsilon,
 *   beneficiary/victim structure, and classification, and the network edges
 *   record the family. KEY AGENTS (by structural relationship): -
 *   institutional_judiciary: Agenda-setting beneficiary
 *   (institutional/identity_locked) - defines and administers the categories,
 *   collects interpretive authority - government_speech_regulators: Secondary
 *   beneficiary (institutional/mobile) - obtains a defensible framework for
 *   restricting disfavored expression - dissenting_minority_speakers: Primary
 *   target (powerless/trapped) - bear outcome risk of the weighing -
 *   mass_media_organizations: Adapted payer with partial benefit
 *   (powerful/mobile) - bear costs, harvest repeat-player advantage -
 *   ordinary_citizen_speakers: Target (powerless/trapped) - experience
 *   protection as unpredictable incident to incident - legal_predictability:
 *   Non-agent bearer (listed for completeness) - the rule-of-law interest
 *   eroded by case-by-case outcomes - nonlitigating_public: Excluded seat
 *   (moderate/constrained) - affected but outside the conversation -
 *   constitutional_scholars: Analytical observer (analytical/analytical) -
 *   maps the doctrine's movement
 *
 * KEY AGENTS:
 *   - institutional_judiciary: agenda-setting beneficiary (institutional/identity_locked) - defines categories, collects interpretive authority
 *   - government_speech_regulators: secondary beneficiary (institutional/mobile) - drafts and defends restrictions within the framework
 *   - dissenting_minority_speakers: primary payer (powerless/trapped) - expression nearest the boundaries, least able to litigate
 *   - mass_media_organizations: adapted payer with secondary benefit (powerful/mobile) - repeat players who shape the record
 *   - ordinary_citizen_speakers: payer (powerless/trapped) - anticipatory restraint without litigation capacity
 *   - legal_predictability: non-agent bearer (agent: false) - rule-of-law interest in ex ante knowability
 *   - nonlitigating_public: excluded seat (moderate/constrained) - affected audiences outside the courtroom conversation
 *   - constitutional_scholars: analytical observer (analytical/analytical) - critiques the method, supplies rival frameworks' arguments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.6).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.5).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Speech Protection - Categorical Balancing Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '47f1ddda-0d70-4701-bf23-277c4e7a5465').
narrative_ontology:cs_kernel_codification('47f1ddda-0d70-4701-bf23-277c4e7a5465', fixed_text).
narrative_ontology:cs_authority_grounding('47f1ddda-0d70-4701-bf23-277c4e7a5465', lineage).
narrative_ontology:cs_interpretation_layer_present('47f1ddda-0d70-4701-bf23-277c4e7a5465').
narrative_ontology:cs_reading_relation('47f1ddda-0d70-4701-bf23-277c4e7a5465', first_amendment_speech_protection__first_amendment_absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('47f1ddda-0d70-4701-bf23-277c4e7a5465', first_amendment_speech_protection__first_amendment_harm_limited_reading, influences).
narrative_ontology:cs_axiom('47f1ddda-0d70-4701-bf23-277c4e7a5465', foundational, protection_is_graduated_by_value_and_harm).
narrative_ontology:cs_axiom_status(protection_is_graduated_by_value_and_harm, holdable).
narrative_ontology:cs_axiom_grounding('47f1ddda-0d70-4701-bf23-277c4e7a5465', protection_is_graduated_by_value_and_harm, instrumental).
narrative_ontology:cs_axiom('47f1ddda-0d70-4701-bf23-277c4e7a5465', secondary, judicial_line_drawing_is_legitimate).
narrative_ontology:cs_axiom_status(judicial_line_drawing_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('47f1ddda-0d70-4701-bf23-277c4e7a5465', judicial_line_drawing_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('47f1ddda-0d70-4701-bf23-277c4e7a5465', court_administered_speech_categories).
narrative_ontology:cs_drift_state('47f1ddda-0d70-4701-bf23-277c4e7a5465', contemporary_doctrine, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47f1ddda-0d70-4701-bf23-277c4e7a5465', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, government_speech_regulators).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, dissenting_minority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, ordinary_citizen_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, mass_media_organizations).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, mass_media_organizations).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, judicial_interpretive_supremacy).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, case_by_case_balancing_methodology).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, categorical_exclusion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines which kinds of expression fall outside protection (obscenity, incitement, true threats, and successor categories) and decides, appeal by appeal, how expressive value weighs against asserted harms. Collects interpretive authority: every novel speech conflict routes to its docket, and its formulations bind every lower court. Leaving that role would mean renouncing the institution's self-understanding as guardian of expressive rights; no external forum offers an equivalent function.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, beneficiary).

% Executive agencies, prosecutors, and legislative counsel who draft and defend speech restrictions. The category framework gives them a recognized vocabulary - compelling interests, tailoring, defined exclusions - within which restrictions can survive challenge, and lets them select enforcement targets with a working sense of which expression the courts will tolerate regulating. They spend real resources litigating defenses, but on net the framework expands what they may attempt.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, government_speech_regulators, beneficiary,
    institutional, biographical, mobile, national).

% Protesters, dissidents, and members of unpopular movements whose expression most often lands near the category boundaries. Whether their speech is protected depends on how a future panel weighs its value against someone's asserted harm. Few can fund appeals; the practical choice is between self-censorship and gambling on litigation they cannot afford. Exiting the regime would mean leaving the jurisdiction or falling silent.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, dissenting_minority_speakers, payer,
    powerless, biographical, trapped, national).

% Large publishers, broadcasters, and platforms that face the same doctrinal uncertainty but employ specialist counsel and litigate repeatedly. Recurrence gives them advantages under a case-by-case system: they shape the record, accumulate favorable precedent, and can price legal risk into operations. They pay compliance and litigation costs, and they benefit when rivals' or outsiders' expression is the kind that loses the weighing.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, mass_media_organizations, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, mass_media_organizations, beneficiary).

% People whose everyday expression brushes the boundaries - students, employees, online posters - and who experience protection as unpredictable from incident to incident. They almost never litigate; the operative cost is anticipatory restraint. Exit means withdrawing from the forums where the speech occurs.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, ordinary_citizen_speakers, payer,
    powerless, immediate, trapped, national).

% The rule-of-law interest in knowing in advance what expression is punishable. Case-by-case evaluation makes that knowledge unavailable in principle: each dispute is decided by a weighing whose outcome cannot be stated as a rule ex ante. Listed for completeness; as a non-agent it bears erosion without acting, collects nothing, and cannot advocate.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

% Audiences and would-be speakers affected by the boundaries who never appear before any court: the conversation that fixes what may be said happens among judges, litigants, and regulators. They would press for bright-line rules they can rely on without counsel; their access runs through occasional amicus participation and through voting for presidents who appoint justices.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, nonlitigating_public, excluded,
    moderate, biographical, constrained, national).

% Academic commentators who map the doctrine's movement, publish critiques of the weighing method, and supply the arguments rival frameworks use. Analytical seat: collects citations and influence, pays nothing, enforces nothing.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable decision procedure for a continuing stream of speech disputes: sorting the enormous variety of expression into administrable categories lets courts decide cases without resolving the underlying philosophy, and lets lower courts process speech litigation at scale instead of relitigating first principles in every case.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal discretion from speakers and legislatures to the judiciary; moves outcome risk and litigation cost onto speakers, with heaviest incidence on dissidents and minorities; delivers regulatory latitude to government in the cases where the asserted interest outweighs the speech's assigned value.
% ABSENT_VOICES: Nonlitigating speakers and audiences - the people whose expression is chilled or punished but who never reach an appellate court - are structurally outside the conversation that fixes the boundaries; so are textualist critics whose framework the bench-centered process has no slot for. They surface only intermittently as amici or through the indirect channel of judicial appointments.
% DISAPPEARANCE_RATIONALE: If the categorical balancing arrangement vanished overnight, a century of precedent built on its categories would need reinterpretation under whichever successor framework captured the field; pending speech litigation would stall; regulators would lose the vocabulary their restrictions survive on; and speakers' practical risk calculations would reset around the new rule structure. Thousands of settled dispositions depend on it.
% FOUNDING_PROBLEM: Early twentieth-century crisis: wartime sedition prosecutions forced courts to distinguish legitimate restriction of dangerous expression from persecution of dissent, without either rubber-stamping the state or paralyzing it. The arrangement was built to give judges a method for that distinction.
% FOUNDING_PROBLEM_CORROBORATION: Civil-liberties organizations, First Amendment scholars across the ideological spectrum, and the legislators who keep drafting speech laws all attest the underlying problem - reconciling expressive liberty with concrete harms - remains live; their criticism targets the judicial answer, not the existence of the problem. Corroboration from outside the beneficiary set is abundant.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.60: the arrangement delivers strong protection to core political speech while concentrating real burdens at the margins - dissident expression loses the weighing disproportionately, and the entire governed population carries the cost of not knowing in advance which side of a future weighing its speech will land on. Suppression 0.50: stare decisis binds every lower court to the category framework and speakers cannot contract out, but the rival readings remain live in scholarship and dissenting opinions, so alternatives are constrained rather than extinguished. Theater_ratio 0.40: a substantial share of balancing rhetoric functions as post-hoc dignification of preferred outcomes (the long-standing critique that announced factors fail to predict results), while genuine deliberative work persists in a large fraction of cases. Accessibility_collapse 0.35: understanding the regime does not close off alternatives - the sibling readings remain articulable and periodically influential - which is characteristic of a construct that must be defended rather than a limit that simply obtains. Resistance 0.55: continuous scholarly critique, recurring dissenting opinions, legislative frustration with doctrinal indirection, and periodic proposals for rule-like reform. The temporal series run on one shared grid (T=0,18,36,54,72,90,106) with every tracked metric authored at every point. The suppression series is deliberately non-monotonic: enforcement intensity spiked in the wartime and red-scare cycles (T=0 and T=36), dropped after the mid-century liberalization of the categories (T=54), and has slowly re-hardened since. The oscillation tracks external crisis cycles, and the ratchet is part of the extraction mechanism: categories expanded during crises (syndicalism, subversion) persist after the crisis passes, so each cycle leaves the unprotected set larger than the last. Base extractiveness rises modestly and monotonically across the same span - rent layered onto a functioning decision procedure - and theater climbs steadily as the number of announced multi-factor tests outgrows their predictive work.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the agenda-setting seat the arrangement is the only workable craft: hard speech cases exist, someone must decide them, and case-sensitive judgment beats both rubber-stamping the state and paralysis. From the trapped speaker seats the same structure operates as roulette in which protection depends on how a future panel values their particular message - the minority protester and the ordinary poster cannot price the risk, so they discount it by staying silent. The regulator seat experiences opportunity: a recognized vocabulary in which restrictions survive. The scholar seat sees methodological drift the bench does not officially acknowledge. Coalition capacity among the payer seats is real but thin: minority speakers and ordinary citizens share an interest in bright-line protection, but resource asymmetry and the media seat's partial adaptation prevent a durable coalition from forming. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary declares as agenda-setter and beneficiary with identity-locked exit: it collects the interpretive authority the arrangement generates and cannot leave its own role, placing it near the full-beneficiary end. Government speech regulators declare as beneficiaries with mobile exit: the framework expands what they may attempt, though they expend resources defending it, keeping them near but not at the beneficiary pole. Dissenting minority speakers and ordinary citizen speakers declare as victims with trapped exit: they bear the transfer and cannot exit the jurisdiction's speech regime, placing them near the full-target end. Mass media organizations are listed among victims but hold a secondary beneficiary position and mobile exit; the structural derivation from victim-listing alone would read them as near-full targets, so an explicit override sets d to 0.55 - they pay compliance and litigation costs while harvesting repeat-player advantage under a case-by-case system. Legal predictability is authored as a non-agent bearer: it is excluded from directionality derivation by design, since an abstraction must not feed the arithmetic as if it collected or paid. Larger national scope modestly amplifies effective extraction by making the weighing harder to verify from outside the courtroom.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy declaration: the founding problem - adjudicating speech-restriction conflicts without either rubber-stamping the state or paralyzing it - remains live, regenerated by every new communications technology. The tangled_rope claim prevents two symmetric misreadings. Reading the arrangement as pure extraction ignores the real coordination function: any legal system facing a continuing stream of speech disputes needs a decision procedure, and this one processes the docket at scale while extending substantial protection to core expression. Reading it as pure coordination ignores the concentrated interpretive rent (every novel conflict routes to the judiciary's docket and its formulations bind everyone) and the predictable losers the procedure generates. The theater series' slow climb is the early-warning signature worth watching: if the weighing degenerates fully into rationalization while the founding problem migrates to other institutions, the arrangement drifts toward inertial performance - a former decision procedure maintained because the institution cannot imagine relinquishing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the categorical_balancing_reading of the first_amendment_speech_protection kernel; what structural differences would the absolutist_reading and harm_limited_reading instantiate over the same standing arrangement?',
    'Comparative classification of the three sibling stories: identical referent (the standing speech-protection arrangement), reading-indexed epsilon and beneficiary/victim sets; the divergences locate the disagreement.',
    'Under the absolutist reading the judiciary loses discretionary category administration (its beneficiary position shrinks toward clerking a short list of historical exclusions) and predictability rises sharply; under the harm_limited reading the beneficiary set shifts toward demonstrably harmed parties and epsilon tracks empirical harm evidence rather than judicial valuation of speech.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one of three live readings of the First Amendment speech-protection kernel.').

omega_variable(
    disagreement_location_boundary_source,
    'Where exactly is the kernel disagreement located: in the source of the protected/unprotected boundary (judicial valuation of speech versus text and history versus demonstrated harm)?',
    'Run the three readings'' axiom sets against the same canonical case outcomes; the element on which verdicts diverge is the boundary-drawing authority.',
    'Downstream speech-regulation constraints inherit whichever boundary-source prevails; category-based analysis (obscenity, incitement, true threats) is structurally valid only under this reading and would dissolve under the absolutist sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_boundary_source, conceptual, 'Locates the inter-reading disagreement in boundary-drawing authority rather than in the value of speech protection itself.').

omega_variable(
    balancing_sincerity,
    'Is the announced case-by-case weighing a genuine deliberative procedure, or is a substantial share of it post-hoc rationalization of outcomes reached on other grounds?',
    'Systematic coding of opinions: do the stated balancing factors statistically predict outcomes better than panel composition or independently observable outcome preferences?',
    'A high rationalization share drives theater_ratio toward piton-drift territory and weakens the coordination-function claim; demonstrated predictive force of the stated factors supports treating the weighing as real coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_sincerity, empirical, 'Sincerity of the balancing procedure versus result-driven rationalization.').

omega_variable(
    category_net_effect_on_minorities,
    'Does the category system, on net, shelter or expose minority and dissident speech across the full historical record?',
    'Historical cohort analysis: protection rates for dissident and minority speech across crisis and calm periods under the categorical regime, compared against periods and jurisdictions operating nearer the sibling frameworks.',
    'If net exposure, minority speakers sit nearer full-target directionality and the victim declaration strengthens; if net shelter, part of their measured burden is misattributed and the coordination function widens correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_net_effect_on_minorities, empirical, 'Net directional effect of judicial categories on the speakers with least exit.').

omega_variable(
    judiciary_identity_lock_mechanism,
    'Is the judiciary''s maintenance of interpretive control sustained by institutional identity fusion (the Court as guardian of enumerated rights) such that surrendering category-definition would be unthinkable regardless of incentive?',
    'Analysis of internal deliberation records, concurrences resisting clear-rule proposals, and behavior in cases where a per se rule was available and declined.',
    'Identity lock sustains persistence independent of benefit flow, raising piton-resistance if the coordination function decays; if the frame broke, doctrinal simplification toward rule-like protection becomes institutionally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_identity_lock_mechanism, conceptual, 'Whether the agenda-setting seat''s persistence rests on identity fusion rather than net benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 0, 106).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(firs_tr_t0, observed).
narrative_ontology:measurement(firs_tr_t18, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 18, 0.16).
narrative_ontology:measurement_basis(firs_tr_t18, observed).
narrative_ontology:measurement(firs_tr_t36, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 36, 0.26).
narrative_ontology:measurement_basis(firs_tr_t36, observed).
narrative_ontology:measurement(firs_tr_t54, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 54, 0.31).
narrative_ontology:measurement_basis(firs_tr_t54, observed).
narrative_ontology:measurement(firs_tr_t72, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 72, 0.36).
narrative_ontology:measurement_basis(firs_tr_t72, observed).
narrative_ontology:measurement(firs_tr_t90, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 90, 0.39).
narrative_ontology:measurement_basis(firs_tr_t90, observed).
narrative_ontology:measurement(firs_tr_t106, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 106, 0.4).
narrative_ontology:measurement_basis(firs_tr_t106, observed).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(firs_be_t0, observed).
narrative_ontology:measurement(firs_be_t18, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 18, 0.38).
narrative_ontology:measurement_basis(firs_be_t18, observed).
narrative_ontology:measurement(firs_be_t36, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 36, 0.52).
narrative_ontology:measurement_basis(firs_be_t36, observed).
narrative_ontology:measurement(firs_be_t54, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 54, 0.47).
narrative_ontology:measurement_basis(firs_be_t54, observed).
narrative_ontology:measurement(firs_be_t72, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 72, 0.53).
narrative_ontology:measurement_basis(firs_be_t72, observed).
narrative_ontology:measurement(firs_be_t90, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 90, 0.57).
narrative_ontology:measurement_basis(firs_be_t90, observed).
narrative_ontology:measurement(firs_be_t106, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 106, 0.6).
narrative_ontology:measurement_basis(firs_be_t106, observed).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(firs_su_t0, observed).
narrative_ontology:measurement(firs_su_t18, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 18, 0.56).
narrative_ontology:measurement_basis(firs_su_t18, observed).
narrative_ontology:measurement(firs_su_t36, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 36, 0.6).
narrative_ontology:measurement_basis(firs_su_t36, observed).
narrative_ontology:measurement(firs_su_t54, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 54, 0.44).
narrative_ontology:measurement_basis(firs_su_t54, observed).
narrative_ontology:measurement(firs_su_t72, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 72, 0.46).
narrative_ontology:measurement_basis(firs_su_t72, observed).
narrative_ontology:measurement(firs_su_t90, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 90, 0.48).
narrative_ontology:measurement_basis(firs_su_t90, observed).
narrative_ontology:measurement(firs_su_t106, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 106, 0.5).
narrative_ontology:measurement_basis(firs_su_t106, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_harm_limited_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'First Amendment speech protection' into three reading-constraints per the epsilon-invariance principle: first_amendment_absolutist_reading (text-fixed protection, minimal judicial discretion), this file (judicially administered categories via case-by-case weighing), and first_amendment_harm_limited_reading (protection yields to demonstrated unconsented harm). Each carries its own epsilon, beneficiary/victim structure, and classification over the same standing arrangement; the shared kernel is the ratified text. The balancing apparatus supplies the doctrinal terrain - tiers of scrutiny, category tests, interest-weighing vocabulary - within which harm-limited arguments operate, hence the influences edge to that sibling; the absolutist sibling competes as a rival methodology without logical elimination, hence coexists_with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__categorical_balancing_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
