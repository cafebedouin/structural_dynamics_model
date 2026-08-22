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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Reading — Categorical Speech Protection ('No Law Means No Law')
 *   domain: constitutional law / political philosophy / speech regulation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel:
 *   first_amendment_speech_protection, the fixed 1791 text ('Congress shall
 *   make no law... abridging the freedom of speech, or of the press'). The
 *   absolutist reading holds that 'no law' means no law — protection is
 *   categorical except for a narrow, historically fixed set of exclusions.
 *   The arrangement this reading puts in place maximizes the protected speech
 *   set: every speaker inside the protected set receives near-total immunity
 *   from state restriction, and the costs of that immunity — exposure to
 *   protected vilification, harassment, and recruitment propaganda, plus the
 *   foreclosure of protective regulation — fall on targeted minorities,
 *   harassed women, and state legislatures. Per the epsilon-referent rule,
 *   extractiveness below is authored for THIS arrangement (the
 *   categorical-protection regime) as THIS reading assesses it: the reading
 *   concedes the externalized harms are real and growing and defends them as
 *   the price of liberty rather than denying them, which is why epsilon is
 *   substantial yet defended. The sibling readings (harm_limited_reading,
 *   categorical_balancing_reading) are separate constraint stories and are
 *   not described or averaged into this one; this file links them through
 *   network and cs_structure only. Assumption stated: measurements track the
 *   categorical arrangement as actually instantiated in US doctrine and
 *   practice, which approximates this reading more closely than either
 *   sibling.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter (institutional / identity_locked) — administers the categorical line and strikes down restrictions; its authority over the speech boundary depends on the arrangement it enforces
 *   - political_dissidents_and_unpopular_speakers: primary beneficiary (moderate / constrained) — the paradigm protected class
 *   - mass_media_and_publishers: beneficiary (powerful / arbitrage) — operates the circulation machinery the immunity enables
 *   - majoritarian_speech_community: beneficiary (powerful / arbitrage) — receives the same immunity with its prevailing norms insulated from dignity-based regulation
 *   - targeted_racial_and_religious_minorities: primary payer (organized / trapped) — bears the externalized oppression costs; dual-positioned as protected speakers
 *   - women_targeted_by_protected_harassment: payer (moderate / constrained) — bears protected threat and harassment costs
 *   - state_legislatures: payer (institutional / constrained) — bears the standing foreclosure of protective regulatory capacity
 *   - international_human_rights_monitors: analytical observer (institutional / analytical) — assesses the arrangement against comparative and treaty norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.62).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.65).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Reading — Categorical Speech Protection ('No Law Means No Law')").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional law / political philosophy / speech regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '7412012e-c545-4185-be12-763de1faefe7').
narrative_ontology:cs_kernel_codification('7412012e-c545-4185-be12-763de1faefe7', fixed_text).
narrative_ontology:cs_authority_grounding('7412012e-c545-4185-be12-763de1faefe7', lineage).
narrative_ontology:cs_interpretation_layer_present('7412012e-c545-4185-be12-763de1faefe7').
narrative_ontology:cs_reading_relation('7412012e-c545-4185-be12-763de1faefe7', first_amendment_speech_protection__categorical_balancing_reading, forecloses).
narrative_ontology:cs_reading_relation('7412012e-c545-4185-be12-763de1faefe7', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('7412012e-c545-4185-be12-763de1faefe7', foundational, no_law_means_no_law).
narrative_ontology:cs_axiom_status(no_law_means_no_law, holdable).
narrative_ontology:cs_axiom_grounding('7412012e-c545-4185-be12-763de1faefe7', no_law_means_no_law, conventional).
narrative_ontology:cs_axiom('7412012e-c545-4185-be12-763de1faefe7', foundational, historical_exclusions_exhaustive).
narrative_ontology:cs_axiom_status(historical_exclusions_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('7412012e-c545-4185-be12-763de1faefe7', historical_exclusions_exhaustive, conventional).
narrative_ontology:cs_axiom('7412012e-c545-4185-be12-763de1faefe7', secondary, counterspeech_sufficient_remedy).
narrative_ontology:cs_axiom_status(counterspeech_sufficient_remedy, holdable).
narrative_ontology:cs_axiom_grounding('7412012e-c545-4185-be12-763de1faefe7', counterspeech_sufficient_remedy, empirically_contingent).
narrative_ontology:cs_reference_frame('7412012e-c545-4185-be12-763de1faefe7', categorical_no_law_baseline).
narrative_ontology:cs_drift_state('7412012e-c545-4185-be12-763de1faefe7', contemporary_balancing_doctrine, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7412012e-c545-4185-be12-763de1faefe7', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, political_dissidents_and_unpopular_speakers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, mass_media_and_publishers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majoritarian_speech_community).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_racial_and_religious_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, women_targeted_by_protected_harassment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, targeted_racial_and_religious_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, state_legislatures).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, viewpoint_neutrality_principle).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, content_neutrality_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the fixed 1791 text and strikes down speech restrictions falling outside the narrow historical exclusions. Its institutional authority over the speech boundary depends on the categorical line it enforces, and it cannot abandon that role without dissolving its own claim to neutral guardianship — the institution has become the arrangement's administrator.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Pamphleteers, protesters, whistleblowers, and unpopular religious and political movements. They receive near-total immunity from state restriction, and their dissent is precisely the speech the categorical rule shields. Withdrawing from public speech or leaving the polity forfeits the protection's point, so exit is available but self-defeating.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, political_dissidents_and_unpopular_speakers, beneficiary,
    moderate, biographical, constrained, national).

% Operates the circulation machinery — presses, broadcasts, platforms — through which protected speech moves at scale. The immunity lets it publish without pre-clearance; it can restructure, offshore, or re-platform in ways individual speakers cannot, giving it the most maneuverable seat under the arrangement.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, mass_media_and_publishers, beneficiary,
    powerful, generational, arbitrage, global).

% The broad class of speakers whose expression aligns with prevailing norms. They receive the same categorical immunity as dissidents, and their prevailing norms are insulated from dignity-based regulation, since regulation aimed at majority expressive norms is what the categorical rule most reliably forecloses. They can additionally shape platform rules and social pressure outside law.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majoritarian_speech_community, beneficiary,
    powerful, generational, arbitrage, national).

% Bear the arrangement's externalized costs: racial and religious vilification, recruitment propaganda, and coordinated harassment directed at them are protected speech they cannot suppress by law. They cannot exit the polity where the speech circulates. Their members are simultaneously protected speakers whose own dissent the arrangement shields, but that protection cannot be converted into legal defense from the speech that targets them.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_racial_and_religious_minorities, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__absolutist_reading, targeted_racial_and_religious_minorities, beneficiary).

% Bear the arrangement's costs as threats, sexual harassment, and coordinated abuse that fall outside the narrow exclusions. Their remedies are withdrawal from public discourse, private platform rules, or the tort lanes history happens to have carved out; state protective regulation is foreclosed, and exit from public discourse carries its own professional and civic costs.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, women_targeted_by_protected_harassment, payer,
    moderate, biographical, constrained, national).

% Periodically enact — or draft and attempt — the speech-protective regulation their constituents demand: hate-speech codes, harassment statutes, disinformation rules. They see these struck down. They bear the arrangement as a standing limit on regulatory capacity; their recourse is constitutional amendment, which is practically unavailable.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, state_legislatures, payer,
    institutional, generational, constrained, national).

% Assess the arrangement against comparative and treaty norms — peer democracies that restrict group vilification, ICCPR obligations — and publish findings that bind no domestic seat. They take no part in the arrangement and cannot be restricted by it.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, international_human_rights_monitors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible-commitment problem of state speech suppression: by making protection categorical and judicially enforced, it removes the government's ability to decide which viewpoints survive, protecting dissent from the majoritarian and bureaucratic incentives that licensing and seditious-libel regimes historically served. One fixed rule replaces per-restriction adjudication.
% TRANSFER_FUNCTION: Moves legal immunity from state restriction to every speaker inside the protected set, and moves the arrangement's costs — exposure to protected vilification and harassment, plus the loss of protective regulatory capacity — to targeted minorities, harassed women, and state legislatures.
% ABSENT_VOICES: The seats that would trade speech immunity for dignitary protection — targeted minorities seeking enforceable hate-speech regulation, harassed women seeking statutory recourse — are present only as litigants asking the judiciary to abandon the reading; the arrangement is built so their preference cannot win inside it. Historically, the enslaved and the disenfranchised were absent from the founding bargain entirely: the protected speech set was drawn without them, and their successors inherited its costs.
% DISAPPEARANCE_RATIONALE: If categorical protection vanished overnight, legislatures would move within a session to enact the hate-speech, harassment, and disinformation regimes they have repeatedly drafted; platforms and employers would recalibrate moderation against state backstops; and political dissent would face case-by-case risk under whichever coalition held power. The speech economy would reorganize around balancing — which is the sibling readings' arrangement.
% FOUNDING_PROBLEM: State punishment of political dissent: seditious libel, licensing, and prior restraint — the English and colonial practice of letting the sovereign decide which criticism of government could be printed. The 1798 Sedition Act prosecutions were the founding problem made flesh within a decade of the text.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the historical record of the Alien and Sedition Acts, the WWI Espionage Act prosecutions (Debs), and the loyalty-screening era is documented by historians and in the judicial record itself; comparative politics documents the dissent-suppression problem in every peer state lacking a categorical rule. No seat disputes that the founding problem existed; the live dispute is whether it still justifies the full categorical scope.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) is authored as this reading's own concession structure: categorical protection externalizes real, asymmetric, growing costs onto targeted groups — mass press broadened exposure, and platform-scale coordinated harassment raised it sharply — which the reading defends as liberty's price rather than denies; the rising series tracks the growing scale of externalized harm, not any change in the text. Suppression (0.65) is structural: the arrangement persists only by actively striking down the protective legislation the payer seats repeatedly attempt, so the constraint suppresses exactly the alternatives its victims prefer. Theater is low (0.18): the arrangement functionally operates — courts actually invalidate laws — though ceremonial invocation is rising in platform-era debates. Accessibility collapse is moderate (0.5): once the categorical rule is understood, state-protective alternatives collapse for the payer seats while counterspeech, private moderation, and private-ordering exits remain partly available. Resistance is substantial (0.6): hate-speech regulation movements, treaty-body criticism, and recurring legislative attempts. All three tracked metrics run on ONE shared grid (1791, 1798, 1919, 1942, 1964, 1969, 2003, 2026). Suppression_requirement is authored because this story specifically tracks enforcement-capacity history: the arrangement went from essentially unenforced (the 1798 Sedition Act era, when the categorical rule was overridden in practice) through embryonic enforcement (1919 dissents) to peak judicial enforcement (1964-1969) to actively contested enforcement today, when each new legislative attempt must be struck down anew.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the federal judiciary's seat the arrangement is the neutral guardianship it administers: categorical protection reads as the text's plain meaning, and the carve-outs other seats experience as costs read as fidelity. From the dissident and media seats the arrangement is near-pure subsidy: immunity without a bill. From the targeted-minority and harassed-women seats the same structure operates as enforced exposure: the state is disabled from protecting them precisely where harm concentrates. State legislatures experience it as a standing foreclosure of capacity their constituents demand. The engine computes these per-seat classifications from the structural data; the divergence between the judiciary's guardianship experience and the payer seats' exposure experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map directly: dissidents, media, and the majoritarian speech community sit near the beneficiary end — the arrangement subsidizes their speech with no offsetting bill; the media and majoritarian seats carry arbitrage-grade exit, placing them nearest the subsidy pole. The victim declarations place targeted minorities and harassed women near the full-target end: the arrangement's costs concentrate on them, their exit is trapped (the speech circulates where they live), and their secondary beneficiary role does not offset the exposure because the protection they receive as speakers cannot be converted into protection from the speech that targets them. State legislatures bear foreclosed capacity and sit high despite institutional power. The judiciary is the one seat the derivation handles coarsely: as agenda-setter it neither pays the costs nor receives the protection — it collects administrative authority from the arrangement, placing it near-symmetric with a beneficiary tilt. No per-atom override is authored because the override axis (power_atom) cannot separate the judiciary from the same-atom legislatures without misstating one of them; the role declarations carry the differentiation instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state punishment of political dissent — is live, corroborated by the historical record (Alien and Sedition Acts, Espionage Act prosecutions, loyalty-screening era) and by comparative politics, from outside the benefiting parties, so this is not a mandatrophy case: the arrangement's coordination function has not outlived its justification. The tangled_rope classification does the anti-mislabeling work in both directions: the genuine, externally corroborated coordination function (credible commitment against viewpoint-based suppression, dissident protection) blocks a snare reading despite real victims; the declared victims and their trapped exit block a rope reading despite real coordination. The diffuse gain flow — no single seat captures the protection; it accrues across the whole speaker class including minority dissidents — further separates this from capture. The receipt cell this story occupies (diffuse gains, prohibitive fixing) is read against the piton template and rejected: fixing is prohibitive (Article V threshold, entrenched doctrine, identity fusion with national self-concept), but the arrangement is actively maintained by a living agenda-setter against live resistance with a live founding problem — it is not inertial and not mostly performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the first_amendment_speech_protection kernel — what would change structurally if a sibling reading (harm_limited_reading, categorical_balancing_reading) were adopted instead?',
    'Comparative analysis against the sibling stories in this kernel family: the sibling files author the same seats under yield-on-harm and case-by-case-balancing arrangements; the protected-set size, victim set, and foreclosure structure shift accordingly.',
    'Under either sibling, the protected speech set contracts, targeted minorities gain actionable protection (the victim structure weakens or inverts), and state legislatures recover regulatory capacity — this story''s structural profile would move toward rope (if costs become internalized to the coordination) or toward scaffold (if the balancing arrangement is declared transitional).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption changes the protected-set boundary and the victim structure.').

omega_variable(
    externalization_vs_price_of_liberty,
    'Are the harms borne by targeted minorities an extraction this arrangement imposes on them, or a cost the reading may legitimately externalize as the price of liberty?',
    'Not resolvable by data alone: it turns on the weight a polity assigns dignitary and equality interests against speech immunity — a values question the three readings answer differently. Partially informed by the counterspeech_efficacy omega: if the reading''s remedy assumption fails empirically, the externalization loses its defense.',
    'Resolving toward ''extraction'' pushes the arrangement toward snare (coordination story as cover); resolving toward ''legitimate price'' pushes toward rope. This is the exact structural location of the disagreement among the kernel''s readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_vs_price_of_liberty, preference, 'The preference-level ambiguity at the kernel''s core: whether externalized harm counts as extraction.').

omega_variable(
    narrow_exclusions_boundary,
    'Where exactly does the ''narrow historical exclusions'' boundary sit — do true threats, fraud, targeted harassment, and group vilification fall inside the protected set or inside the exclusions?',
    'Doctrinal history of the reading''s own adherents (Black would exclude less than Douglas; the per se tradition wavered on threats and obscenity) plus the historical record of which categories the founding and early republic treated as punishable.',
    'A wider exclusion set lowers measured extractiveness (fewer harms externalized); a narrower one raises it. The reading''s epsilon is boundary-sensitive in a way the balancing sibling''s is not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_exclusions_boundary, conceptual, 'Boundary ambiguity inside the reading: which categories the historical exclusions fix.').

omega_variable(
    counterspeech_efficacy,
    'Does counterspeech actually remediate the harm protected speech inflicts on targeted groups at scale, or does the reading''s operative remedy assumption fail where harm is systemic and coordinated?',
    'Platform-era network studies of counterspeech against coordinated harassment campaigns; longitudinal outcomes for targeted groups in high-exposure versus moderated speech environments.',
    'If counterspeech fails at scale, the price-of-liberty defense loses its empirical footing: the externalized costs become uncompensated extraction, epsilon rises, and the arrangement drifts toward snare. If it succeeds, the coordination-side reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterspeech_efficacy, empirical, 'Empirical footing of the reading''s operative remedy assumption (the counterspeech_sufficient_remedy axiom).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1791, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement_basis(firs_tr_t1791, observed).
narrative_ontology:measurement(firs_tr_t1798, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1798, 0.15).
narrative_ontology:measurement_basis(firs_tr_t1798, observed).
narrative_ontology:measurement(firs_tr_t1919, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1919, 0.18).
narrative_ontology:measurement_basis(firs_tr_t1919, observed).
narrative_ontology:measurement(firs_tr_t1942, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1942, 0.15).
narrative_ontology:measurement_basis(firs_tr_t1942, observed).
narrative_ontology:measurement(firs_tr_t1964, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1964, 0.12).
narrative_ontology:measurement_basis(firs_tr_t1964, observed).
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1969, 0.1).
narrative_ontology:measurement_basis(firs_tr_t1969, observed).
narrative_ontology:measurement(firs_tr_t2003, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2003, 0.12).
narrative_ontology:measurement_basis(firs_tr_t2003, observed).
narrative_ontology:measurement(firs_tr_t2026, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(firs_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(firs_be_t1791, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1791, 0.32).
narrative_ontology:measurement_basis(firs_be_t1791, observed).
narrative_ontology:measurement(firs_be_t1798, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1798, 0.34).
narrative_ontology:measurement_basis(firs_be_t1798, observed).
narrative_ontology:measurement(firs_be_t1919, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1919, 0.38).
narrative_ontology:measurement_basis(firs_be_t1919, observed).
narrative_ontology:measurement(firs_be_t1942, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1942, 0.44).
narrative_ontology:measurement_basis(firs_be_t1942, observed).
narrative_ontology:measurement(firs_be_t1964, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1964, 0.48).
narrative_ontology:measurement_basis(firs_be_t1964, observed).
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1969, 0.5).
narrative_ontology:measurement_basis(firs_be_t1969, observed).
narrative_ontology:measurement(firs_be_t2003, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2003, 0.55).
narrative_ontology:measurement_basis(firs_be_t2003, observed).
narrative_ontology:measurement(firs_be_t2026, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(firs_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1791, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement_basis(firs_su_t1791, observed).
narrative_ontology:measurement(firs_su_t1798, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1798, 0.05).
narrative_ontology:measurement_basis(firs_su_t1798, observed).
narrative_ontology:measurement(firs_su_t1919, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1919, 0.25).
narrative_ontology:measurement_basis(firs_su_t1919, observed).
narrative_ontology:measurement(firs_su_t1942, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1942, 0.45).
narrative_ontology:measurement_basis(firs_su_t1942, observed).
narrative_ontology:measurement(firs_su_t1964, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1964, 0.65).
narrative_ontology:measurement_basis(firs_su_t1964, observed).
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1969, 0.7).
narrative_ontology:measurement_basis(firs_su_t1969, observed).
narrative_ontology:measurement(firs_su_t2003, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2003, 0.65).
narrative_ontology:measurement_basis(firs_su_t2003, observed).
narrative_ontology:measurement(firs_su_t2026, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(firs_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% Kernel family decomposition: the natural-language label 'First Amendment speech protection' covers three structurally distinct arrangements — categorical protection with a closed exclusion set (this file), yield-on-demonstrated-harm, and case-by-case balancing. Each gets its own epsilon, beneficiary/victim structure, and classification; they are linked here and in the sibling files. The upstream claim (the fixed text and its historical exclusions) is the common substrate; the readings diverge on whether the exclusion set is closed. This story links both siblings; per the family rule, each sibling links back to at least one family member.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
