% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Case-by-Case Balancing Regime for Speech Protection
 *   domain: constitutional law/political philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the speech-protection-boundary
 *   kernel: the balancing reading, under which the line between protected and
 *   unprotected speech is drawn case by case, by weighing First Amendment
 *   interests against other constitutional values and demonstrated harms,
 *   with the gatekeeping role distributed across the judiciary rather than
 *   fixed in categorical rules. Under this reading the boundary genuinely
 *   shifts with context; coded speech and systemic-harm claims receive
 *   intermediate scrutiny; and no speaker knows in advance which side of the
 *   line their next expression will fall on. The arrangement solves a real
 *   adjudication problem (mixed cases that categorical rules mishandle) while
 *   imposing asymmetric costs: restriction risk and uncertainty fall on
 *   boundary-zone speakers, and adjudicative authority accrues to the bench.
 *   Sibling readings (an absolutist reading and a harm-limited reading) are
 *   separate constraints in separate files and are NOT described or averaged
 *   inside this one. KEY AGENTS (by structural relationship): -
 *   constitutional_judiciary: agenda-setting adjudicator
 *   (institutional/constrained) — administers the boundary case by case and
 *   holds the accumulated precedent - executive_security_agencies:
 *   beneficiary (powerful/constrained) — collects restriction latitude case
 *   by case - incumbent_political_establishment: beneficiary
 *   (institutional/arbitrage) — tilts the weights through appointments and
 *   advocacy - dissident_speakers: primary payer (powerless/trapped) — speaks
 *   nearest the boundary, absorbs adverse determinations -
 *   minority_advocacy_speakers: payer (organized/constrained) — speech
 *   systematically at risk of being scored as disorder -
 *   investigative_journalists: dual payer/beneficiary (moderate/constrained)
 *   — two-sided uncertainty at the boundary - civil_liberties_organizations:
 *   analytical observer (organized/analytical) — audits outcomes, contests
 *   the method - unrepresented_speech_communities: excluded
 *   (powerless/trapped) — interests enter weighings only as abstractions
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.54).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.58).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Case-by-Case Balancing Regime for Speech Protection").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional law/political philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '5accc8c7-95ba-4dfd-8dea-914a24fb612e').
narrative_ontology:cs_kernel_codification('5accc8c7-95ba-4dfd-8dea-914a24fb612e', fixed_text).
narrative_ontology:cs_authority_grounding('5accc8c7-95ba-4dfd-8dea-914a24fb612e', lineage).
narrative_ontology:cs_interpretation_layer_present('5accc8c7-95ba-4dfd-8dea-914a24fb612e').
narrative_ontology:cs_reading_relation('5accc8c7-95ba-4dfd-8dea-914a24fb612e', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5accc8c7-95ba-4dfd-8dea-914a24fb612e', speech_protection_boundary__harm_limited_reading, influences).
narrative_ontology:cs_axiom('5accc8c7-95ba-4dfd-8dea-914a24fb612e', foundational, protection_determined_by_contextual_weighing).
narrative_ontology:cs_axiom_status(protection_determined_by_contextual_weighing, holdable).
narrative_ontology:cs_axiom_grounding('5accc8c7-95ba-4dfd-8dea-914a24fb612e', protection_determined_by_contextual_weighing, instrumental).
narrative_ontology:cs_axiom('5accc8c7-95ba-4dfd-8dea-914a24fb612e', foundational, speech_values_enter_weighing_without_lexical_priority).
narrative_ontology:cs_axiom_status(speech_values_enter_weighing_without_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('5accc8c7-95ba-4dfd-8dea-914a24fb612e', speech_values_enter_weighing_without_lexical_priority, deontological).
narrative_ontology:cs_reference_frame('5accc8c7-95ba-4dfd-8dea-914a24fb612e', contextual_weighing_norm).
narrative_ontology:cs_drift_state('5accc8c7-95ba-4dfd-8dea-914a24fb612e', contemporary_tiered_scrutiny_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5accc8c7-95ba-4dfd-8dea-914a24fb612e', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, executive_security_agencies).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, incumbent_political_establishment).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, dissident_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, minority_advocacy_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, investigative_journalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, investigative_journalists).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, judicial_discretion_legitimacy).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, context_sensitive_interpretivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates each contested speech case by weighing expressive interests against public safety, dignity, equality, and security commitments, and sets the operative boundary through accumulated precedent. It cannot step outside the case-by-case method without dismantling decades of its own rulings, and its institutional standing now rests on being the venue where these weighings occur.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Petitions courts to restrict specific speech in specific circumstances — wartime messaging, foreign-influence content, security-relevant material — and obtains restriction latitude that fixed categorical rules would deny. It operates inside the adjudicative process rather than running it.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, executive_security_agencies, beneficiary,
    powerful, biographical, constrained, national).

% Established parties, officeholders, and aligned media benefit when weighing tilts toward public order and stability. They shape the weights indirectly through appointment politics, litigation funding, and amicus advocacy, and can route around unfavorable rulings through legislative workarounds.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, incumbent_political_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Unpopular political movements, antiwar organizers, and radical critics speak closest to the boundary and bear restriction risk in every adjudication. Their speech targets domestic institutions, so relocating jurisdictions means abandoning the object of the speech; historically they have absorbed the largest share of adverse determinations.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, dissident_speakers, payer,
    powerless, biographical, trapped, national).

% Civil-rights and minority-community advocates press claims whose expressive value weighs differently across contexts. They carry the risk that their speech is scored as threat or disorder rather than contribution; organizational resources buy litigation access but not favorable weights.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, minority_advocacy_speakers, payer,
    organized, biographical, constrained, national).

% Reporters and documentary makers publish material that sits near the boundary — source identities, leaked documents, provocative imagery. Contextual weighing sometimes saves publications a fixed rule would ban and sometimes restricts material a fixed rule would protect, so they live with two-sided uncertainty and route publication decisions through anticipated adjudication.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, investigative_journalists, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__balancing_reading, investigative_journalists, beneficiary).

% Litigate on behalf of speakers across the ideological spectrum, audit adjudicated outcomes, and publish doctrinal criticism. They hold no adjudicative power and collect no revenue from the arrangement, but their casework supplies much of the evidence about how the weighing distributes protection.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, civil_liberties_organizations, observer,
    organized, generational, analytical, national).

% Non-English-speaking communities, poor speakers, and future generations lack litigation access. Their interests enter weighings only as abstractions invoked by others, and precedents set today bind their expressive space tomorrow without anyone having spoken for them.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, unrepresented_speech_communities, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves recurring conflicts between expressive liberty and other constitutional commitments — public safety, dignity, equality, national security — case by case, producing context-sensitive determinations where any fixed categorical rule would either overprotect or underprotect some class of cases.
% TRANSFER_FUNCTION: Moves adjudicative authority and decision-making discretion from speakers and from categorical rules to judicial gatekeepers; moves restriction risk onto speakers whose expression falls near the shifting boundary; moves public legitimacy over speech questions to the courts as the standing arbiter of competing values.
% ABSENT_VOICES: Speech communities without litigation access, non-English-speaking speakers whose coded expression receives scrutiny under standards they had no hand in shaping, and future generations bound by precedents accumulated today would object if present; they are represented only as abstractions in others' briefs.
% DISAPPEARANCE_RATIONALE: If the case-by-case weighing arrangement vanished overnight, hundreds of pending adjudications would need an immediate replacement method; courts would collapse onto either categorical protection rules or harm-conditioned permission rules; the distribution of protected and unprotected speech would shift substantially in both directions at once; and judicial authority over speech questions would either evaporate or reorganize around whichever replacement rule won.
% FOUNDING_PROBLEM: Early twentieth-century jurisprudence confronted speech cases that categorical rules handled badly — wartime dissent, labor agitation, later obscenity and national-security material — where neither blanket protection nor blanket restriction matched considered judgment, and adjudication was built to decide these mixed cases flexibly one at a time.
% FOUNDING_PROBLEM_CORROBORATION: Civil-liberties organizations and legal historians attest the founding problem from outside the adjudicative bench: the documented record of wartime, labor, and civil-rights speech determinations shows the recurring mixed cases the method was built for, and comparative constitutional scholarship shows parallel conflicts arising across jurisdictions. The underlying conflict persists and has intensified with online coded speech and systemic-harm claims.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim and the metrics are independent authored facts. I claim tangled_rope because the structure shows all three signatures: a genuine coordination function (mixed speech cases that no categorical rule decides well), asymmetric incidence (restriction risk and uncertainty concentrate on boundary-zone speakers while adjudicative authority accrues to the bench), and active enforcement (every contested case must be adjudicated; the boundary exists only through continuous judicial labor). The metrics describe actual operation: extractiveness 0.54 reflects restriction risk plus uncertainty costs borne by payers, tempered by the real protection many speakers receive under contextual weighing that a stricter rule would deny; suppression 0.58 is the raw structural force of state-backed adjudication that speakers cannot opt out of — suppression is NOT scaled by power or scope, only extractiveness is; theater_ratio 0.40 reflects the substantial performative share of multi-factor weighing (elaborate rubrics that frequently rationalize conclusions reached earlier) alongside real adjudication; accessibility_collapse 0.45 because understanding the arrangement opens no alternative forum within the legal frame, yet doctrinal alternatives remain intellectually live and periodically resurface; resistance 0.60 because civil-liberties litigation and scholarly critique actively contest the method from both directions. The temporal series is deliberately monotonic-rising, not cyclical: the boundary zone expanded across the interval (wartime and security speech, then harassment and coded speech, then online systemic-harm claims), pulling more expression into adjudication, formalizing the weighing apparatus, and maturing the enforcement infrastructure — an enforcement ratchet, not an oscillation. Identity-lock note: the judiciary exhibits institutional identity fusion — the bench has become its gatekeeping function; appellate courts' self-concept is constituted through case-by-case adjudication, which is why exit is constrained despite formal tenure. If that institutional frame broke (e.g., a wholesale migration to categorical tiers treated as settled law), the arrangement's persistence mechanism would change from continuous adjudication to inherited rule-following.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine derives that divergence from the structural data. From the constitutional_judiciary seat the arrangement is a working adjudication machine it staffs and legitimates — coordination-dominant, with the extraction term damped by its beneficiary position. From the dissident_speakers and minority_advocacy_speakers seats the same structure operates as a standing restriction lottery — extraction-dominant, amplified by trapped exit. The investigative_journalists seat sits near symmetric: contextual weighing both saves and restricts them. The excluded seat (unrepresented_speech_communities) is commentary-grade only and drives no correction. Same-level dynamics: dissident and minority-advocacy speakers hold comparable nominal social standing yet experience different effective positions because their exit options differ (trapped versus constrained) and their organizational litigation access differs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d end: constitutional_judiciary collects adjudicative authority and precedent power; executive_security_agencies collect restriction latitude without running the process; incumbent_political_establishment collects order-tilted outcomes with arbitrage-grade influence over the weights. Payers map to the high-d end: dissident_speakers (powerless, trapped) sit nearest the full-target position; minority_advocacy_speakers (organized, constrained) slightly less exposed due to litigation access; investigative_journalists derive a mid-range d from their dual payer/beneficiary declaration. Scope is national throughout, so the engine's scope amplification applies uniformly rather than differentiating seats. No directionality overrides are declared: the beneficiary/victim declarations plus exit options already produce the correct qualitative ordering, and the dual-positioned journalist seat is expressed through roles rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: mixed speech cases keep arriving, and no corroborating source outside the beneficiary set attests that the problem is solved. The founding_problem_status=live combined with disappearance_verdict=world_rearranges produces no capture/zombie mismatch flag. Claiming tangled_rope rather than rope prevents the fair-process framing (courts merely deciding hard cases even-handedly) from laundering the asymmetric incidence; claiming tangled_rope rather than snare prevents the dissident-seat experience (a restriction lottery run by insiders) from erasing the genuine coordination function that categorical-rule regimes demonstrably lack. The piton path is closed by the receipt surface: gains demonstrably accrue to a named seat (the bench), so this is not diffuse-cost inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the speech_protection_boundary kernel (the balancing_reading). What structurally changes if a sibling reading is instantiated instead, and where exactly is the disagreement located?',
    'Comparative analysis across the three sibling stories: the absolutist_reading collapses the boundary to the imminent-lawless-action exception (shrinking the payer set and eliminating distributed judicial discretion); the harm_limited_reading converts protection into conditional permission keyed to dignity and equality harm (expanding the payer set and recentralizing the criterion). The disagreement is located in the locus of boundary-setting authority: categorical rule versus distributed judicial weighing versus harm criterion.',
    'Classification is reading-relative by design: each sibling carries its own epsilon, beneficiary/victim structure, and type. No resolution merges them; the corpus compares them as distinct constraints over one kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the dispute sits.').

omega_variable(
    weighing_neutrality_audit,
    'Does case-by-case weighing actually weigh neutrally, or do the implicit weights systematically favor incumbent, order-aligned, and officially intelligible speech over dissident and minority speech?',
    'Outcome audits of adjudicated speech cases stratified by speaker type, ideology, and resource level: win rates, reversal rates on review, and correlation between speaker position and adverse determination, controlling for case salience.',
    'If the weights are systematically tilted, effective extraction on the dissident and minority-advocacy seats is materially higher than the authored baseline suggests, and the arrangement drifts snare-ward from those seats; if weights are roughly neutral, the tangled_rope reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weighing_neutrality_audit, empirical, 'Whether the weighing''s implicit weights are neutral or incumbent-tilted.').

omega_variable(
    tier_recategorization_drift,
    'Has the tiered-scrutiny architecture (fixed categories of low-value speech receiving defined levels of scrutiny) quietly converted case-by-case weighing back into quasi-categorical rules, such that the standing arrangement no longer instantiates the balancing reading it descends from?',
    'Doctrinal analysis of whether intermediate-scrutiny tiers function as fixed categories in application or as starting points that individualized weighing routinely overrides; measure the frequency with which tier assignments are overturned by case-specific factors.',
    'If tiers operate as fixed categories, this story''s referent has drifted toward a hybrid categorical-plus-weighing constraint; epsilon, stakeholder structure, and the drift_state declaration would need re-authoring, and the reading''s reference frame would be substantially stale rather than merely drifted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tier_recategorization_drift, conceptual, 'Whether stabilized scrutiny tiers have recategorized what was authored as open-ended weighing.').

omega_variable(
    boundary_uncertainty_chill,
    'How large is the anticipatory chilling effect on boundary-zone speakers of not knowing in advance which side of a context-shifting boundary their expression will fall on?',
    'Survey and behavioral evidence comparing expressive activity by boundary-zone speakers under contextual-protection regimes versus categorical-rule regimes, including suppressed-publication rates among journalists and self-censorship measures among activists.',
    'Higher measured chill raises the suppression actually borne by the payer seats beyond the structural scalar, tightening the case for treating the uncertainty itself as an extraction channel; negligible chill would support a lower effective burden on boundary-zone seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_uncertainty_chill, empirical, 'Magnitude of anticipatory self-restriction induced by boundary instability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 1938, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1938, speech_protection_boundary__balancing_reading, theater_ratio, 1938, 0.22).
narrative_ontology:measurement_basis(spee_tr_t1938, observed).
narrative_ontology:measurement(spee_tr_t1956, speech_protection_boundary__balancing_reading, theater_ratio, 1956, 0.26).
narrative_ontology:measurement_basis(spee_tr_t1956, observed).
narrative_ontology:measurement(spee_tr_t1974, speech_protection_boundary__balancing_reading, theater_ratio, 1974, 0.3).
narrative_ontology:measurement_basis(spee_tr_t1974, observed).
narrative_ontology:measurement(spee_tr_t1992, speech_protection_boundary__balancing_reading, theater_ratio, 1992, 0.33).
narrative_ontology:measurement_basis(spee_tr_t1992, observed).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_boundary__balancing_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement_basis(spee_tr_t2010, observed).
narrative_ontology:measurement(spee_tr_t2026, speech_protection_boundary__balancing_reading, theater_ratio, 2026, 0.4).
narrative_ontology:measurement_basis(spee_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1938, speech_protection_boundary__balancing_reading, base_extractiveness, 1938, 0.34).
narrative_ontology:measurement_basis(spee_be_t1938, observed).
narrative_ontology:measurement(spee_be_t1956, speech_protection_boundary__balancing_reading, base_extractiveness, 1956, 0.39).
narrative_ontology:measurement_basis(spee_be_t1956, observed).
narrative_ontology:measurement(spee_be_t1974, speech_protection_boundary__balancing_reading, base_extractiveness, 1974, 0.43).
narrative_ontology:measurement_basis(spee_be_t1974, observed).
narrative_ontology:measurement(spee_be_t1992, speech_protection_boundary__balancing_reading, base_extractiveness, 1992, 0.47).
narrative_ontology:measurement_basis(spee_be_t1992, observed).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__balancing_reading, base_extractiveness, 2010, 0.51).
narrative_ontology:measurement_basis(spee_be_t2010, observed).
narrative_ontology:measurement(spee_be_t2026, speech_protection_boundary__balancing_reading, base_extractiveness, 2026, 0.54).
narrative_ontology:measurement_basis(spee_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1938, speech_protection_boundary__balancing_reading, suppression_requirement, 1938, 0.38).
narrative_ontology:measurement_basis(spee_su_t1938, observed).
narrative_ontology:measurement(spee_su_t1956, speech_protection_boundary__balancing_reading, suppression_requirement, 1956, 0.43).
narrative_ontology:measurement_basis(spee_su_t1956, observed).
narrative_ontology:measurement(spee_su_t1974, speech_protection_boundary__balancing_reading, suppression_requirement, 1974, 0.47).
narrative_ontology:measurement_basis(spee_su_t1974, observed).
narrative_ontology:measurement(spee_su_t1992, speech_protection_boundary__balancing_reading, suppression_requirement, 1992, 0.51).
narrative_ontology:measurement_basis(spee_su_t1992, observed).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__balancing_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement_basis(spee_su_t2010, observed).
narrative_ontology:measurement(spee_su_t2026, speech_protection_boundary__balancing_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(spee_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how speech is protected' decomposes into three structurally distinct constraints over one kernel (speech_protection_boundary), per the epsilon-invariance principle. The absolutist_reading instantiates a categorical-protection constraint with a minimal harm exception; the balancing_reading (this file) instantiates a distributed-adjudication constraint whose boundary moves with context; the harm_limited_reading instantiates a conditional-permission constraint keyed to dignity and equality harm. Their epsilon values differ because their referents differ: the absolutist arrangement extracts little (few speakers restricted, little discretion exercised), the balancing arrangement extracts moderately (restriction risk concentrated on boundary-zone speakers, adjudicative authority accrued to the bench), and the harm-limited arrangement extracts broadly (any speech causing significant dignitary or egalitarian harm loses protection). The balancing reading structurally influences the harm-limited reading — its demonstrated-harms term normalized harm assessment inside speech adjudication, changing the legitimacy conditions under which harm-based readings argue — without foreclosing it; it coexists with the absolutist reading as rival live positions held by different juristic factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
