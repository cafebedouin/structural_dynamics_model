% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Democratic Self-Governance Reading of Speech Protection (Political Speech Hierarchy)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the democratic-participation reading of the
 *   contested speech-protection kernel: the view, associated with the
 *   Meiklejohnian self-governance tradition, that the First Amendment's core
 *   function is to protect speech necessary for citizens to govern
 *   themselves, and that political expression therefore sits atop an internal
 *   hierarchy of protected speech. Commercial speech, obscenity-adjacent
 *   expression, and private/workplace speech receive lesser protection
 *   precisely because they are read as further from the self-governance
 *   function. This is a genuine tangled rope: the coordination function
 *   (protecting the electorate's ability to deliberate and hold power
 *   accountable) is real, but it is achieved by actively downgrading the
 *   protection of everyone whose speech cannot be recharacterized as
 *   political — a live extraction that grows as litigants and courts contest
 *   the classification boundary. This is a KERNEL READING, not the full
 *   doctrine: sibling readings (absolutist, harm-threshold, marketplace,
 *   dignity) are separate constraints with separate ε values, and this story
 *   authors only the democratic-participation position's own structural
 *   claims.
 *
 * KEY AGENTS:
 *   - electoral_candidates: Primary beneficiary (organized/mobile) — receives strict-scrutiny-tier protection
 *   - commercial_speakers: Primary target (moderate/constrained) — receives only intermediate scrutiny
 *   - workplace_and_private_speakers: Primary target (powerless/trapped) — receives minimal hierarchy benefit
 *   - judges_applying_the_hierarchy: Agenda-setter (institutional/analytical) — administers the classification line
 *   - constitutional_scholars: Analytical observer — traces the doctrine's drift and coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.42).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.38).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Democratic Self-Governance Reading of Speech Protection (Political Speech Hierarchy)").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '8c55facd-f486-41d3-a644-803761d24c3f').
narrative_ontology:cs_kernel_codification('8c55facd-f486-41d3-a644-803761d24c3f', fixed_text).
narrative_ontology:cs_authority_grounding('8c55facd-f486-41d3-a644-803761d24c3f', lineage).
narrative_ontology:cs_interpretation_layer_present('8c55facd-f486-41d3-a644-803761d24c3f').
narrative_ontology:cs_reading_relation('8c55facd-f486-41d3-a644-803761d24c3f', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c55facd-f486-41d3-a644-803761d24c3f', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c55facd-f486-41d3-a644-803761d24c3f', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('8c55facd-f486-41d3-a644-803761d24c3f', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('8c55facd-f486-41d3-a644-803761d24c3f', foundational, political_speech_occupies_constitutional_apex).
narrative_ontology:cs_axiom_status(political_speech_occupies_constitutional_apex, holdable).
narrative_ontology:cs_axiom_grounding('8c55facd-f486-41d3-a644-803761d24c3f', political_speech_occupies_constitutional_apex, instrumental).
narrative_ontology:cs_axiom('8c55facd-f486-41d3-a644-803761d24c3f', foundational, protection_tier_tracks_self_governance_proximity).
narrative_ontology:cs_axiom_status(protection_tier_tracks_self_governance_proximity, holdable).
narrative_ontology:cs_axiom_grounding('8c55facd-f486-41d3-a644-803761d24c3f', protection_tier_tracks_self_governance_proximity, conventional).
narrative_ontology:cs_reference_frame('8c55facd-f486-41d3-a644-803761d24c3f', meiklejohnian_self_governance_primacy).
narrative_ontology:cs_drift_state('8c55facd-f486-41d3-a644-803761d24c3f', contemporary_commercial_speech_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8c55facd-f486-41d3-a644-803761d24c3f', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, electoral_candidates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_organizers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, press_covering_government).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, artists_and_entertainers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, workplace_and_private_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, non_electoral_advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, self_governance_requires_informed_electorate).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, political_speech_is_constitutionally_central).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the strongest tier of protection for campaign speech, criticism of incumbents, and policy advocacy. Courts apply strict scrutiny to any restriction touching electoral speech, giving candidates near-maximal latitude to say almost anything framed as political.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, electoral_candidates, beneficiary,
    organized, biographical, mobile, national).

% Advocacy groups, party organizers, and issue campaigners benefit from the doctrine's core premise that speech necessary to self-governance sits at the top of the protective hierarchy. Their organizing, canvassing, and petitioning enjoy the thickest constitutional shield.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_organizers, beneficiary,
    moderate, generational, mobile, national).

% Journalists and outlets reporting on elections, legislation, and government conduct receive robust protection because their speech is classified as core self-governance speech, giving them stronger defenses against defamation suits and prior restraint than other categories of publisher.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, press_covering_government, beneficiary,
    institutional, generational, mobile, national).

% Officeholders benefit from the doctrine's protection when speaking, but are also the frequent targets of the strongest-protected political criticism the reading enables, since the hierarchy specifically shields speech critical of government.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders, payer).

% Advertisers and businesses whose speech is classified as commercial rather than political receive only intermediate scrutiny protection. Restrictions on their speech that would be struck down instantly if reclassified as political survive routinely because the hierarchy places them lower.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, commercial_speakers, payer,
    moderate, biographical, constrained, national).

% Creative and expressive work not framed as civic or electoral argument is treated as less central to the constitutional purpose, leaving it more exposed to obscenity, decency, and content restrictions than expression a court can characterize as political commentary.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, artists_and_entertainers, payer,
    powerless, biographical, constrained, national).

% Ordinary speech in workplaces, private disputes, and everyday life gets no boost from the hierarchy because it is not classifiable as speech necessary for self-governance. Employers and private actors can restrict it with far less constitutional friction than they could restrict a campaign flyer.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, workplace_and_private_speakers, payer,
    powerless, immediate, trapped, local).

% Groups organizing around causes courts do not readily classify as tied to elections or lawmaking (labor solidarity actions, some forms of protest speech, cultural advocacy) find their speech pushed into a lower protective tier, making them more vulnerable to time/place/manner restrictions than groups whose speech maps cleanly onto electoral participation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, non_electoral_advocacy_groups, payer,
    moderate, generational, constrained, national).

% Courts decide, case by case, what counts as political speech necessary for self-governance versus commercial, artistic, or private speech. This classification power determines which tier of scrutiny applies and is the mechanism through which the hierarchy is administered and enforced.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, judges_applying_the_hierarchy, agenda_setter,
    institutional, generational, analytical, national).

% Analyze whether the political/non-political line tracks genuine self-governance functions or imports judicial value judgments about which speech matters, and document how the hierarchy has shifted across doctrinal eras.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__democratic_participation_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__democratic_participation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates the strongest constitutional protection on the category of speech most directly load-bearing for democratic self-governance — electoral debate, criticism of officials, and policy argument — so that political power cannot use ordinary legal tools to silence the speech citizens need to hold it accountable.
% TRANSFER_FUNCTION: Moves protective strength away from commercial, artistic, workplace, and non-electoral advocacy speech and concentrates it on speech classifiable as political, shifting litigation risk, censorship exposure, and doctrinal certainty from candidates, organizers, and press toward everyone else whose speech is coded as lower-tier.
% ABSENT_VOICES: Artists, workplace speakers, and cause-based advocacy groups whose expression does not map cleanly onto electoral or legislative process have no seat in defining what counts as 'necessary for self-governance' — that boundary is drawn by judges applying precedent built mostly around campaign and press cases.
% DISAPPEARANCE_RATIONALE: If the political-speech hierarchy vanished and all speech received uniform scrutiny (either uniformly strict or uniformly relaxed), election-related and government-criticism speech would lose its privileged doctrinal position — either becoming as restrictable as commercial speech, or commercial/artistic speech would become as protected as political speech. Litigation strategy, campaign finance doctrine, and media law would all reorganize around whatever replaced the hierarchy.
% FOUNDING_PROBLEM: Courts needed a principled basis for giving the strongest protection to speech about elections, government, and public affairs — the speech through which citizens govern themselves — distinguishing it from categories (commercial advertising, obscenity, fighting words) the tradition treated as less central to the constitutional design.
% FOUNDING_PROBLEM_CORROBORATION: Judges and constitutional scholars in the self-governance tradition (echoing Meiklejohn) attest the problem remains live: elections and accountability speech still require doctrinal protection against government suppression. Critics from outside the political-speech beneficiary class — media law scholars studying commercial speech doctrine, labor speech advocates, and artists challenged under obscenity standards — attest that the hierarchy has calcified into a classification game where litigants strategically recast their speech as 'political' to access stronger protection, and that the line no longer tracks any principled self-governance function.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).
:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than severe because the reading's downgrade of non-political speech is real but partial — commercial and artistic speech still receive substantial (intermediate) protection, not none. Suppression is moderate (0.38): the hierarchy does not ban non-political speech, it merely subjects it to a lower tier of scrutiny, which is real friction but not categorical exclusion. Theater ratio is low-moderate (0.22) and rising, reflecting the increasing incidence of litigants strategically recharacterizing commercial or private speech as 'political' to access the higher tier — a symptom of the classification boundary becoming a site of doctrinal gamesmanship rather than principled self-governance analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Electoral candidates, political organizers, and press covering government sit near the beneficiary end: the doctrine was built around their speech and gives them the thickest shield. Commercial speakers, artists, workplace speakers, and non-electoral advocacy groups sit toward the target end: the same doctrinal architecture that elevates political speech necessarily assigns them a lower tier by omission, and they bear the costs of restrictions that would fail under strict scrutiny but survive under intermediate or rational-basis review. Incumbent officeholders are dual-positioned — protected as speakers, exposed as targets of the strongest-protected criticism, which is the doctrine functioning as intended rather than a flaw.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting the electorate's capacity for self-governing deliberation against government suppression of political speech) remains genuinely live — elections and accountability speech still need doctrinal armor. What has drifted is the administration of the boundary: the line between 'political' and 'non-political' speech has become increasingly contested and strategically exploited rather than tracking a stable, principled self-governance function, which is what keeps this classified as tangled rope rather than pure rope. The reading is not a zombie mandate (the coordination function is real and defended by outside corroborators), but the classification apparatus surrounding it shows rising theater as parties learn to game the boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_nonpolitical_boundary_stability,
    'Is the line between ''political speech necessary for self-governance'' and ''non-political speech'' a principled, stable classification, or is it an increasingly manipulable litigation target that courts apply inconsistently?',
    'Longitudinal doctrinal analysis of how courts have classified borderline cases (commercial speech with political content, artistic works with social commentary, workplace speech touching public concern) over multiple decades to detect drift or increasing inconsistency in the classification.',
    'If the boundary is stable and principled, the hierarchy functions closer to genuine coordination around a real self-governance function. If the boundary is unstable and strategically gamed, the hierarchy functions increasingly as an extraction mechanism where sophisticated litigants recharacterize speech to access higher protection, disadvantaging less resourced parties who cannot afford the reclassification litigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_nonpolitical_boundary_stability, empirical, 'Whether the political/non-political classification line is principled or increasingly gamed.').

omega_variable(
    kernel_reading_framing_choice,
    'Why author this reading (democratic_participation) as the primary lens rather than treating the self-governance rationale as merely one factor within a broader harm-threshold or marketplace analysis?',
    'Compare the doctrinal weight courts actually place on self-governance rationale (e.g., strict scrutiny triggers, campaign finance jurisprudence) against alternative readings'' doctrinal footprints (harm-based restrictions, marketplace rationales in defamation law) to see which reading better predicts case outcomes across a large sample.',
    'If the democratic_participation reading better predicts outcomes in election and government-speech cases specifically, it justifies treating it as a distinct, dominant reading in that domain rather than folding it into marketplace or absolutist framings; if predictive power is weak, the reading may be better understood as post-hoc rationalization for outcomes reached on other grounds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, conceptual, 'Alternative framing: is self-governance a distinct reading or a subordinate factor within another reading''s analysis?').

omega_variable(
    hierarchy_versus_categorical_exclusion,
    'Does the internal hierarchy (political speech gets more protection) function as genuine tiered coordination, or does it functionally exclude certain categories of speech (obscenity-adjacent art, workplace dissent) as a side effect of the hierarchy''s low tier being effectively unprotected in practice?',
    'Empirical study of outcomes for artists, workplace speakers, and commercial speakers under intermediate/rational-basis scrutiny compared to strict scrutiny outcomes for political speech, measuring actual restriction survival rates.',
    'If low-tier categories are restricted at rates approaching categorical exclusion, the tangled_rope classification understates the extraction and the constraint functions closer to a snare for those categories; if low-tier speech still enjoys meaningful protection, tangled_rope is the accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hierarchy_versus_categorical_exclusion, empirical, 'Whether the lower protective tier functions as genuine reduced protection or effective exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__democratic_participation_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__democratic_participation_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__democratic_participation_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__democratic_participation_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__democratic_participation_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__democratic_participation_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 50, 0.37).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__democratic_participation_reading, 0.1).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of speech_protection_kernel, each authored as a separate constraint story per the ε-invariance principle. The democratic_participation_reading introduces the distinguishing structural feature of an internal hierarchy among protected speech categories based on proximity to self-governance function — a feature absent from the absolutist_reading (near-categorical protection irrespective of content), the harm_threshold_reading (protection turns on victim harm, not speech category), the marketplace_reading (protection turns on truth-discovery value, not political centrality), and the dignity_reading (protection turns on non-subordination, not political function). Each sibling carries its own ε reflecting its own extraction profile; this reading's ε (0.42) reflects the moderate extraction from non-political speakers relative to the political-speech beneficiary class.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
