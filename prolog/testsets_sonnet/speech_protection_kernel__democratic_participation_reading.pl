% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Speech Protection Kernel — Democratic Participation Reading (Political Speech Primacy)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the democratic-participation reading of the
 *   speech-protection kernel: the constitutional doctrine that erects an
 *   internal hierarchy within protected speech, reserving the strongest
 *   scrutiny for political and electoral expression on the theory that
 *   self-governance requires citizens to be able to criticize and replace
 *   those who hold power. This is a distinct constraint from the absolutist
 *   reading (near-categorical protection regardless of content), the
 *   harm-threshold reading (protection conditioned on absence of demonstrable
 *   harm), the marketplace reading (protection grounded in truth-discovery),
 *   and the dignity reading (protection conditioned on non-subordination) —
 *   each of those is authored as its own sibling constraint with its own
 *   epsilon, beneficiary/victim structure, and classification. The
 *   self-governance rationale genuinely solves a real coordination problem
 *   (keeping government-criticism channels open against incumbent
 *   self-interest) but does so by systematically under-protecting
 *   non-political speakers relative to what a content-neutral reading of the
 *   same text would produce, which is the asymmetric extraction component.
 *
 * KEY AGENTS:
 *   - electoral_candidates: primary beneficiary (organized/mobile) — receives strongest doctrinal shield
 *   - political_advocacy_organizations: primary beneficiary (organized/mobile) — core protected activity
 *   - commercial_speakers: primary target (moderate/constrained) — intermediate scrutiny only
 *   - workplace_and_private_speakers: primary target (powerless/trapped) — outside doctrinal core entirely
 *   - courts_adjudicating_speech_tiers: agenda-setter (institutional/analytical) — draws the classification line
 *   - legal_scholars_and_civil_liberties_observers: analytical observer — evaluates whether tiering tracks the stated rationale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.38).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.42).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Speech Protection Kernel — Democratic Participation Reading (Political Speech Primacy)").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '55b5091f-360b-4e73-adfb-edce3a09d12d').
narrative_ontology:cs_kernel_codification('55b5091f-360b-4e73-adfb-edce3a09d12d', fixed_text).
narrative_ontology:cs_authority_grounding('55b5091f-360b-4e73-adfb-edce3a09d12d', lineage).
narrative_ontology:cs_interpretation_layer_present('55b5091f-360b-4e73-adfb-edce3a09d12d').
narrative_ontology:cs_reading_relation('55b5091f-360b-4e73-adfb-edce3a09d12d', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('55b5091f-360b-4e73-adfb-edce3a09d12d', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('55b5091f-360b-4e73-adfb-edce3a09d12d', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('55b5091f-360b-4e73-adfb-edce3a09d12d', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('55b5091f-360b-4e73-adfb-edce3a09d12d', foundational, political_speech_necessary_for_self_governance).
narrative_ontology:cs_axiom_status(political_speech_necessary_for_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('55b5091f-360b-4e73-adfb-edce3a09d12d', political_speech_necessary_for_self_governance, instrumental).
narrative_ontology:cs_axiom('55b5091f-360b-4e73-adfb-edce3a09d12d', foundational, internal_hierarchy_among_protected_categories_permissible).
narrative_ontology:cs_axiom_status(internal_hierarchy_among_protected_categories_permissible, holdable).
narrative_ontology:cs_axiom_grounding('55b5091f-360b-4e73-adfb-edce3a09d12d', internal_hierarchy_among_protected_categories_permissible, conventional).
narrative_ontology:cs_reference_frame('55b5091f-360b-4e73-adfb-edce3a09d12d', self_governance_centrality_framework).
narrative_ontology:cs_drift_state('55b5091f-360b-4e73-adfb-edce3a09d12d', contemporary_campaign_finance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55b5091f-360b-4e73-adfb-edce3a09d12d', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, electoral_candidates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_advocacy_organizations).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, journalists_covering_government).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, citizen_voters).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, artistic_and_cultural_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, workplace_and_private_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, speakers_of_contested_political_status).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, journalists_covering_government).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, self_governance_theory_of_the_first_amendment).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, political_speech_centrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the strongest available protection for campaign speech, criticism of incumbents, and policy advocacy. Courts apply strict scrutiny to any restriction touching their core electoral messaging. Their exit option is robust: they can shift venues, media, and framing without losing the tier of protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, electoral_candidates, beneficiary,
    organized, biographical, mobile, national).

% Lobby, publish, and organize around matters of public concern and government accountability. The doctrine gives their core advocacy activity the tier-one shield; they invest heavily in ensuring their speech is legally characterized as political rather than commercial or private to retain that shield.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Reporting on elections, legislation, and officials sits at the doctrinal core and receives maximal protection. But investigative work that strays into commercial disclosure, trade secrets, or private figures' affairs falls outside the privileged tier and faces the ordinary, lower level of protection — creating pressure to frame stories in explicitly political terms to retain the higher shield.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, journalists_covering_government, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, journalists_covering_government, payer).

% Depend on a robust flow of political information and open channels of political criticism to exercise self-governance. They cannot personally litigate speech protections but are the intended ultimate beneficiaries of the doctrine's self-governance rationale — they benefit passively from the tier structure without controlling it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, citizen_voters, beneficiary,
    powerless, generational, trapped, national).

% Advertising, product claims, and commercial disclosures receive intermediate scrutiny at best, meaning restrictions that would fail against political speech readily survive against commercial speech. They bear the doctrinal hierarchy directly: identical communicative harm is regulated more freely because the speech is categorized as commercial rather than political.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, commercial_speakers, payer,
    moderate, biographical, constrained, national).

% Novels, art, music, and entertainment expression are protected but sit doctrinally below core political speech; obscenity, indecency, and content-based restrictions are more easily sustained against them because courts do not read them as necessary to self-governance. They have no lever to reclassify their work into the protected tier.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, artistic_and_cultural_speakers, payer,
    powerless, biographical, constrained, national).

% Employees disciplined for workplace speech, or private individuals in interpersonal disputes, find their expression outside the doctrinal core; employers and private actors can restrict their speech with far less First Amendment friction than a government actor restricting a candidate's campaign speech. They have essentially no exit — they cannot relitigate their speech into a higher-protection category.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, workplace_and_private_speakers, payer,
    powerless, immediate, trapped, local).

% Speech at the boundary — labor organizing, corporate-sponsored issue advocacy, anonymous online political mobilization, speech by non-citizens on public affairs — must fight to be recognized as 'political' at all. Losing that classification fight drops them into a lower-protection tier; the classification contest itself is a recurring cost this reading imposes on boundary speakers that core political speakers never face.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, speakers_of_contested_political_status, payer,
    powerless, biographical, constrained, national).

% Draw and redraw the line between political and non-political speech case by case, administering the tiered-scrutiny framework. They set which category a given utterance falls into and thus which level of protection attaches, exercising real discretion that determines outcomes for every other seat.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, courts_adjudicating_speech_tiers, agenda_setter,
    institutional, generational, analytical, national).

% Study whether the self-governance rationale coherently explains the doctrine's actual tiering, or whether the political/non-political line has become a proxy for protecting establishment political actors while leaving commercial, artistic, and marginal speakers more exposed than the self-governance theory would predict.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, legal_scholars_and_civil_liberties_observers, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates the strongest constitutional protection on the speech genuinely necessary for citizens to monitor, criticize, and replace their government — solving the coordination problem of keeping channels of electoral accountability open against government self-interest in suppressing criticism.
% TRANSFER_FUNCTION: Moves protective doctrinal resources (strict scrutiny, high burden on government to justify restriction) toward political and electoral speech, and away from commercial, artistic, workplace, and boundary-status speech, which receive comparatively weaker protection under the same constitutional text.
% ABSENT_VOICES: Speakers whose expression serves self-governance-adjacent but not doctrinally 'political' functions — satirists, artists using metaphor to criticize power, workers whose labor speech is economic in form but political in substance — are not systematically heard in the classification proceedings that decide their tier; the classification is typically litigated after the restriction has already occurred.
% DISAPPEARANCE_RATIONALE: If the political-speech-primacy tier collapsed into a single undifferentiated protection standard (either uniformly higher or uniformly lower), electoral and advocacy speech would either lose its heightened shield against government retaliation, or commercial/artistic/workplace speech would gain protection currently withheld from it — either direction reallocates real litigation outcomes, campaign finance doctrine, and press practice.
% FOUNDING_PROBLEM: Democracies need government criticism and electoral speech to be maximally resistant to incumbent suppression, because incumbents have the strongest incentive and the greatest capacity to censor speech that threatens their hold on power.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars studying backsliding democracies and international human rights bodies (e.g., UN Human Rights Committee commentary on political expression) independently corroborate that incumbent suppression of electoral criticism remains an active, ongoing risk, not a solved historical problem — this corroboration comes from outside the domestic courts and advocacy organizations that most directly benefit from the doctrine.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.38 at interval end) because the tiering doctrine does not extract wealth or labor directly — it reallocates a scarce good (constitutional protection strength) unevenly across speaker categories. Suppression (0.42) reflects the real coercive weight borne by lower-tier speakers who face restrictions that would be struck down instantly if applied to core political speech. Theater ratio is modest but rising (0.28) — the political/non-political classification exercise increasingly functions as a proxy fight over which speakers get access to the high-protection tier, rather than a clean application of the self-governance rationale, which is the drift this reading's own courts would need to police.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of an electoral candidate or advocacy organization, the doctrine reads as principled protection of democracy's lifeblood. From the seat of a commercial speaker or workplace speaker facing an easily-sustained restriction on formally identical expressive conduct, the same doctrine reads as an arbitrary hierarchy that happens to privilege the speech of the politically organized and disadvantage everyone else. The engine computes these as structurally different seat outcomes from the same base data — this is expected, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Electoral candidates, advocacy organizations, and (derivatively) citizen-voters sit near the beneficiary end: the doctrine was built for and primarily serves their speech. Commercial speakers, artistic speakers, workplace speakers, and boundary-status political speakers sit toward the target end: identical government interference in their expression survives constitutional challenge far more easily solely because of how their speech is categorized, not because of anything about the speech's actual content or effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (incumbent suppression of electoral criticism) remains empirically live per external corroboration, which is what keeps this reading from being a pure zombie mandate. But the tiering mechanism itself has drifted: courts increasingly must adjudicate ambiguous boundary cases (labor speech, corporate issue advocacy, anonymous political mobilization) where the classification contest has become a recurring extraction site independent of the original self-governance rationale — a mandatrophy-adjacent drift worth tracking even though the core mandate is not dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_nonpolitical_line_stability,
    'Is the line between ''political'' and ''non-political'' speech a stable, principled classification, or does it function as a proxy that tracks which speakers are already organized and litigation-resourced?',
    'Longitudinal case-law analysis tracking whether classification outcomes correlate more strongly with speaker organizational capacity/resources than with the content-based self-governance criteria the doctrine claims to apply.',
    'If classification tracks resources rather than content, the democratic-participation reading functions partly as a tangled rope that launders resource-based advantage through a self-governance rationale; if it tracks content reliably, the reading is closer to a genuine, well-targeted rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_nonpolitical_line_stability, empirical, 'Whether the political/non-political classification is principled or a resource proxy.').

omega_variable(
    kernel_reading_choice_grounds,
    'Why does this analysis adopt the democratic-participation reading rather than one of the four sibling readings (absolutist, harm-threshold, marketplace, dignity) as the operative frame for evaluating speech doctrine?',
    'The reading is adopted because U.S. constitutional doctrine (Buckley v. Valeo, Citizens United''s self-governance rationale, the commercial-speech doctrine''s intermediate-scrutiny tier) demonstrably instantiates internal hierarchy rather than categorical or harm-conditional protection; a jurisdiction or era with a different doctrinal structure (e.g., one adopting the dignity reading''s subordination test) would license selecting a different sibling as the operative story.',
    'Under the absolutist_reading, this same body of case law would appear as unprincipled defection from categorical protection rather than a coherent internal-hierarchy scheme; under the dignity_reading, the same commercial/artistic speaker disadvantage would be evaluated on entirely different grounds (subordination effects rather than self-governance centrality), potentially reversing which speakers count as victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_grounds, conceptual, 'Framing under-determination: why this reading rather than a sibling reading was selected as the operative frame.').

omega_variable(
    self_governance_rationale_natural_or_constructed,
    'Is the self-governance rationale for the political-speech-primacy tier a principled philosophical discovery about what democracy requires, or a constructed doctrinal choice that happens to benefit incumbent-adjacent political actors (candidates, established advocacy organizations) at the expense of less-organized speakers?',
    'Compare outcomes under jurisdictions/eras adopting a different kernel reading (e.g. dignity or harm-threshold) to see whether self-governance functions as a load-bearing philosophical principle or as a post-hoc justification for an already-existing pattern of protecting organized political speech.',
    'If constructed, the tiering''s beneficiary concentration among already-organized political actors is not incidental but load-bearing, strengthening the tangled_rope classification; if principled, the tier structure is closer to genuine, well-targeted coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_governance_rationale_natural_or_constructed, conceptual, 'Whether the self-governance rationale is philosophically load-bearing or a constructed cover for incumbent-adjacent benefit concentration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(spee_tr_t12, speech_protection_kernel__democratic_participation_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__democratic_participation_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(spee_tr_t36, speech_protection_kernel__democratic_participation_reading, theater_ratio, 36, 0.23).
narrative_ontology:measurement(spee_tr_t48, speech_protection_kernel__democratic_participation_reading, theater_ratio, 48, 0.26).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__democratic_participation_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(spee_be_t12, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 12, 0.29).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 24, 0.32).
narrative_ontology:measurement(spee_be_t36, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 36, 0.35).
narrative_ontology:measurement(spee_be_t48, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 48, 0.37).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(spee_su_t12, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(spee_su_t36, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 36, 0.38).
narrative_ontology:measurement(spee_su_t48, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 48, 0.4).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the speech_protection_kernel. Each reading is authored as an independent constraint with its own epsilon, beneficiary/victim structure, and claimed type, per the eps-invariance principle: the underlying constitutional text is identical across readings, but the structural claim about what the text protects and why differs enough that no single epsilon value could honestly describe all five. The democratic_participation_reading is distinguished by internal tiering (political speech gets more protection than non-political speech of otherwise similar content), which none of the sibling readings share as their organizing principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
