% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist Reading of Speech Protection (Brandenburg Standard)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents an absolutist reading of speech protection,
 *   primarily embodied by the Brandenburg v. Ohio standard (1969), which
 *   limits unprotected speech to direct incitement of imminent lawless
 *   action. This reading maximizes the protected set of speech, with the
 *   consequence that minoritized communities bear the aggregate harm of
 *   speech that falls short of this high bar. The claimed type is 'rope' from
 *   the perspective of those who believe in maximal speech protection, but
 *   the metrics reflect the low-level, diffuse extraction experienced by
 *   victims of harmful speech.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.25).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.1).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist Reading of Speech Protection (Brandenburg Standard)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '971302b9-c6a1-4517-ad96-df3901589569').
narrative_ontology:cs_kernel_codification('971302b9-c6a1-4517-ad96-df3901589569', fixed_text).
narrative_ontology:cs_authority_grounding('971302b9-c6a1-4517-ad96-df3901589569', lineage).
narrative_ontology:cs_interpretation_layer_present('971302b9-c6a1-4517-ad96-df3901589569').
narrative_ontology:cs_reading_relation('971302b9-c6a1-4517-ad96-df3901589569', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('971302b9-c6a1-4517-ad96-df3901589569', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('971302b9-c6a1-4517-ad96-df3901589569', foundational, marketplace_of_ideas).
narrative_ontology:cs_axiom_status(marketplace_of_ideas, holdable).
narrative_ontology:cs_axiom_grounding('971302b9-c6a1-4517-ad96-df3901589569', marketplace_of_ideas, deontological).
narrative_ontology:cs_axiom('971302b9-c6a1-4517-ad96-df3901589569', foundational, minimal_state_intervention).
narrative_ontology:cs_axiom_status(minimal_state_intervention, holdable).
narrative_ontology:cs_axiom_grounding('971302b9-c6a1-4517-ad96-df3901589569', minimal_state_intervention, deontological).
narrative_ontology:cs_reference_frame('971302b9-c6a1-4517-ad96-df3901589569', uninhibited_public_discourse).
narrative_ontology:cs_drift_state('971302b9-c6a1-4517-ad96-df3901589569', contemporary_digital_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('971302b9-c6a1-4517-ad96-df3901589569', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, public_discourse).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from broad protection for their expression, even if controversial or offensive, as long as it does not directly incite imminent violence. They face minimal legal risk for most forms of speech.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, speakers, beneficiary,
    moderate, biographical, mobile, national).

% Benefits from a robust and uninhibited exchange of ideas, including those that are unpopular or challenging, which is seen as essential for democratic self-governance and the search for truth. This is an abstract good, not an agent.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, public_discourse, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(speech_protection_boundary__absolutist_reading, public_discourse).

% Bear the aggregate harm of hate speech, harassment, and discriminatory rhetoric that falls short of the Brandenburg standard. They experience this as an externality of broad speech protection, with limited legal recourse to mitigate its impact.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    powerless, generational, trapped, local).

% Interpret and enforce the Brandenburg standard, balancing the protection of speech with the narrow exception for incitement. They are bound by precedent but also shape the evolving understanding of the standard.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, courts, agenda_setter,
    institutional, generational, constrained, national).

% Are constrained in their ability to regulate speech, even when it causes significant social harm, due to the high bar set by the Brandenburg standard. They bear the political cost of public dissatisfaction with unregulated harmful speech.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, legislatures, payer,
    institutional, generational, constrained, national).

% Analyze the application and implications of the Brandenburg standard, debating its philosophical underpinnings, practical effects, and potential alternatives. They influence judicial and public understanding of speech rights.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, high bar for unprotected speech, providing predictability for speakers and minimizing chilling effects on legitimate expression. It coordinates expectations around the scope of free speech.
% TRANSFER_FUNCTION: Transfers the burden of social harm from broadly protected speech onto minoritized communities, while transferring broad expressive freedom to speakers and the public discourse.
% ABSENT_VOICES: Victims of hate speech and harassment, who are often marginalized and lack the institutional power to effectively advocate for stronger speech regulation. Their experiences of aggregate harm are often discounted in favor of abstract speech principles.
% DISAPPEARANCE_RATIONALE: If the Brandenburg standard vanished overnight, the legal landscape for speech would become highly uncertain. Legislatures would likely pass more restrictive speech laws, courts would struggle with a new balancing test, and speakers would face greater risk of prosecution, fundamentally altering public discourse.
% FOUNDING_PROBLEM: The problem of overbroad speech restrictions that chilled legitimate political dissent and expression, particularly during periods of social unrest and fear.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties advocates and many legal scholars attest that the problem of chilling legitimate speech remains live, citing historical and ongoing attempts to suppress unpopular views. Critics, however, argue that the standard has swung too far, creating new problems of harm to vulnerable groups.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).
:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint primarily functions to protect speech, not to extract resources. However, it is not zero because the broad protection imposes a cost on minoritized communities who experience the negative externalities of harmful speech. Suppression is low (0.1) as the standard aims to minimize state suppression of speech. Theater ratio is very low (0.05) as the standard is genuinely applied, not merely performed. Accessibility collapse is high (0.8) because alternatives to this broad protection (e.g., more restrictive speech laws) are largely foreclosed by this interpretation. Resistance is low (0.15) from the perspective of those who benefit from broad speech rights, but higher from those who advocate for greater regulation of harmful speech.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers and civil liberties advocates, this is a pure 'rope' that enables robust public discourse. From the perspective of minoritized communities, it functions as a low-level 'snare' that extracts their safety and dignity in exchange for others' expressive freedom. The engine's classification will reflect this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and the abstract concept of 'public discourse' are beneficiaries, as the standard maximizes their freedom of expression. Minimized communities are victims, as they bear the social costs of speech that is protected under this standard but causes them harm. Courts act as agenda-setters, interpreting and applying the standard. Legislatures are payers, as their ability to address social harms through speech regulation is constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_harm_quantification,
    'How can the aggregate harm to minoritized communities from protected speech be reliably quantified and weighed against the benefits of broad speech protection?',
    'Longitudinal sociological studies, public health data, and economic analyses of the impact of hate speech and harassment on vulnerable populations.',
    'If aggregate harm is demonstrably severe, it could shift the perceived extractiveness of this reading upward, potentially reclassifying it from a Rope to a Tangled Rope or Snare from the perspective of victims. It would also strengthen arguments for alternative readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_harm_quantification, empirical, 'Quantifying the diffuse, cumulative harm of protected speech.').

omega_variable(
    chilling_effect_vs_harm_tradeoff,
    'Is the risk of chilling legitimate speech (by adopting a more restrictive standard) greater or lesser than the harm caused by currently protected speech (under the Brandenburg standard)?',
    'Comparative legal analysis of jurisdictions with different speech standards, empirical studies on self-censorship rates, and public opinion surveys on perceived expressive freedom vs. safety.',
    'If the chilling effect is found to be minimal, it weakens the primary justification for the absolutist reading and strengthens arguments for harm-limited or balancing approaches. If harm is found to be minimal, it reinforces the absolutist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_vs_harm_tradeoff, conceptual, 'The fundamental tradeoff between free speech and harm prevention.').

omega_variable(
    absolutist_vs_balancing_framing,
    'Is the ''absolutist'' framing of speech protection a genuine structural commitment to maximal liberty, or a rhetorical device to resist any regulation of speech?',
    'Analysis of judicial opinions and legislative debates for consistency in applying absolutist principles across diverse contexts, even when politically unpopular or socially costly.',
    'If found to be purely rhetorical, it would expose the ''absolutist_reading'' as a Snare, where the coordination story (maximal liberty) is cover for extraction (unfettered expression for powerful groups at the expense of vulnerable ones).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absolutist_vs_balancing_framing, conceptual, 'Whether the absolutist stance is a genuine principle or a strategic framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_protection_boundary__absolutist_reading, theater_ratio, 1969, 0.05).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_boundary__absolutist_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_boundary__absolutist_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_boundary__absolutist_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_boundary__absolutist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__absolutist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__absolutist_reading, base_extractiveness, 1969, 0.2).
narrative_ontology:measurement(spee_be_t1980, speech_protection_boundary__absolutist_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(spee_be_t1990, speech_protection_boundary__absolutist_reading, base_extractiveness, 1990, 0.23).
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__absolutist_reading, base_extractiveness, 2000, 0.24).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__absolutist_reading, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__absolutist_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_protection_boundary__absolutist_reading, suppression_requirement, 1969, 0.1).
narrative_ontology:measurement(spee_su_t1980, speech_protection_boundary__absolutist_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(spee_su_t1990, speech_protection_boundary__absolutist_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__absolutist_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__absolutist_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__absolutist_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
