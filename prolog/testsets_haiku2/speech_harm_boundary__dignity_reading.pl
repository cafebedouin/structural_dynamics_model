% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Speech Subordinate to Human Dignity (Categorical Exclusion Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the dignity reading of the
 *   speech-harm-boundary kernel. It asserts that speech denying the
 *   personhood or human dignity of targeted identity groups is categorically
 *   excluded from constitutional protection — not balanced against
 *   countervailing interests, but ruled out on principle. The reading's core
 *   premise is that human dignity is not a value that can be traded off
 *   against speaker freedom; dignity-denying speech is a form of violence
 *   that precedes and enables material violence against persecuted groups.
 *   The constraint operates through judicial doctrine (defining exclusion
 *   categories), legislative prohibition, and platform liability (enforcement
 *   at scale). The constraint's extractiveness is high because it places
 *   heavy restrictions on certain speakers and requires constant
 *   boundary-maintenance; its theater is low because the enforcement function
 *   is genuine (not performative) even as boundary cases proliferate.
 *
 * KEY AGENTS:
 *   - Targeted identity groups: the beneficiary seat, experiencing protection from categorical dehumanization
 *   - Speakers of identity-harm: the primary payer, identity-locked into excluded positions
 *   - Judicial interpreter authority: the agenda-setter, administering the dignity doctrine and boundary criteria
 *   - Contested boundary cases: the payer trapped by categorical rules that do not distinguish intent
 *   - Competing absolutist regimes: the excluded seat, geopolitically barred from shaping the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.82).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.76).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Speech Subordinate to Human Dignity (Categorical Exclusion Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '952390f9-394c-4205-993c-703952c7b0de').
narrative_ontology:cs_kernel_codification('952390f9-394c-4205-993c-703952c7b0de', fixed_text).
narrative_ontology:cs_authority_grounding('952390f9-394c-4205-993c-703952c7b0de', lineage).
narrative_ontology:cs_interpretation_layer_present('952390f9-394c-4205-993c-703952c7b0de').
narrative_ontology:cs_reading_relation('952390f9-394c-4205-993c-703952c7b0de', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('952390f9-394c-4205-993c-703952c7b0de', speech_harm_boundary__harm_balancing_reading, influences).
narrative_ontology:cs_axiom('952390f9-394c-4205-993c-703952c7b0de', foundational, human_dignity_subordinates_speech_freedom).
narrative_ontology:cs_axiom_status(human_dignity_subordinates_speech_freedom, holdable).
narrative_ontology:cs_axiom_grounding('952390f9-394c-4205-993c-703952c7b0de', human_dignity_subordinates_speech_freedom, deontological).
narrative_ontology:cs_axiom('952390f9-394c-4205-993c-703952c7b0de', foundational, personhood_denial_speech_categorically_unprotected).
narrative_ontology:cs_axiom_status(personhood_denial_speech_categorically_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('952390f9-394c-4205-993c-703952c7b0de', personhood_denial_speech_categorically_unprotected, deontological).
narrative_ontology:cs_reference_frame('952390f9-394c-4205-993c-703952c7b0de', dignity_supremacy_framework).
narrative_ontology:cs_drift_state('952390f9-394c-4205-993c-703952c7b0de', contemporary_rights_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('952390f9-394c-4205-993c-703952c7b0de', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, targeted_identity_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, judicial_interpreter_authority).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_identity_harm).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, hate_speech_researchers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, contested_boundary_cases).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, platform_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of historically persecuted groups (religious minorities, ethnic communities, racial groups, gender-persecuted persons) who are protected from categorical speech denying their human status or personhood. The constraint prevents systematic defamation campaigns, genocidal propaganda, and dignity-stripping speech that historically precedes violence. They experience protection as enabling participation in public discourse without existential identity threat.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, targeted_identity_groups, beneficiary,
    organized, generational, constrained, national).

% Individuals and organizations whose speech denies the personhood or human dignity of targeted groups. Under this reading they are categorically excluded from protection: Holocaust denial, assertions that a group 'cannot truly belong,' gender-denying speech targeting trans persons, assertions of racial hierarchy as biological fact. Their exit is identity-locked because for ideological advocates, renouncing the core claim means abandoning the identity framework itself.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, speakers_of_identity_harm, payer,
    moderate, biographical, identity_locked, national).

% Constitutional courts or high judicial bodies that define the boundary between protected and unprotected speech under the dignity reading. They set the categorical exclusion criteria, interpret what constitutes personhood-denial, and enforce the doctrine through injunctions, criminal sanctions, or platform liability. Their power consists in administering the constraint and adjudicating the line.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, judicial_interpreter_authority, agenda_setter,
    institutional, generational, analytical, national).

% Scholars studying hate speech mechanisms, including those who need to cite, analyze, or reproduce hate speech texts for research. Under categorical exclusion, they bear costs from restricted republication, archive limitations, and the need to work around content removal. Their constraint is not identity-locked but professional: the research requires accessing the speech to understand it.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, hate_speech_researchers, payer,
    powerful, biographical, constrained, global).

% Speakers of edge-case speech that reasonable parties disagree on: satire of hateful groups (does it reinforce or undermine the ideology?), academic discussion of racial science (does scholarly framing insulate from harm?), reclamation speech (targeted group members reclaiming slurs — are they protected differently?). Under categorical application, these speakers risk exclusion despite the absence of intentional identity-harm; they are trapped because the exclusion mechanism does not distinguish intent.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, contested_boundary_cases, payer,
    powerless, biographical, trapped, local).

% Private digital platforms that host speech and face liability or regulatory pressure to remove speech categorically excluded under this reading. They become enforcers of the constraint on a global scale, moderation at speed necessitating over-removal; their constraint is the regulatory pressure to comply with exclusion categories that are legally sharp but operationally blunt.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, platform_operators, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, platform_operators, agenda_setter).

% Jurisdictions and constitutional traditions (notably US absolutism) whose speech protection regime does not recognize categorical dignity-based exclusions. They are structurally excluded from the reading's framing because this reading asserts its categorical principle forecloses their balancing-based approach — they have no voice in how the dignity reading operates, only external pressure to adopt it.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, competing_speech_regimes, excluded,
    powerful, generational, trapped, global).

% Records the structural relationship between speech protection, human dignity, enforcement machinery, and the categories of constraint. No seat in the conflict, but positioned to observe how exclusion boundaries are drawn and maintained.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__dignity_reading, judicial_interpreter_authority).
narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundaries of acceptable political discourse by establishing that speech denying personhood or human dignity is outside the protection regime — solving the coordination problem of how targeted groups can participate in politics without accepting their own categorical dehumanization as a legitimate political position.
% TRANSFER_FUNCTION: Transfers the right to speak to speakers whose speech does not deny the personhood of targeted groups; denies that right to speakers of identity-harm. Operationally: legal authority, public platforms, and social standing flow away from speakers whose core claims violate dignity doctrine; legal liability and exclusion flow toward them.
% ABSENT_VOICES: Speakers of identity-harm and boundary-case speakers are procedurally excluded — they have no seat at the table where the dignity categories are defined, and their disagreement with the exclusion is itself read as evidence of harm-intent rather than as a legitimate constitutional alternative. Competing jurisdictions with absolutist regimes are geopolitically excluded; they cannot shape the dignity reading's doctrine from within it.
% DISAPPEARANCE_RATIONALE: If categorical dignity-based speech exclusion vanished overnight, hate-speech doctrine would collapse to either absolutism or proportional balancing; political discourse in targeted communities would shift as dignity-denying speech re-entered legitimacy; the legal liability structures for platforms would reorganize; the boundary between protected and unprotected speech would become contestable again rather than categorically settled.
% FOUNDING_PROBLEM: Historical precedent: genocidal propaganda campaigns preceded major atrocities (Nazi hate speech before the Holocaust; Rwandan radio broadcasts inciting genocide). The founding problem is preventing the replication of this machinery — the speech acts that logically precede and enable mass violence against identity groups.
% FOUNDING_PROBLEM_CORROBORATION: Historians and genocide scholars attest the precedent is empirically real. The judicial authority attests the founding problem remains live and requires categorical prevention. Competing regimes and absolutist scholars attest the problem is overstated and the categorical cure worse than the disease. No corroboration from outside all three positions; the corroboration split is itself the contested structure.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82 at interval end) because the constraint restricts the speech of speakers whose core identity-claims are deemed dignity-violating, traps them in identity-locked exit, and escalates restrictions over time as boundary doctrine hardens. Suppression is high (0.76) and rising because the constraint requires continuous enforcement against speakers who reject its premises and believe they are defending legitimate positions. Theater is low (0.22) and stable because the enforcement is genuinely directed at the stated goal (preventing dignity-harm) rather than at substitute goals; the low theater reflects the fact that this is a coherent constraint, not an inertial relic. The measurement series shows extractiveness and suppression both rising through t=25 and then plateauing, indicating initial doctrine-hardening followed by stabilization as the boundaries become settled. Suppression rises faster than extractiveness because enforcement capacity builds before the exclusion reach plateaus. Speakers of identity-harm and boundary-case speakers experience different directionalities: the former are trapped and identity-locked (d near 1.0, full target), the latter constrained but not identity-fused (d near 0.7, high target but with some exit cognition).
 *
 * PERSPECTIVAL GAP:
 *   The judicial authority and the targeted groups see the constraint as legitimate protection; speakers of identity-harm see it as unjust persecution; boundary-case speakers see it as overly blunt. From the authority's seat, the constraint solves the genocide-prevention problem categorically and cleanly. From the speaker-of-identity-harm's seat, the constraint is pure extraction masked as protection — the authority is using dignity doctrine to suppress dissent from a particular view of what groups 'truly are.' The engine computes these seats' type classifications differently because the structural data (power, exit, beneficiary/victim position) is radically different — the same constraint appears as a legitimate rope from the protection seat and as a snare from the suppressed speaker's seat. The perspectival gap is not an error; it is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading produces sharply divergent d values across seats. Targeted identity groups are beneficiaries with low exit cost (if the constraint were removed, they would still exist, and could participate differently — d near 0.1, beneficiary direction). Speakers of identity-harm are full targets: their core speech is categorically denied protection, and their exit is identity-locked (cannot renounce the claim without dissolving the identity — d near 1.0, full target direction). Judicial authorities are near-symmetric on the execution side (they maintain the constraint) but near-beneficiary on the legitimacy side (the constraint validates their interpretive authority — d near 0.3, beneficiary-leaning). Boundary-case speakers are trapped without identity-lock (d near 0.65, high target but mobile-adjacent — they could exit through reframing, but the constraint does not make that clear). Platform operators face institutional pressure (d near 0.75, target direction due to liability exposure and moderation at scale). Competing speech regimes are structurally excluded (d analytically undefined, no seat in this reading's framework — they exist in a different constraint's framework entirely). The engine computes these from power + exit + beneficiary/victim declarations; the commentary explains the structural asymmetry that produces the divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (genocide prevention), and this constraint is not yet mandatrophic. However, the measurement data show that the extractiveness and suppression both rose and then plateaued, which is consistent with doctrine-hardening followed by institutional stabilization. If the founding problem were to shift (if historical atrocity risk diminished while the enforcement machinery remained), mandatrophy could emerge. The constraint currently operates as a tangled rope: it has a genuine coordination function (preventing coordinated identity-harm campaigns) AND asymmetric extraction (suppressing certain speakers). The extraction does not dominate the coordination because the founding problem remains live and the suppression is directed at actualizing the coordination goal, not at substitute goals. An alternative reading (the harm-balancing reading) would assess the same suppression data as indicating excessive extraction; the dignity reading reads it as necessary enforcement cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_denial_boundary_clarity,
    'What constitutes ''speech denying personhood or human dignity''? Are the categorical boundaries sufficiently clear to avoid false exclusions, or do reasonable parties disagree on whether particular speech crosses the line?',
    'Mapping of actual judicial decisions on boundary cases (satire, academic speech, reclamation) against the declared categorical criteria. If courts frequently disagree on whether speech is excluded, the boundary is under-specified.',
    'If the boundary is vague, trapped boundary-case speakers increase, suppression grows due to over-enforcement, and the constraint shifts toward snare characteristics (extraction beyond coordination function). If the boundary is precise and courts agree, the constraint remains coherent tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personhood_denial_boundary_clarity, empirical, 'Whether categorical dignity exclusion criteria are sufficiently determinate').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression (0.76) primarily structural (external barriers, legal penalties, platform removal) or do speakers of identity-harm internalize the constraint, accepting dignity doctrine as legitimate even if they disagree with its application?',
    'Post-removal suppression trajectory: if suppression persists after legal penalties are lifted (through internalized belief that the speech is wrong), suppression was partially internalized. If suppression collapses when enforcement ceases, suppression was structural.',
    'Internalized suppression indicates the constraint has deeper cultural legitimacy and stronger hold, making it more stable but potentially more coercive. Structural suppression indicates the constraint requires continuous enforcement and is more subject to resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Locus of suppression mechanism for identity-harm speakers').

omega_variable(
    false_positive_exclusion_rate,
    'What proportion of speech caught by the categorical exclusion criteria is actually genocidal propaganda versus legitimate boundary-case speech (satire, academic discussion, reclamation, group self-criticism)?',
    'Systematic review of removed content and judicial appeal outcomes. High false-positive rate (>20% of removed speech is borderline or misclassified) indicates over-enforcement; low rate (<5%) indicates precision.',
    'High false-positive rate would indicate the constraint operates more as snare (collateral suppression of non-target speech) than as coordinating rope (preventing genocide precursor speech). The false positive rate directly modulates whether boundary-case speakers should be read as intentional payers or as accidental targets of blunt machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_positive_exclusion_rate, empirical, 'Proportion of speech excluded that does not actually deny personhood').

omega_variable(
    competing_reading_coexistence,
    'Can the dignity reading and the absolutist reading coexist in a single global communication space, or does the adoption of one reading necessarily foreclose the other?',
    'Geopolitical analysis: jurisdictions with strong dignity enforcement coexist with absolutist jurisdictions. The test is whether speech excluded under dignity doctrine can migrate to absolutist jurisdictions and re-enter political legitimacy, or whether the constraint''s scope is globally enforcing.',
    'If readings coexist across jurisdictions, the constraint is regional/sectional (tangled rope in some territories, rope in others). If the scope is globally enforcing (through platform pressure, international norms, capital flows), the constraint has larger effective extraction and approaches snare characteristics even within absolutist jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_reading_coexistence, conceptual, 'Whether the dignity reading forecloses or coexists with absolutism globally').

omega_variable(
    kernel_reading_foreclosure,
    'Does the dignity reading''s core premise (human dignity subordinates speech freedom) logically foreclose the absolutist reading''s core premise (speech freedom is near-absolute), or can both be held simultaneously by different parties?',
    'Logical analysis: if dignity supremacy is a foundational axiom of this reading, does that axiom directly contradict the absolutist axiom of speech supremacy, or do they occupy different domains (both could be true if they regulate different aspects of rights)?',
    'If foreclosure is genuine (logical incompatibility), the relation is forecloses; if the readings simply emphasize different values without logical contradiction, the relation is coexists_with. The determination affects whether this reading''s authority can ground a global system or only a regional one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether dignity_reading and absolutist_reading are logically incompatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t5, speech_harm_boundary__dignity_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__dignity_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(spee_tr_t15, speech_harm_boundary__dignity_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__dignity_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(spee_tr_t25, speech_harm_boundary__dignity_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(spee_tr_t30, speech_harm_boundary__dignity_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__dignity_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(spee_be_t5, speech_harm_boundary__dignity_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__dignity_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(spee_be_t15, speech_harm_boundary__dignity_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__dignity_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(spee_be_t25, speech_harm_boundary__dignity_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__dignity_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__dignity_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(spee_su_t5, speech_harm_boundary__dignity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__dignity_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(spee_su_t15, speech_harm_boundary__dignity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__dignity_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(spee_su_t25, speech_harm_boundary__dignity_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__dignity_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__dignity_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__dignity_reading, 0.14).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).

% DUAL FORMULATION NOTE:
% The speech-harm boundary is a contested kernel with three readings: absolutist (near-absolute protection), dignity (categorical dignity-based exclusions, THIS constraint), and harm_balancing (presumptive protection yielding to demonstrated proportional harm). Each reading instantiates a distinct constraint with different ε, beneficiary/victim structures, and enforcement asymmetries. The readings coexist across different jurisdictions and judicial coalitions; they do not share a single framework. Decomposition follows ε-invariance (OQ-254): the readings have substantially different extractiveness profiles (absolutist ≈0.1, dignity ≈0.82, balancing ≈0.45) and different beneficiary structures. The dignity reading is linked to siblings via this network array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
