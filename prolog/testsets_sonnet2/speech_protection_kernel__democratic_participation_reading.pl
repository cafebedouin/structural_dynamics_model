% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Political-Speech-Primacy Reading of Speech Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the democratic-participation reading of the
 *   speech-protection kernel: the constitutional claim that speech protection
 *   is strongest, and least susceptible to government restriction, precisely
 *   when the expression is instrumentally necessary to self-governance —
 *   political campaigning, election-related advocacy, and reporting on
 *   government. The reading builds a tiered-scrutiny architecture in which
 *   political speech occupies the top tier and other categories of expression
 *   (commercial, artistic, workplace, and speech that functions as harassment
 *   or defamation of private targets) receive progressively weaker
 *   protection. Over roughly six decades of doctrinal development the tier
 *   has expanded to cover forms of political spending and association further
 *   from core electoral advocacy, extending the protective ceiling's reach
 *   while its extraction on adjacent, lower-tier speakers has grown
 *   proportionally. This is the theory's own constraint under its own lights;
 *   the sibling readings (absolutist, harm-threshold, marketplace, dignity)
 *   are separate constraints with their own ε, authored elsewhere and linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - electoral_candidates: top-tier beneficiary of the protective hierarchy
 *   - political_journalists: top-tier beneficiary, press function tied to self-governance
 *   - commercial_speakers: intermediate-tier payer, subject to disclosure and misleading-claims regulation unavailable against political speech
 *   - workplace_speakers: powerless payer, sorted out of the protected core by employment-context doctrine
 *   - judiciary: agenda-setter administering the tiered sorting mechanism
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
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Political-Speech-Primacy Reading of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, 'cf763dc4-2346-4118-969c-d7a6037e3150').
narrative_ontology:cs_kernel_codification('cf763dc4-2346-4118-969c-d7a6037e3150', fixed_text).
narrative_ontology:cs_authority_grounding('cf763dc4-2346-4118-969c-d7a6037e3150', lineage).
narrative_ontology:cs_interpretation_layer_present('cf763dc4-2346-4118-969c-d7a6037e3150').
narrative_ontology:cs_reading_relation('cf763dc4-2346-4118-969c-d7a6037e3150', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('cf763dc4-2346-4118-969c-d7a6037e3150', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf763dc4-2346-4118-969c-d7a6037e3150', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('cf763dc4-2346-4118-969c-d7a6037e3150', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('cf763dc4-2346-4118-969c-d7a6037e3150', foundational, protection_scales_with_self_governance_necessity).
narrative_ontology:cs_axiom_status(protection_scales_with_self_governance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('cf763dc4-2346-4118-969c-d7a6037e3150', protection_scales_with_self_governance_necessity, instrumental).
narrative_ontology:cs_axiom('cf763dc4-2346-4118-969c-d7a6037e3150', foundational, internal_hierarchy_among_protected_categories_is_legitimate).
narrative_ontology:cs_axiom_status(internal_hierarchy_among_protected_categories_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('cf763dc4-2346-4118-969c-d7a6037e3150', internal_hierarchy_among_protected_categories_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('cf763dc4-2346-4118-969c-d7a6037e3150', core_political_speech_protection_against_incumbent_entrenchment).
narrative_ontology:cs_drift_state('cf763dc4-2346-4118-969c-d7a6037e3150', contemporary_campaign_finance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf763dc4-2346-4118-969c-d7a6037e3150', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, electoral_candidates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_journalists).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, advocacy_organizations).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, artists_and_entertainers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, workplace_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, targets_of_low_value_speech_categories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, self_governance_theory_of_the_first_amendment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive maximal judicial scrutiny of any restriction on their campaign speech, core political advocacy, and criticism of incumbents. Courts treat this speech as occupying the top tier of the protective hierarchy, making regulation of their core messaging extremely difficult to sustain.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, electoral_candidates, beneficiary,
    organized, biographical, mobile, national).

% Reporting and commentary on government, elections, and public affairs receives the strongest available protection because it is classified as speech necessary to self-governance. They can publish sharply critical material about officials with near-immunity from defamation and regulatory pressure that would apply to comparable claims in commercial contexts.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_journalists, beneficiary,
    organized, biographical, mobile, national).

% Issue-advocacy and lobbying speech tied to legislative and electoral outcomes sits inside the protected core. This lets them spend, organize, and publish with minimal restriction, so long as their activity can be characterized as political rather than commercial or private.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Benefit from the same protective ceiling when speaking to constituents and campaigning, but also absorb the cost of being maximally exposed to criticism and satire that the hierarchy places beyond their power to suppress — a structural check on incumbency they cannot opt out of while claiming the doctrine's protection for themselves.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders, payer).

% Advertising and commercial disclosure speech sits in an intermediate tier and can be regulated (mandated disclosures, restrictions on misleading claims) using standards that would fail if applied to core political speech. A business cannot invoke the same immunity a candidate invokes for functionally similar persuasive messaging.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, commercial_speakers, payer,
    moderate, biographical, constrained, national).

% Obscenity doctrine and content-based restrictions on artistic and entertainment expression rest on the premise that such speech is further from the self-governance core, so it draws weaker protection than expression a court characterizes as political — even when the underlying communicative or social value is comparably high.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, artists_and_entertainers, payer,
    moderate, biographical, constrained, national).

% Private-sector employees whose on-the-job speech touches matters of public concern get inconsistent, weaker protection than a candidate or journalist saying the same thing, because courts sort their speech by employment context and personal grievance rather than granting it the political-speech ceiling — leaving them exposed to workplace discipline for expression that would be untouchable coming from a political actor.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, workplace_speakers, payer,
    powerless, biographical, trapped, regional).

% Individuals targeted by speech sorted into lower-protection categories (commercial fraud, some defamation, certain forms of harassment adjacent to political speech) sometimes find courts reluctant to act because the speaker recharacterizes the expression as political commentary to claim the top tier, shifting the burden of harm onto the target.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, targets_of_low_value_speech_categories, payer,
    powerless, biographical, trapped, national).

% Courts construct and police the tiered-scrutiny architecture, deciding case by case whether speech counts as core political expression, commercial speech, or another lower-tier category. This sorting function is the mechanism through which the doctrine's hierarchy is actually enforced.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__democratic_participation_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__democratic_participation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, administrable priority rule for adjudicating speech disputes by identifying the category of expression most instrumentally necessary to self-governance — political speech about candidates, elections, and government — and giving it the strongest presumption against regulation, so democratic contestation is not vulnerable to majoritarian suppression.
% TRANSFER_FUNCTION: Moves protective certainty toward speakers whose expression can be characterized as political (candidates, journalists, advocacy groups) and away from speakers in adjacent categories (commercial, artistic, workplace, private) who receive weaker doctrinal shields for functionally comparable expressive activity, and away from targets of speech who cannot obtain relief once a court sorts the speech into the protected tier.
% ABSENT_VOICES: Speakers whose expression falls into the classification's lower tiers rarely get to argue for reclassification on equal footing — commercial speakers, workers, and artists are sorted into weaker categories by doctrine developed primarily in cases about political and press speech, with little input from those bearing the weaker-protection consequences. Targets of political-speech-immunized harassment or defamation are also structurally underrepresented in the doctrine's own self-justifying case law.
% DISAPPEARANCE_RATIONALE: If the political-speech-primacy hierarchy vanished and courts applied a flat standard of review across all speech categories, campaign regulation, commercial disclosure law, obscenity doctrine, and workplace speech protections would all have to be rebuilt from a different baseline — either flattening upward (near-absolutist protection everywhere) or flattening downward (uniform balancing tests), each producing a substantially different speech landscape for every stakeholder listed.
% FOUNDING_PROBLEM: The doctrine was built to prevent government from insulating itself and incumbents from criticism and electoral challenge by using generally-applicable speech restrictions — the paradigm case being seditious libel and restrictions on election-related advocacy.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and civil liberties scholars outside the political-speech-benefiting class (academics, some judges in dissent) attest the core anti-sedition, anti-incumbent-entrenchment problem remains partially live but argue the doctrine has been extended well past that founding rationale to shield speech (e.g., large-scale campaign spending, mischaracterized commercial advocacy) with only an attenuated connection to self-governance; officeholders and advocacy groups who benefit from the current scope attest the doctrine tracks the founding problem faithfully.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.42 at interval end) rather than high because the coordination function — protecting self-governance from incumbent entrenchment — is genuine and substantial; the extractive component comes from the doctrine's internal hierarchy systematically under-protecting non-political categories that are functionally similar in communicative value. Suppression (0.38) reflects the doctrine's real coercive machinery (injunctions against commercial claims, obscenity prosecutions, workplace discipline upheld under weaker standards) applied unevenly across tiers, not applied to political speech itself. Accessibility collapse is moderate (0.35): speakers in lower tiers still have some doctrinal argument space (they can attempt recharacterization as political speech), so alternatives have not fully collapsed. Resistance (0.55) is substantial because commercial speech advocates, artists, and labor-speech advocates actively litigate for tier reclassification.
 *
 * DIRECTIONALITY LOGIC:
 *   Electoral candidates, journalists, and advocacy organizations are structural beneficiaries: the doctrine was substantially built around their expressive activity and grants them the strongest presumption against regulation, so their directionality sits near the beneficiary end. Commercial speakers, artists, and workplace speakers are targets of the hierarchy's downward sort — the same doctrine that maximally protects the top tier explicitly licenses weaker protection, hence weaker positional leverage, for them; their directionality sits nearer the target end despite none of them being powerless in an absolute sense (commercial speakers especially are often well-resourced, but structurally disadvantaged relative to political speakers under this specific doctrine). Workplace speakers and targets of low-value-category speech are powerless and trapped, producing the highest effective extraction in the set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing incumbent-entrenching suppression of political challengers and critics) is genuinely still live in some jurisdictions and contexts, which is why founding_problem_status is authored as contested rather than dead — this blocks a naive mandatrophy verdict. But the doctrine's contemporary reach into large-scale campaign financing and non-electoral political advocacy is defended by beneficiaries using the same founding rationale that justified core anti-sedition protection, which is exactly the drift pattern the R5 corroboration check is designed to surface: self-asserted continuity of purpose from the benefiting class, contested by outside commentators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_speech_boundary_indeterminacy,
    'Where exactly does ''speech necessary to self-governance'' end and other categories of expression begin — is the boundary principled or does it track which speakers have resources to litigate for reclassification?',
    'Longitudinal doctrinal analysis of category-boundary cases: track whether reclassification success correlates with speaker resources/organization rather than with a stable definitional test for political necessity.',
    'If reclassification success tracks speaker power rather than a principled boundary, the tiering functions partly as a resource-sorting mechanism dressed as a self-governance theory, strengthening the tangled_rope reading; if the boundary is stably principled, the extractive component is smaller than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_speech_boundary_indeterminacy, empirical, 'Whether the political/non-political speech boundary is principled or resource-tracking.').

omega_variable(
    self_governance_theory_scope_creep,
    'Has the doctrine''s protective ceiling for ''political speech necessary to self-governance'' expanded to cover activity (e.g., large-scale independent campaign spending) that is only loosely connected to the founding anti-entrenchment rationale?',
    'Compare the doctrine''s founding-era case set (core electoral advocacy, seditious libel) against its contemporary scope; assess whether the connection to self-governance has attenuated as the beneficiary class has grown.',
    'If scope has crept well beyond the founding rationale, the founding_problem_status of ''contested'' should be read as leaning toward partial obsolescence for the doctrine''s outer applications even where its core remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_governance_theory_scope_creep, conceptual, 'Whether doctrinal scope has outrun the founding self-governance rationale.').

omega_variable(
    reading_framing_kernel_vs_tiering_mechanism,
    'Is the correct unit of analysis the self-governance JUSTIFICATION for heightened political-speech protection (a normative kernel reading), or the TIERED-SCRUTINY MECHANISM courts use to implement it (a separate, more mechanical constraint about how courts sort speech into categories)?',
    'Compare classification outcomes if this story were authored around the tiering mechanism itself (as an administrative sorting apparatus) versus around the self-governance justification (as a normative theory) — check whether ε and stakeholder structure diverge meaningfully between the two framings.',
    'If the mechanism framing would produce a markedly different beneficiary/victim structure (e.g., emphasizing judicial discretion and litigation-resource asymmetry over the self-governance rationale itself), that divergence would indicate two distinct constraints requiring separate stories rather than one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_kernel_vs_tiering_mechanism, conceptual, 'Whether the normative justification and its implementing mechanism are one constraint or two.').


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
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 50, 0.36).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, dignity_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraints decomposing the natural-language 'speech protection kernel.' Each sibling reading (absolutist, harm_threshold, marketplace, dignity) is authored as a separate constraint with its own ε, beneficiary/victim structure, and claimed type, per the ε-invariance principle — the readings are not observable-selections on one constraint but structurally distinct claims about what grounds and bounds speech protection. This reading's ε (0.42, tangled_rope) reflects a genuine coordination function (protecting political contestation) layered with asymmetric extraction (weaker protection for non-political speakers); other readings will show different ε because they ground protection differently (categorical absolutism, harm-avoidance, truth-discovery, anti-subordination) and therefore sort winners and losers differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
