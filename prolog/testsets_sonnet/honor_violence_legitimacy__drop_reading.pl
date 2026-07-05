% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Dueling as Legitimate-but-Costly Honor Remedy (Drop Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   Between roughly 1750 and 1900, dueling among European and American gentry
 *   declined sharply in frequency while remaining, on this reading,
 *   structurally available as a legitimate response to insult. Codes of honor
 *   (Code Duello and its analogues) continued to be published, revised, and
 *   consulted; seconds continued to negotiate terms; the social meaning of
 *   accepting or declining a challenge continued to carry weight. What
 *   changed was the cost environment: state prosecution of duelists
 *   intensified, public opinion increasingly treated fatal outcomes as
 *   scandal rather than honor vindicated, and life insurance/family-ruin
 *   considerations made the external costs of actually fighting prohibitive
 *   for most who held the option. The category is authored as piton rather
 *   than mountain or rope because the constraint increasingly persists
 *   through elaborate procedural performance (challenge-and-graceful-decline
 *   theater) rather than active function — the underlying legitimacy claim
 *   (honor as violence-remediable injury) is neither actively defended by
 *   rising resistance nor naturally emergent; it is inertially maintained by
 *   gentry-class beneficiaries who gain status from possessing the remedy
 *   without needing to use it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.38).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.42).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Dueling as Legitimate-but-Costly Honor Remedy (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '49ea802e-2370-4484-8e74-c3dc12e80f6b').
narrative_ontology:cs_kernel_codification('49ea802e-2370-4484-8e74-c3dc12e80f6b', distributed).
narrative_ontology:cs_authority_grounding('49ea802e-2370-4484-8e74-c3dc12e80f6b', practice).
narrative_ontology:cs_interpretation_layer_present('49ea802e-2370-4484-8e74-c3dc12e80f6b').
narrative_ontology:cs_reading_relation('49ea802e-2370-4484-8e74-c3dc12e80f6b', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('49ea802e-2370-4484-8e74-c3dc12e80f6b', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('49ea802e-2370-4484-8e74-c3dc12e80f6b', foundational, honor_remedy_legitimacy_survives_disuse).
narrative_ontology:cs_axiom_status(honor_remedy_legitimacy_survives_disuse, holdable).
narrative_ontology:cs_axiom_grounding('49ea802e-2370-4484-8e74-c3dc12e80f6b', honor_remedy_legitimacy_survives_disuse, conventional).
narrative_ontology:cs_axiom('49ea802e-2370-4484-8e74-c3dc12e80f6b', foundational, cost_not_concept_governs_practice_frequency).
narrative_ontology:cs_axiom_status(cost_not_concept_governs_practice_frequency, holdable).
narrative_ontology:cs_axiom_grounding('49ea802e-2370-4484-8e74-c3dc12e80f6b', cost_not_concept_governs_practice_frequency, empirically_contingent).
narrative_ontology:cs_reference_frame('49ea802e-2370-4484-8e74-c3dc12e80f6b', gentry_honor_remedy_intact).
narrative_ontology:cs_drift_state('49ea802e-2370-4484-8e74-c3dc12e80f6b', late_nineteenth_century, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('49ea802e-2370-4484-8e74-c3dc12e80f6b', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, gentry_class_honor_holders).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, dueling_code_arbiters).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, duelists_and_families).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, lower_status_men_denied_the_remedy).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__drop_reading, honor_as_violence_remediable_injury).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain the standing option to answer an insult with a formal challenge, which continues to function as a marker of class distinction and personal seriousness even as fewer men actually fight. The mere availability of the remedy preserves social rank without requiring its exercise; the cost of dueling (legal risk, injury, death, prosecution under increasingly enforced statutes) has simply made most holders choose not to invoke it, not lose the right to invoke it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, gentry_class_honor_holders, beneficiary,
    powerful, generational, constrained, national).

% Seconds, codes of honor (like the Irish Code Duello), and informal courts of gentlemen continue to administer the rules governing when a challenge is proper and how it may be honorably declined or settled short of violence. Their institutional role persists and even grows in importance as the actual practice becomes rarer, since managing de-escalation without dishonor becomes the primary function.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, dueling_code_arbiters, agenda_setter,
    organized, generational, mobile, national).

% The men who do still duel, and their families, bear escalating external costs: criminal prosecution, social scandal if the law intervenes, financial ruin from fines, and the ever-present risk of death or maiming. Because the practice is rare rather than abolished, each remaining case draws disproportionate legal and public attention, and there is no clean exit — declining a challenge under the still-live code carries reputational cost even as accepting carries mortal and legal cost.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, duelists_and_families, payer,
    moderate, biographical, trapped, regional).

% Men outside the gentry class experience insult and injury to reputation but have no standing to issue or receive a formal challenge under the code; the remedy's structural legitimacy protects an option they cannot use, while ordinary defamation or assault against them is handled (if at all) through ordinary criminal law, not honor mechanisms.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, lower_status_men_denied_the_remedy, excluded,
    powerless, biographical, trapped, local).

% Increasingly criminalize dueling and prosecute survivors and seconds, raising the external cost of the practice without formally abolishing its social legitimacy. Their enforcement is what drives the frequency drop this reading identifies, while the honor code itself remains outside their jurisdiction to redefine.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, state_prosecutors_and_legislators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, state_prosecutors_and_legislators, agenda_setter).

% Study the decline curve of dueling and debate whether it reflects changed incentives (this reading) or changed meaning of honor itself (the contraction reading), or both operating together (the composite reading). Their disagreement is exactly the kernel contest this story is one reading of.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dueling code coordinates elite conflict resolution by providing a ritualized, rule-bound alternative to unstructured violence or unresolved insult — it channels honor disputes through seconds, negotiated terms, and socially recognized off-ramps (apology, retraction) that let most challenges end without bloodshed.
% TRANSFER_FUNCTION: Moves risk of death, injury, and legal jeopardy onto the individual duelists and their families while the social capital of 'possessing a live honor remedy' remains distributed to the entire gentry class, whether or not any given member ever fights. Exclusion from the code transfers unredressed reputational injury onto lower-status men with no formal claim to defend.
% ABSENT_VOICES: Lower-status men who suffer the same insults and injuries gentry duelists formally remedy have no voice in the code and are not consulted on its persistence; women, who could not duel under any reading, are structurally absent from the entire mechanism despite bearing derivative costs (widowhood, family ruin) when duels occur.
% DISAPPEARANCE_RATIONALE: Proponents of this reading hold that if the legal deterrents (prosecution, dueling statutes) vanished overnight, dueling frequency would rise back toward historical levels because the underlying legitimacy structure — the code, the seconds, the social meaning of the challenge — never actually disappeared, only went unused under cost pressure. Historians favoring the contraction reading would predict little change, since they hold the conceptual redefinition of honor (not cost) did the work. This unresolved disagreement is the kernel contest itself.
% FOUNDING_PROBLEM: Elite societies needed a mechanism to resolve insults to personal and family honor that ordinary courts could not adjudicate (courts handle property and injury, not reputation and 'face'), while also constraining the potential for insult-driven violence to become open-ended blood feud.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians examining prosecution records and surviving codes of honor attest the formal remedy structure persisted well after actual practice collapsed (external, non-beneficiary corroboration). Contemporary gentry defenders of the code, by contrast, self-report the problem as still live ('a man must still be able to answer an insult') — that account comes only from within the beneficiary class and is treated here as testimony, not settled fact.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, contested).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).
:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38) and roughly flat because the drop reading holds the underlying transfer structure (risk onto duelists, status-good to the gentry class as a whole) is constant across the interval — only the frequency of activation changes, not the structure's intrinsic extraction rate. Theater ratio rises sharply (0.15 to 0.55) because as actual duels become rare, the social machinery around the code (elaborate rules for honorable decline, seconds' negotiations that resolve disputes without violence, published codes maintained more as reference than as active playbook) increasingly substitutes performative maintenance for functional exercise — this is precisely the piton signature. Suppression_requirement rises (0.2 to 0.42) tracking increasing state prosecution effort required to keep actual duel frequency down, which is the drop reading's causal mechanism made visible: it is EXTERNAL suppression (legal) driving down practice, not internal collapse of the code's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the gentry-holder seat, the code registers as a stable, low-cost, purely honorific institution — nothing has changed structurally, and its declining exercise looks like prudence, not decay. From the duelist-family seat actually caught in a live challenge, the same code registers as a trap: legitimate enough that declining costs status, dangerous enough that accepting risks death and prosecution, with no clean exit available under the code's own terms. The engine should compute these seats as structurally different types from the same authored data, which is the point of authoring only the drop mechanism here rather than blending it with the contraction reading's redefinition claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Gentry_class_honor_holders sit near the beneficiary end: they retain the status good of possessing a live honor remedy at near-zero cost, since most never invoke it. Duelists_and_families sit near the full-target end: they are trapped by a code that still carries real reputational cost for declining even as it carries escalating legal and mortal cost for accepting, with the external-cost regime (this reading's central variable) squeezing them from both directions. Dueling_code_arbiters are structurally positioned as agenda-setters whose institutional relevance is arguably enhanced, not diminished, by the drop in actual duels, since managing honorable de-escalation becomes their primary function. Lower_status_men are excluded entirely from the remedy and bear its costs (unredressed insult) without any of its benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (courts cannot adjudicate honor/reputation, and unstructured insult-violence risks open blood feud) is contested as live or dead: the drop reading implicitly treats it as still live in principle (the remedy is retained, not abolished) but rarely activated because the cost of activation has risen, which is a different claim from the contraction reading's assertion that the problem itself has been redefined out of existence. This is exactly the ambiguity the piton classification is built to hold: a structure that persists past the point of frequent use, defended by inertia and performative maintenance (rising theater_ratio) rather than either active necessity or active resistance to its removal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drop_vs_contraction_mechanism,
    'Did dueling''s decline occur because external costs made a still-legitimate practice too expensive to exercise (this reading), or because the concept of honor was redefined to exclude violence as legitimate (the contraction reading), or both simultaneously (the composite reading)?',
    'Comparative analysis of published honor codes and etiquette literature across the interval: if codes continue to explicitly endorse dueling as proper while prosecution records show rising enforcement, that supports the drop mechanism; if codes themselves are rewritten to characterize dueling as dishonorable or barbaric, that supports the contraction mechanism; textual evidence of both operating in different social strata or time bands would support the composite reading.',
    'If contraction dominates, this story''s classification should shift toward describing an obsolete mountain-like norm (naturalized redefinition) rather than a piton (inertially retained legitimacy); if drop dominates, the piton framing here is correct; if composite, neither single-mechanism story is complete and the composite_reading becomes the operative account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drop_vs_contraction_mechanism, conceptual, 'The kernel-defining ambiguity between the three sibling readings of honor_violence_legitimacy.').

omega_variable(
    gentry_status_good_persistence,
    'Does the mere structural availability of the dueling option continue to confer real status value on gentry holders who never exercise it, or has that status value itself quietly decayed even while the formal legitimacy claim persists?',
    'Social-historical analysis of how contemporaries who declined challenges were actually treated — rising social acceptance of declining (versus continued stigma) would indicate the status good is decaying independent of formal legitimacy, undermining the drop reading''s core claim that legitimacy remained intact.',
    'If declining lost its stigma entirely, the drop reading collapses toward the contraction reading even while formal codes remained unrevised — legitimacy on paper without legitimacy in practice is itself evidence of conceptual redefinition, not mere cost pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gentry_status_good_persistence, empirical, 'Whether the status benefit gentry holders derive from possessing the remedy is itself intact or eroding.').

omega_variable(
    class_exclusion_naturalization,
    'Is the exclusion of lower-status men from the dueling remedy a background fact treated as natural by all seats, or is it itself a live source of contest that this reading under-weights by treating the code as a purely intra-gentry mechanism?',
    'Examine period commentary and legal challenges (if any) from non-gentry actors objecting to their exclusion from honor remedies, versus alternative remedies (defamation suits, informal violence) they used instead.',
    'If exclusion was actively contested rather than simply structural, the victim set for this reading should be expanded and the suppression metric increased to reflect active exclusion enforcement, not just external legal cost on duelists themselves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(class_exclusion_naturalization, empirical, 'Whether class-based exclusion from the honor remedy was contested or naturalized during the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__drop_reading, theater_ratio, 1750, 0.15).
narrative_ontology:measurement_basis(hono_tr_t1750, observed).
narrative_ontology:measurement(hono_tr_t1780, honor_violence_legitimacy__drop_reading, theater_ratio, 1780, 0.22).
narrative_ontology:measurement_basis(hono_tr_t1780, observed).
narrative_ontology:measurement(hono_tr_t1810, honor_violence_legitimacy__drop_reading, theater_ratio, 1810, 0.34).
narrative_ontology:measurement_basis(hono_tr_t1810, observed).
narrative_ontology:measurement(hono_tr_t1840, honor_violence_legitimacy__drop_reading, theater_ratio, 1840, 0.45).
narrative_ontology:measurement_basis(hono_tr_t1840, observed).
narrative_ontology:measurement(hono_tr_t1870, honor_violence_legitimacy__drop_reading, theater_ratio, 1870, 0.51).
narrative_ontology:measurement_basis(hono_tr_t1870, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__drop_reading, theater_ratio, 1900, 0.55).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__drop_reading, base_extractiveness, 1750, 0.3).
narrative_ontology:measurement_basis(hono_be_t1750, observed).
narrative_ontology:measurement(hono_be_t1780, honor_violence_legitimacy__drop_reading, base_extractiveness, 1780, 0.32).
narrative_ontology:measurement_basis(hono_be_t1780, observed).
narrative_ontology:measurement(hono_be_t1810, honor_violence_legitimacy__drop_reading, base_extractiveness, 1810, 0.35).
narrative_ontology:measurement_basis(hono_be_t1810, observed).
narrative_ontology:measurement(hono_be_t1840, honor_violence_legitimacy__drop_reading, base_extractiveness, 1840, 0.37).
narrative_ontology:measurement_basis(hono_be_t1840, observed).
narrative_ontology:measurement(hono_be_t1870, honor_violence_legitimacy__drop_reading, base_extractiveness, 1870, 0.38).
narrative_ontology:measurement_basis(hono_be_t1870, observed).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__drop_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement_basis(hono_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__drop_reading, suppression_requirement, 1750, 0.2).
narrative_ontology:measurement_basis(hono_su_t1750, observed).
narrative_ontology:measurement(hono_su_t1780, honor_violence_legitimacy__drop_reading, suppression_requirement, 1780, 0.28).
narrative_ontology:measurement_basis(hono_su_t1780, observed).
narrative_ontology:measurement(hono_su_t1810, honor_violence_legitimacy__drop_reading, suppression_requirement, 1810, 0.34).
narrative_ontology:measurement_basis(hono_su_t1810, observed).
narrative_ontology:measurement(hono_su_t1840, honor_violence_legitimacy__drop_reading, suppression_requirement, 1840, 0.39).
narrative_ontology:measurement_basis(hono_su_t1840, observed).
narrative_ontology:measurement(hono_su_t1870, honor_violence_legitimacy__drop_reading, suppression_requirement, 1870, 0.41).
narrative_ontology:measurement_basis(hono_su_t1870, observed).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__drop_reading, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement_basis(hono_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__drop_reading, 0.1).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_violence_legitimacy kernel. The drop_reading (this file) holds legitimacy was structurally retained while external cost drove down frequency. The contraction_reading holds the concept of honor itself was redefined to exclude violence, making dueling unthinkable rather than merely expensive. The composite_reading holds both mechanisms operated simultaneously and is not reducible to either alone. Each story carries its own epsilon and its own metric profile per the epsilon-invariance principle; they are linked here rather than merged because they make incompatible claims about WHY frequency dropped, which is exactly the structural disagreement the kernel-reading apparatus exists to hold open rather than resolve prematurely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
