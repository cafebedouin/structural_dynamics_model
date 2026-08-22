% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect Reading of the Sovereignty/Intervention Tension
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This story instantiates the Responsibility to Protect (R2P) reading of
 *   the Article 2(7)/Chapter VII tension: sovereignty is treated as
 *   conditional on a state's protection of its own population, and systematic
 *   atrocity (genocide, ethnic cleansing, war crimes, crimes against
 *   humanity) triggers a responsibility on the part of the international
 *   community to respond, up to and including coercive intervention
 *   authorized through the Security Council. This is presented as a single,
 *   structurally coherent reading — not a blend of this reading with the
 *   sovereignty-first reading, which is a distinct sibling constraint
 *   (article_2_7_chapter_vii_tension__sovereignty_first_reading) with its own
 *   ε, beneficiaries, and victims. Under the R2P reading's own lights, the
 *   standing arrangement it critiques and would override is strict
 *   Westphalian non-interference; ε is authored here for that standing
 *   arrangement (the sovereignty-first status quo) as the R2P reading itself
 *   assesses it — high, because the reading judges the non-interference norm
 *   to have functioned, historically, as a shield for atrocity.
 *
 * KEY AGENTS:
 *   - persecuted_civilian_populations: primary beneficiary (powerless/trapped) — cannot exit their own state; the doctrine's entire justification runs through their protection
 *   - targeted_state_governments: primary target (powerful/constrained) — sovereignty becomes conditional and forfeitable once atrocity is found
 *   - intervening_coalition_states: agenda-setters (institutional/arbitrage) — invoke the doctrine, control evidentiary thresholds, face little symmetric risk
 *   - un_security_council: formal authorization chokepoint (institutional/analytical) — selective application governed by permanent-member veto
 *   - weaker_un_member_states and sovereignty_first_reading_adherents: excluded voices — object to selective application from outside the Security Council room
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.68).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.58).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect Reading of the Sovereignty/Intervention Tension").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '7df178c4-3fc1-407a-aa8d-ec5951153a0f').
narrative_ontology:cs_kernel_codification('7df178c4-3fc1-407a-aa8d-ec5951153a0f', distributed).
narrative_ontology:cs_authority_grounding('7df178c4-3fc1-407a-aa8d-ec5951153a0f', distributed).
narrative_ontology:cs_reading_relation('7df178c4-3fc1-407a-aa8d-ec5951153a0f', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('7df178c4-3fc1-407a-aa8d-ec5951153a0f', foundational, sovereignty_conditional_on_population_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_population_protection, holdable).
narrative_ontology:cs_axiom_grounding('7df178c4-3fc1-407a-aa8d-ec5951153a0f', sovereignty_conditional_on_population_protection, deontological).
narrative_ontology:cs_axiom('7df178c4-3fc1-407a-aa8d-ec5951153a0f', foundational, systematic_atrocity_triggers_international_responsibility).
narrative_ontology:cs_axiom_status(systematic_atrocity_triggers_international_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('7df178c4-3fc1-407a-aa8d-ec5951153a0f', systematic_atrocity_triggers_international_responsibility, conventional).
narrative_ontology:cs_reference_frame('7df178c4-3fc1-407a-aa8d-ec5951153a0f', un_charter_westphalian_non_interference).
narrative_ontology:cs_drift_state('7df178c4-3fc1-407a-aa8d-ec5951153a0f', post_rwanda_srebrenica_reckoning, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7df178c4-3fc1-407a-aa8d-ec5951153a0f', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_civilian_populations).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_state_governments).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, westphalian_sovereignty_norm).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_as_responsibility_doctrine).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, population_protection_supersedes_non_interference).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face systematic atrocity (genocide, war crimes, ethnic cleansing, crimes against humanity) at the hands of their own state or a collapsed state's factions. Under the R2P reading, their protection is the entire justification for overriding the non-interference norm; they cannot exit their own state and depend entirely on external actors choosing to invoke the responsibility to protect on their behalf.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_civilian_populations, beneficiary,
    powerless, immediate, trapped, national).

% Lose the presumption of inviolable domestic jurisdiction the moment their conduct toward their own population is characterized as mass atrocity. Under this reading their sovereignty becomes conditional and forfeit-able; their only routes out of intervention are halting the atrocity, securing patron-state protection in the Security Council, or absorbing intervention. They experience the constraint as targeted extraction of their governing authority.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_state_governments, payer,
    powerful, biographical, constrained, national).

% Powerful states and coalitions invoke the R2P reading to authorize or justify intervention (military, sanctions, tribunal referral) against a target state's declared will. They administer the invocation, control the evidentiary threshold for what counts as 'systematic atrocity,' and bear little symmetric risk of having the doctrine turned against their own conduct given their Security Council leverage.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_coalition_states, agenda_setter,
    institutional, generational, arbitrage, global).

% The formal chokepoint where R2P's Pillar Three (coercive response) is authorized; permanent members' veto power means the doctrine's application is selective — invoked against weaker or unaligned states, blocked for allies of a permanent member. Administers whether the reading translates into authorized action or remains rhetorical.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, un_security_council, observer).

% Smaller and non-aligned states largely lack veto power or coalition-building capacity to invoke R2P defensively or to block its selective application against themselves. They observe the doctrine applied asymmetrically and worry their own sovereignty is more conditional than that of great powers, but have limited voice in Security Council deliberations.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, weaker_un_member_states, excluded,
    moderate, generational, constrained, global).

% States and blocs (often post-colonial or historically intervened-upon) that hold the sibling reading — sovereignty as foundational, intervention requiring explicit consent or narrow Chapter VII inter-state-aggression authorization. They are structurally present in the same UN system but are not the authors of this reading; their objections surface in General Assembly debate and Non-Aligned Movement statements rather than in Security Council authorization.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_first_reading_adherents, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__r2p_reading, diffuse).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__r2p_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a normative and procedural mechanism by which the international community can coordinate a collective response when a state's own government becomes the perpetrator or enabler of mass atrocity against its population, closing the gap left by pure non-interference in cases where the state itself is the threat.
% TRANSFER_FUNCTION: Moves the presumption of protected domestic jurisdiction away from the targeted state and toward external actors authorized to act; correspondingly moves protective attention and resources toward the persecuted population, at the cost of the target government's governing authority and, cumulatively, of the general strength of the non-interference norm for all states.
% ABSENT_VOICES: Sovereignty-first adherents and many weaker UN member states would object that the doctrine is applied selectively against states without great-power patrons, but their objections are voiced in the General Assembly and NAM communiques rather than in the Security Council chamber where authorization actually happens.
% DISAPPEARANCE_RATIONALE: If the R2P reading vanished overnight, the international system would revert to treating systematic domestic atrocity as presumptively shielded by Article 2(7) absent Security Council authorization on narrower grounds; humanitarian intervention would lose its primary post-1990s normative vocabulary, tribunal referrals and coercive responses to mass atrocity would require new justificatory frameworks, and targeted governments would regain a stronger presumption of non-interference.
% FOUNDING_PROBLEM: Genocide and mass atrocity (Rwanda, Srebrenica) proceeded with the international community largely inert because the prevailing sovereignty norm treated internal state conduct as off-limits to external action absent classic inter-state aggression; the 2001 ICISS report and 2005 World Summit sought a doctrine under which the international community would not stand by again.
% FOUNDING_PROBLEM_CORROBORATION: UN member states nearly universally endorsed the 2005 World Summit language in principle. Independent scholars and the ICISS commission (outside any single beneficiary government) attest the founding problem — inaction in the face of genocide — remains structurally live given ongoing mass-atrocity situations where intervention was not invoked (Syria, Xinjiang, Tigray) alongside cases where it was (Libya 2011), suggesting selective rather than principled application; sovereignty-first states dispute the doctrine's legitimacy altogether rather than merely its consistency.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68 at interval end) because, from the R2P reading's own structural position, the doctrine strips targeted states of the normal presumption of inviolable domestic jurisdiction and does so asymmetrically — invoked against weaker or unaligned states (Libya 2011) while blocked or unused for others facing comparable atrocity but shielded by great-power patronage (Syria, Xinjiang). Suppression is moderate-high (0.58): the doctrine's force depends on Security Council authorization machinery that itself suppresses dissenting outcomes via veto, and on coercive capacity (military, sanctions) to make intervention real. Theater ratio is elevated (0.42) and rising through 2018 before a partial correction: much invocation of R2P language in diplomatic statements is rhetorical without triggering Pillar Three action, particularly after the contested 2011 Libya intervention chilled subsequent enthusiasm for coercive follow-through. The measurement series show extractiveness peaking around the 2011 Libya intervention (0.70) — the clearest instance of coercive R2P application — then settling to a lower but still elevated post-Libya plateau as states grew more cautious about authorizing force under the doctrine's banner.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening-coalition and Security-Council agenda-setter seats, this reading operates as a genuine, hard-won coordination achievement correcting the Rwanda/Srebrenica failure of inaction. From the targeted-state-government seat, the identical structure operates as a selectively-applied extraction of governing authority, justified after the fact by atrocity findings that the target state disputes or contextualizes differently. The engine computes these as different seat classifications from the same structural data; neither seat's report is more 'true' than the other — both are read off the authored power/exit positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations are the clearest beneficiary (d near the full-beneficiary end): the doctrine exists to protect them and they bear the atrocity cost the doctrine is meant to prevent. Targeted state governments sit at the full-target end: sovereignty is explicitly conditioned and can be overridden specifically because of their conduct. Intervening coalition states and the Security Council's permanent members sit close to the beneficiary/agenda-setter end despite bearing intervention costs, because they control invocation and are structurally insulated from having the doctrine turned against their own domestic conduct (their veto power forecloses that symmetry). Weaker UN member states and sovereignty-first adherents are structurally excluded from authorship of this reading even though the reading's application could someday reach them; their directionality reflects latent vulnerability rather than current extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genocidal inaction, per Rwanda and Srebrenica) is judged contested rather than resolved: mass-atrocity situations continue to arise (Syria, Xinjiang, Tigray) where R2P was invoked rhetorically but not translated into Pillar Three action, while Libya 2011 stands as the clearest coercive application and is widely read — including by states outside the intervening coalition — as having been used in a manner exceeding its protective mandate (regime change rather than narrowly-targeted civilian protection). This tangled-rope classification, rather than collapsing the doctrine into pure extraction or pure coordination, preserves the genuine coordination function (a real gap existed and something like R2P fills it) while registering that its application carries systematic, asymmetric costs onto specific target states not proportionate to a neutral atrocity-severity threshold — exactly the pattern a Snare classification would miss (denying any real coordination function) and a pure Rope classification would also miss (denying the asymmetric extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_objectivity,
    'Is the evidentiary threshold for ''systematic atrocity'' triggering R2P applied consistently across cases, or does great-power patronage determine which atrocities trigger intervention and which do not?',
    'Comparative case analysis across R2P invocations (Libya, Cote d''Ivoire) versus non-invocations (Syria, Xinjiang, Myanmar) controlling for atrocity severity and casualty counts, isolating permanent-member alignment as the residual explanatory variable.',
    'If threshold application tracks great-power alignment rather than atrocity severity, the R2P reading''s extraction is better characterized as instrumentalized selectively rather than as a neutral protective doctrine — pushing the classification further toward snare-like asymmetry even while the coordination function nominally persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_threshold_objectivity, empirical, 'Whether R2P invocation tracks atrocity severity or great-power interest.').

omega_variable(
    regime_change_mandate_creep,
    'Did the 2011 Libya intervention exceed the R2P mandate (civilian protection) by pursuing regime change, and does that precedent structurally discredit future invocations regardless of atrocity severity?',
    'Comparative reading of Security Council Resolution 1973''s stated mandate against NATO''s actual operational conduct and outcome; assess subsequent state voting behavior on later R2P-framed resolutions (e.g., Syria) for evidence of Libya-precedent chilling effect.',
    'If Libya is read as mandate creep, subsequent Security Council reluctance to authorize coercive R2P action (contributing to Syria inaction) is directly downstream of this reading''s own credibility damage — a self-undermining dynamic internal to the reading''s operational history.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_change_mandate_creep, conceptual, 'Whether Libya 2011 discredited the coercive pillar of R2P for subsequent cases.').

omega_variable(
    committer_framing_kernel_location,
    'This constraint is one reading (r2p_reading) of the article_2_7_chapter_vii_tension kernel; the sibling sovereignty_first_reading holds the opposite core premise (sovereignty as foundational, intervention limited to consent or narrow inter-state Chapter VII authorization). Where exactly does the disagreement locate — is it a disagreement about facts (does atrocity actually occur as claimed), about doctrine (is sovereignty ever conditional), or about institutional trust (can the Security Council be trusted to apply either doctrine even-handedly)?',
    'Textual and voting-record analysis of General Assembly and Security Council debates distinguishing factual disputes (contested atrocity findings) from doctrinal disputes (rejecting conditional sovereignty as a category) from institutional-trust disputes (accepting conditional sovereignty in principle but distrusting its application via a veto-encumbered Council).',
    'If the disagreement is primarily institutional-trust rather than doctrinal, a reformed authorization mechanism (e.g., a veto-restraint pledge in mass-atrocity situations, as France and others have proposed) could substantially narrow the kernel contest without either reading abandoning its core premise; if the disagreement is doctrinal at root, no procedural reform resolves it and the readings remain genuinely incommensurable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_kernel_location, conceptual, 'Whether the r2p/sovereignty-first kernel contest is factual, doctrinal, or institutional-trust in character.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1990, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(arti_tr_t1999, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 1999, 0.3).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(arti_tr_t2011, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2011, 0.35).
narrative_ontology:measurement(arti_tr_t2018, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2018, 0.5).
narrative_ontology:measurement(arti_tr_t2025, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t1990, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(arti_be_t1999, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 1999, 0.48).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(arti_be_t2011, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2011, 0.7).
narrative_ontology:measurement(arti_be_t2018, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement(arti_be_t2025, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1990, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(arti_su_t1999, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 1999, 0.42).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(arti_su_t2011, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2011, 0.62).
narrative_ontology:measurement(arti_su_t2018, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(arti_su_t2025, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension__sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% This story and article_2_7_chapter_vii_tension__sovereignty_first_reading are sibling readings of the same kernel (article_2_7_chapter_vii_tension): a single persisting textual/doctrinal commitment (the Article 2(7) non-interference guarantee read against Chapter VII enforcement powers) that different parties read into opposite constraints. This reading (r2p_reading) authors ε against the standing sovereignty-first arrangement it contests, treating the non-interference norm's shielding of atrocity as the extractive baseline and R2P's conditional-sovereignty doctrine as the corrective. The sibling reading authors ε against R2P's use as a doctrine of selective intervention, treating unauthorized or ideologically-motivated intervention as its own extractive baseline. Per the ε-invariance principle and DP-001, these are NOT the same constraint measured two ways — they have different beneficiary/victim structures, different claimed types, and are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
