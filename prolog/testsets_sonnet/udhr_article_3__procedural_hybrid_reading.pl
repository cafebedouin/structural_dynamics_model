% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3 — Procedural Hybrid Reading (Due Process Without Substantive Resolution)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the procedural hybrid reading of UDHR Article 3:
 *   the guarantee is read to secure habeas corpus and freedom from torture as
 *   a genuine, enforceable procedural floor, while deliberately declining to
 *   resolve whether 'security of person' also obligates material provision.
 *   This is one of three structurally distinct readings of the same kernel
 *   text — the negative-liberty reading (Article 3 as pure non-interference)
 *   and the positive-entitlement reading (Article 3 as welfare obligation)
 *   are separate constraints with their own ε values, not alternate
 *   measurements of this one. The hybrid reading's ε is moderate: real
 *   coordination value in the procedural core (torture prohibition, habeas
 *   access) coexists with a persistent extraction dynamic where states
 *   capture legitimacy from the procedural floor while using the reading's
 *   own unresolved scope to deflect welfare claims and, via
 *   emergency/security carve-outs, to narrow the procedural floor's actual
 *   reach for disfavored populations (noncitizens, detainees classified
 *   outside 'ordinary' process).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.42).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.48).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3 — Procedural Hybrid Reading (Due Process Without Substantive Resolution)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '4caf3f87-2f64-4b2b-a8cf-e1b32839924f').
narrative_ontology:cs_kernel_codification('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', fixed_text).
narrative_ontology:cs_authority_grounding('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', practice).
narrative_ontology:cs_interpretation_layer_present('4caf3f87-2f64-4b2b-a8cf-e1b32839924f').
narrative_ontology:cs_reading_relation('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', foundational, procedural_floor_sufficient_without_substantive_resolution).
narrative_ontology:cs_axiom_status(procedural_floor_sufficient_without_substantive_resolution, holdable).
narrative_ontology:cs_axiom_grounding('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', procedural_floor_sufficient_without_substantive_resolution, conventional).
narrative_ontology:cs_axiom('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', secondary, due_process_guarantee_severable_from_distributive_question).
narrative_ontology:cs_axiom_status(due_process_guarantee_severable_from_distributive_question, holdable).
narrative_ontology:cs_axiom_grounding('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', due_process_guarantee_severable_from_distributive_question, instrumental).
narrative_ontology:cs_reference_frame('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', drafting_era_negative_positive_compromise).
narrative_ontology:cs_drift_state('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', post_2001_security_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4caf3f87-2f64-4b2b-a8cf-e1b32839924f', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, detained_persons_with_habeas_access).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, states_seeking_legitimacy_without_redistribution).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, indefinitely_detained_noncitizens).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, welfare_claimants_denied_substantive_remedy).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, due_process_minimalism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratify and cite Article 3 to claim human-rights compliance through habeas corpus and anti-torture guarantees, while resisting any reading that would obligate welfare or material provision. Enforce the procedural floor selectively and invoke national security exceptions to narrow its reach. Capture the legitimacy benefit of 'due process' without the fiscal or redistributive cost of a positive-entitlement reading.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, states_seeking_legitimacy_without_redistribution, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, states_seeking_legitimacy_without_redistribution, beneficiary).

% Where courts function and habeas petitions are heard, gain a genuine check against arbitrary detention and torture. Their protection depends entirely on functioning judicial review remaining available and on the state not classifying their detention outside ordinary process (emergency, immigration, extraordinary rendition categories).
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, detained_persons_with_habeas_access, beneficiary,
    powerless, immediate, constrained, national).

% Held in immigration detention, military custody, or extraordinary-rendition regimes that states argue fall outside ordinary due process categories. The procedural hybrid reading is satisfied on paper (torture prohibited, some review nominally available) while the actual habeas mechanism is delayed, jurisdictionally evaded, or rendered theatrical. They bear the gap between the guarantee's text and its operational reach.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, indefinitely_detained_noncitizens, payer,
    powerless, biographical, trapped, national).

% Face life-threatening deprivation (housing, healthcare, subsistence) and attempt to invoke Article 3's 'security of person' language for material relief. Courts applying the procedural hybrid reading dismiss these claims as outside Article 3's scope, since the hybrid reading explicitly declines to resolve the welfare question. They pay the cost of the kernel's unresolved substantive contest by having no forum in which their claim can even be adjudicated on the merits.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, welfare_claimants_denied_substantive_remedy, payer,
    powerless, biographical, constrained, national).

% Adjudicate habeas and torture claims under Article 3's procedural core, building case law that stabilizes the hybrid reading's boundaries. Their jurisprudence determines, case by case, how far 'due process' extends into emergency detention and how firmly closed the door stays against welfare claims — the reading's actual content is produced here, not in the text.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, human_rights_courts_and_tribunals, observer).

% Argue that security of person is empty without material security, and press courts and treaty bodies to read welfare obligations into Article 3. They are structurally excluded from the procedural hybrid reading's own terms — the hybrid reading's entire function is to avoid settling their claim rather than to rule on its merits.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_advocates_pressing_positive_reading, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__procedural_hybrid_reading, states_seeking_legitimacy_without_redistribution).
narrative_ontology:fixing_cost_class(udhr_article_3__procedural_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a genuinely functioning, widely-adopted floor: no state may torture and every detained person nominally has access to judicial review of the legality of detention. This is a real, hard-won coordination achievement distinct from the contested welfare question.
% TRANSFER_FUNCTION: Moves legitimacy and diplomatic standing to states that can point to procedural compliance (habeas mechanisms, torture prohibitions on the books) without transferring resources or enforceable material claims to persons whose security depends on welfare provision. Within the procedural core itself, it moves protection from arbitrary state violence to detained persons where courts actually function, and withholds it from those the state can classify outside ordinary process.
% ABSENT_VOICES: Advocates for the positive-entitlement reading are structurally absent from this reading's own operation: the hybrid reading's coherence depends on never adjudicating whether Article 3 obligates material provision, so their claim is never heard on the merits within this framework, only deflected to a different forum or held permanently open.
% DISAPPEARANCE_RATIONALE: If the procedural hybrid reading vanished, states could either fall back to the narrower negative-liberty reading (habeas and torture prohibition would likely survive as customary international law regardless, since they are independently entrenched in other instruments) or be pushed toward the positive-entitlement reading by default, since the hybrid reading's specific function — holding the substantive question open — would no longer be available as a legitimating middle path. Whether the world 'rearranges' depends entirely on which sibling reading fills the vacuum, which is precisely the unresolved contest this reading exists to defer.
% FOUNDING_PROBLEM: Drafters of the UDHR in 1948 could not secure agreement between states favoring a minimal negative-liberty conception of security and states (particularly those aligned with socialist welfare models) favoring positive material guarantees; Article 3's text was drafted broadly enough that both blocs could ratify it without resolving the disagreement.
% FOUNDING_PROBLEM_CORROBORATION: Drafting-history scholarship (travaux préparatoires of the UN Commission on Human Rights, 1947-48) documents the negative-liberty/positive-entitlement split among drafters from outside either camp's later advocacy. Independent legal historians and comparative constitutional scholars attest the ambiguity was a deliberate compromise, not an oversight; this corroboration comes from historical record rather than from states or advocacy groups who benefit from either reading.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, contested).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects a genuine but partial coordination achievement: the torture prohibition and habeas mechanism function as real constraints in many jurisdictions, but the post-2001 period shows extraction rising sharply as states built emergency-detention and rendition architectures explicitly designed to sit in the procedural gaps the hybrid reading leaves open (measurement series shows the 2001 spike). Suppression (0.48) is driven by jurisdictional and classificatory maneuvering — the state doesn't deny the procedural right exists, it disputes whether this detainee is covered by it. Theater ratio (0.4) captures the gap between formal habeas availability and actual judicial review in emergency-detention contexts, which widened sharply post-2001 and has only partially receded.
 *
 * PERSPECTIVAL GAP:
 *   From the state agenda-setter seat, Article 3's procedural core is a stable, well-functioning coordination achievement that appropriately leaves substantive distributive questions to domestic political processes. From the detained-noncitizen or welfare-claimant seat, the same unresolved scope is experienced as the specific mechanism by which their claim is never heard: the hybrid reading doesn't deny their claim, it structurally prevents the claim from being cognizable at all under this reading. The engine should compute these as different seat-level types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   States are the structural agenda-setters and primary beneficiaries: they administer both the procedural floor and its exceptions, and capture legitimacy from the former while retaining discretion via the latter. Human rights courts share agenda-setting power but their actual reach depends on which cases reach them. Detained persons with functioning habeas access are genuine (if narrow) beneficiaries. Indefinitely detained noncitizens and denied welfare claimants are targets — the hybrid reading's deliberate non-resolution of scope is precisely what leaves them without a forum, converting an unresolved kernel-level contest into concrete, borne cost for identifiable populations.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading's founding problem (irreconcilable drafting-era disagreement over the scope of security) is genuinely still contested in international law and practice — this is not a dead mandate propped up by theater. However, the specific procedural architecture built around it (habeas mechanisms with jurisdictional carve-outs) has in places calcified into a mechanism whose primary operative function, for detained noncitizens, is to demonstrate formal compliance while permitting substantive evasion. This is a tangled_rope pattern precisely because it is NOT simply degraded (piton) — states actively invest in maintaining and refining the carve-out architecture, which requires ongoing enforcement effort, not mere inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_stability_vs_deferral,
    'Is the procedural hybrid reading a genuinely stable, principled position (due process is sufficient and appropriate; substantive distribution is properly a matter for domestic politics), or is it structurally a deferral mechanism that persists only because it lets states and courts avoid ruling on the harder substantive question?',
    'Track whether human rights tribunals citing the hybrid reading ever explicitly justify the scope limitation on principled grounds versus treating it as an unexamined default; examine dissenting opinions and minority reports in cases where welfare claims were framed as Article 3 claims and dismissed.',
    'If principled, this reading is closer to a genuine rope with a bounded coordination function. If deferral, the reading is closer to a tangled_rope whose extraction consists precisely in its refusal to resolve a contest it has the institutional standing to resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_stability_vs_deferral, conceptual, 'Whether the hybrid reading''s non-resolution is principled or extractive deferral.').

omega_variable(
    emergency_carveout_capture,
    'Are emergency-detention and national-security carve-outs to the procedural core a legitimate, narrowly-tailored exception consistent with the hybrid reading''s own logic, or have they been captured by states to functionally nullify the procedural floor for disfavored populations (noncitizens, terrorism suspects)?',
    'Comparative empirical study of habeas grant rates and time-to-review for citizens versus noncitizens, and for ordinary versus emergency-classified detention, across ratifying states over the measured interval.',
    'High capture would indicate the procedural hybrid reading''s protective function is substantially theatrical for the populations most in need of it, supporting a higher effective extraction reading for that subgroup even if the aggregate metric looks moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_carveout_capture, empirical, 'Whether security carve-outs have captured the procedural floor''s protective function.').

omega_variable(
    kernel_framing_alternative,
    'An alternative framing treats the ''kernel'' not as the UDHR text itself but as the broader post-1948 human-rights legitimacy regime that the text serves — under that framing, all three readings might be better modeled as competing legitimation strategies within a single extraction-authority structure rather than as independent readings of a fixed text.',
    'Compare treaty-body jurisprudence trajectories: if courts consistently cite drafting history to justify staying within one reading''s boundaries, the fixed-text framing holds; if courts opportunistically switch readings based on litigant identity or political context, the legitimation-strategy framing is better supported.',
    'Under the fixed-text framing (adopted here), this story''s cs_structure correctly uses kernel_codification=fixed_text. Under the alternative framing, authority_grounding might shift toward extraction with the kernel understood as the legitimacy claim itself rather than the text, which would not change this story''s classification but would change how the sibling relations are read.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the kernel is the UDHR text itself or the legitimacy regime built on it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(udhr_tr_t1966, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1966, 0.28).
narrative_ontology:measurement(udhr_tr_t1984, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1984, 0.3).
narrative_ontology:measurement(udhr_tr_t2001, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2001, 0.48).
narrative_ontology:measurement(udhr_tr_t2012, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2012, 0.44).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1948, 0.3).
narrative_ontology:measurement(udhr_be_t1966, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1966, 0.33).
narrative_ontology:measurement(udhr_be_t1984, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1984, 0.37).
narrative_ontology:measurement(udhr_be_t2001, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2001, 0.45).
narrative_ontology:measurement(udhr_be_t2012, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2012, 0.44).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(udhr_su_t1966, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1966, 0.32).
narrative_ontology:measurement(udhr_su_t1984, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1984, 0.35).
narrative_ontology:measurement(udhr_su_t2001, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(udhr_su_t2012, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).

% DUAL FORMULATION NOTE:
% This story is the procedural_hybrid_reading member of the udhr_article_3 kernel family (three siblings). negative_liberty_reading claims a low-extraction, tightly-bounded coordination function (pure non-interference); positive_entitlement_reading claims a higher-extraction, more contested tangled_rope or snare structure depending on how welfare-obligation enforcement is authored. This hybrid reading sits structurally between them by design: it extracts a real, moderate-ε coordination value from the procedural core while using its own declared non-resolution of the welfare question as the mechanism by which welfare claimants and irregularly-classified detainees bear costs the other two readings would resolve one way or the other. Each sibling carries its own ε, its own beneficiary/victim structure, and its own claimed_type; do not average or reconcile them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
