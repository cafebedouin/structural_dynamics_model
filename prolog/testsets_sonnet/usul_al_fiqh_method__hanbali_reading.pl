% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Textualist-Restrictive Reading of Usul al-Fiqh
 *   domain: religious/legal
 *
 * SUMMARY:
 *   The Hanbali reading of usul al-fiqh (Islamic legal source methodology) is
 *   one of four historically institutionalized readings of a single contested
 *   kernel: how should Quran, hadith, analogy, consensus, and custom be
 *   weighted and ordered in deriving legal rulings? The Hanbali reading
 *   maximizes textual restrictiveness — treating authenticated text as
 *   exhausting legitimate derivation wherever it speaks, minimizing qiyas to
 *   genuine textual silence, preferring even weak hadith over analogical
 *   reasoning, and actively blocking innovations (sadd al-dhara'i) that might
 *   open a gateway to bid'a. This produces a coordination benefit
 *   (methodological stability, resistance to doctrinal drift) bundled with an
 *   extraction cost borne by rationalist jurists and customary communities
 *   whose preferred derivation tools are structurally disfavored within
 *   Hanbali institutions. This story generates ONLY the Hanbali reading as a
 *   clean, ε-invariant constraint; the sibling readings (Hanafi, Maliki,
 *   Shafi'i) are separate constraint stories linked via
 *   network.affects_constraints, not alternative measurements folded into
 *   this one.
 *
 * KEY AGENTS:
 *   - hanbali_textualist_scholars: agenda_setter/beneficiary (institutional/identity_locked) — administer and are constituted by the restrictive method
 *   - sunna_preservation_advocates: beneficiary (organized/constrained) — receive doctrinal stability
 *   - rationalist_jurists: payer (moderate/constrained) — disfavored analogical toolkit
 *   - customary_practice_communities: payer (powerless/trapped) — local adaptation foreclosed by sadd al-dhara'i
 *   - adaptive_local_muftis: payer (moderate/constrained) — interest-responsive rulings foreclosed
 *   - comparative_fiqh_scholars: observer (analytical) — sees the full kernel structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.42).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.58).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Textualist-Restrictive Reading of Usul al-Fiqh").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "religious/legal").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '8d6a5f12-98ec-45c5-a959-3a287a9b9d99').
narrative_ontology:cs_kernel_codification('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', fixed_text).
narrative_ontology:cs_authority_grounding('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', lineage).
narrative_ontology:cs_interpretation_layer_present('8d6a5f12-98ec-45c5-a959-3a287a9b9d99').
narrative_ontology:cs_reading_relation('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', usul_al_fiqh_method__hanafi_reading, influences).
narrative_ontology:cs_reading_relation('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', foundational, weak_hadith_preferred_over_qiyas).
narrative_ontology:cs_axiom_status(weak_hadith_preferred_over_qiyas, holdable).
narrative_ontology:cs_axiom_grounding('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', weak_hadith_preferred_over_qiyas, conventional).
narrative_ontology:cs_axiom('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', foundational, sadd_al_dharai_as_general_preservation_principle).
narrative_ontology:cs_axiom_status(sadd_al_dharai_as_general_preservation_principle, holdable).
narrative_ontology:cs_axiom_grounding('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', sadd_al_dharai_as_general_preservation_principle, instrumental).
narrative_ontology:cs_reference_frame('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', companion_era_textual_sufficiency).
narrative_ontology:cs_drift_state('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', post_classical_juristic_systematization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8d6a5f12-98ec-45c5-a959-3a287a9b9d99', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, sunna_preservation_advocates).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, adaptive_local_muftis).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, textual_fidelity_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, sadd_al_dharai_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and transmit the methodological rule that Quran and authenticated hadith exhaust legitimate legal derivation wherever they speak, that qiyas is admissible only in genuine textual silence, and that weak hadith is preferred to qiyas even where analogical reasoning would reach further. They administer sadd al-dhara'i to block innovations before they can establish precedent. Their scholarly authority and communal standing are constituted by fidelity to this method; abandoning it would dissolve their distinguishing claim within the broader Sunni tradition.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars, agenda_setter,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars, beneficiary).

% Lay and clerical communities who value the restrictive method as a bulwark against perceived doctrinal drift. They receive a stable, textually anchored legal identity and a clear boundary against bid'a, at the cost of reduced room for locally adaptive rulings.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, sunna_preservation_advocates, beneficiary,
    organized, generational, constrained, regional).

% Jurists trained in analogical and reasoned methods (closer to Hanafi ra'y) find their preferred derivation tools systematically deprioritized within Hanbali institutions — weak hadith is favored over their qiyas-based reasoning even when the analogy is well-formed. They can practice elsewhere, but within Hanbali-dominant courts and fatwa councils their methodological toolkit is structurally disfavored.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, regional).

% Communities whose local 'urf (custom) or accumulated practical adaptation would, under a Maliki or Hanafi reading, carry evidentiary or interest-based weight. Under the Hanbali reading, sadd al-dhara'i treats novel local practice with suspicion as a potential gateway to innovation, foreclosing customary accommodations even where no text directly forbids them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_practice_communities, payer,
    powerless, generational, trapped, local).

% Local jurisconsults who would otherwise issue interest-responsive rulings (maslaha-style) for changed circumstances find the innovation-blocking apparatus foreclosing that route; they must instead search harder for direct textual warrant or a weak hadith, even when a straightforward analogy would resolve the case more responsively to present conditions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, adaptive_local_muftis, payer,
    moderate, biographical, constrained, local).

% Study the four major methodological readings comparatively, documenting how each school's source-hierarchy commitments produce different rulings from the same textual corpus, without themselves being bound by any single school's method.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_fiqh_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable, and textually anchored method for deriving rulings across a vast and dispersed community, minimizing the risk that legal derivation drifts from the founding textual corpus through unchecked reasoning or accumulated local custom.
% TRANSFER_FUNCTION: Moves interpretive authority away from rationalist and customary sources of law and concentrates it in textualist scholars who control which hadith are authenticated and when textual silence is genuinely established; the cost of foreclosed analogical and customary routes is borne by jurists and communities who would have benefited from them.
% ABSENT_VOICES: Communities whose lived customary practice is treated as a suspect gateway to bid'a are not represented in the methodological deliberation that forecloses their practice; rationalist jurists trained outside the Hanbali tradition are structurally disfavored within Hanbali institutions without a formal channel to contest the source-hierarchy itself.
% DISAPPEARANCE_RATIONALE: Hanbali scholars and sunna-preservation communities would say the tradition's distinguishing methodological identity dissolves without the restrictive hierarchy, collapsing into a generic reasoned jurisprudence. Rationalist jurists and customary communities would say rulings would simply adapt more readily to context, with little practical loss to the underlying textual corpus, which remains available to all schools regardless of hierarchy.
% FOUNDING_PROBLEM: Early-generation concern that expansive reasoned opinion (ra'y) and locally variable custom were producing rulings drifting from the Prophetic model, especially as the community expanded beyond Hijaz into regions with divergent customary law.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars attest the drift risk remains live wherever local custom or rationalist method operates without textual constraint. Comparative fiqh scholars and historians of Islamic law, writing from outside the Hanbali tradition, note that Hanafi and Maliki institutions have operated for centuries without equivalent doctrinal drift, suggesting the founding problem was addressed adequately by alternative methodological safeguards and that the restrictive hierarchy now functions substantially to preserve school identity and scholarly authority rather than to solve an unresolved problem.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, contested).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).
:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects moderate but real cost imposed on rationalist and customary legal development — the constraint does not extract wealth or labor in the manner of an economic snare, but it extracts interpretive authority and forecloses methodological options that would otherwise be available, and this cost has grown modestly as sadd al-dhara'i doctrine hardened over centuries into a more systematic blocking apparatus. Suppression (0.58) is higher than extractiveness because the constraint's persistence depends on actively policing what counts as genuine textual silence and what counts as an impermissible innovation — this is coercive gatekeeping, not mere preference. Theater ratio is low (0.2) because the scholarly apparatus (hadith authentication, source-hierarchy adjudication) performs a genuine and continuously exercised function, not empty ritual. Accessibility collapse (0.62) is moderate-high: once a ruling area is treated as textually settled, alternative derivation routes are foreclosed within Hanbali institutions, though the broader Sunni tradition retains the sibling readings as live alternatives elsewhere. Resistance (0.55) reflects the genuine and sustained rationalist/customary pushback this reading has met historically, including within intra-Sunni polemic.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali textualist scholars are the clearest beneficiaries: their institutional authority, transmitted scholarly lineage, and communal legitimacy are directly constituted by administering and defending the restrictive hierarchy — they hold identity_locked exit because abandoning the method would dissolve the school's distinguishing claim. Sunna preservation advocates benefit from doctrinal stability without administering it directly. Rationalist jurists, customary communities, and adaptive muftis are targets: their preferred derivation tools (qiyas, 'urf, maslaha) are structurally deprioritized, and the powerless customary communities in particular have trapped exit — they cannot simply relocate to a Hanafi or Maliki jurisdiction without abandoning their local social fabric.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is warranted because a genuine coordination function exists and is not merely cover: methodological stability against unchecked reasoned drift is a real problem for a geographically dispersed religious-legal tradition, and the Hanbali reading solves it more thoroughly than any sibling reading. But this coordination function is bundled with asymmetric extraction — rationalist jurists and customary communities bear a real cost that the coordination benefit does not return to them, and the sadd al-dhara'i enforcement apparatus is actively maintained rather than passively inherited. Classifying this as a pure mountain (natural, inevitable, textual necessity) would obscure the beneficiary/victim structure; classifying it as a pure snare would obscure the genuine and historically demonstrated coordination value of methodological stability. The founding_problem mismatch (status: contested, corroboration noting sibling schools solved the same problem differently without equivalent restrictiveness) is exactly the kind of signal the R5 genealogy interview is designed to surface without treating it as dispositive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_silence_genuine_or_constructed,
    'Is ''genuine textual silence'' (the threshold that permits qiyas under this reading) an objectively determinable textual fact, or is the determination itself shaped by which cases Hanbali scholars are institutionally motivated to treat as settled versus open?',
    'Comparative analysis of cases where Hanbali, Hanafi, and Shafi''i scholars disagree about whether a given text is silent on a matter — divergence would indicate the silence threshold is partly interpretive rather than a fixed textual fact.',
    'If the silence threshold is substantially interpretive, the restrictive hierarchy''s claim to superior textual fidelity is weakened, and the extraction from rationalist/customary alternatives looks less like principled textualism and more like a discretionary boundary drawn to favor the administering school.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_silence_genuine_or_constructed, conceptual, 'Whether the textual-silence threshold gating qiyas is objectively fixed or interpretively constructed by the administering scholarly body.').

omega_variable(
    kernel_sibling_readings_structural_delta,
    'Given that Hanafi, Maliki, and Shafi''i readings of the same kernel (usul_al_fiqh_method) produce demonstrably different rulings from the same textual corpus without any reading claiming the others are illegitimate Islamic jurisprudence, is the Hanbali reading''s restrictiveness a genuinely superior fidelity mechanism or one defensible methodological choice among several live options within Sunni orthodoxy?',
    'This is the committer-structure question the kernel framing exists to hold: compare this story''s beneficiary/victim structure and ε against the sibling stories (hanafi_reading, maliki_reading, shafii_reading) via network.affects_constraints; no single reading''s internal self-justification resolves the question of which reading the wider tradition should adopt.',
    'If treated as one live option among several rather than the superior fidelity mechanism, the extraction from rationalist and customary communities is harder to justify purely as protecting textual integrity, since sibling readings protect textual integrity by their own lights while producing different, less restrictive outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_readings_structural_delta, conceptual, 'Whether the Hanbali reading''s restrictiveness is uniquely justified fidelity or one of several coexisting orthodox methodological choices — the core committer-frame ambiguity for this kernel.').

omega_variable(
    sadd_al_dharai_scope_creep,
    'Has the scope of sadd al-dhara''i (blocking innovations) expanded over the historical interval beyond its founding application to clear cases of harm-facilitation, into a general-purpose tool for foreclosing any novel local practice or interest-based ruling?',
    'Historical survey of fatwa collections across the measured interval, tracking the range and novelty of practices to which sadd al-dhara''i was applied, weighted against the founding juristic examples used to establish the doctrine.',
    'If scope has expanded substantially, the rising suppression_requirement trajectory reflects genuine enforcement-apparatus hardening rather than stable application of a fixed doctrine, supporting the T17-style hypothesis that extraction has accumulated onto an originally narrower coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_al_dharai_scope_creep, empirical, 'Whether sadd al-dhara''i''s practical scope has grown beyond its founding application, accumulating suppressive force over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__hanbali_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanbali_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__hanbali_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(usul_tr_t80, usul_al_fiqh_method__hanbali_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__hanbali_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(usul_be_t80, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(usul_su_t60, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(usul_su_t80, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 80, 0.57).
narrative_ontology:measurement(usul_su_t100, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanbali_reading, 0.1).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraint stories decomposing the single natural-language concept 'usul al-fiqh methodology' per the ε-invariance principle: each of the four historically institutionalized readings (Hanafi, Hanbali, Maliki, Shafi'i) of the shared kernel usul_al_fiqh_method has its own beneficiary/victim structure and its own ε, because each reading orders the same source materials (Quran, hadith, qiyas, ijma, custom) differently and produces materially different rulings and materially different victim sets. They are linked via affects_constraints rather than merged, per the committer-frame rules: none of the four readings forecloses all the others as illegitimate Islamic jurisprudence, though this reading (Hanbali) does foreclose the specific claim that expansive ra'y-based reasoning is textually faithful, which the Hanafi reading depends on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
