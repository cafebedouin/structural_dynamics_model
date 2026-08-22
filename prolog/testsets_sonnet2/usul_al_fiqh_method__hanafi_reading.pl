% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Reading of Usul al-Fiqh: Expansive Qiyas, Ra'y, and Istihsan
 *   domain: religious/legal
 *
 * SUMMARY:
 *   This story instantiates the Hanafi reading of the contested usul al-fiqh
 *   kernel: the methodological question of how much interpretive latitude
 *   jurists possess when deriving rulings from revealed sources. The Hanafi
 *   reading grants qiyas expansive applicability wherever text is silent,
 *   allows ra'y (reasoned opinion) to supplement where analogy itself reaches
 *   its limits, and authorizes istihsan (juristic preference) to override the
 *   'obvious' analogical answer when public interest counsels otherwise. This
 *   is one reading among (at least) four live readings of the same underlying
 *   kernel — Maliki, Shafi'i, and Hanbali readings apply structurally
 *   different source hierarchies and are NOT described here; each is authored
 *   as its own constraint with its own epsilon.
 *
 * KEY AGENTS:
 *   - rationalist_trained_jurist_class: agenda_setter/beneficiary (institutional/arbitrage) — administers and collects prestige from expansive method
 *   - kufan_juridical_lineage: beneficiary (organized/identity_locked) — institutional continuity depends on method's legitimacy
 *   - state_administrators_seeking_flexible_rulings: beneficiary (powerful/mobile) — historically favored flexible rulings for governance needs
 *   - textualist_claimants_to_interpretive_limits: payer (organized/constrained) — their boundary claim is structurally defeated
 *   - hadith_centered_scholars: payer (organized/constrained) — subordinated when qiyas preferred over weak hadith
 *   - laypersons_seeking_predictable_rulings: payer (powerless/trapped) — bear unpredictability of discretionary rulings
 *   - comparative_legal_historians: observer (analytical/analytical) — sees the full four-school structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.42).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.38).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Reading of Usul al-Fiqh: Expansive Qiyas, Ra'y, and Istihsan").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "religious/legal").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '8f86b49b-79ec-4685-be8e-690806fd4afa').
narrative_ontology:cs_kernel_codification('8f86b49b-79ec-4685-be8e-690806fd4afa', distributed).
narrative_ontology:cs_authority_grounding('8f86b49b-79ec-4685-be8e-690806fd4afa', lineage).
narrative_ontology:cs_interpretation_layer_present('8f86b49b-79ec-4685-be8e-690806fd4afa').
narrative_ontology:cs_reading_relation('8f86b49b-79ec-4685-be8e-690806fd4afa', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f86b49b-79ec-4685-be8e-690806fd4afa', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f86b49b-79ec-4685-be8e-690806fd4afa', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('8f86b49b-79ec-4685-be8e-690806fd4afa', foundational, textual_silence_licenses_expansive_analogical_reasoning).
narrative_ontology:cs_axiom_status(textual_silence_licenses_expansive_analogical_reasoning, holdable).
narrative_ontology:cs_axiom_grounding('8f86b49b-79ec-4685-be8e-690806fd4afa', textual_silence_licenses_expansive_analogical_reasoning, conventional).
narrative_ontology:cs_axiom('8f86b49b-79ec-4685-be8e-690806fd4afa', foundational, juristic_preference_overrides_strict_analogy_for_public_interest).
narrative_ontology:cs_axiom_status(juristic_preference_overrides_strict_analogy_for_public_interest, holdable).
narrative_ontology:cs_axiom_grounding('8f86b49b-79ec-4685-be8e-690806fd4afa', juristic_preference_overrides_strict_analogy_for_public_interest, instrumental).
narrative_ontology:cs_reference_frame('8f86b49b-79ec-4685-be8e-690806fd4afa', kufan_rationalist_derivation_tradition).
narrative_ontology:cs_drift_state('8f86b49b-79ec-4685-be8e-690806fd4afa', post_hadith_compilation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8f86b49b-79ec-4685-be8e-690806fd4afa', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, kufan_juridical_lineage).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, state_administrators_seeking_flexible_rulings).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_claimants_to_interpretive_limits).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, hadith_centered_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, laypersons_seeking_predictable_rulings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in the Kufan analogical tradition, this class administers qiyas, ra'y, and istihsan as live interpretive tools. Their professional standing and authority to issue rulings depend on the wide berth these methods give them; they set the boundaries of when text is 'silent' and when public interest justifies departing from strict analogy. They are the ones who collect prestige, patronage, and judicial appointments from being the class capable of this reasoning.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class, beneficiary).

% The institutional lineage tracing to Abu Hanifa and his students benefits from the school's methodology being treated as legitimate and expansive; their scholarly reputation, endowed teaching positions, and continuity of doctrine depend on istihsan and ra'y remaining valid tools rather than being disciplined into narrow textualism.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, kufan_juridical_lineage, beneficiary,
    organized, civilizational, identity_locked, continental).

% Historically, Abbasid and later Ottoman state administration favored Hanafi flexibility because it accommodated administrative necessity, taxation, and governance questions the fixed text did not anticipate. They benefit from a jurisprudence pliable enough to ratify state interest under the banner of public good (istihsan).
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, state_administrators_seeking_flexible_rulings, beneficiary,
    powerful, generational, mobile, continental).

% Scholars and communities who hold that legal derivation should be tightly bound to Quran and authenticated hadith experience the Hanafi method as eroding the very boundary they consider load-bearing: once qiyas and istihsan are admitted this expansively, the claim that jurists cannot legislate beyond revealed text is structurally defeated in practice, even where denied in principle. Their exit is to align with rival schools, but they remain embedded in a shared legal-theological universe where Hanafi rulings carry real force.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_claimants_to_interpretive_limits, payer,
    organized, civilizational, constrained, continental).

% Scholars who prioritize weak or single-narrator hadith over analogical reasoning find their preferred sources subordinated: the Hanafi method prefers qiyas over what it judges to be insufficiently corroborated hadith. This transfers interpretive authority away from hadith transmission networks toward jurists skilled in analogical technique.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hadith_centered_scholars, payer,
    organized, generational, constrained, regional).

% Ordinary petitioners bringing novel questions to a Hanafi qadi cannot predict outcomes as easily as they could under a maximally textualist regime, because istihsan explicitly authorizes departure from the 'obvious' analogical answer when the jurist judges public interest requires it. They bear the cost of interpretive unpredictability and must trust the jurist's discretion rather than a fixed rule.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, laypersons_seeking_predictable_rulings, payer,
    powerless, biographical, trapped, local).

% Study the four schools' divergent methodologies as data on how legal systems balance textual fidelity against administrative flexibility, without a stake in any single school's authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working method for deriving rulings on the very large set of questions the Quran and hadith corpus do not directly address, allowing a functioning legal system to operate across a rapidly expanding, administratively complex empire without requiring new revelation for every novel case.
% TRANSFER_FUNCTION: Moves interpretive authority from those who control hadith transmission and narrow textual reading toward jurists trained in analogical and rationalist technique; moves outcome-predictability away from laypersons and toward the discretion of the trained jurist and, historically, toward state interests the jurist judges as public interest.
% ABSENT_VOICES: Hadith-transmission scholars whose narrations are judged insufficiently corroborated are structurally out-voted by the preference for qiyas in exactly the cases where their material would otherwise govern; they are present in the broader discourse but lose in the specific derivation. Laypersons affected by istihsan rulings have no seat in how 'public interest' is defined.
% DISAPPEARANCE_RATIONALE: If expansive qiyas, ra'y, and istihsan were withdrawn from Hanafi method overnight, the school would collapse into a narrower textualism resembling the Hanbali or Shafi'i approach; the class of jurists whose authority rests on rationalist technique would lose their distinguishing function, administrative rulings historically justified via istihsan would need re-derivation, and the school's institutional identity — built across centuries of Kufan legal culture — would need to reconstitute itself around a different source hierarchy entirely.
% FOUNDING_PROBLEM: Early Kufan jurists faced a rapidly expanding set of novel legal, commercial, and administrative questions arising in a cosmopolitan, non-Hijazi environment with a comparatively thin regional hadith corpus; a method was needed that did not require an authenticated narration for every case.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists themselves attest the founding problem persists in every generation (novel cases always outrun explicit text). Historians of Islamic law and rival-school scholars (Shafi'i and Hanbali sources) attest that the founding problem of textual scarcity in Kufa has been substantially resolved by later hadith compilation and transmission networks, and that the continued breadth of istihsan and ra'y now functions less as a response to textual silence and more as an entrenched interpretive prerogative of the jurist class.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).
:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: the coordination function (deriving workable rulings for an empire whose administrative reality outran the explicit text) is genuine and substantial, not a pure cover story. But the same mechanism structurally transfers interpretive authority to a specific class (rationalist-trained jurists) and away from both hadith-transmission scholars and laypersons who cannot predict outcomes once istihsan licenses departure from analogy. Suppression is moderate (0.38): the method does not physically coerce, but it does structurally discipline dissenting textualist claims by treating them as a minority position within an established, state-supported school. Theater ratio stays low-moderate (0.22) because the jurisprudential activity is substantively functional, not primarily performative — though the entrenchment of istihsan as institutional prerogative rather than active response to textual silence introduces a modest theatrical component over time (reflected in the rising measurement series).
 *
 * PERSPECTIVAL GAP:
 *   From the jurist class's own seat, expansive qiyas/ra'y/istihsan is simply sound legal method responding to real interpretive need — a Rope. From the textualist and hadith-centered seats, the same operation is what defeats their claim that revealed text should bound jurisprudence — an actively enforced, asymmetric transfer of interpretive authority. The engine computes these seat-level divergences from the declared structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The rationalist-trained jurist class and the Kufan lineage are structural beneficiaries: their professional authority is constituted by the breadth of these methods, and derivation power flows to them (low d). State administrators historically benefited from the flexibility to ratify administrative necessity as istihsan (low-moderate d). Textualist claimants and hadith-centered scholars are structural targets: the method's operation is precisely what defeats the boundary they wish to hold, even without any single act of coercion against them (high d). Laypersons sit as diffuse, powerless payers: they bear unpredictability without collecting the interpretive discretion themselves (high d, trapped exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — textual scarcity in a cosmopolitan Kufan environment lacking dense regional hadith transmission — has been substantially addressed by nine centuries of hadith compilation and cross-regional scholarly exchange since. Yet the methodological breadth persists and, per rival-school and historian corroboration, increasingly serves as an entrenched interpretive prerogative rather than a live response to textual silence. This is exactly the kind of tension the tangled_rope classification is meant to preserve rather than collapse into either 'pure extraction' or 'pure coordination': the coordination function was real at founding and remains partially live (novel questions do still arise), while the extraction component (jurist-class authority preserved beyond strict necessity) has grown. Classifying this as a simple Rope would erase the victim structure; classifying it as a Snare would erase the genuine, still-partially-live coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_multiplicity,
    'Is the usul_al_fiqh_method kernel genuinely a single contested commitment with four structurally distinct readings, or are the Hanafi, Maliki, Shafi''i, and Hanbali methodologies better understood as four separate, non-competing legal systems that happen to share vocabulary?',
    'Comparative analysis of cross-school citation practice, mutual recognition doctrine (whether one school''s qadis recognize rulings derived under another school''s method as valid), and historical instances of school conversion or synthesis (e.g., talfiq) would indicate whether the schools function as readings of one kernel or as separate kernels entirely.',
    'If they are one kernel with four readings, the reading_relations declared here (coexists_with the other three) are structurally accurate. If they are separate kernels, the entire committer frame should be dissolved and each school authored as an independent, non-kernel constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity, conceptual, 'Whether the four schools are readings of one kernel or four separate kernels.').

omega_variable(
    istihsan_scope_boundary,
    'Where does istihsan''s authorized departure from strict analogy actually stop — is it bounded by identifiable jurisprudential criteria the Hanafi tradition itself specifies, or is the boundary determined ad hoc by the presiding jurist''s judgment of public interest?',
    'Textual analysis of classical Hanafi usul works (e.g., al-Sarakhsi, al-Bazdawi) to determine whether istihsan''s criteria are formally specified and consistently applied across derivations, versus historical case analysis showing inconsistent or outcome-driven invocation.',
    'A tightly bounded istihsan supports the tangled_rope reading (real coordination function with disciplined limits); an ad hoc, outcome-driven istihsan would push the classification toward snare, since the ''public interest'' exception would function primarily as unconstrained jurist discretion rather than principled method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_scope_boundary, empirical, 'Whether istihsan is a bounded doctrine or unconstrained discretion.').

omega_variable(
    founding_problem_persistence,
    'Does the original founding problem (textual scarcity requiring extensive qiyas) still hold in any meaningful sense today, given nine centuries of hadith compilation, or has it been fully resolved such that the method''s continued breadth is pure institutional inertia dressed as jurisprudential necessity?',
    'Survey of contemporary Hanafi fatwa literature to assess what proportion of derivations genuinely address textual silence versus what proportion invoke istihsan/ra''y to override available, authenticated textual guidance.',
    'If contemporary practice shows the method still substantially responds to genuine textual silence, the tangled_rope classification (real coordination function persisting) holds. If practice shows istihsan primarily overriding available text, the constraint would trend toward piton (atrophied original function, persisting institutional prerogative) or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem remains live in contemporary practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__hanafi_reading, theater_ratio, 200, 0.13).
narrative_ontology:measurement(usul_tr_t500, usul_al_fiqh_method__hanafi_reading, theater_ratio, 500, 0.17).
narrative_ontology:measurement(usul_tr_t800, usul_al_fiqh_method__hanafi_reading, theater_ratio, 800, 0.19).
narrative_ontology:measurement(usul_tr_t1100, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1100, 0.21).
narrative_ontology:measurement(usul_tr_t1300, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1300, 0.22).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 200, 0.34).
narrative_ontology:measurement(usul_be_t500, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 500, 0.38).
narrative_ontology:measurement(usul_be_t800, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 800, 0.4).
narrative_ontology:measurement(usul_be_t1100, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1100, 0.41).
narrative_ontology:measurement(usul_be_t1300, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1300, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 200, 0.25).
narrative_ontology:measurement(usul_su_t500, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 500, 0.3).
narrative_ontology:measurement(usul_su_t800, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 800, 0.33).
narrative_ontology:measurement(usul_su_t1100, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1100, 0.36).
narrative_ontology:measurement(usul_su_t1300, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1300, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This is one of four sibling constraints decomposing the natural-language concept 'usul al-fiqh methodology' per the ε-invariance principle. Each of the four classical schools reads the source-hierarchy question differently, producing different beneficiary/victim structures and different epsilon values: Hanafi (this story, epsilon=0.42, moderate rationalist latitude), Maliki (Medinan practice + unrestricted maslaha as independent sources), Shafi'i (strict hadith-authentication prerequisite, narrowest qiyas), Hanbali (maximal textual restrictiveness, innovation-blocking). All four are linked via affects_constraints as members of the same kernel family; none is the 'correct' reading — each is a live, coexisting position held by a distinct scholarly and legal tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
