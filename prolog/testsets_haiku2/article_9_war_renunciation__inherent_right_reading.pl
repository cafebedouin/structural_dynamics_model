% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 War Renunciation with Inherent Self-Defense Right
 *   domain: constitutional_law/security_policy
 *
 * SUMMARY:
 *   Article 9 of Japan's post-WWII constitution contains the text 'the
 *   Japanese people forever renounce war, as a sovereign right of the nation,
 *   and the threat or use of force, as a means of settling international
 *   disputes.' The inherent-right reading interprets this as a renunciation
 *   of aggressive war (war-as-instrument-of-foreign-policy) while preserving
 *   a state's natural right to repel invasion. This reading permits the
 *   Self-Defense Forces to exist and expand as long as they remain nominally
 *   defensive. It contrasts with the strict-pacifist reading, which treats
 *   the language as a categorical prohibition on any armed forces, and the
 *   collective-self-defense reading, which extends inherent rights to
 *   defending allies when Japan's survival is threatened. This constraint
 *   story instantiates ONLY the inherent-right reading; the other readings
 *   are sibling constraints with their own ε, beneficiary/victim structures,
 *   and types.
 *
 * KEY AGENTS:
 *   - National Security Establishment: Institutional power. Agenda-setter. Interprets and administers Article 9 as permitting defensive forces. Benefits from the reading because it legitimates existing SDF and defense budgets.
 *   - Constitutional Pacifist Movement: Organized power. Payer. Advocates the strict reading and bears the cost of an interpretation imposed against their constitutional understanding.
 *   - Regional Alliance Partners: Institutional power. Beneficiary. Gain deterrent capacity and strategic stability from Japan's military capability under this reading.
 *   - Neighboring States: Institutional power. Mixed role (payer + beneficiary). Experience both stabilizing effect and security concern from Japan's defensive capacity.
 *   - Supreme Court: Institutional power. Observer. Holds constitutional review authority but has historically abstained from directly constraining military scope.
 *   - International Law Community: Analytical power. Observer. Provides external perspective on whether 'minimum necessary' remains credible as SDF capabilities expand.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.38).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.22).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 War Renunciation with Inherent Self-Defense Right").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'a13b3b0c-e11a-4225-8bc0-9078f1a21b66').
narrative_ontology:cs_kernel_codification('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', fixed_text).
narrative_ontology:cs_authority_grounding('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', extraction).
narrative_ontology:cs_interpretation_layer_present('a13b3b0c-e11a-4225-8bc0-9078f1a21b66').
narrative_ontology:cs_reading_relation('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', article_9_war_renunciation__collective_self_defense_reading, coexists_with).
narrative_ontology:cs_axiom('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', foundational, inherent_right_self_defense_preserved).
narrative_ontology:cs_axiom_status(inherent_right_self_defense_preserved, holdable).
narrative_ontology:cs_axiom_grounding('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', inherent_right_self_defense_preserved, deontological).
narrative_ontology:cs_axiom('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', foundational, aggressive_war_vs_defense_distinction).
narrative_ontology:cs_axiom_status(aggressive_war_vs_defense_distinction, holdable).
narrative_ontology:cs_axiom_grounding('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', aggressive_war_vs_defense_distinction, conventional).
narrative_ontology:cs_reference_frame('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', sovereign_defense_with_pacifist_renunciation).
narrative_ontology:cs_drift_state('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a13b3b0c-e11a-4225-8bc0-9078f1a21b66', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, national_security_establishment).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, regional_alliance_partners).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, constitutional_pacifist_movement).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, regional_peace_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, neighboring_states).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, neighboring_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9's war renunciation as prohibiting aggressive war only, not defensive capacity. Administers the Self-Defense Forces within this interpretation. Justifies organizational scope and equipment as 'minimum necessary' for territorial protection. Sets the boundary between constitutional permissibility and excess through doctrine and training. Benefits from the reading because it legitimates existing military institutions and budgets.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, national_security_establishment, agenda_setter,
    institutional, generational, constrained, national).

% Advocates the strict reading that Article 9's language 'never be maintained' means categorical prohibition on any armed forces. Bears the cost of an interpretation imposed against their constitutional reading. They can exit by emigrating, shifting to different political movements, or accepting the majority interpretation; they cannot change the constraint from inside.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_pacifist_movement, payer,
    organized, biographical, mobile, national).

% Benefit from Japan's military capacity as a deterrent to regional instability and as a potential collective-defense participant. Their strategic position depends partly on the security establishment's interpretation of Article 9 permitting defensive capability development. They have constrained exit: they could seek alternative security arrangements but the regional balance would shift substantially.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, regional_alliance_partners, beneficiary,
    institutional, generational, constrained, regional).

% Experience Japan's military capacity as both a stabilizing factor (reducing power vacuums, deterring third-party aggression) and a security concern (military modernization, reinterpretation of defensive scope). They cannot exit the regional system and must respond to how the inherent-right reading allows military development.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, neighboring_states, payer,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, neighboring_states, beneficiary).

% Holds power to adjudicate the constitutionality of security establishment actions and interpretations, but has historically abstained from direct review of military scope decisions on justiciability grounds. Observes but does not directly enforce or constrain the reading.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% Analyzes whether the inherent-right reading aligns with international humanitarian law and UN Charter provisions on self-defense. Provides external perspective on whether 'minimum necessary' remains credible as Japanese military capacity expands, but holds no enforcement power over Japanese constitutional interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, international_law_community, observer,
    analytical, civilizational, analytical, global).

% Would argue that the categorical language of Article 9 forecloses the inherent-right reading entirely and that any military forces violate the constitutional text. They are excluded from the authoritative interpretation process because the security establishment and majority political coalition have closed off that reading path, though they retain voice through domestic legal and political channels.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, strict_pacifist_reading_community, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__inherent_right_reading, national_security_establishment).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__inherent_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework that permits Japan to maintain territorial defensive capacity without violating the constitutional renunciation of war. Solves the coordination problem of how a pacifist textual commitment can coexist with a state's inherent right to repel invasion — the reading coordinates between the constitutional prohibition on aggressive war and the practical necessity of defensive readiness.
% TRANSFER_FUNCTION: Transfers interpretive authority from the constitutional text's apparent categorical language to the security establishment's administrative judgment about what constitutes 'minimum necessary' defense. What flows from strict reading to inherent-right reading is the power to define the boundary; what flows from the pacifist movement to the security establishment is legitimacy to maintain organized armed forces.
% ABSENT_VOICES: The strict pacifist reading is structurally excluded from the authoritative interpretation process, though advocates remain legally able to petition courts and contest in political arenas. Their constitutional reading is not represented in the security establishment's doctrine-setting or force-structure decisions. International peace advocates outside Japan also have no seat in how Article 9 is operationalized.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the strict pacifist reading became constitutional law, Japan would liquidate or fundamentally transform the Self-Defense Forces, eliminate defense budgets at current scale, and reorient regional security relationships entirely. Neighboring states would respond to the power vacuum. If instead the strict reading were formally displaced and replaced by explicit collective-defense authority (the collective_self_defense_reading), Japan's force structure would expand and its treaty obligations would change fundamentally.
% FOUNDING_PROBLEM: After World War II occupation, Japan adopted a pacifist constitution. However, the Cold War and regional tensions (Korean War, Soviet submarine activity, territorial claims) created practical pressure to maintain some defensive capacity. The inherent-right reading solved this by interpreting Article 9 as renouncing aggressive war while preserving the state's natural right to defend itself — allowing the Self-Defense Forces to exist within a pacifist constitutional frame.
% FOUNDING_PROBLEM_CORROBORATION: The security establishment attests the problem is live: contemporary regional military buildups, territorial disputes, and potential invasion scenarios justify defensive forces. The pacifist movement attests the founding problem is a false framing: the occupation-era constitutional commitment was deliberately pacifist, and reinterpreting it via 'inherent rights' doctrine violates the text's intent. Independent constitutional scholars are divided; some support the inherent-right reading as a coherent reconciliation, others argue it is textual deviation justified only by geopolitical pressure rather than constitutional principle.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).
:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at present) because the reading transfers interpretive authority from text to bureaucracy: the security establishment gains the power to define 'minimum necessary' without explicit textual guidance. The boundary is real — the reading does constrain military scope relative to what a pure power-state reading would permit — but the boundary has drifted over 79 years as regional threats and technological change have expanded what counts as 'minimum necessary.' Suppression is low (0.22) because the reading does not require coercive elimination of alternatives: the pacifist movement can organize, advocate, and petition courts; they simply lack current political power to change the interpretation. Theater ratio has risen over time (1947: 0.22 → 2026: 0.41) as the performance of constitutional constraint has grown relative to functional defense change — the reading provides ritual reassurance that forces remain limited, while actual force structure changes respond to regional dynamics. Accessibility of the strict pacifist reading has collapsed (measured in the accessibility_collapse metric at 0.68): once the inherent-right reading became institutionalized doctrine, shifting back to strict pacifism requires overcoming not just political opposition but decades of established military structures, alliances, and strategic assumptions. Resistance is high (0.74) because the pacifist movement actively contests the reading through legal challenges, political movements, and constitutional advocacy; this resistance has not diminished the interpretation's entrenchment, indicating that the reading persists not by consent but by institutional weight and political majority.
 *
 * PERSPECTIVAL GAP:
 *   From the security establishment's seat, the constraint is genuine coordination: it reconciles a pacifist text with an inherent right to defense, solving a real geopolitical necessity. From the pacifist movement's seat, the same constraint is extractive — it privileges the security establishment's interpretation over the text's apparent meaning, imposing that interpretation against their constitutional reading. The engine computes this divergence from structural data: the security establishment has institutional power and controls the authoritative reading; the pacifist movement has only organized power and mobile exit (political voice, court petition, emigration), which is asymmetric. The beneficiary/victim split captures this asymmetry: the security establishment collects institutional legitimacy and resources; the pacifist movement pays the cost of a constitutional reading imposed against their understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   The national security establishment sits near d=0.1 (beneficiary end): it collects the authority to interpret Article 9, legitimacy to maintain forces, and budget allocation. Its exit is constrained institutionally (the role itself is bound to the interpretation), but its power is institutional, so the directionality is toward subsidy — the constraint secures its position. The pacifist movement sits near d=0.85 (target end): it bears the cost of a constitutional reading imposed against their understanding, has organized power (moderate exit, can petition and advocate but cannot force change), and the constraint suppresses their preferred reading by establishing an alternative as authoritative. Neighboring states sit near d=0.5: they gain military stabilization (subsidy) but also carry security costs (asymmetric military capabilities in the region), so their directionality is symmetric. The security establishment's directionality is not overridden; the structural derivation captures the true relationship. The pacifist movement's directionality is also not overridden — they are targets under this reading, with constrained exit and no power to change the interpretation unilaterally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was live in 1947–1960: Cold War regional pressure created a genuine coordination need — how to permit defensive readiness under a pacifist constitution. By 2000–2015, the founding problem status shifted: regional threats remained contested, but the reading had ossified into institutional doctrine. The core coordination function (reconciling pacifism with defense) is real and persistent, but the reading's execution has drifted from 'minimum necessary' toward 'whatever the security establishment judges necessary.' The theater ratio at 0.41 indicates that performance of constraint has grown — the reading increasingly serves to legitimize force expansion within a pacifist frame rather than to actually limit it. The rising extractiveness (1947: 0.18 → 2015: 0.41 → 2026: 0.38) followed by slight decrease reflects a peak of doctrine expansion around 2015 (collective-self-defense legislation, reinterpretation debates) followed by domestic political stabilization. The constraint is classified as tangled_rope (not pure rope, not snare) because it genuinely coordinates between constitutional commitment and geopolitical necessity AND asymmetrically extracts interpretive authority from the pacifist reading to the security establishment. The enforcement requirement (requires_active_enforcement: true) is high: the reading persists because the security establishment continuously reiterates it, defends it against legal challenges, and administers it through doctrine. If enforcement ceased (courts accepted pacifist arguments, political opposition succeeded), the reading would collapse. This active enforcement distinguishes it from a genuine coordination mechanism that would persist by participant preference.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimal_necessity_boundary_drift,
    'What constitutes ''minimum necessary'' defensive capacity, and does that boundary track actual defensive requirement or expand with technological innovation and regional threat perception?',
    'Comparative analysis: (a) independent assessment of credible territorial threats Japan faces; (b) documentation of Self-Defense Forces equipment acquisition and expansion decisions over time; (c) security-specialist evaluation of whether force structure tracks threat or exceeds it; (d) cross-national comparison with other constitutional democracies'' defensive-force sizing under similar threat profiles.',
    'If force structure systematically exceeds credible threat requirements, the constraint has drifted from proportionality gate to legitimacy cover — extractiveness classification would shift upward, theater_ratio vindicated as rising performance of constraint over actual function. If force structure tracks threat, the reading retains genuine coordination function despite boundary ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimal_necessity_boundary_drift, empirical, 'Whether the ''minimum necessary'' boundary is drifting or stable relative to actual defensive requirements.').

omega_variable(
    textual_intent_vs_institutional_practice,
    'Was the occupation-era constitutional framers'' intent genuinely pacifist (categorical prohibition on military forces), or did they intend to permit defensive forces under the ''renounce war'' language?',
    'Historical analysis of constitutional drafting (GHQ documents, Diet debate transcripts, framers'' testimony), constitutional scholarship consensus, comparative textual analysis with other constitutions'' self-defense clauses, and examination of whether early SDF establishment (1950–1954) was seen as constitutional violation or legitimate interpretation at the time.',
    'If framers'' intent was categorical pacifism, the inherent-right reading is a post-hoc reinterpretation imposed by geopolitical pressure; the reading''s legitimacy derives from institutional weight, not constitutional fidelity. If framers'' intent was to permit defensive forces, the reading is constitutionally faithful. Classification impact: interpretation-as-imposition elevates extractiveness and theater-ratio concerns; interpretation-as-fidelity maintains coordination function as primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_intent_vs_institutional_practice, empirical, 'Whether the inherent-right reading aligns with original constitutional intent or is a post-hoc institutional reinterpretation.').

omega_variable(
    reading_foreclosure_under_collective_defense_pressure,
    'Can the inherent-right reading (individual defense only) coexist with the collective-self-defense reading (defense of allies) within a single institutional framework, or do they eventually foreclose each other as doctrine expands?',
    'Monitoring of Japanese security policy trajectory: if collective-defense legislation succeeds in redefining ''self-defense'' to include regional alliance obligations, the inherent-right reading''s proportionality constraint weakens (scope expands to cover allied territories). If courts or political opposition succeed in maintaining the individual-defense boundary despite regional pressure, the readings coexist.',
    'If collective-defense reading forecloses the inherent-right reading, this constraint transitions from tangled_rope (with pacifist coordination + security extraction) to snare-flavored (pure power projection dressed as defense). If they coexist stably, the constraint persists as currently classified. Classification impact on the collective-self-defense sibling: if it forecloses this reading, that sibling classifies as snare or piton (defense talk covering regional power expansion); if it coexists, both remain tangled_rope with different scope boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_under_collective_defense_pressure, conceptual, 'Whether the inherent-right and collective-self-defense readings can coexist or eventually foreclose each other.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the pacifist reading suppressed by structural barriers (institutional power imbalance, political majority opposition) or by internalized acceptance among pacifists that the inherent-right reading is constitutionally legitimate?',
    'Post-suppression trajectory: if Japan''s political landscape shifted (courts accepted pacifist challenges, electoral coalition changed), would pacifist organizing capacity and framing immediately recover, or is suppression partly internalized in cultural acceptance of the inherent-right reading even by non-committed actors?',
    'If suppression is purely structural, removing institutional barriers would restore the pacifist reading''s viability. If partly internalized, suppression persists even after structural barriers weaken — the reading has shaped cultural understandings of what constitutionalism permits. This affects the constraint''s long-term persistence and the accessibility_collapse metric: internalized suppression means alternatives are genuinely harder to imagine, not just politically defeated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether pacifist-reading suppression is structural, internalized, or both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1947, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1947, 0.22).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1960, 0.28).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2000, 0.39).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(arti_tr_t2026, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1947, 0.18).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(arti_be_t2026, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1947, 0.08).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2000, 0.21).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2015, 0.23).
narrative_ontology:measurement(arti_su_t2026, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__inherent_right_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, japan_sdf_force_structure_legitimacy).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, us_japan_security_alliance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Article 9 kernel. The strict_pacifist_reading instantiates the categorical prohibition interpretation (separate constraint, higher extractiveness, snare-classified at the institutional level because pacifist voice is excluded from authority). The collective_self_defense_reading extends the inherent right to allied defense (separate constraint, scope-expanded, influences this constraint by creating institutional pressure to broaden 'self-defense' definition). All three readings share the same constitutional text but produce different constraints because they assign different ε (extractiveness) values, beneficiary/victim structures, and structural types. Network edges link them: this constraint influences the collective-defense sibling (by establishing that some inherent-right reading is constitutionally permissible, creating pressure to expand its scope); this constraint is foreclosed by the strict-pacifist sibling in the pure logical sense (both cannot be the authoritative reading simultaneously), but they coexist empirically as competing institutional and political readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
