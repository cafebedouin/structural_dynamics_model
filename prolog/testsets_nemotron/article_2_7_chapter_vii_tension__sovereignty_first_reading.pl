% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: State Sovereignty as Foundational Constraint on Intervention (Article 2(7) / Chapter VII Tension - Sovereignty-First Reading)
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty-first reading of the
 *   Article 2(7) / Chapter VII tension kernel. The UN Charter establishes
 *   state sovereignty as foundational (Article 2(1), 2(7)) while authorizing
 *   Security Council enforcement action under Chapter VII for threats to
 *   international peace and security. The sovereignty-first reading
 *   interprets Chapter VII as limited to inter-state aggression, treating
 *   internal atrocities as domestic jurisdiction protected by the
 *   non-intervention principle. This reading has been the dominant
 *   operational framework since 1945, though its extractiveness has increased
 *   as internal conflicts and atrocities have become the predominant form of
 *   mass violence. Post-colonial states adopted the sovereignty shield to
 *   protect against former colonial powers; authoritarian states use it to
 *   insulate domestic repression; permanent Security Council members
 *   (especially Russia and China) wield the veto to block interventions that
 *   might set precedents threatening their own domestic arrangements. The
 *   constraint extracts from populations under atrocity by denying them
 *   external protection, while the coordination function — preventing great
 *   power war through territorial inviolability — is genuine but increasingly
 *   decoupled from the extraction it enables.
 *
 * KEY AGENTS:
 *   - post_colonial_states: Primary beneficiaries (powerless/moderate, constrained exit) — gained independence under sovereignty norm, now use it to block external scrutiny
 *   - authoritarian_states: Primary beneficiaries (powerful/institutional, arbitrage exit) — exploit sovereignty to repress domestic populations without external interference
 *   - permanent_security_council_members: Agenda setters / beneficiaries (institutional, arbitrage) — control the authorization gate via veto, extract geopolitical concessions for consent
 *   - populations_under_domestic_atrocity: Primary victims (powerless, trapped/identity_locked) — no exit, no voice in Security Council, bear full cost of non-intervention
 *   - internal_opposition_movements: Victims (powerless/moderate, trapped) — cannot appeal to international protection without state consent
 *   - stateless_persons_in_contested_territories: Victims (powerless, trapped) — fall through sovereignty cracks, no state claims responsibility
 *   - r2p_advocates_civil_society: Excluded (organized, constrained) — would object to sovereignty absolutism but lack formal standing in Security Council
 *   - international_legal_scholars: Observers (analytical, analytical) — analyze the constraint's evolution and legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.85).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "State Sovereignty as Foundational Constraint on Intervention (Article 2(7) / Chapter VII Tension - Sovereignty-First Reading)").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '6073f266-0841-482c-9316-ecd040bfbb05').
narrative_ontology:cs_kernel_codification('6073f266-0841-482c-9316-ecd040bfbb05', formalized).
narrative_ontology:cs_authority_grounding('6073f266-0841-482c-9316-ecd040bfbb05', lineage).
narrative_ontology:cs_interpretation_layer_present('6073f266-0841-482c-9316-ecd040bfbb05').
narrative_ontology:cs_reading_relation('6073f266-0841-482c-9316-ecd040bfbb05', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('6073f266-0841-482c-9316-ecd040bfbb05', foundational, sovereignty_as_precondition_of_order).
narrative_ontology:cs_axiom_status(sovereignty_as_precondition_of_order, holdable).
narrative_ontology:cs_axiom_grounding('6073f266-0841-482c-9316-ecd040bfbb05', sovereignty_as_precondition_of_order, conventional).
narrative_ontology:cs_axiom('6073f266-0841-482c-9316-ecd040bfbb05', foundational, chapter_vii_limited_to_interstate_aggression).
narrative_ontology:cs_axiom_status(chapter_vii_limited_to_interstate_aggression, holdable).
narrative_ontology:cs_axiom_grounding('6073f266-0841-482c-9316-ecd040bfbb05', chapter_vii_limited_to_interstate_aggression, conventional).
narrative_ontology:cs_axiom('6073f266-0841-482c-9316-ecd040bfbb05', secondary, non_intervention_as_jus_cogens).
narrative_ontology:cs_axiom_status(non_intervention_as_jus_cogens, holdable).
narrative_ontology:cs_axiom_grounding('6073f266-0841-482c-9316-ecd040bfbb05', non_intervention_as_jus_cogens, conventional).
narrative_ontology:cs_reference_frame('6073f266-0841-482c-9316-ecd040bfbb05', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('6073f266-0841-482c-9316-ecd040bfbb05', post_r2p_2005_world_summit, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6073f266-0841-482c-9316-ecd040bfbb05', '2026-08-15T14:32:00Z').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_security_council_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, internal_opposition_movements).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, stateless_persons_in_contested_territories).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, non_intervention_principle).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, territorial_integrity_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained independence under the sovereignty norm; invoke Article 2(7) to block external interference in domestic affairs. Benefit from the constraint's protection against former colonial powers and great power intervention. Exit is constrained: leaving the UN system forfeits diplomatic recognition and development aid; forming alternative regional bodies (AU, ASEAN) still operates within the sovereignty framework.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states, beneficiary,
    moderate, generational, constrained, global).

% Use sovereignty as a shield for domestic repression — the constraint denies international legal basis for intervention in internal atrocities. They actively shape the norm through diplomatic coalitions (e.g., Like-Minded Group at UNHRC). Exit is arbitrage-grade: they can threaten withdrawal from treaties, form alternative institutions, or leverage great power patronage; the constraint serves them, they do not serve it.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_states, beneficiary,
    powerful, biographical, arbitrage, global).

% Control the Chapter VII authorization gate via veto power. They administer the constraint: decide when sovereignty yields to enforcement. Benefit from the veto as geopolitical leverage — extract concessions for consent, block precedents threatening their own domestic arrangements (Chechnya, Xinjiang, etc.). The constraint's enforcement machinery (SC resolutions, peacekeeping mandates) is their instrument. Exit is arbitrage: they wrote the rules and can paralyze modification.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_security_council_members, beneficiary).

% Bear the full cost of the sovereignty barrier: mass killing, displacement, torture, starvation with no legal pathway to external protection. No standing in Security Council; no exit from the state perpetrating atrocities (borders closed, identity documents controlled). Identity_locked in the sense that their survival is bound to the territory the constraint protects from intervention. The constraint extracts their security and lives as the price of the international order's stability.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% Political movements facing state repression cannot appeal to international protection without host state consent. The constraint denies them a legal basis for external support. Exit is constrained: can flee as refugees (losing political base), seek asylum (individual not collective), or persist domestically. Some gain limited NGO/diplomatic protection but no structural right to intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, internal_opposition_movements, payer,
    moderate, biographical, constrained, national).

% Fall through the sovereignty constraint's cracks: no state claims responsibility for their protection, and the non-intervention principle blocks external actors from assuming responsibility. Examples: Rohingya in Myanmar/Rakhine, Palestinians in occupied territories, Kurdish populations across four states. The constraint's state-centric architecture has no seat for them.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, stateless_persons_in_contested_territories, payer,
    powerless, immediate, trapped, local).

% NGOs, legal scholars, former officials advocating for R2P implementation. They would object to sovereignty absolutism and argue for a legal duty to protect. Excluded from Security Council authorization decisions — no formal standing, influence only through norm advocacy, General Assembly resolutions, and public pressure. Exit is constrained: they operate within the UN system they seek to reform; leaving forfeits influence.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, r2p_advocates_civil_society, excluded,
    organized, generational, constrained, global).

% Analyze the constraint's evolution, legitimacy, and interpretation. Provide the epistemic infrastructure for both readings: sovereignty-first scholars (Chesterman, Werner) and R2P scholars (Evans, Bellamy, Welsh). Do not collect from or pay into the constraint; their situation is analytical distance.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great power war by establishing territorial inviolability as a systemic baseline: states agree not to intervene in each other's domestic jurisdiction, reducing flashpoints. Provides a universal membership framework (UN) where all states have equal sovereign standing regardless of power. Enables diplomatic channels, dispute resolution, and predictable inter-state relations.
% TRANSFER_FUNCTION: Transfers protection-from-intervention from atrocity populations to their perpetrator states. The constraint moves the security of vulnerable populations into the discretion of the states that threaten them, and moves authorization power into the hands of five permanent Security Council members who extract geopolitical concessions for consent. The coordination benefit (inter-state peace) is distributed globally; the extraction cost is concentrated on internally targeted populations.
% ABSENT_VOICES: Populations under atrocity (no standing), stateless persons (no state representative), future generations (no voice in current veto decisions), and R2P advocates (excluded from authorization body). They are absent because the constraint's architecture is state-centric: only states sit at the Security Council, and only states can invoke Chapter VII. The General Assembly (where all states have voice) has no enforcement power.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first reading vanished overnight (i.e., Chapter VII were interpreted to authorize intervention for internal atrocities without consent), the Security Council would become an active atrocity-prevention body. Permanent members would lose veto leverage over internal affairs. Authoritarian states would face credible intervention threat. Post-colonial states would lose the sovereignty shield. The UN system would reorganize around a protection mandate rather than a non-intervention mandate — great power conflict risk would rise, but atrocity prevention would become legally structured.
% FOUNDING_PROBLEM: Prevent great power war by establishing territorial inviolability and non-intervention as the baseline of international order, after two world wars caused by interventionist pretexts and territorial revisionism.
% FOUNDING_PROBLEM_CORROBORATION: Sovereignty-first proponents (China, Russia, Non-Aligned Movement, many Global South states) attest the problem is live: great power competition is returning, and the sovereignty norm is the primary barrier against interventionist wars. R2P proponents (Western NGOs, some Western states, UN Secretariat under Annan/Ki-moon, African Union) attest the problem is substantially solved: nuclear deterrence and economic interdependence prevent great power war; the remaining mass atrocities are internal, and the sovereignty norm now enables rather than prevents them. The 2005 World Summit Outcome Document (para 138-139) records both positions without resolution — the founding problem's status is formally contested within the UN's own founding document's successor.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint blocks protection for atrocity populations while the coordination benefit (inter-state peace) has partially decoupled — great power war has been deterred by nuclear weapons and economic interdependence more than by sovereignty norms. Suppression (0.85) is very high: the constraint is actively enforced through Security Council vetoes, diplomatic pressure against intervention coalitions, and the structural exclusion of non-state voices from authorization decisions. Theater ratio (0.32) is moderate: the sovereignty norm performs real coordination (diplomatic channels, dispute resolution, territorial stability) but a growing share of its enforcement activity serves to protect extractive domestic arrangements. Accessibility collapse (0.72) is high but not absolute: R2P, humanitarian intervention precedents (Kosovo 1999, Libya 2011), and regional organization practice create partial alternatives. Resistance (0.58) is significant: civil society advocacy, R2P norm entrepreneurship, and occasional unauthorized interventions (Kosovo) contest the constraint, but the structural gate (Security Council veto) remains intact.
 *
 * PERSPECTIVAL GAP:
 *   From the post-colonial/authoritarian state seat, the constraint is genuine coordination: it protects hard-won independence from great power intervention. From the permanent Security Council member seat, it is a managed coordination mechanism that extracts geopolitical leverage via the veto. From the atrocity population seat, it is pure extraction: they bear the full cost of non-intervention with zero exit and zero voice. The engine computes this divergence from the structural data — beneficiaries have low d (subsidized), victims have high d (extracted), agenda_setters sit near d=0.5 (costs ≈ benefits but with veto power). The sovereignty-first reading's claimed_type (tangled_rope) reflects the author's assessment that genuine coordination AND asymmetric extraction coexist; the R2P reading would claim snare (pure extraction cover). The engine will compute per-seat types independently.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (post_colonial_states, authoritarian_states, permanent_security_council_members) collect protection from external intervention — the constraint subsidizes their domestic autonomy. Permanent members additionally extract geopolitical rent via the veto. Victims (populations_under_domestic_atrocity, internal_opposition_movements, stateless_persons) bear the full cost: they are denied protection, have no exit (trapped/identity_locked), and no standing in the authorization body. Excluded agents (r2p_advocates) would challenge the constraint but are structurally barred from the decision forum. Observers (scholars) have analytical exit. Directionality derivation: beneficiaries → low d; victims → high d (trapped/identity_locked exit amplifies); agenda_setters (P5) → d ~0.5 (veto power gives them control over the constraint's operation, but they also bear reputational costs of blocking).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great power war through territorial inviolability) is CONTESTED as live: nuclear deterrence and economic interdependence may have superseded sovereignty norms as the primary war-prevention mechanism, but sovereignty advocates argue the norm remains necessary as a backstop. The constraint persists despite the founding problem's contested status because the beneficiaries (states with veto power or sovereignty-dependent legitimacy) control the modification gate. This is classic mandatrophy: the arrangement's mandate (prevent inter-state war) has atrophied in causal primacy, but the constraint persists because the actors who could change it benefit from its current form. The high theater ratio trajectory (0.18→0.32) tracks this: increasing performative maintenance of a coordination story that less and less explains the constraint's actual operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_naturalness_ambiguity,
    'Is the absolute sovereignty constraint a genuine structural feature of the international system (mountain-like) or a constructed norm that benefits identifiable state actors (tangled rope/snare)?',
    'Historical counterfactual analysis: if the UN Charter were rewritten without Article 2(7), would intervention patterns change, or do material power distributions produce the same blocking effect? Also: measure whether states that invoke sovereignty most loudly are those with domestic atrocity records.',
    'If constructed, the constraint''s high extraction from atrocity populations is illegitimate coordination cover; if natural-law-like, the extraction is the price of systemic stability. Determines whether FSM (false summit mountain) signature triggers reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_naturalness_ambiguity, conceptual, 'Whether sovereignty-as-barrier is a natural law of international order or a constructed norm shielding extractive state behavior').

omega_variable(
    chapter_vii_interpretation_scope,
    'Does ''threat to international peace and security'' in Chapter VII structurally permit authorization for purely internal atrocities, or is the inter-state aggression limitation a genuine textual constraint?',
    'Analyze Security Council practice 1945-present: count Chapter VII resolutions authorizing intervention in purely internal conflicts without state consent. Code for whether the atrocity had cross-border spillover (refugees, regional destabilization) as a textual hook.',
    'If Chapter VII already permits internal atrocity intervention, the sovereignty-first reading''s extraction is inflated — the constraint is the reading itself, not the Charter. If the inter-state limit is binding, the extraction is structural to the legal architecture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chapter_vii_interpretation_scope, empirical, 'Whether the Charter text itself blocks internal atrocity intervention or whether the sovereignty-first reading imposes a restrictive interpretation').

omega_variable(
    r2p_customary_status,
    'Has the Responsibility to Protect (R2P) crystallized into customary international law that structurally modifies the sovereignty constraint, or does it remain a political declaration without legal force?',
    'Track state practice and opinio juris: UNGA resolutions referencing R2P, regional organization interventions (ECOWAS, AU), ICJ advisory opinions, and whether states invoke R2P as legal obligation vs. political commitment in General Assembly debates.',
    'If R2P is customary law, the sovereignty-first reading''s high epsilon is contested by an emerging counter-constraint; if purely political, the extraction stands unmodified. Affects network coupling with r2p_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(r2p_customary_status, conceptual, 'Whether R2P has legal force that structurally erodes the sovereignty barrier').

omega_variable(
    committer_frame_kernel_reading,
    'This constraint is one reading (sovereignty_first_reading) of the contested kernel article_2_7_chapter_vii_tension. The sibling reading is r2p_reading. What structural elements differ between readings?',
    'Compare the two constraint stories'' base_properties: extractiveness, beneficiaries, victims, claimed_type. The sovereignty-first reading assigns high epsilon to the blocking arrangement; the R2P reading assigns high epsilon to the non-intervention arrangement. The kernel is the UN Charter Articles 2(7) and Chapter VII; readings differ on which arrangement is the standing one under contest.',
    'Confirms this is a kernel reading subject to Rules 1-4. The omega documents committer structure without inventing schema fields. Sibling constraint_id: article_2_7_chapter_vii_tension__r2p_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Commiter-frame metadata: kernel_id, reading_id, sibling readings, structural delta').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art27c7_sov1_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(art27c7_sov1_tr_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(art27c7_sov1_tr_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(art27c7_sov1_tr_t1990, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(art27c7_sov1_tr_t2001, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2001, 0.3).
narrative_ontology:measurement(art27c7_sov1_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.31).
narrative_ontology:measurement(art27c7_sov1_tr_t2011, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2011, 0.32).
narrative_ontology:measurement(art27c7_sov1_tr_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(art27c7_sov1_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(art27c7_sov1_be_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(art27c7_sov1_be_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(art27c7_sov1_be_t1990, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(art27c7_sov1_be_t2001, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(art27c7_sov1_be_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2005, 0.71).
narrative_ontology:measurement(art27c7_sov1_be_t2011, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2011, 0.75).
narrative_ontology:measurement(art27c7_sov1_be_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(art27c7_sov1_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.65).
narrative_ontology:measurement(art27c7_sov1_su_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1960, 0.72).
narrative_ontology:measurement(art27c7_sov1_su_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1975, 0.78).
narrative_ontology:measurement(art27c7_sov1_su_t1990, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1990, 0.81).
narrative_ontology:measurement(art27c7_sov1_su_t2001, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2001, 0.83).
narrative_ontology:measurement(art27c7_sov1_su_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2005, 0.84).
narrative_ontology:measurement(art27c7_sov1_su_t2011, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2011, 0.85).
narrative_ontology:measurement(art27c7_sov1_su_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.12).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, responsibility_to_protect_norm).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_intervention_precedents).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_veto_power).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, regional_organization_intervention_authority).

% DUAL FORMULATION NOTE:
% Kernel decomposition: article_2_7_chapter_vii_tension splits into (1) sovereignty_first_reading — high epsilon blocking intervention, beneficiaries=post-colonial/authoritarian states, victims=atrocity populations, claimed_type=tangled_rope; (2) r2p_reading — high epsilon from non-intervention, beneficiaries=atrocity populations, victims=states subject to unauthorized intervention, claimed_type=snare (from R2P perspective) or tangled_rope (from sovereignty perspective). The ε values differ because each reading assesses the standing arrangement from its own lights: sovereignty-first sees the Charter framework as the arrangement (extraction = blocking); R2P sees the non-intervention practice as the arrangement (extraction = allowing atrocities). Network edges link both to shared upstream constraints (Charter text, veto power) and downstream effects (intervention precedents, regional practice).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, institutional, 0.35).
constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
