% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: UN Security Council P5 Veto as Great-Power War Prevention Mechanism
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint story captures the coordination_reading of the UN
 *   Security Council P5 veto (Article 27(3) of the UN Charter). Under this
 *   reading, the veto is a deliberately designed coordination mechanism: by
 *   requiring unanimity among the five nuclear-armed permanent members for
 *   any Chapter VII enforcement action, it ensures that the Security Council
 *   can never authorize military action against a great power's vital
 *   interests. This eliminates the structural trigger for great-power war
 *   that would exist if a majority could outvote a nuclear state. The
 *   beneficiary is the international system as a whole — all states gain the
 *   collective good of avoided nuclear confrontation. No victim class exists
 *   under this reading because the veto extracts no targeted transfer; it
 *   merely constrains the decision rule to match the underlying reality of
 *   nuclear deterrence. The claimed type is rope (pure coordination), with
 *   low extractiveness (0.08) deriving from the collective-action failure
 *   risk that would exist without the unanimity gate, low suppression (0.12)
 *   because the constraint is self-enforcing through mutual interest rather
 *   than coercion, and low theater (0.15) because the veto's war-prevention
 *   function is genuine and continuously operative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.08).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.12).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "UN Security Council P5 Veto as Great-Power War Prevention Mechanism").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, '3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1').
narrative_ontology:cs_kernel_codification('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', formalized).
narrative_ontology:cs_authority_grounding('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', lineage).
narrative_ontology:cs_interpretation_layer_present('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1').
narrative_ontology:cs_reading_relation('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', foundational, unanimity_gate_prevents_great_power_war).
narrative_ontology:cs_axiom_status(unanimity_gate_prevents_great_power_war, holdable).
narrative_ontology:cs_axiom_grounding('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', unanimity_gate_prevents_great_power_war, empirically_contingent).
narrative_ontology:cs_axiom('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', foundational, collective_security_requires_great_power_consent).
narrative_ontology:cs_axiom_status(collective_security_requires_great_power_consent, holdable).
narrative_ontology:cs_axiom_grounding('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', collective_security_requires_great_power_consent, conventional).
narrative_ontology:cs_reference_frame('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', charter_collective_security_framework).
narrative_ontology:cs_drift_state('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c5e8ee3-e85c-4ca5-ac22-bc01741e5ca1', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, middle_powers).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, small_states).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, collective_security_doctrine).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, great_power_war_prevention).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, nuclear_deterrence_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the veto power under UN Charter Article 27(3). They administer the unanimity gate that prevents Security Council authorization of military action against any P5 member's vital interests. They benefit from the veto both as a shield against great-power confrontation and as a structural guarantee of their great-power status. Their exit from the constraint is effectively impossible — the veto is constitutive of their institutional position — but they face no pressure to exit because the arrangement serves their core security interests.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__coordination_reading, p5_states, beneficiary).

% Lack veto power but benefit from the great-power war prevention the unanimity gate enables. The veto ensures that no Security Council resolution can trigger direct military confrontation between nuclear-armed states, which would risk global catastrophe. Their exit options are constrained — leaving the UN system forfeits the collective security framework entirely — but they do not bear extraction costs from the veto itself; they gain stability without paying a transfer.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_states, beneficiary,
    moderate, biographical, constrained, global).

% States with significant regional influence but no veto (e.g., India, Brazil, Germany, Japan, South Africa). They benefit from the great-power stability the veto secures while sometimes advocating for Security Council reform. Their exit options are relatively mobile — they can build alternative regional security architectures — but they remain embedded in the UN-centered system because the veto's war-prevention function operates at the global level where no regional substitute exists.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, middle_powers, beneficiary,
    organized, biographical, mobile, regional).

% States with minimal structural power in the international system. They are the most dependent on the collective security framework the veto underwrites — without great-power restraint, they face disproportionate risk from major-power conflict. Their exit options are effectively trapped: they cannot construct alternative security guarantees and have no leverage to reform the veto. Yet under this reading they are not victims; the veto's existence protects them from the worst systemic outcome (great-power war) even as it denies them formal influence.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, small_states, beneficiary,
    powerless, immediate, trapped, local).

% Analyze the veto's operation from outside the political constellation. They document veto use patterns, assess whether the unanimity gate correlates with reduced great-power conflict, and evaluate reform proposals. Their seat is purely analytical — they neither collect from nor pay into the constraint — but their work informs the legitimacy discourse that surrounds the veto's persistence.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of great-power cooperation under anarchy: without a unanimity gate, any Security Council majority could authorize military action against a nuclear-armed state's perceived vital interests, creating a structural trigger for nuclear confrontation. The veto makes the Security Council a forum where great powers only commit to enforcement when all agree, transforming a potential war machine into a coordination mechanism.
% TRANSFER_FUNCTION: Transfers decision authority from majority-rule to unanimity among the five nuclear-armed permanent members. No material resource flow occurs — the transfer is purely procedural: the power to authorize Chapter VII enforcement action moves from a potential majority coalition to a required consensus of the P5. All states receive the collective good of avoided great-power war; no state pays a targeted extraction.
% ABSENT_VOICES: States and civil society actors who argue that the veto paralyzes the Security Council on mass atrocities (Syria, Myanmar, Ukraine) and that a qualified-majority or responsibility-to-protect override would better serve human security. These voices are structurally excluded by the Charter's amendment formula (Article 108), which gives each P5 an absolute veto over any change to the veto itself. They would object to the coordination framing because from their position the veto's cost is measured in unaddressed crises, not in avoided great-power war.
% DISAPPEARANCE_RATIONALE: If the veto vanished overnight, the Security Council could authorize enforcement action against a P5 state's interests by majority vote. At least one P5 would likely treat this as an existential threat, triggering either withdrawal from the UN system or direct military confrontation. The post-1945 great-power peace — sustained in significant part by the veto's guarantee that no nuclear state can be outvoted into war — would lose its institutional anchor. The world would rearrange into either a new great-power concert or open conflict.
% FOUNDING_PROBLEM: How to create a collective security organization that the great powers would actually join and not veto into paralysis — the League of Nations failed because great powers could be outvoted on enforcement, leading to withdrawal and irrelevance. The UN Charter's answer: give each great power a veto so that the Security Council never authorizes action against a great power's core interests, making participation rational for all.
% FOUNDING_PROBLEM_CORROBORATION: UN Charter drafters (San Francisco 1945 records) explicitly designed the veto to ensure great-power participation. Cold War historians (Gaddis, Westad) document that the veto prevented Security Council authorization of direct NATO-Warsaw Pact conflict. Nuclear deterrence theorists (Schelling, Jervis, Sagan) outside the P5 beneficiary set confirm that institutionalizing mutual restraint reduces accidental escalation risk. No credible scholar argues the great-power war problem is solved; the founding problem remains live.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.08) because the veto does not transfer resources from one group to another — it changes the decision threshold from majority to unanimity. The 'cost' is foregone enforcement actions that would have triggered great-power conflict; this is a coordination cost, not an extraction. Suppression is low (0.12) because the veto requires no active enforcement machinery: P5 states exercise it voluntarily because the alternative (a Security Council that can authorize war against them) is worse. Theater ratio is low (0.15) — the veto is used substantively (blocking actions that would threaten P5 interests), not performatively. Accessibility collapse is moderate (0.45) because alternative collective security designs (League of Nations, qualified-majority proposals) have been tried and failed, but reform discussions persist. Resistance is low (0.25) because all P5 benefit and non-P5 states gain systemic stability; opposition comes from excluded voices on humanitarian grounds, not from structural victims.
 *
 * PERSPECTIVAL GAP:
 *   The coordination_reading and oligopoly_reading will compute radically different per-seat classifications from the same structural data. Under coordination_reading, all seats experience rope (pure coordination). Under oligopoly_reading, P5 states are beneficiaries/agenda_setters extracting from non-P5 payers, yielding snare or tangled_rope classifications for payer seats. This divergence IS the measurement — the kernel's contested nature manifests as seat-type disagreement across readings. The engine computes this from the different beneficiary/victim declarations each reading authors.
 *
 * DIRECTIONALITY LOGIC:
 *   All stakeholder seats are beneficiaries or agenda_setters — no payer seats exist under this reading. The P5 states are agenda_setters (they administer the veto) and beneficiaries (they gain security). Non-P5, middle powers, and small states are beneficiaries at different power levels with different exit options, but all gain the collective good of avoided great-power war. The engine's directionality derivation will assign low d-values (near beneficiary end) to all seats because no structural extraction occurs. The international_legal_scholars observer seat gets analytical exit_options and d=0.5 by default.
 *
 * MANDATROPHY ANALYSIS:
 *   The veto's founding problem (ensuring great-power participation in collective security) remains live — great-power war prevention is not a solved problem. The arrangement has not atrophied into piton because its core function is actively exercised (vetoes cast on Syria, Ukraine, Israel-Palestine resolutions) and the structural condition it addresses (nuclear-armed great powers) persists. The coordination reading classifies it as rope because the unanimity gate continues to serve its design purpose: no Security Council resolution has compelled a nuclear state into war it rejects. Mandatrophy is resolved only if great-power war becomes structurally impossible without the veto — a condition not met.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (coordination_reading) of the contested kernel article_27_veto_power. What are the structural consequences of treating the veto as a coordination mechanism versus an extraction mechanism versus a sovereignty instantiation?',
    'Comparative classification of the three sibling readings using the same engine: compute per-seat types for each reading''s beneficiary/victim declarations and metric profiles. The divergence in seat-type outputs maps the kernel''s contestation structure.',
    'If coordination_reading computes rope for all seats while oligopoly_reading computes snare/tangled_rope for payer seats, the kernel''s classification is reading-dependent — confirming that ''the veto'' is not a single constraint but a family of constraints sharing a label. This validates the ε-invariance principle: different readings = different constraints = different ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer structure: this reading''s place in the article_27_veto_power kernel family').

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the veto''s unanimity gate genuinely operate as a pure coordination mechanism (all states benefit, no extraction), or does it covertly extract from non-P5 states by denying them equal participation in collective security decisions?',
    'Empirical analysis of veto use patterns: if vetoes overwhelmingly block actions that would have targeted P5 vital interests (coordination function), the coordination reading holds. If vetoes routinely block humanitarian interventions that pose no great-power war risk (extraction function), the oligopoly reading gains support. Quantitative coding of veto episodes by threat-to-P5-vital-interests vs. humanitarian-protection categories.',
    'If vetoes frequently block non-great-power-war-threatening resolutions, the coordination_reading''s ''no victims'' claim fails — non-P5 states bear costs (unaddressed crises) without consent, making them payers. This would shift the constraint toward tangled_rope or snare under empirical scrutiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the veto''s empirical use pattern matches its coordination design or reveals extraction').

omega_variable(
    great_power_war_counterfactual,
    'Would great-power war have occurred without the veto, or did other factors (nuclear deterrence, economic interdependence, bipolar/ multipolar balance) independently prevent it?',
    'Counterfactual historical analysis: compare Security Council veto episodes with near-miss crises (Cuban Missile Crisis, Able Archer, Kargil) where the veto was not the operative restraint. Assess whether the veto added marginal war-prevention value beyond structural deterrence.',
    'If the veto''s marginal contribution to war prevention is negligible, its coordination function is overstated — the low ε attributed to coordination cost would be misassigned; the constraint might be piton (atrophied function) or snare (extraction persisting after function loss). If marginal contribution is substantial, the coordination reading''s low ε is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(great_power_war_counterfactual, empirical, 'Whether the veto''s war-prevention function is causally effective or epiphenomenal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article_27_veto_power__coordination_reading_tr_t1945, article_27_veto_power__coordination_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_tr_t1955, article_27_veto_power__coordination_reading, theater_ratio, 1955, 0.12).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_tr_t1965, article_27_veto_power__coordination_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_tr_t1975, article_27_veto_power__coordination_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_tr_t1985, article_27_veto_power__coordination_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_tr_t1995, article_27_veto_power__coordination_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_tr_t2005, article_27_veto_power__coordination_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_tr_t2015, article_27_veto_power__coordination_reading, theater_ratio, 2015, 0.16).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_tr_t2024, article_27_veto_power__coordination_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(article_27_veto_power__coordination_reading_be_t1945, article_27_veto_power__coordination_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_be_t1955, article_27_veto_power__coordination_reading, base_extractiveness, 1955, 0.07).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_be_t1965, article_27_veto_power__coordination_reading, base_extractiveness, 1965, 0.06).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_be_t1975, article_27_veto_power__coordination_reading, base_extractiveness, 1975, 0.08).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_be_t1985, article_27_veto_power__coordination_reading, base_extractiveness, 1985, 0.07).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_be_t1995, article_27_veto_power__coordination_reading, base_extractiveness, 1995, 0.09).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_be_t2005, article_27_veto_power__coordination_reading, base_extractiveness, 2005, 0.08).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_be_t2015, article_27_veto_power__coordination_reading, base_extractiveness, 2015, 0.08).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_be_t2024, article_27_veto_power__coordination_reading, base_extractiveness, 2024, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(article_27_veto_power__coordination_reading_su_t1945, article_27_veto_power__coordination_reading, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_su_t1955, article_27_veto_power__coordination_reading, suppression_requirement, 1955, 0.12).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_su_t1965, article_27_veto_power__coordination_reading, suppression_requirement, 1965, 0.1).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_su_t1975, article_27_veto_power__coordination_reading, suppression_requirement, 1975, 0.15).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_su_t1985, article_27_veto_power__coordination_reading, suppression_requirement, 1985, 0.12).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_su_t1995, article_27_veto_power__coordination_reading, suppression_requirement, 1995, 0.1).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_su_t2005, article_27_veto_power__coordination_reading, suppression_requirement, 2005, 0.12).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_su_t2015, article_27_veto_power__coordination_reading, suppression_requirement, 2015, 0.13).
narrative_ontology:measurement(article_27_veto_power__coordination_reading_su_t2024, article_27_veto_power__coordination_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__coordination_reading, 0.1).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, un_chapter_vii_authorization).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, security_council_reform_proposals).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is the coordination_reading of the article_27_veto_power kernel. It decomposes the natural-language concept 'the P5 veto' into a structurally distinct claim: veto as collective-action solution preventing great-power war. The sibling readings oligopoly_reading (veto as oligopoly extraction) and sovereignty_reading (veto as sovereignty instantiation) are separate constraints with their own ε, stakeholders, and classifications. All three are linked via affects_constraints to model the kernel's constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
