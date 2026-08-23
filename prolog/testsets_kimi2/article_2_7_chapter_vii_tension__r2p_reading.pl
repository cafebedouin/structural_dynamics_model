% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P) Reading of the Article 2(7)/Chapter VII Tension
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates the Responsibility to Protect (R2P) reading
 *   of the Article 2(7)/Chapter VII tension in the UN Charter. Under this
 *   reading, sovereignty is conditional on a state's protection of its
 *   population, and systematic atrocities trigger an international
 *   responsibility to intervene, authorized through Chapter VII. The sibling
 *   sovereignty_first_reading treats sovereignty as unconditional and
 *   intervention as limited to interstate aggression. This reading authors
 *   high extractiveness because it legitimizes coercive override of state
 *   territorial autonomy. Key agents include persecuted populations
 *   (beneficiaries of protection), targeted states (bearers of sovereignty
 *   loss), the UN Security Council (enforcement gatekeeper), and humanitarian
 *   advocacy networks.
 *
 * KEY AGENTS:
 *   - persecuted_populations: Primary beneficiary (powerless/trapped) â receives protection claims but no agency over intervention
 *   - targeted_states: Primary target (institutional/constrained) â bears sovereignty extraction via intervention
 *   - un_security_council: Agenda setter (institutional/constrained) â adjudicates and enforces the intervention mandate
 *   - humanitarian_civil_society: Secondary beneficiary (organized/mobile) â gains legitimacy and access when norm is invoked
 *   - global_south_sovereignty_advocates: Excluded voice (organized/constrained) â objects to R2P as neo-colonial sovereignty override
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.82).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.78).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P) Reading of the Article 2(7)/Chapter VII Tension").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, 'b1466b8b-34f0-47b2-8bb7-0a06bd3a6796').
narrative_ontology:cs_kernel_codification('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796', formalized).
narrative_ontology:cs_authority_grounding('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796', lineage).
narrative_ontology:cs_interpretation_layer_present('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796').
narrative_ontology:cs_reading_relation('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796', foundational, sovereignty_conditional_on_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_protection, holdable).
narrative_ontology:cs_axiom_grounding('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796', sovereignty_conditional_on_protection, conventional).
narrative_ontology:cs_axiom('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796', foundational, systematic_atrocity_triggers_intervention_duty).
narrative_ontology:cs_axiom_status(systematic_atrocity_triggers_intervention_duty, holdable).
narrative_ontology:cs_axiom_grounding('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796', systematic_atrocity_triggers_intervention_duty, conventional).
narrative_ontology:cs_reference_frame('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796', conditional_sovereignty_legitimacy).
narrative_ontology:cs_drift_state('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796', post_libya_intervention_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b1466b8b-34f0-47b2-8bb7-0a06bd3a6796', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_civil_society).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Civilian populations at risk of or undergoing systematic atrocities within a state. They are the nominal beneficiaries of external protection under R2P but exercise no control over intervention decisions, timing, or form. Their physical exit is often blocked by the violence itself, and their voice is mediated by humanitarian advocates.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, national).

% Sovereign states whose internal conduct triggers R2P claims. They bear the extraction in the form of military intervention, coercive sanctions, or loss of territorial autonomy when the international community activates the norm. Their sovereignty claim under Article 2(7) is structurally overridden by Chapter VII enforcement.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_states, payer,
    institutional, generational, constrained, national).

% The Council is the formal enforcement gate for coercive measures under the UN Charter. It adjudicates whether an internal atrocity constitutes a threat to international peace justifying intervention, but its decision-making is constrained by permanent-member vetoes and geopolitical rivalries.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% NGOs and advocacy networks that document atrocities, mobilize public pressure, and lobby for R2P activation. They gain operational access, funding, and normative legitimacy when the responsibility to protect is invoked, though they do not control the military or institutional enforcement apparatus.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_civil_society, beneficiary,
    organized, biographical, mobile, global).

% A coalition of states and movements, particularly from the post-colonial Global South, that view R2P as a neo-colonial override of hard-won sovereignty. They are present in UN General Assembly debates but structurally excluded from Security Council decision-making on intervention, and their objections are overridden by Chapter VII activation.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, global_south_sovereignty_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating collective international action to halt systematic atrocities when a state fails to protect its own population, overcoming the collective-action problem of unilateral intervention and free-riding on security provision.
% TRANSFER_FUNCTION: Transfers the authority to use coercive force across borders from the targeted state to an international or regional coalition under Security Council mandate, and transfers the cost of protection from the persecuted population to intervening actors and the targeted state.
% ABSENT_VOICES: Targeted-state populations who do not consent to foreign military action; General Assembly majorities and non-permanent Security Council members opposing intervention but lacking veto power; anti-imperialist movements in the Global South who see R2P as sovereignty erosion by former colonial powers.
% DISAPPEARANCE_RATIONALE: If the R2P norm vanished, states facing internal crises would face no credible institutional threat of external intervention over sovereignty objections; atrocities would proceed without the Charter-based brake of Security Council deliberation; the post-2005 international architecture would revert toward strict Westphalian non-interference, and humanitarian NGOs would lose their primary legitimacy mechanism for cross-border protection claims.
% FOUNDING_PROBLEM: The Holocaust and subsequent genocides revealed that unconditional state sovereignty shields perpetrators of mass atrocities from external accountability, creating a coordination failure where no institutional actor was authorized to stop crimes against humanity within borders.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian advocacy networks and the UN Secretary-General's office attest the problem remains live, citing ongoing atrocities. Permanent Security Council members and targeted states attest the founding problem has been weaponized to justify geopolitical intervention; independent historians corroborate the original problem (atrocities under sovereignty) but dispute whether R2P is the appropriate solution. No single external authority corroborates both the problem and this specific arrangement simultaneously.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the norm authorizes military intervention, sanctions, and territorial override â severe extraction from targeted-state sovereignty. Suppression (0.78) reflects the active suppression of Article 2(7) non-intervention claims when Chapter VII is invoked. Theater ratio (0.45) captures the performative diplomacy at the UN (debates, resolutions, panel reports) that often substitutes for effective protection, particularly visible in the Syria case where theater exceeded action. Accessibility collapse (0.60) because alternatives like strict non-intervention are delegitimized but not eliminated (the sovereignty-first reading persists). Resistance (0.75) from targeted states and Global South coalitions that contest the norm's legitimacy. The measurement series trace the post-2005 ratchet: initial modest extraction rising through the Libya intervention, a partial pullback after legitimacy crisis, then renewed extraction as great-power competition repurposes the norm.
 *
 * PERSPECTIVAL GAP:
 *   The persecuted-population seat experiences the constraint as potential salvation or catastrophic risk (protection versus collateral damage), yielding a bifurcated perception. The targeted-state seat experiences pure extraction of sovereignty. The Security Council seat experiences a legal-political tool whose utility varies with permanent-member interests. The sovereignty-first reading would compute this constraint as a snare or severe tangled_rope from the targeted-state seat, while the beneficiary seat might compute a scaffold or rope. The engine should produce divergent per-seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations have low directionality (near-beneficiary) because the constraint is structurally designed to deliver protection to them, though their lack of agency and exit options prevents full subsidy. Targeted states have high directionality (near-target) because the constraint extracts sovereignty, territorial integrity, and political autonomy from them. The UN Security Council sits near symmetric because it both gains enforcement authority and bears political cost. Humanitarian civil society has low directionality because it collects legitimacy and operational access without paying the enforcement cost.
 *
 * MANDATROPHY ANALYSIS:
 *   R2P was built to solve the coordination failure of stopping atrocities (founding problem: Holocaust, Rwanda). It still coordinates some genuine protection (Libya 2011 initially halted mass violence), but the Libya aftermath (regime change, state collapse) and Syria non-intervention (despite atrocities) show the mandate has atrophied: it now serves as a selective legitimacy wrapper for geopolitical intervention. The coordination function is genuine but increasingly captured by great-power agendas, preventing mislabeling as pure extraction (snare) while documenting the hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_cover_vs_genuine_protection,
    'Is R2P invoked primarily in response to genuine protection needs, or does it function as a legitimacy wrapper for geopolitical regime change?',
    'Comparative case analysis of R2P invocations versus non-invocations with similar atrocity profiles, controlling for strategic interest of permanent Security Council members.',
    'If primarily cover, the coordination function is hollow and the constraint shifts toward snare; if genuine, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_cover_vs_genuine_protection, empirical, 'Whether protection claims track atrocity severity or geopolitical interest.').

omega_variable(
    sovereignty_conditionality_constructed,
    'Is the conditional sovereignty norm an emergent feature of international order or a deliberately constructed intervention mandate?',
    'Genealogical analysis of UN Charter drafting history, post-Cold-War doctrinal evolution, and state practice to determine whether the conditionality was invented or discovered.',
    'If purely constructed by powerful states, the constraint is a tangled_rope or snare; if it reflects an emergent convergence, it moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_conditionality_constructed, conceptual, 'Naturalness of the sovereignty-protection bargain.').

omega_variable(
    r2p_sibling_reading_boundary,
    'Does the R2P reading foreclose the sovereignty-first reading within a single legal framework, or can both coexist as live interpretive positions?',
    'Jurisprudential analysis of whether any single legal order can simultaneously hold that sovereignty is unconditional and that it is conditional on protection.',
    'If foreclosed, the kernel is in zero-sum contest; if coexistent, the tension is managed through political negotiation rather than logical contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(r2p_sibling_reading_boundary, conceptual, 'Logical relationship between R2P and sovereignty-first readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art27_r2p_tr_t0, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(art27_r2p_tr_t5, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(art27_r2p_tr_t10, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(art27_r2p_tr_t15, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(art27_r2p_tr_t20, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(art27_r2p_tr_t25, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(art27_r2p_be_t0, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(art27_r2p_be_t5, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(art27_r2p_be_t10, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(art27_r2p_be_t15, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(art27_r2p_be_t20, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(art27_r2p_be_t25, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(art27_r2p_su_t0, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(art27_r2p_su_t5, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(art27_r2p_su_t10, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(art27_r2p_su_t15, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(art27_r2p_su_t20, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(art27_r2p_su_t25, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% This constraint and sovereignty_first_reading are dual formulations of the same Charter tension. The r2p_reading extracts from targeted states by making sovereignty conditional on protection; the sovereignty_first_reading extracts from persecuted populations by shielding perpetrators behind sovereignty walls. They share the same kernel but instantiate different epsilon values, beneficiary/victim structures, and directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
