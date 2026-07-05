% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Article 2(7) Sovereignty-First Reading: Non-Intervention as Foundational Norm
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-first reading of the Article
 *   2(7)/Chapter VII tension: state sovereignty is treated as the Charter's
 *   foundational commitment, with intervention permissible only via explicit
 *   host-state consent or a Security Council Chapter VII authorization
 *   narrowly construed around threats to international peace and security
 *   arising from inter-state aggression. Under this reading, systematic
 *   domestic atrocity that does not spill across borders or otherwise
 *   threaten international peace does not, by itself, trigger a right or duty
 *   to intervene. The 1994 Rwanda genocide (extraction spike to 0.74) and the
 *   extended Syrian civil war (post-2011 plateau near 0.73-0.78) are read
 *   here as the doctrine successfully holding the non-intervention line
 *   rather than as failures needing correction — that is precisely the
 *   interpretive commitment that distinguishes this reading from the R2P
 *   reading, which is authored as a separate sibling constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.71).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Article 2(7) Sovereignty-First Reading: Non-Intervention as Foundational Norm").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '96f1a3e6-a24f-4847-9f61-acf5b45d9fce').
narrative_ontology:cs_kernel_codification('96f1a3e6-a24f-4847-9f61-acf5b45d9fce', fixed_text).
narrative_ontology:cs_authority_grounding('96f1a3e6-a24f-4847-9f61-acf5b45d9fce', lineage).
narrative_ontology:cs_interpretation_layer_present('96f1a3e6-a24f-4847-9f61-acf5b45d9fce').
narrative_ontology:cs_reading_relation('96f1a3e6-a24f-4847-9f61-acf5b45d9fce', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('96f1a3e6-a24f-4847-9f61-acf5b45d9fce', foundational, sovereignty_precedes_protection_obligation).
narrative_ontology:cs_axiom_status(sovereignty_precedes_protection_obligation, holdable).
narrative_ontology:cs_axiom_grounding('96f1a3e6-a24f-4847-9f61-acf5b45d9fce', sovereignty_precedes_protection_obligation, conventional).
narrative_ontology:cs_axiom('96f1a3e6-a24f-4847-9f61-acf5b45d9fce', foundational, consent_or_council_authorization_exhausts_legitimate_intervention_basis).
narrative_ontology:cs_axiom_status(consent_or_council_authorization_exhausts_legitimate_intervention_basis, holdable).
narrative_ontology:cs_axiom_grounding('96f1a3e6-a24f-4847-9f61-acf5b45d9fce', consent_or_council_authorization_exhausts_legitimate_intervention_basis, conventional).
narrative_ontology:cs_reference_frame('96f1a3e6-a24f-4847-9f61-acf5b45d9fce', westphalian_state_consent_primacy).
narrative_ontology:cs_drift_state('96f1a3e6-a24f-4847-9f61-acf5b45d9fce', post_rwanda_srebrenica_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('96f1a3e6-a24f-4847-9f61-acf5b45d9fce', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_state_blocs).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_security_council_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, domestic_atrocity_populations).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, ethnic_minorities_under_state_violence).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, civilian_populations_in_intrastate_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke Article 2(7) to bar UN scrutiny of internal repression, framing any external concern as illegal interference. Actively lobby within the General Assembly and regional blocs to keep the non-intervention norm maximally strong, since it directly shields their domestic conduct from consequence.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, agenda_setter).

% Champion strict sovereignty as a hard-won post-independence protection against renewed great-power intervention dressed as humanitarianism. Their historical experience of colonial domination gives the norm genuine legitimacy for them even as it is instrumentalized by some member states to shield abuse.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_state_blocs, beneficiary,
    organized, generational, constrained, global).

% Hold veto power over any Chapter VII authorization, meaning intervention only occurs when it serves at least one permanent member's interest or none object. This gives them decisive control over when the sovereignty shield is lifted, independent of atrocity severity.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_security_council_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Face state or state-tolerated violence with no external recourse absent Security Council authorization or state consent, both of which the perpetrating state or its patrons can block. Their only exits are flight, submission, or armed resistance; none involve the international legal order acting on their behalf.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, domestic_atrocity_populations, payer,
    powerless, immediate, trapped, local).

% Experience the sovereignty-first reading as a structural bar to protection: the same norm that stops other states from meddling in their affairs also stops other states from stopping their persecution. Displacement or statelessness are the only individually available exits.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, ethnic_minorities_under_state_violence, payer,
    powerless, biographical, trapped, local).

% Live under the operative rule that intrastate violence, however severe, does not on its own trigger Chapter VII unless framed as a threat to international peace and security. Their fate depends on Council politics unrelated to the violence's severity.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, civilian_populations_in_intrastate_conflict, payer,
    powerless, immediate, trapped, local).

% Document and report on internal atrocities but hold no enforcement power under this reading; their findings inform but cannot trigger action without state consent or a Council vote. Structurally positioned to see the harm without capacity to act on it.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_secretariat_and_special_rapporteurs, excluded,
    moderate, biographical, constrained, global).

% Analyze the tension between Article 2(7) and Chapter VII across cases (Rwanda, Syria, Myanmar) and debate whether the sovereignty-first reading reflects the Charter's original design or a captured interpretation defending impunity.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents powerful states from using humanitarian pretexts to invade weaker states for strategic gain, and protects the postwar order's core bargain that member states will not have their internal governance overridden by external force absent collective, veto-checked authorization.
% TRANSFER_FUNCTION: Moves protective capacity away from populations suffering domestic atrocity and toward the perpetrating or tolerating state's continued unchallenged authority; the 'cost' of preserving universal non-intervention is borne by whichever population's government chooses violence against it.
% ABSENT_VOICES: The populations actually experiencing atrocity have no seat at the Security Council table and no standing to trigger Chapter VII themselves; their suffering enters the record only through diplomatic intermediaries whose home states may have reasons to block action.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first reading were abandoned overnight, post-colonial blocs and authoritarian regimes would lose a legal shield against external interference (a major rearrangement for them), while advocates of stronger intervention would say the world simply started protecting people the Charter's letter had abandoned. Whether the world 'rearranges' or 'corrects' is exactly the site of the kernel dispute.
% FOUNDING_PROBLEM: The UN Charter was drafted to prevent both interstate aggression (the immediate memory of WWII) and great-power domination of weaker states through pretextual intervention (the memory of colonialism and gunboat diplomacy) — Article 2(7) was built to guarantee newly independent and smaller states protection from precisely the kind of external meddling that had defined the prior century.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial state representatives at the UN attest the founding problem (great-power intervention pretexts) remains fully live, citing recent interventions justified on shifting humanitarian grounds. Independent legal historians and the ICISS (International Commission on Intervention and State Sovereignty) report, produced outside any single beneficiary state, attest that the founding problem has partially transformed: the drafters did not anticipate mass atrocity by a state against its own population as a distinct category the Charter's silence would leave unaddressed.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.78 at present) because the doctrine's practical effect, however coordination-legitimate its origin, is to leave populations under state violence with no international legal recourse when their own government is the perpetrator or an enabling patron sits on the Council. Suppression (0.71) reflects the active diplomatic and procedural machinery — veto threats, non-intervention resolutions, sovereignty invocations at the Human Rights Council — that keeps the norm's exclusionary force operative. Theater ratio (0.42) captures the growing gap between rhetorical commitments to human rights monitoring (special rapporteurs, commissions of inquiry) and their actual enforcement capacity, which has grown as a share of UN activity without a corresponding growth in intervention authority.
 *
 * PERSPECTIVAL GAP:
 *   From the permanent members' and authoritarian regimes' seats, the norm reads as principled, stable coordination — sovereignty as bedrock, exactly as claimed. From the trapped, powerless populations' seats, the identical structure reads as an extraction mechanism that converts a norm meant to prevent invasion into a shield for domestic violence. The engine's per-seat computation should surface this divergence without either side's framing overriding the other's.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and post-colonial blocs sit near the beneficiary end: the constraint subsidizes their freedom from external scrutiny (regimes) or protects a hard-won historical guarantee (blocs), even though these are distinct motivations bundled under one norm. Permanent Security Council members are structural agenda-setters whose veto determines when the shield lifts, giving them power independent of the underlying atrocity. Domestic atrocity populations are the clearest targets: trapped, powerless, and structurally excluded from the very process that could authorize their protection — the engine should register these as the constraint's actual burden-bearers, not the notionally 'coordinated' UN member states as a class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power intervention pretexts) remains partly live — the coordination function is real and not merely inertial. But the doctrine's persistence in cases like Rwanda and Syria, where the underlying violence had no meaningful inter-state character, suggests the mandate has been stretched to cover a class of harm (mass domestic atrocity) the founders did not resolve rather than one they resolved and this reading preserves. Classifying this as tangled_rope rather than snare or pure mountain acknowledges both the genuine coordination function (protecting weak states from pretextual invasion) and the asymmetric extraction (protecting perpetrating states from any accountability) operating through the identical structure — a pure snare framing would erase the coordination function that post-colonial blocs genuinely rely on and value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_first_kernel_reading_choice,
    'Is the sovereignty-first reading of Article 2(7)/Chapter VII the Charter''s intended design, or a reading that has hardened into cover for regimes seeking impunity?',
    'Historical analysis of the Charter''s drafting record (San Francisco Conference debates) compared against the actual pattern of Security Council action and inaction across post-1945 mass atrocity cases; corroboration from the ICISS report and comparable independent commissions.',
    'If the drafting record supports strict sovereignty as intended, this reading is the more textually faithful one and the R2P reading is an evolving gloss; if the record shows the drafters did not contemplate intrastate atrocity as a category, this reading''s claim to fidelity weakens and its extraction profile looks more like doctrinal capture by beneficiary states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_first_kernel_reading_choice, conceptual, 'Whether this reading reflects original Charter design or a beneficiary-favorable interpretive drift.').

omega_variable(
    sibling_reading_structural_delta,
    'Where exactly does the r2p_reading''s structure diverge from this reading — in the beneficiary/victim assignment, in the triggering threshold for legitimate action, or in both?',
    'Direct comparison of the two sibling constraint files'' base_properties.beneficiaries/victims and extraction trajectories across the same historical case set (Rwanda, Kosovo, Libya, Syria).',
    'Clarifies whether the kernel dispute is fundamentally about who counts as sovereign-protected versus population-protected, or about procedural triggers for Council action — this shapes what evidence would move real-world doctrine between the readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Where the two kernel readings'' structural claims actually diverge.').

omega_variable(
    veto_capture_vs_genuine_restraint,
    'Does the permanent members'' veto power over Chapter VII authorization function primarily as a genuine restraint against pretextual intervention, or as a mechanism for those members to selectively shield allies from accountability?',
    'Pattern analysis of veto use and threatened use across atrocity cases, cross-referenced against the vetoing member''s bilateral relationship with the state in question.',
    'If veto use correlates strongly with ally protection rather than principled restraint, the coordination story softens and the tangled_rope classification''s extraction component is better evidenced; if veto use shows genuine restraint against unrelated pretextual cases, the coordination function is stronger than the extraction reading credits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_capture_vs_genuine_restraint, empirical, 'Whether Security Council veto practice reflects principled restraint or selective shielding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(arti_tr_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(arti_tr_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(arti_tr_t1994, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1994, 0.35).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(arti_tr_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(arti_be_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(arti_be_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(arti_be_t1994, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1994, 0.74).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2015, 0.73).
narrative_ontology:measurement(arti_be_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(arti_su_t1960, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(arti_su_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(arti_su_t1994, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1994, 0.65).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(arti_su_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).

% DUAL FORMULATION NOTE:
% This story and article_2_7_chapter_vii_tension__r2p_reading are the two declared readings of a single contested kernel (article_2_7_chapter_vii_tension). They are authored as separate constraints per the ε-invariance principle: sovereignty_first_reading measures extraction as the cost borne by populations denied intervention under a strict-sovereignty gate (high ε, tangled_rope); r2p_reading would measure extraction differently, centered on the risks of intervention itself and erosion of the consent principle (its own ε and classification, authored separately). The two share drafting history and institutional machinery but diverge in beneficiary/victim structure and triggering conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
