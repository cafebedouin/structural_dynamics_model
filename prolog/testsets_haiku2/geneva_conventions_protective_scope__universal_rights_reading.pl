% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Protections Universal Scope (Human Rights Floor Reading)
 *   domain: international_humanitarian_law/legal_theory/armed_conflict
 *
 * SUMMARY:
 *   This constraint embodies ONE reading of the contested Geneva Conventions
 *   protective scope: the universal rights reading asserts that humanitarian
 *   protections apply to all persons affected by armed conflict regardless of
 *   combatant status, uniform, command structure, or state recognition. This
 *   reading unifies Common Article 3 (minimum protections in all conflicts)
 *   with the human rights law overlay (universal dignity, non-derogable
 *   rights) to create a single protective floor rather than tiered standards
 *   based on conflict type or actor classification. The reading emerged
 *   post-1977 Protocols and crystallized in post-2001 responses to indefinite
 *   detention and interrogation practices. It benefits non-state actors,
 *   detainees, and civilian populations; it restricts state military
 *   operational flexibility by foreclosing the 'unprivileged belligerent'
 *   classification that historically placed certain captives outside Geneva
 *   scope. The constraint is claimed as tangled_rope because it solves a
 *   genuine humanitarian coordination problem (constraining violence when
 *   reciprocity breaks down) while simultaneously extracting operational
 *   restrictions from state militaries that view such restrictions as
 *   competitive disadvantage in asymmetric conflict.
 *
 * KEY AGENTS:
 *   - state_militaries — institutional power, subject to operational restriction, agenda-setting role in treaty interpretation
 *   - non_state_armed_groups — organized power, structured benefit from protection parity, constrained exit
 *   - civilian_populations — powerless, beneficiary of expanded non-combatant immunity, trapped by conflict
 *   - detainees_all_status — powerless, primary beneficiary of universal floor (torture prohibition, trial rights), trapped
 *   - international_humanitarian_law_community — institutional, observer/beneficiary, reinforces and interprets this reading
 *   - state_centric_reading_advocates — excluded institutional voice, would argue universalism destroys reciprocity incentive
 *   - hybrid_proportionality_advocates — excluded institutional voice, would argue context-dependent standards are more coherent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.42).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Protections Universal Scope (Human Rights Floor Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, 'df993ce7-e419-44f3-aed9-a546484704be').
narrative_ontology:cs_kernel_codification('df993ce7-e419-44f3-aed9-a546484704be', fixed_text).
narrative_ontology:cs_authority_grounding('df993ce7-e419-44f3-aed9-a546484704be', lineage).
narrative_ontology:cs_interpretation_layer_present('df993ce7-e419-44f3-aed9-a546484704be').
narrative_ontology:cs_reading_relation('df993ce7-e419-44f3-aed9-a546484704be', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('df993ce7-e419-44f3-aed9-a546484704be', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('df993ce7-e419-44f3-aed9-a546484704be', foundational, universal_human_dignity_overrides_status).
narrative_ontology:cs_axiom_status(universal_human_dignity_overrides_status, holdable).
narrative_ontology:cs_axiom_grounding('df993ce7-e419-44f3-aed9-a546484704be', universal_human_dignity_overrides_status, deontological).
narrative_ontology:cs_axiom('df993ce7-e419-44f3-aed9-a546484704be', foundational, common_article_3_customary_binding_all_parties).
narrative_ontology:cs_axiom_status(common_article_3_customary_binding_all_parties, holdable).
narrative_ontology:cs_axiom_grounding('df993ce7-e419-44f3-aed9-a546484704be', common_article_3_customary_binding_all_parties, conventional).
narrative_ontology:cs_reference_frame('df993ce7-e419-44f3-aed9-a546484704be', universal_humanitarian_floor_1977_protocols_forward).
narrative_ontology:cs_drift_state('df993ce7-e419-44f3-aed9-a546484704be', contemporary_post_2001_detention_crisis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('df993ce7-e419-44f3-aed9-a546484704be', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detainees_all_status).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, wounded_sick_shipwrecked).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_flexibility).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_asymmetric_warfare_capability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, international_humanitarian_law_community).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_strategists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, state militaries bear the heaviest compliance burden: they must extend protections (shelter, medical care, humane treatment, trial fairness) to all captured persons regardless of uniform status, legitimate authority claim, or combatant privilege recognition. The reading restricts targeting of unprivileged belligerents, detainees without trial, and wounded combatants. States experience this as a restriction on asymmetric warfare capability and operational flexibility. Yet states also set the formal agenda through treaty interpretation and enforcement structures they dominate in international law-making.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_militaries, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, state_militaries, agenda_setter).

% Benefit from the reading because it classifies them as bound by the same humanitarian floor that applies to state forces, and grants their members protection if captured rather than treating them as common criminals or unlawful combatants. The reading dissolves the distinction between privileged and unprivileged belligerency in favor of a universal protective floor. Non-state groups' capacity to mount resistance is structurally enhanced by the claim that they are entitled to the same restraint Geneva requires of states.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    organized, generational, constrained, global).

% Benefit from the expanded reading because it establishes civilians as subjects of protection regardless of state military assertions that civilian casualties are lawful collateral damage. The reading reinforces non-combatant immunity by tying it to universal human rights law, not to state discretion. Civilians caught in asymmetric conflicts (where state forces claim the other side wears no uniform and thus bears no protection) gain arguable legal standing to challenge targeting decisions that this reading's framework would curtail.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, biographical, trapped, global).

% Persons detained during armed conflict who would not qualify for prisoner-of-war status under traditional Geneva reading (captured insurgents, unprivileged belligerents, suspected facilitators) receive protections under this reading: prohibition on torture, right to information about charges, access to medical care. The reading forecloses the 'unlawful combatant' classification that states used to deny Geneva protections in post-2001 detention scenarios.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detainees_all_status, beneficiary,
    powerless, immediate, trapped, global).

% Legal scholars, treaty monitors (ICRC), and human rights bodies benefit from and reinforce this reading because it provides a coherent, universal framework for evaluating armed conflict conduct. The reading's appeal to common humanity and irreducible dignity over bifurcated status categories aligns with the professional consensus that emerged post-Cold War and strengthened post-2001 detention crises. This community interprets, applies, and defends the reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_humanitarian_law_community, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, international_humanitarian_law_community, observer).

% Experience the reading as operationally restrictive: they cannot treat certain classes of captured persons as beyond protection, cannot employ indefinite detention without trial, cannot employ certain interrogation techniques. The reading eliminates a category of persons (unprivileged belligerents) that states had treated as falling outside Geneva scope. Strategists in asymmetric contexts experience this as a loss of operational freedom because the reading extends protections to adversaries they had previously classified as not entitled to them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_strategists, payer,
    institutional, biographical, constrained, global).

% Legal theorists and state governments that hold the competing state-centric reading are excluded from the inner logic of this constraint: they would argue that universal extension eliminates the incentive structure that traditionally bound states to comply (reciprocity among uniformed forces) and creates obligations to non-state actors that states did not bargain for. Their voice is present in counter-interpretations and state practice divergence but is structurally subordinated by this reading's claim to have grounded protection in universal human rights rather than state consent.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_centric_reading_advocates, excluded,
    institutional, generational, constrained, global).

% Advocates for calibrated, context-dependent application (high standards for international armed conflict, lower for internal conflicts) are excluded from this reading's categorical rejection of status-based gradation. This reading treats the scale as irrelevant to the minimum floor, while the hybrid reading argues that conflict character should determine the package of protections. Their technical expertise on proportionality and operational constraint is present but subordinated.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, hybrid_proportionality_advocates, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__universal_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform humanitarian floor (prohibition on torture, medical care for wounded, humane treatment of detainees, protection of civilians) that applies to all armed conflict participants regardless of state recognition, uniform status, or combatant privilege classification. Solves the coordination problem of how to constrain violence in asymmetric conflicts where traditional reciprocity between uniformed forces breaks down.
% TRANSFER_FUNCTION: Transfers protections (legal standing, immunity from certain targeting, entitlement to care, procedural fairness in detention) from a limited set of recognized combatants (uniformed state forces meeting Article 4 criteria) to a universal set including non-state fighters, insurgents, detainees without formal status, and all civilians. The reading restricts state operational flexibility (targeting, interrogation, detention scope) and transfers legal standing to non-state actors and powerless detainees.
% ABSENT_VOICES: Military commanders executing asymmetric operations who would argue that universal protection of all combatants removes the legal distinction that incentivizes compliance; states that practice detention without trial on the grounds that some captives are not prisoners of war; scholars of strategic deterrence who would argue that humanitarianism without status distinction eliminates reciprocal incentives. These voices are excluded from the consensus that this reading mobilizes.
% DISAPPEARANCE_RATIONALE: If this reading's protections disappeared, state militaries would revert to full discretion in detaining, interrogating, and targeting unprivileged combatants and civilians in asymmetric conflicts. Post-2001 detention practices (indefinite holds without trial, enhanced interrogation) would have no legal barrier. Non-state actors would lose arguable protection status. The humanitarian framework itself would fragment back into bilateral/reciprocal treaties binding only consenting parties. The reorganization would manifest as resumed mass detention, revived interrogation practices, and expanded targeting of civilian populations without legal constraint.
% FOUNDING_PROBLEM: Armed conflicts involving non-state actors, irregular forces, and asymmetric tactics created ambiguity in traditional Geneva protections: who qualifies as a prisoner of war, what protections apply to those who don't, how to constrain violence when one side lacks uniforms or unified command. The 1977 Protocols and the human rights law overlay were developed to address this: establishing that humanitarian protection does not depend on status but on humanity itself.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC (Customary IHL study), international courts (ICJ, ICC), human rights bodies (UN Human Rights Council), and a broad consensus of legal scholars outside state military establishments attest that the founding problem persists: asymmetric conflicts continue to generate disputes over detainee status and protection scope. States engaging in counterinsurgency, and a minority scholarly tradition emphasizing state sovereignty in war classification, would argue the problem has been 'solved' by privilege-based frameworks, but corroboration from the monitoring and judicial community supporting this reading overwhelmingly supports the live status.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that state operational flexibility is substantially constrained by the prohibition on indefinite detention, torture, targeting unprivileged combatants, and denying detainees trial or legal status. The reading raises the cost of asymmetric warfare for state actors. Yet extractiveness is not at the snare ceiling because the constraint solves a real problem (humanitarian restraint in asymmetric conflict) that benefits even state signatories by establishing reciprocal expectations and reducing escalation pressure. Suppression is moderate (0.42) because the reading is enforced by treaty bodies, courts, and monitoring organizations, but enforcement is incomplete: states regularly violate detention standards, interrogation rules, and targeting prohibitions, and the suppressive machinery (treaties, ICC, human rights courts) faces state non-cooperation and forum-shopping. Theater ratio (0.31) indicates some performative activity: states conduct compliance reviews, issue military manuals citing Geneva protections, while simultaneously maintaining detention facilities and practices that violate the reading's standards. Accessibility collapse (0.78) is high because once the reading is articulated, the alternative (unlimited state discretion in detention and targeting) becomes structurally indefensible in public discourse, even where states violate it in practice — the reading has become the recognized baseline. Resistance (0.71) is substantial: military establishments actively resist, states file reservations, and scholars advance competing readings; the constraint persists despite organized opposition. The measurement trajectory shows extractiveness rising early (2001-2015, post-Abu Ghraib, post-rendition crisis) as the reading gained coherence and institutional traction, then stabilizing as it crystallized into customary law consensus. Theater ratio rises slightly as states invest in compliance facades.
 *
 * PERSPECTIVAL GAP:
 *   From state seats (military, diplomatic, strategic), the reading appears as an imposed floor that states did not bargain for in the original Geneva framework. States view themselves as the rule-makers and see courts applying the reading as judicial usurpation. From non-state seats, the reading appears as recognition: it says non-state combatants are legal subjects entitled to protections, not outlaws. From the powerless detained seat, the reading is the only barrier against state torture. The gap is not resolvable by choosing a 'correct' perspective — it is structural. The engine's per-seat computation of effective extraction captures this divergence; it is not a side effect but the main measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries are the primary targets (high d, near 1.0): they bear the operational restrictions, face enforcement via international courts, and lose discretionary authority over detention and targeting. The reading explicitly narrows their freedom of action. Non-state armed groups are the primary beneficiaries (low d, near 0.0): the reading grants them protection parity, makes their captures subject to legal trial rather than summary detention, and prevents their characterization as outside the law. Civilian populations and detainees are beneficiaries (low d): the reading protects them from targeting and torture. The international humanitarian law community (scholars, ICRC, courts) are beneficiaries in a secondary sense (low d): the reading aligns with their professional consensus and provides them institutional role as interpreters and monitors. State military strategists specifically experience high directionality (near 1.0) because the reading restricts asymmetric tactics that depend on detainee exploitation and unrestricted targeting. The reading derives beneficiaries via the explicit protective scope (non-state groups, detainees, civilians) and victims via the operational restrictions (state flexibility). The derivation is strong and unambiguous: no power-level modulation is needed; the structural data produce the directionality cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT exhibit mandatrophy (constraint whose founding problem has been solved but which persists). The founding problem is live: asymmetric conflicts continue to generate ambiguity about who qualifies for what protections, and state practice continues to diverge from the reading's standards (indefinite detention, enhanced interrogation, targeting practices). The reading is not a zombie constraint maintained for theatrical reasons; it addresses an ongoing governance failure. The mismatch between stated commitment and actual practice (states adopt the reading in treaty and doctrine but violate it operationally) is evidence of suppression and resistance, not evidence that the founding problem is dead. Theater ratio indicates some performative compliance activity, but this is performance around an active constraint, not performance by a constraint with no function. The reading remains contested at the highest levels of state practice, which confirms that the founding problem persists: how to constrain violence in conflicts where traditional reciprocity and uniform-based privilege structures don't map onto asymmetric reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_reciprocal_incentive,
    'Does extending protections to unprivileged belligerents remove the reciprocity incentive that traditionally motivated state compliance with Geneva standards, thereby making the constraint self-undermining?',
    'Empirical: comparative analysis of state compliance rates before and after universal-scope legal doctrine became standard, controlling for enforcement capacity and signatory power. Structural: game-theoretic analysis of compliance incentives under status-dependent vs. universal frameworks.',
    'If universal scope destroys reciprocity incentives, the reading produces compliance collapse over time (suppression rises, enforcement hardening occurs). If reciprocity incentives persist despite universalism (states comply because they fear reputational/legal cost and domestic legitimacy concerns, not because of status-based bargaining), the reading represents a stable equilibrium with ongoing enforcement. The classification hinges on whether suppression increases or stabilizes as the reading matures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_reciprocal_incentive, empirical, 'Whether universal protection scope undermines state compliance incentives').

omega_variable(
    status_ambiguity_enforcement,
    'In practice, how do courts and enforcement bodies determine whether a person falls under the universal floor when their status is contested (part-time combatant, forced recruitment, identity fusion with group)? Is the universal scope meaningful or does status-determination collapse into the same status-based inquiry the reading claims to transcend?',
    'Doctrinal analysis of ICC and international court rulings on detention status; empirical review of detention review procedures and how status determinations actually occur when universal scope is the legal standard.',
    'If enforcement practice re-creates de facto status categories despite the reading''s universalist framing, extractiveness is lower (less operational restriction) because states retain practical discretion through status-determination disputes. If enforcement consistently applies universal protections regardless of status ambiguity, extractiveness stays high. This affects classification stability: a reading that forecloses status in law but allows it in practice generates oscillating theater ratio as performative compliance masks practical status-based discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_ambiguity_enforcement, empirical, 'Whether universal scope enforcement practice re-creates implicit status categories').

omega_variable(
    non_state_asymmetric_advantage,
    'Does this reading''s extension of protections to non-state actors provide them with an asymmetric legal advantage by granting their combatants protection parity while states retain command responsibility and targeting restrictions that non-state groups can evade?',
    'Strategic analysis of how non-state groups exploit the reading to gain legitimacy and legal status (ability to claim combatant privilege, protection if captured) while their lack of centralized command and formal law-of-war training allows them to evade the discipline and targeting restrictions that bind state forces. Empirical: frequency of International Court findings that non-state group operations violate the reading despite their nominal protection entitlements.',
    'If non-state groups systematically exploit the reading while evading its discipline, the reading may be re-classified as snare-like toward states (asymmetric restriction) rather than tangled-rope (symmetric coordination). If non-state groups face enforcement pressure parallel to states (ICC prosecutes both, targeting restrictions apply to both in practice), the tangled-rope classification stands. The omega identifies whether the reading''s beneficiary structure is durable or whether it will be contested as strategically asymmetric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_state_asymmetric_advantage, conceptual, 'Whether the reading''s protection parity creates asymmetric advantage favoring non-state actors').

omega_variable(
    kernel_reading_codification_status,
    'Has this reading (universal scope) achieved customary international law status independent of treaty text, or does it remain a contested interpretation of treaty language advanced by some courts and scholars against state resistance?',
    'Doctrinal analysis: review state practice, treaty reservations, military manuals, and court findings to assess whether universal scope is practiced as binding law or treated as aspirational/controversial interpretation. Analysis of state opinio juris statements regarding universal protection scope.',
    'If the reading has achieved customary law status, its legitimacy is high and suppression may decrease over time as the reading becomes the recognized baseline. If the reading remains contested interpretation, suppression stays high because states can claim they are not bound by non-consensual judicial expansion. Classification may shift from tangled_rope (if customary law) to snare (if reading is enforced despite state non-consent). This is the framing determination most dependent on committer judgment: does the reading describe what law IS or what law SHOULD BE.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_codification_status, conceptual, 'Whether universal scope has achieved customary law status or remains contested interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gene_tr_t5, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(gene_tr_t25, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(gene_tr_t35, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 35, 0.31).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gene_be_t5, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(gene_be_t25, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(gene_be_t35, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gene_su_t5, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(gene_su_t25, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(gene_su_t35, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 35, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__universal_rights_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Geneva Conventions protective scope kernel. The state-centric reading restricts protections to uniformed combatants; the hybrid-proportionality reading scales protections by conflict type; the universal-rights reading (this file) extends protections to all persons regardless of status. The three readings have different beneficiary structures, different ε values, and different classification outcomes. They are linked as a constraint family; each should be read as a separate constraint story with its own metrics and stakeholders, not as a measurement-basis disagreement about one constraint. The network edges indicate which readings affect which: universal-rights influences both siblings by raising the legitimacy cost of status-based exclusions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
