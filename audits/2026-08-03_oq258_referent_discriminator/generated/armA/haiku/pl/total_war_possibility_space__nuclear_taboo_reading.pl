% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Total War Normative Prohibition (Nuclear Taboo Reading)
 *   domain: geopolitical/institutional/normative
 *
 * SUMMARY:
 *   Under this reading, total war became normatively prohibited through the
 *   construction and maintenance of a taboo, independent of—and potentially
 *   despite—material capability to wage it. The constraint is maintained
 *   through liberal democratic institutional agendas (NPT, IAEA, declaratory
 *   policy cycles, diplomatic enforcement) that declare total war unthinkable
 *   and sanction any public argument to the contrary. The reading predicts:
 *   if norm entrepreneurs (major powers) exit the constraint's maintenance,
 *   the taboo weakens rapidly; if non-nuclear powers gain nuclear capability,
 *   they face a different constraint structure (deterrence rather than
 *   taboo). The kernel contest frames this as ONE reading of a contested
 *   terrain—total war's possibility space—where deterrence equilibrium and
 *   space contraction offer structurally different accounts of why total war
 *   does not occur.
 *
 * KEY AGENTS:
 *   - Liberal democratic security establishments: set and enforce the taboo through institutional and diplomatic machinery
 *   - Non-proliferation regime institutions (IAEA, NPT framework): institutionalize and codify the taboo
 *   - Nuclear-weapon states: bound by and benefit from the taboo indefinitely
 *   - Non-nuclear states: constrained by non-proliferation limits, exposed to proxy violence, excluded from nuclear deterrence
 *   - Asymmetrically vulnerable populations: redirected away from total war but into sub-total conflict without exit
 *   - Strategic theorists and norm entrepreneurs who dissent: structurally excluded from norm-setting discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.38).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.62).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Total War Normative Prohibition (Nuclear Taboo Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "geopolitical/institutional/normative").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, 'c2663f23-589d-4bb6-a5d0-3aa8c049734e').
narrative_ontology:cs_kernel_codification('c2663f23-589d-4bb6-a5d0-3aa8c049734e', formalized).
narrative_ontology:cs_authority_grounding('c2663f23-589d-4bb6-a5d0-3aa8c049734e', extraction).
narrative_ontology:cs_interpretation_layer_present('c2663f23-589d-4bb6-a5d0-3aa8c049734e').
narrative_ontology:cs_reading_relation('c2663f23-589d-4bb6-a5d0-3aa8c049734e', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2663f23-589d-4bb6-a5d0-3aa8c049734e', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('c2663f23-589d-4bb6-a5d0-3aa8c049734e', foundational, taboo_independence_from_capability).
narrative_ontology:cs_axiom_status(taboo_independence_from_capability, holdable).
narrative_ontology:cs_axiom_grounding('c2663f23-589d-4bb6-a5d0-3aa8c049734e', taboo_independence_from_capability, deontological).
narrative_ontology:cs_axiom('c2663f23-589d-4bb6-a5d0-3aa8c049734e', foundational, norm_maintenance_as_constitutive).
narrative_ontology:cs_axiom_status(norm_maintenance_as_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('c2663f23-589d-4bb6-a5d0-3aa8c049734e', norm_maintenance_as_constitutive, conventional).
narrative_ontology:cs_reference_frame('c2663f23-589d-4bb6-a5d0-3aa8c049734e', post_1968_non_proliferation_consensus).
narrative_ontology:cs_drift_state('c2663f23-589d-4bb6-a5d0-3aa8c049734e', contemporary_strategic_flexibility_rhetoric, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('c2663f23-589d-4bb6-a5d0-3aa8c049734e', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_institutions).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, states_excluded_from_nuclear_capability).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, asymmetrically_vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, taboo_as_constitutive_force).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, norm_independence_from_material_capability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulate and enforce the no-total-war norm through diplomatic channels, multilateral institutions, and selective interventionism. They maintain the taboo through rhetorical reinforcement, sanctions against norm violators, and alliance management. They benefit from a world where great-power war remains constrained by shared normative commitment rather than capability parity alone.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, liberal_democratic_security_establishments, agenda_setter,
    institutional, generational, arbitrage, global).

% The IAEA, NPT framework, UN security architecture, and associated treaty bodies derive their legitimacy and operational scope from the norm that nuclear weapons must remain restricted. They administer the institutional machinery that codifies the taboo, conduct inspections, and coordinate enforcement. Their institutional survival and budget depend on the taboo remaining normatively binding.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_institutions, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_institutions, agenda_setter).

% Possess the material capability for total war but are normatively bound—and bind themselves through declaratory policy—never to employ nuclear weapons except in existential defense. This taboo locks in their relative strategic advantage: they can sustain the prohibition indefinitely because their superior conventional capabilities make total war unnecessary. Non-use is structurally costless to them.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, beneficiary,
    institutional, generational, trapped, global).

% Accept non-proliferation constraints on their own nuclear development in exchange for security assurances from nuclear weapon states that the taboo will hold. They bear the cost of living under a norm they did not author and cannot unilaterally break without severe sanctions. If the taboo weakens, their relative vulnerability increases without remedy.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, states_excluded_from_nuclear_capability, payer,
    moderate, biographical, constrained, national).

% Inhabit regions where nuclear-armed powers conduct proxy wars, conventional campaigns, or economic coercion under the protection of the total-war taboo. They are shielded from total war but exposed to the full range of sub-total violence. The taboo redirects great-power competition toward their territories and into their conflicts without granting them nuclear deterrence or meaningful exit.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, asymmetrically_vulnerable_populations, payer,
    powerless, biographical, trapped, local).

% Strategic theorists, military planners, and political actors who contest the taboo's binding force or argue for strategic flexibility in extreme scenarios. They are structurally excluded from norm-setting conversations because their participation would delegitimize the taboo; the taboo's enforcement machinery treats their arguments as dangerous precisely because they contain logical force.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs_and_declaratory_dissenters, excluded,
    moderate, biographical, constrained, global).

% Documents, interprets, and contests whether the prohibition on total war is genuinely independent of material capability (the taboo reading) or whether mutual vulnerability remains the true constraint (deterrence reading) or whether nuclear weapons removed total war from strategic possibility altogether (space-contraction reading). Observations feed back into policy arguments but do not directly set the constraint.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, international_relations_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, liberal_democratic_security_establishments).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a shared normative frame that channels great-power strategic competition away from total war and into proxy conflict, diplomatic maneuvering, and sub-existential competitions. The coordination problem solved: how to keep the greatest powers from destroying civilization when they all possess the material means and have powerful incentives to do so—answered through taboo rather than capability limitation.
% TRANSFER_FUNCTION: Moves strategic restraint from nuclear-capable states (they pledge non-use) to institutionalized norm-enforcement (NPT, IAEA, declaratory policy cycles) and extracts compliance burden from non-nuclear states (they accept non-proliferation limits without nuclear deterrent). The transfer is abstract but real: strategic autonomy in exchange for participation in the norm-architecture.
% ABSENT_VOICES: Strategic theorists who contest the taboo's independence from deterrence; military planners in non-aligned states who would pursue nuclear capability to escape the asymmetry; populations in proxy-war zones who would argue for escalation options or non-involvement rather than sub-total violence without exit. These voices are systematically excluded because their presence would rhetorically weaken the taboo.
% DISAPPEARANCE_RATIONALE: The constraint's supporters argue that if the taboo disappeared overnight, great-power war would resume with full ferocity (world rearranges into total war). The skeptics argue that mutual vulnerability would persist and deter total war anyway, so the taboo is mere theatre (world unchanged). Still others argue the strategic landscape changed so completely that total war became materially impossible regardless (world rearranges around space contraction, not taboo).
% FOUNDING_PROBLEM: After 1945, great powers possessed the material means to annihilate civilization. Strategic doctrine initially incorporated nuclear weapons as war-fighting tools. The founding problem: how to prevent existential miscalculation when the stakes are absolute and the capability is distributed.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear strategists and security establishment insiders attest the founding problem remains live—deterrence is fragile, stability is contingent, the taboo must be actively maintained. Skeptical scholars and some military theorists attest the problem has been partly or entirely displaced by material factors (mutual vulnerability, military-technical constraints, space contraction). International institutions attest the taboo has become institutionalized and self-reinforcing; outside observers note that institutional entrenchment can outlast functional necessity.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.38 at interval end) because the taboo primarily forecloses a possibility rather than actively transferring resources—the extraction is abstract (strategic autonomy traded for institutional participation). Suppression is substantial (0.62) because the constraint's persistence depends on actively excluding and delegitimizing contrary arguments; any strategic theorist who argues for flexibility in extreme scenarios is treated as dangerous precisely because their logic contains force. Theater is moderate (0.41): the institutional machinery is real, but an increasing share of enforcement activity is rhetorical—repeatedly stating that total war is unthinkable—rather than structural prevention. Accessibility_collapse is high (0.78): once the taboo is understood, actors perceive total war as normatively foreclosed, and exit requires breaking with the entire security architecture. Resistance is moderate (0.55): the taboo meets steady resistance from strategic skeptics and non-aligned states who question its binding force, but the organized institutional coalition is strong enough to sustain it. Measurements show low extractiveness early (the taboo as fresh norm) rising to modest levels later (the taboo as institutional burden, with increasing theater ratio) as the lived cost of maintaining it grows and its functional justification becomes more contested.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (liberal democracies) and institutional beneficiaries experience the taboo as a successful coordination mechanism they maintain for everyone's benefit. Non-nuclear states and vulnerable populations experience it as an imposed constraint they cannot unilaterally break, with asymmetric costs. Strategic skeptics experience it as a rhetorical suppression mechanism that discredits coherent arguments about deterrence and capability. The engine should compute different effective extraction values across these seats: high-d (targeting) for excluded dissenters, lower-d for non-nuclear states (constrained but protected), and lowest-d for the institutional agenda-setter. The claim is Tangled Rope (coordination + asymmetry + enforcement), which preserves the intuition that the taboo solves a real coordination problem while also extracting compliance costs from those with no exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democracies and the NPT regime are structural beneficiaries: they author and enforce the norm, collect institutional legitimacy and budget from it, and maintain strategic advantage indefinitely (their superior conventional capability makes the taboo costless to them). Non-nuclear states are dual-positioned: they benefit from protection against total war but pay the cost of non-proliferation constraints and exposure to proxy violence—net d around 0.5-0.6. Asymmetrically vulnerable populations are near-target (d near 0.7): they bear the redirected violence without meaningful choice or reciprocal protection. Excluded norm entrepreneurs have the highest d (near 0.9): their arguments are systematically suppressed because they threaten the taboo, and their exclusion from discourse is the enforcement mechanism itself. The constraint's effective extraction is amplified by institutional power and global spatial scope—the taboo's enforcement machinery can reach across borders and into domestic discourse through multilateral instruments.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mischaracterization of the total-war taboo as either a purely coordinating Rope (it genuinely solves the coordination problem of how to avoid mutual annihilation) or a pure Snare (it does distribute costs asymmetrically and requires active suppression of contrary arguments). The Tangled Rope framing captures that both functions coexist: the taboo coordinates by channeling competition away from total war, AND it extracts by locking non-nuclear states into non-proliferation regimes they did not author. The mandatrophy question—whether the taboo's founding problem (how to prevent existential miscalculation) remains live or is dead—is exactly the question that produces the constraint family's divergence: the deterrence reading says the problem is solved by capability parity (the taboo is decorative), the space-contraction reading says the problem is solved by technical impossibility (the taboo is reinforcement), and the taboo reading says the problem is solved by norm-maintenance (the taboo is constitutive). The classification does not resolve which reading is correct; it documents that UNDER the taboo reading, the constraint is Tangled Rope, with measurable extractiveness on non-nuclear states and measurable suppression of dissenting argument.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_coexistence_taboo_vs_deterrence,
    'Can the taboo reading and the deterrence reading coexist within a single strategic framework, or do their core premises about the SOURCE of total-war prohibition logically foreclose one another?',
    'Examine IR scholarship that attempts to integrate norm-based and capability-based explanations: do they succeed in maintaining both as constitutive factors, or do they implicitly prioritize one over the other as the true cause?',
    'If they coexist, both readings are live positions with different author coalitions. If they foreclose, the engine''s reading_relations entry should record ''forecloses'' rather than ''coexists_with'', and the kernel admits no single unified theory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_taboo_vs_deterrence, conceptual, 'Whether sibling readings logically exclude or accommodate each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(tota_tr_t0, projected).
narrative_ontology:measurement(tota_tr_t10, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(tota_tr_t10, observed).
narrative_ontology:measurement(tota_tr_t20, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(tota_tr_t20, observed).
narrative_ontology:measurement(tota_tr_t40, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement_basis(tota_tr_t40, observed).
narrative_ontology:measurement(tota_tr_t60, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(tota_tr_t60, observed).
narrative_ontology:measurement(tota_tr_t80, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement_basis(tota_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(tota_be_t0, projected).
narrative_ontology:measurement(tota_be_t10, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(tota_be_t10, observed).
narrative_ontology:measurement(tota_be_t20, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(tota_be_t20, observed).
narrative_ontology:measurement(tota_be_t40, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement_basis(tota_be_t40, observed).
narrative_ontology:measurement(tota_be_t60, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 60, 0.37).
narrative_ontology:measurement_basis(tota_be_t60, observed).
narrative_ontology:measurement(tota_be_t80, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement_basis(tota_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(tota_su_t0, projected).
narrative_ontology:measurement(tota_su_t10, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(tota_su_t10, observed).
narrative_ontology:measurement(tota_su_t20, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(tota_su_t20, observed).
narrative_ontology:measurement(tota_su_t40, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(tota_su_t40, observed).
narrative_ontology:measurement(tota_su_t60, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(tota_su_t60, observed).
narrative_ontology:measurement(tota_su_t80, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement_basis(tota_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the total_war_possibility_space kernel. The sibling readings instantiate the deterrence-based and space-contraction-based accounts of what prevents total war. All three readings share the same referent (what constrains great-power war in the nuclear age) but derive fundamentally different structural claims about the SOURCE of that constraint. The readings are linked because they represent a genuine kernel contest: scholars, strategists, and policymakers genuinely dispute whether the taboo, mutual vulnerability, or technical impossibility is the true constraint. Each reading produces a different constraint story with independent ε values, different beneficiary/victim structures, and different structural predictions. The ε-invariance principle requires decomposition: one kernel, multiple readings, multiple constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
