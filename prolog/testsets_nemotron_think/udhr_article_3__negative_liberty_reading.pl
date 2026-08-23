% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3 — Negative Liberty Reading (State Prohibition on Arbitrary Deprivation of Life/Liberty)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   The negative liberty reading of UDHR Article 3 ('Everyone has the right
 *   to life, liberty and security of person') interprets the provision as a
 *   categorical prohibition on state deprivation of life and liberty except
 *   through narrow procedural justice. Security is defined exclusively as
 *   freedom from state violence — not as state-provided welfare or protection
 *   from private actors. This reading has expanded dramatically since 1948:
 *   from a procedural floor (fair trial before execution) to a substantive
 *   ceiling (abolition of capital punishment, restrictive self-defense,
 *   positive obligations to investigate state killings). The reading claims
 *   Mountain status (natural law, inalienable right) but operates as a
 *   constructed legal constraint requiring active judicial enforcement, with
 *   asymmetric extraction from state authorities and collective security
 *   apparatus toward individual rights-holders.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.78).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.65).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, mountain).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3 — Negative Liberty Reading (State Prohibition on Arbitrary Deprivation of Life/Liberty)").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).
domain_priors:emerges_naturally(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, 'dcdad456-50b6-4ace-9bae-79044adb8835').
narrative_ontology:cs_kernel_codification('dcdad456-50b6-4ace-9bae-79044adb8835', fixed_text).
narrative_ontology:cs_authority_grounding('dcdad456-50b6-4ace-9bae-79044adb8835', lineage).
narrative_ontology:cs_interpretation_layer_present('dcdad456-50b6-4ace-9bae-79044adb8835').
narrative_ontology:cs_reading_relation('dcdad456-50b6-4ace-9bae-79044adb8835', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('dcdad456-50b6-4ace-9bae-79044adb8835', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('dcdad456-50b6-4ace-9bae-79044adb8835', foundational, state_shall_not_deprive_life_arbitrarily).
narrative_ontology:cs_axiom_status(state_shall_not_deprive_life_arbitrarily, holdable).
narrative_ontology:cs_axiom_grounding('dcdad456-50b6-4ace-9bae-79044adb8835', state_shall_not_deprive_life_arbitrarily, deontological).
narrative_ontology:cs_axiom('dcdad456-50b6-4ace-9bae-79044adb8835', foundational, security_is_freedom_from_state_violence).
narrative_ontology:cs_axiom_status(security_is_freedom_from_state_violence, holdable).
narrative_ontology:cs_axiom_grounding('dcdad456-50b6-4ace-9bae-79044adb8835', security_is_freedom_from_state_violence, deontological).
narrative_ontology:cs_axiom('dcdad456-50b6-4ace-9bae-79044adb8835', secondary, procedural_justice_requires_effective_investigation).
narrative_ontology:cs_axiom_status(procedural_justice_requires_effective_investigation, holdable).
narrative_ontology:cs_axiom_grounding('dcdad456-50b6-4ace-9bae-79044adb8835', procedural_justice_requires_effective_investigation, empirically_contingent).
narrative_ontology:cs_axiom('dcdad456-50b6-4ace-9bae-79044adb8835', secondary, capital_punishment_is_per_se_arbitrary_deprivation).
narrative_ontology:cs_axiom_status(capital_punishment_is_per_se_arbitrary_deprivation, holdable).
narrative_ontology:cs_axiom_grounding('dcdad456-50b6-4ace-9bae-79044adb8835', capital_punishment_is_per_se_arbitrary_deprivation, deontological).
narrative_ontology:cs_reference_frame('dcdad456-50b6-4ace-9bae-79044adb8835', post_war_human_rights_settlement).
narrative_ontology:cs_drift_state('dcdad456-50b6-4ace-9bae-79044adb8835', contemporary_judicial_activism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dcdad456-50b6-4ace-9bae-79044adb8835', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individual_rights_holders).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_authorities).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, collective_security_apparatus).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, state_monopoly_violence_requires_procedural_legitimation).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, individual_life_liberty_lexically_prior_to_collective_security).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All human persons subject to state jurisdiction. They gain procedural protection against arbitrary killing, detention, or disappearance by state agents. They cannot exit the state's territorial reach; their protection depends entirely on the constraint's enforcement. They bear no cost of the constraint but are its intended beneficiaries.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individual_rights_holders, beneficiary,
    powerless, biographical, trapped, universal).

% Executive, legislative, and security branches of government. They lose discretionary power to deprive life/liberty without narrow procedural justification. They bear compliance costs: judicial oversight, due process procedures, abolition of capital punishment, restrictive self-defense doctrines. They can partially exit via sovereignty claims, non-ratification, or derogation clauses, but face diplomatic and legitimacy costs.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_authorities, payer,
    institutional, generational, arbitrage, national).

% Police, military, intelligence agencies, and emergency management bodies. They lose operational flexibility: preventive detention, shoot-to-kill policies, emergency powers, and broad self-defense doctrines are constrained. They argue these restrictions impede protection of the public from terrorism, organized crime, and civil unrest. They cannot exit the constraint but seek doctrinal carve-outs (e.g., 'ticking bomb' exceptions).
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, collective_security_apparatus, payer,
    organized, biographical, constrained, national).

% Domestic constitutional courts, supreme courts, and regional human rights courts (ECtHR, IACtHR, AfCHPR). They interpret and enforce the constraint, defining 'arbitrary,' 'procedural justice,' and 'due process.' They expand the reading over time (abolishing death penalty, requiring effective investigations). They administer the constraint but do not directly collect its benefits or pay its costs.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, judicial_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, judicial_bodies, observer).

% UN Human Rights Committee, Special Rapporteurs, treaty bodies, and NGOs (Amnesty, HRW). They monitor compliance, issue concluding observations, and name violators. They have no enforcement power but shape legitimacy and diplomatic pressure. They are analytical observers of the constraint's operation across jurisdictions.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, international_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% Individuals harmed by non-state actors (crime, domestic violence, terrorism) who might benefit from stronger state protective capacities that the constraint restricts. They are not represented in the drafting or interpretation of the negative liberty reading; their security interests are structurally excluded from the coordinate frame.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, victims_of_private_violence, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the limitation of state violence against individuals by establishing a universal procedural baseline: the state may not kill, detain, or disappear persons except through narrowly defined, judicially supervised processes. This solves the coordination problem of mutual vulnerability to sovereign power — each person gains protection only if all are protected, and the state's monopoly on violence is legitimated only through procedural constraint.
% TRANSFER_FUNCTION: Transfers discretionary authority over life and liberty decisions from state authorities (executive, security forces) to procedural and judicial mechanisms. The state loses the capacity for summary action, preventive detention without charge, unrestricted capital punishment, and broad self-defense justifications. Individuals gain procedural guarantees: habeas corpus, fair trial, prohibition of torture, effective investigation of state killings. The transfer is asymmetric — the state pays concentrated institutional costs; individuals receive diffuse but existential protections.
% ABSENT_VOICES: Victims of private violence (crime, terrorism, domestic abuse) who would trade procedural constraints on state power for stronger protective capacities are structurally excluded. Communities that prioritize collective security over individual procedural guarantees — especially in high-violence contexts — have no seat in the interpretive community. Their absence is not accidental: the negative liberty reading defines security AS freedom from state violence, rendering private violence a separate policy domain rather than a constitutional concern.
% DISAPPEARANCE_RATIONALE: If the negative liberty reading vanished overnight, states would revert to broader discretionary powers: capital punishment would expand (currently abolished in 112+ countries largely due to this reading), preventive detention regimes would proliferate, 'shoot-to-kill' policies would normalize, and judicial oversight of security operations would contract. The global human rights architecture built on Article 3 (ICCPR Art. 6, ECHR Art. 2, ACHR Art. 4) would lose its structural core. The world would rearrange toward sovereign impunity in life/liberty decisions.
% FOUNDING_PROBLEM: The founding problem was arbitrary state killing and imprisonment — the historical experience of sovereigns exercising unchecked power over life and death without procedural accountability. The UDHR drafters (1948) and ICCPR negotiators (1966) sought to constitutionalize the Nuremberg principle: state power over life/liberty requires legal justification, not mere sovereign will.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of pre-1948 state practice and the Nuremberg trials corroborate the founding problem (arbitrary sovereign power). However, the negative liberty reading's maximalist extension — categorical abolition of capital punishment, expansive due process requiring effective investigation of every state killing, restrictive self-defense doctrines — goes beyond the 1948/1966 consensus. No external corroboration exists for the claim that the founding problem requires abolishing the death penalty entirely; the ICCPR explicitly permits it (Art. 6.2). The reading's expansion is driven by judicial interpretation (e.g., ECtHR, HRC General Comments), not the original founding consensus.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, ExtMetricName, E),
    domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(udhr_article_3__negative_liberty_reading),
    narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the reading extracts profound institutional concessions from states: abolition of death penalty (112+ countries), mandatory judicial review of all killings, effective investigation duties, exclusionary rules for evidence, and restrictive self-defense doctrines. Suppression (0.65) is substantial because states cannot easily exit — treaty obligations, diplomatic pressure, and domestic constitutionalization create lock-in — but sovereignty provides partial arbitrage (derogations, non-compliance, withdrawal). Theater ratio (0.38) is moderate: the procedural machinery (courts, treaty bodies, reporting cycles) is real and functional, but a growing share of activity performs compliance without transforming security practice (ritualistic reporting, symbolic judgments). Accessibility collapse (0.72) is high for a claimed Mountain — alternative readings (positive entitlement, procedural hybrid) exist but are structurally marginalized in the interpretive community. Resistance (0.58) is significant: states resist through non-ratification, derogations, 'war on terror' exceptions, and judicial pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the individual rights-holder seat, the constraint is a Mountain — a non-negotiable floor of existence. From the state authority seat, it is a Tangled Rope — genuine coordination (legitimating the monopoly on violence) with asymmetric extraction (loss of sovereign discretion). From the collective security seat, it approaches Snare — the coordination story (procedural legitimacy) is experienced as cover for judicial encroachment on operational judgment. The engine will compute these per-seat types from the structural data; the claimed Mountain type reflects only the beneficiary seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual rights-holders are full beneficiaries (d ≈ 0.0): they collect existential protections without bearing enforcement costs. State authorities are full targets (d ≈ 1.0): they bear concentrated compliance costs (judicial oversight, lost operational tools) with no direct benefit. Collective security apparatus sits at high target (d ≈ 0.85): they lose tactical flexibility and argue the constraint increases public victimization. Judicial bodies are near-symmetric (d ≈ 0.5): they gain institutional authority but bear legitimacy burdens. International monitors are analytical (d = 0.5 by definition). The directionality derives from beneficiary/victim declarations plus exit options: individuals are trapped (no exit from state jurisdiction), states have arbitrage (sovereignty), security apparatus is constrained (institutional role).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary sovereign killing) remains live in many jurisdictions, but the reading's expansion — particularly categorical death penalty abolition and expansive positive investigative duties — exceeds the founding mandate. The constraint has not resolved its mandatrophy: it claims the founding problem is unchanged while its operational scope has grown. This is not a degraded Piton (the constraint remains actively enforced and expandable) but a living contested interpretation. The mandatrophy risk is that the reading becomes a vehicle for judicial policy-making beyond procedural constraint, which the positive entitlement and procedural hybrid readings contest from different directions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the negative liberty reading a distinct constraint from the positive entitlement and procedural hybrid readings of UDHR Article 3, or are they measurement perspectives on a single constraint?',
    'Apply the ε-invariance test: if measuring the constraint via capital punishment abolition yields high ε but measuring via torture prohibition yields low ε, they are distinct constraints. The negative liberty reading''s ε (0.78) is assessed against the standing arrangement of state killing power; the positive entitlement reading''s ε would be assessed against state welfare provision. Different referents → different constraints.',
    'If distinct, each reading gets its own constraint story with independent classification. If unified, the ε variance would be an observable-dependent artifact violating DP-001. The decomposition into three stories (this one plus two siblings) follows the BGS worked example: disambiguate the colloquial label ''Article 3'' into structurally precise claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three readings of UDHR Article 3 are one constraint or a constraint family.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the negative liberty reading a genuine natural law (Mountain) or a constructed legal constraint that benefits identifiable agents (individuals, judicial bodies)?',
    'Historical-genealogical analysis: trace the reading''s expansion from 1948 procedural floor to contemporary substantive ceiling. If the expansion tracks judicial institutional interest (court authority, treaty body mandate) rather than empirical reduction in state killing, the natural law claim is a false summit. Compare state killing rates before/after doctrinal expansions (e.g., death penalty abolition → homicide rates; investigative duties → police killings).',
    'If constructed, the False Summit Mountain signature triggers (Mountain with declared beneficiaries + omegas) and reclassifies toward Tangled Rope. The beneficiaries (individuals, judicial bodies) are identifiable and the constraint requires active enforcement — both inconsistent with genuine Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'Natural law claim vs. institutional construction for FSM evaluation.').

omega_variable(
    procedural_justice_scope,
    'How narrow is ''narrow procedural justice''? Does it require only fair trial before execution, or does it entail effective investigation of every state killing, prohibition of preventive detention, and restrictive self-defense doctrines?',
    'Comparative jurisprudence: map the divergence between ICCPR Art. 6 (permits death penalty with safeguards) and the HRC/ECtHR reading (effectively requires abolition). Track the doctrinal ratchet: each judicial decision expands ''procedural justice'' to include new positive obligations. The endpoint determines whether the reading is a stable Mountain or an expanding extraction mechanism.',
    'If ''narrow procedural justice'' is inherently expandable (no fixed boundary), the constraint is not a Mountain but a Scaffold or Tangled Rope with no sunset. If it has a fixed core (fair trial, habeas corpus, torture prohibition) with contested penumbra, the Mountain claim applies only to the core.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_justice_scope, conceptual, 'Boundary of the procedural core vs. expansive penumbra.').

omega_variable(
    capital_punishment_abolition_necessity,
    'Is abolition of capital punishment structurally required by the negative liberty reading, or is it a contingent judicial expansion?',
    'Textual-historical analysis: ICCPR Art. 6.2 explicitly permits death penalty ''for the most serious crimes in accordance with the law.'' The Second Optional Protocol (1989) makes abolition a separate treaty obligation. If the negative liberty reading requires abolition, it forecloses the ICCPR''s own framework. Check whether any state complies with the reading while retaining death penalty (none — abolition is now the reading''s defining marker).',
    'If abolition is required, the reading extracts a massive institutional concession (foregoing the ultimate penal sanction) from states that ratified ICCPR but not the Protocol. This asymmetric extraction (states pay, individuals benefit) supports Tangled Rope classification. If contingent, the reading''s ε is lower and Mountain claim stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_punishment_abolition_necessity, conceptual, 'Whether death penalty abolition is internal to the reading or an external accretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_art3_neg_lib_tr_t1948, udhr_article_3__negative_liberty_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(udhr_art3_neg_lib_tr_t1966, udhr_article_3__negative_liberty_reading, theater_ratio, 1966, 0.28).
narrative_ontology:measurement(udhr_art3_neg_lib_tr_t1976, udhr_article_3__negative_liberty_reading, theater_ratio, 1976, 0.31).
narrative_ontology:measurement(udhr_art3_neg_lib_tr_t1989, udhr_article_3__negative_liberty_reading, theater_ratio, 1989, 0.33).
narrative_ontology:measurement(udhr_art3_neg_lib_tr_t2000, udhr_article_3__negative_liberty_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(udhr_art3_neg_lib_tr_t2010, udhr_article_3__negative_liberty_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(udhr_art3_neg_lib_tr_t2024, udhr_article_3__negative_liberty_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(udhr_art3_neg_lib_be_t1948, udhr_article_3__negative_liberty_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(udhr_art3_neg_lib_be_t1966, udhr_article_3__negative_liberty_reading, base_extractiveness, 1966, 0.52).
narrative_ontology:measurement(udhr_art3_neg_lib_be_t1976, udhr_article_3__negative_liberty_reading, base_extractiveness, 1976, 0.58).
narrative_ontology:measurement(udhr_art3_neg_lib_be_t1989, udhr_article_3__negative_liberty_reading, base_extractiveness, 1989, 0.65).
narrative_ontology:measurement(udhr_art3_neg_lib_be_t2000, udhr_article_3__negative_liberty_reading, base_extractiveness, 2000, 0.71).
narrative_ontology:measurement(udhr_art3_neg_lib_be_t2010, udhr_article_3__negative_liberty_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(udhr_art3_neg_lib_be_t2024, udhr_article_3__negative_liberty_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(udhr_art3_neg_lib_su_t1948, udhr_article_3__negative_liberty_reading, suppression_requirement, 1948, 0.4).
narrative_ontology:measurement(udhr_art3_neg_lib_su_t1966, udhr_article_3__negative_liberty_reading, suppression_requirement, 1966, 0.48).
narrative_ontology:measurement(udhr_art3_neg_lib_su_t1976, udhr_article_3__negative_liberty_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(udhr_art3_neg_lib_su_t1989, udhr_article_3__negative_liberty_reading, suppression_requirement, 1989, 0.6).
narrative_ontology:measurement(udhr_art3_neg_lib_su_t2000, udhr_article_3__negative_liberty_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(udhr_art3_neg_lib_su_t2010, udhr_article_3__negative_liberty_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement(udhr_art3_neg_lib_su_t2024, udhr_article_3__negative_liberty_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__negative_liberty_reading, 0.1).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, iccpr_article_6).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, iccpr_article_9).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, echr_article_2).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, echr_article_5).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, achr_article_4).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, achr_article_7).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, afchr_article_4).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, afchr_article_5).

% DUAL FORMULATION NOTE:
% This constraint is one member of the udhr_article_3 constraint family (kernel_id: udhr_article_3). The three readings decompose the single textual provision into structurally distinct claims with different ε values, beneficiary/victim structures, and temporal trajectories. The negative liberty reading (this story) has high ε via expansive procedural obligations; the positive entitlement reading would have high ε via resource transfer obligations; the procedural hybrid reading has lower ε via procedural floor only. They are linked via affects_constraints and share the kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__negative_liberty_reading, institutional, 0.15).
constraint_indexing:directionality_override(udhr_article_3__negative_liberty_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
