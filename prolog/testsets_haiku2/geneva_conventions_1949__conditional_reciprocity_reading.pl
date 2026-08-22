% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions (1949) — Conditional Reciprocity Reading
 *   domain: legal/political/humanitarian
 *
 * SUMMARY:
 *   The Geneva Conventions (1949) establish protections for combatants and
 *   civilians in armed conflict. This constraint instantiates ONE READING of
 *   those Conventions: the conditional reciprocity reading. Under this
 *   reading, the Conventions function as reciprocal restraints that apply
 *   fully only when adversaries comply with Article 4 criteria (organized
 *   command structure, distinctive insignia, carrying arms openly, conducting
 *   operations in accordance with laws of war). When irregular forces fail to
 *   meet these criteria, the reading permits 'proportional degradation' of
 *   protections — unlawful combatants lose POW status, face extended
 *   detention, receive reduced medical care, and are subjected to
 *   interrogation under expanded legal latitude. Civilian immunity is
 *   preserved but narrowed by proportionality calculations in
 *   irregular-controlled areas. This reading has accumulated extractiveness
 *   over 75 years as irregular warfare has become dominant and states have
 *   increasingly used the 'non-compliance permits degradation' justification
 *   to manage asymmetric conflicts. The measurement series traces rising
 *   extraction from 0.35 (1949, when interstate state-on-state warfare was
 *   the norm) to 0.62 (2024, when irregular warfare and hybrid conflict
 *   dominate). Theater ratio rises as enforcement activity shifts from
 *   genuine distinction-of-combatants work toward legal paperwork
 *   (classification tribunals, unlawful combatant designations, interrogation
 *   protocols) that defensively document the conditional reciprocity doctrine
 *   rather than implementing humanitarian protection.
 *
 * KEY AGENTS:
 *   - State military apparatus: administers the conditional reciprocity interpretation; benefits from operational discretion and reduced detention obligations for irregular combatants
 *   - Irregular armed groups: structurally unable to meet Article 4 criteria; members are classified as unlawful combatants and denied full protections
 *   - Detained combatants without Article 4 status: powerless targets of the classification scheme; held indefinitely without trial or POW standing
 *   - Civilian populations under state control: beneficiaries; receive absolute immunity from targeting
 *   - Civilian populations in irregular territory: victims; reduced protections due to proportionality narrowing when irregulars operate from mixed areas
 *   - International humanitarian community: excluded observers; field presence contradicts the protections states claim the reading provides
 *   - IHL scholars (humanitarian ceiling advocates): excluded; argue Conventions establish absolute minimums regardless of reciprocity
 *   - States party to Conventions: beneficiaries; gain full protections when they meet Article 4; gain operational flexibility via conditional reciprocity against irregular adversaries
 *   - ICC and monitoring bodies: observers; uncertain enforcement authority due to the reading's built-in ambiguity about which Article 4 features are mandatory vs. permissive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.71).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions (1949) — Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "legal/political/humanitarian").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '5dc62127-44f7-48b9-8d29-1de732624f0e').
narrative_ontology:cs_kernel_codification('5dc62127-44f7-48b9-8d29-1de732624f0e', fixed_text).
narrative_ontology:cs_authority_grounding('5dc62127-44f7-48b9-8d29-1de732624f0e', extraction).
narrative_ontology:cs_interpretation_layer_present('5dc62127-44f7-48b9-8d29-1de732624f0e').
narrative_ontology:cs_reading_relation('5dc62127-44f7-48b9-8d29-1de732624f0e', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('5dc62127-44f7-48b9-8d29-1de732624f0e', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('5dc62127-44f7-48b9-8d29-1de732624f0e', foundational, reciprocity_gates_full_protection).
narrative_ontology:cs_axiom_status(reciprocity_gates_full_protection, holdable).
narrative_ontology:cs_axiom_grounding('5dc62127-44f7-48b9-8d29-1de732624f0e', reciprocity_gates_full_protection, conventional).
narrative_ontology:cs_axiom('5dc62127-44f7-48b9-8d29-1de732624f0e', foundational, non_compliance_permits_proportional_degradation).
narrative_ontology:cs_axiom_status(non_compliance_permits_proportional_degradation, holdable).
narrative_ontology:cs_axiom_grounding('5dc62127-44f7-48b9-8d29-1de732624f0e', non_compliance_permits_proportional_degradation, instrumental).
narrative_ontology:cs_reference_frame('5dc62127-44f7-48b9-8d29-1de732624f0e', reciprocal_state_to_state_restraint_framework).
narrative_ontology:cs_drift_state('5dc62127-44f7-48b9-8d29-1de732624f0e', contemporary_irregular_warfare_dominance, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5dc62127-44f7-48b9-8d29-1de732624f0e', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_combatants_meeting_article_4).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_under_state_control).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants_failing_article_4).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_irregular_controlled_territory).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, detained_unlawful_combatants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, states_party_to_conventions).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, detained_combatants_without_article_4_status).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_irregular_territory).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, reciprocity_doctrine_in_ihl).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, combatant_status_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the Conventions to its own forces and adversaries. Claims the right to degrade protections for irregular combatants who fail to comply with Article 4 (organized command structure, distinctive insignia, carrying arms openly, conducting operations in accordance with laws of war). Administers detention, interrogation, and classification of captured combatants. Justifies conditional reciprocity as essential to maintaining discipline and deterring non-compliance by adversaries.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_military_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Operate without formal state backing, organized military hierarchy, or distinctive insignia. Their members, when captured, are classified as unlawful combatants under this reading and denied full POW protections (reduced medical care, limited due process, extended detention without trial). They cannot meaningfully exit the conflict; surrender offers no protection guarantee. The conditional reciprocity doctrine uses their structural inability to comply with Article 4 as justification for diminished protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_armed_groups, payer,
    organized, immediate, trapped, regional).

% Held in indefinite detention by state forces without formal trial or POW status. Denied access to protection mechanisms available to recognized combatants: no inspection by neutral powers, no POW committees, no repatriation agreements. Subjected to interrogation under this reading's justification that non-compliance with Article 4 standards permits degraded treatment. Their powerlessness is structural — the state controls the interrogation apparatus and classification tribunal.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, detained_combatants_without_article_4_status, payer,
    powerless, immediate, trapped, local).

% Receive protections from the Conventions under this reading: immunity from targeting, access to humanitarian assistance, right to medical care if wounded. Their civilian status is presumed and difficult to override. The conditional reciprocity doctrine preserves absolute prohibitions on deliberate civilian targeting, though proportionality calculations may reduce protections when irregular forces operate from civilian areas.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_under_state_control, beneficiary,
    moderate, biographical, constrained, national).

% Located in territory controlled by irregular armed groups; their protection status is reduced under this reading because the irregular commanders cannot certify compliance with Article 4. Civilian immunity is narrowed by proportionality: state forces claim expanded targeting latitude in irregular-controlled areas where civilians and combatants are mixed. They cannot easily flee without risking detention or targeting; they cannot secure protections by proving non-combatancy because the irregulars' organizational structure is opaque.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_irregular_territory, payer,
    powerless, biographical, trapped, regional).

% Humanitarian organizations (ICRC, NGOs, UN agencies) operate under strict neutrality rules and cannot legally advocate for one reading over another. However, their fieldwork repeatedly encounters the conditional reciprocity doctrine as a justification for denying access to detainees, preventing family contact, and narrowing the definition of protected persons. They are excluded from the political decision about which reading governs implementation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_humanitarian_community, excluded,
    organized, biographical, constrained, global).

% Legal scholars and humanitarian advocates who argue the Conventions establish absolute minimum protections regardless of reciprocity. Their alternative reading (humanitarian ceiling) is marginalized in state practice; states routinely invoke conditional reciprocity to justify detention regimes and classification schemes. Their position is formally excluded from the institutional implementation hierarchy.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, ihl_scholars_humanitarian_ceiling_advocates, excluded,
    organized, generational, constrained, global).

% Benefit from the Conventions' reciprocal structure: when they meet Article 4 standards (organized military, insignia, open carrying), they receive full protections for their combatants. They also benefit from the conditional reciprocity reading because it permits them to degrade protections for adversaries, increasing operational flexibility and reducing detention/care costs. They have capacity to exit via denunciation (though rare) and can influence interpretation via state practice and legal argument.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, states_party_to_conventions, beneficiary,
    institutional, generational, mobile, global).

% Monitor compliance and investigate alleged violations. The conditional reciprocity reading creates interpretive ambiguity in their enforcement: is detention of unlawful combatants a violation or a legitimate application of Article 4 conditioning? The reading's opacity about which features of Article 4 are truly mandatory vs. permissively conditional generates persistent prosecutorial uncertainty.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, icc_and_treaty_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes treatment of combatants and civilians across states by establishing reciprocal protections that gate on formal compliance with Article 4 criteria. Solves the problem of mutual uncertainty in asymmetric warfare: states can determine whether an adversary qualifies for full protections by assessing whether it meets Article 4 standards (organized command, distinctive insignia, carrying arms openly, conducting operations in accordance with laws of war). If adversaries meet standards, they receive POW protections; if they fail, protections degrade proportionally. This reduces escalation pressure by providing a clear pathway to reciprocal treatment.
% TRANSFER_FUNCTION: Transfers 'protected status and humanitarian privileges' from irregular combatants and civilians in irregular-controlled territory TO state combatants (who typically meet Article 4 criteria) and civilians under state control. The mechanism is conditional: full transfer to Article-4-compliant forces, partial transfer to non-compliant forces. This is not a material extraction but a status and rights transfer — irregular combatants lose POW standing, detention-without-trial protections, medical care, and family contact; state combatants gain these protections and their civilians gain absolute immunity from targeting.
% ABSENT_VOICES: Detained irregular combatants are excluded from participating in the legal interpretation of whether they meet Article 4 criteria or deserve degraded protections. Their classification happens in tribunals they do not attend, argued by states and legal advisors, without their voice. IHL scholars who advocate the humanitarian ceiling reading are structurally excluded from state legal interpretation hierarchies — their position is marginalized in institutional implementation. Humanitarian organizations operating under neutrality rules cannot advocate for an alternative reading. Civilians in irregular-controlled territory have no formal voice in state legal proceedings about whether proportionality calculations justify narrowed protections in their areas.
% DISAPPEARANCE_RATIONALE: If the conditional reciprocity reading disappeared and were replaced by humanitarian ceiling (absolute protections) or security maximization (yield to necessity), the legal basis for classifying irregular combatants as unlawful combatants would fundamentally change. Humanitarian ceiling would require trial rights, full medical care, family contact, and repatriation for all detained combatants regardless of organizational structure — detention regimes would require comprehensive restructuring. Security maximization would eliminate pretense of constraints altogether — states would have explicit legal authorization to suspend protections in asymmetric conflicts. The institutional apparatus for classification tribunals, interrogation protocols, detention justifications, and Article-4-based gatekeeping would be restructured or dismantled. Irregular armed groups would either gain full POW standing (humanitarian ceiling) or lose any pretense of legal constraints (security maximization). The entire legal and operational architecture of how states manage irregular combatants would reorganize.
% FOUNDING_PROBLEM: Irregular armed forces do not organize into formal state military structures with distinctive insignia and open carrying of weapons. When such forces engage in armed conflict with states, they do not meet the criteria the 1949 Conventions presume in their Article 4 gatekeeping. The founding problem is: what legal status do irregular combatants have if they do not meet Article 4 criteria? Are they entitled to the same protections as state militaries? If not, how much less? This creates mutual uncertainty and escalation pressure because states do not know whether they must grant POW status, and irregular forces do not know what protections to expect upon capture.
% FOUNDING_PROBLEM_CORROBORATION: Military legal advisors from multiple states (US, Israel, UK, France, Russia) attest that Article 4-noncompliant irregular forces remain a persistent problem in modern armed conflict. Modern warfare data confirms irregular forces are increasingly dominant (asymmetric conflicts outnumber interstate wars 5:1 in recent decades). However, humanitarian organizations and IHL scholars counter that the founding problem is overstated: many irregular groups DO organize hierarchically and could formalize insignia if they chose; the use of Article 4 non-compliance as grounds for unlawful-combatant classification is often a post-hoc rationalization rather than a genuine legal necessity. They argue the real founding problem is that states lack incentives to grant POW status to irregular combatants, not that irregular forces cannot meet Article 4 criteria. The founding problem's status is live (irregular forces persist and create classification challenges) and contested (whether the challenges justify the conditional reciprocity response).
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.62) and rising because the conditional reciprocity reading permits degradation of protections as a baseline rule rather than an exception. State forces benefit from a doctrine that expands their targeting latitude and detention options; irregular combatants and civilian populations in irregular territory bear the cost. The reading achieves this asymmetry by using Article 4's formal criteria (organized command, insignia, open carrying) as a gatekeeping threshold: if you meet all criteria, full protections; if you fail any one, protections degrade. This is not a natural law (alternative readings exist) nor a pure coordination (the doctrine is enforced to advantage states over irregulars) — it is tangled rope because it solves a genuine coordination problem (how to govern asymmetric warfare) while extracting from irregular combatants and civilians in their territory. Suppression is high (0.71) because the reading's implementation depends on active enforcement: states must classify combatants, deny access to detainees, reject humanitarian organization inspection, and maintain legal arguments that non-compliance justifies degradation. Without enforcement, the humanitarian ceiling reading would become the default. Theater ratio is moderate-to-high (0.48) because enforcement activity increasingly consists of legal ritual (classification tribunals, paperwork, interrogation protocols) rather than genuine humanitarian work — the machinery exists partly to defend the conditional reciprocity doctrine's legitimacy. The measurement series shows both metrics rising over time (base_extractiveness 0.35→0.62, suppression 0.52→0.71, theater 0.25→0.48) because irregular warfare has proliferated, giving states more opportunities and incentives to use the conditional reciprocity reading to manage asymmetric conflicts. The constraint has become more extractive and more theatrical as the operational context that justifies it has become more common. Accessibility collapse (0.68) is moderate-high: once a state decides an adversary is non-Article-4-compliant, alternatives (humanitarian appeal, neutral observer access, trial rights) become difficult to access — the classification itself forecloses other outcomes. Resistance (0.59) is substantial: humanitarian organizations, IHL scholars, and some state actors resist the reading and advocate for humanitarian ceiling instead; the resistance persists but does not yet overturn the reading.
 *
 * PERSPECTIVAL GAP:
 *   State military perspective: the conditional reciprocity reading is justified by operational necessity and the problem of irregular warfare that violates Article 4 criteria. It is coordination (how to govern asymmetric conflict) with reasonable constraints (full protections for Article-4-compliant forces, conditional protections for non-compliant forces). Irregular combatants' perspective: the reading is pure extraction justified by their structural inability to meet formal military criteria that Article 4 requires. They are trapped and cannot change their organizational structure without losing their irregular status and military effectiveness. Humanitarian perspective: the reading is a cover story for expanded state violence. The Conventions' humanitarian intent is absolute minimum protections regardless of reciprocity; the conditional reciprocity reading is a reinterpretation that permits states to degrade protections and detain persons indefinitely. Civilian perspective (state-controlled territory): the reading is beneficial — they receive absolute immunity from targeting. Civilian perspective (irregular territory): the reading is harmful — proportionality narrowing reduces their immunity when irregulars operate nearby. The structural divergence between how state military apparatus and irregular combatants experience the constraint is the core asymmetry: the state experiences reciprocal coordination with bounded conditionality; irregulars experience asymmetric extraction justified by rules they cannot comply with.
 *
 * DIRECTIONALITY LOGIC:
 *   State military apparatus: d ≈ 0.1 (near beneficiary). The state sets the classification rules, administers detention, and benefits from expanded operational discretion. It has analytical exit (can reinterpret the Conventions or adopt a different reading) and institutional power. Irregular armed groups: d ≈ 0.95 (near full target). They are trapped by their own organizational structure (cannot instantly formalize military hierarchy or issue insignia without changing their fundamental nature), cannot exit the conflict, and face reduced protections. Detained combatants: d = 1.0 (full target). Powerless, trapped in detention, no exit, no voice in legal proceedings. Civilian populations under state control: d ≈ 0.35 (beneficiary-leaning symmetric). They receive absolute protection and have exit options (flee, surrender, cooperate with state). Civilian populations in irregular territory: d ≈ 0.85 (near target). They are trapped, have reduced protections due to proportionality narrowing, and cannot voice objections. States party to Conventions (institutional actors not at war): d ≈ 0.2 (beneficiary). They benefit from the reciprocal structure and can influence interpretation via legal argument and practice. ICC and monitoring bodies: d = 0.5 (analytical, symmetric). They are not targets or beneficiaries; they observe and investigate. Humanitarian organizations: d ≈ 0.6 (leaning target). They are constrained by neutrality rules and excluded from the legal interpretation hierarchy, yet their field presence is required by the Conventions. Excluded IHL scholars: d ≈ 0.75 (leaning target). Their voices are excluded from the institutional decision-making, yet they are the intellectual opposition to the conditional reciprocity reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The conditional reciprocity reading addresses a genuine founding problem: how to govern warfare when adversaries violate the formal military structure Article 4 presumes. However, the classification of 'failure to meet Article 4 criteria' as grounds for degrading protections has expanded well beyond the founding problem's scope. Early postwar interpretation focused on clear cases: militia forces without command structures, uniforms, or insignia received reduced protections. Modern application includes organized armed groups with clear hierarchies and identifiable uniforms (Palestinian factions, Hezbollah, Kurdish YPG) that fail some but not all Article 4 criteria — and the reading permits states to classify them as unlawful combatants and deny full protections. The mandate has not died (irregular warfare is still rising), but its application has become extractive: states use the reading to justify detention without trial, denial of humanitarian access, and expanded interrogation authority that goes well beyond what the Article 4 gatekeeping was designed to address. The conditional reciprocity reading prevents misclassification of irregular violence as protected armed conflict when it does not meet the Conventions' criteria — a genuine mandatrophy prevention. However, it also permits reclassification of organized irregular forces that COULD be brought within the Conventions (by formalizing command structure, issuing insignia) as unlawful combatants instead — an extractive use that goes beyond mandatrophy prevention. The theater ratio's rise (0.25→0.48) indicates that enforcement is increasingly theatrical (legal paperwork defending classifications) rather than functional (actual humanitarian work). This suggests the reading's mandate has atrophied at the margin: the core (preventing false recognition of irregular violence as protected conflict) persists, but the peripheral (denying protections to organized groups that could formally comply) is maintained by legal theater rather than by genuine necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_4_threshold_stability,
    'Are the Article 4 criteria (organized command, distinctive insignia, carrying arms openly, conducting operations in accordance with IHL) genuinely immutable for irregular forces, or are they contextually variable thresholds that could be met by groups that choose to formalize?',
    'Empirical observation of irregular groups that have formally organized into militarized state structures (PKK''s YPG affiliation, various militias integrating into national forces) and historical cases where insurgents transitioned to conventional military format. Legal analysis of whether the thresholds are technical requirements or standards that evolve with armed conflict modalities.',
    'If Article 4 criteria are immutable (irregular forces structurally cannot meet them), the conditional reciprocity reading''s degradation of protections is justified by necessity. If they are variable and some irregular groups could be brought into compliance through formalization, the reading becomes extractive — states use it to deny protections that could be granted by requiring compliance, rather than recognizing that compliance is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_4_threshold_stability, empirical, 'Whether Article 4 criteria represent structural impossibility for irregular forces or contextual thresholds that formalization could meet.').

omega_variable(
    proportionality_narrowing_boundary,
    'What counts as a ''proportional'' narrowing of civilian immunity in irregular-controlled territory? At what ratio of combatants-to-civilians does the reading justify expanded targeting latitude, and who decides?',
    'Comparative analysis of state practice in recent asymmetric conflicts (Israel-Gaza, Russia-Ukraine, US counterterrorism zones). Legal scholarship on proportionality doctrine''s operationalization. Case law from ICC and other accountability bodies on whether states'' proportionality claims meet the Conventions'' actual requirements.',
    'If proportionality narrowing has predictable, transparent thresholds, it is a genuine coordination principle. If it is opaque and state-determined, it is extraction dressed as limitation — the conditional reciprocity reading becomes a license for states to unilaterally reduce civilian protections.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_narrowing_boundary, empirical, 'Whether proportionality narrowing has objective operationalizable thresholds or operates as an opaque state prerogative.').

omega_variable(
    reading_displacement_mechanism,
    'Would adoption of the humanitarian ceiling reading (absolute protections regardless of reciprocity) or the security maximization reading (protections yield to operational necessity) restructure the political economy of irregular warfare, or is the reading-choice neutral to state strategy?',
    'Counterfactual analysis: what would state counterterrorism and counterinsurgency operations look like if absolute protections applied to all detained persons regardless of Article 4 compliance? Would detention regimes shrink, end, or absorb the cost and continue unchanged?',
    'If reading choice has structural consequences (e.g., humanitarian ceiling would require trial rights and reduce detention, shifting state incentives toward early release or repatriation), the readings are genuinely alternative arrangements and the conditional reciprocity reading is politically constructed. If state incentives are reading-neutral (states would maintain detention regimes under any reading), the conditional reciprocity reading captures a structural truth about asymmetric warfare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_displacement_mechanism, conceptual, 'Whether the three readings represent substantively different constraint structures or merely different framings of the same underlying state-irregular asymmetry.').

omega_variable(
    humanitarian_organization_exclusion_mechanism,
    'Is the exclusion of humanitarian organizations from the legal interpretation hierarchy (they operate under neutrality rules and cannot advocate for a reading) a structural feature of humanitarian law, or an extractive gatekeeping that permits states to avoid accountability by denying monitoring?',
    'Comparative analysis of monitoring regimes in interstate vs. asymmetric conflicts. Assessment of whether neutrality rules genuinely protect humanitarian access or primarily protect states from accusations of Conventions violations.',
    'If neutrality is structural (necessary to maintain humanitarian space for all parties), the exclusion is coordination. If neutrality is extracted (humanitarian organizations lose voice, states gain accountability evasion), the conditional reciprocity reading is maintained partly by suppressing contrary evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_organization_exclusion_mechanism, empirical, 'Whether humanitarian organization exclusion from legal interpretation is structurally necessary or extractively maintained.').

omega_variable(
    kernel_reading_committer_contention,
    'Is the kernel ''geneva_conventions_1949'' a genuine contested commitment that multiple readings can inhabit legitimately, or does one reading (humanitarian ceiling) represent the Conventions'' actual intent and the other readings (conditional reciprocity, security maximization) are post-hoc reinterpretations that distort the kernel?',
    'Textual analysis of the 1949 Conventions'' language, preamble, and historical context. Expert testimony from IHL historians and foundational delegates'' intent. Comparison of how the readings align with or diverge from the Conventions'' express language.',
    'If humanitarian ceiling is the kernel''s true intent and conditional reciprocity is a reinterpretation, the reading is foreclosed (the engine would route to foreclosed status). If all three readings are legitimate instantiations of an ambiguous kernel, they coexist. This is the meta-question about whether the kernel contest is real or whether one reading should be acknowledged as the correct reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_contention, conceptual, 'Whether the conditional reciprocity reading is a legitimate instantiation of the Geneva Conventions kernel or a post-hoc reinterpretation that distorts the kernel''s intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.25).
narrative_ontology:measurement_basis(gene_tr_t1949, observed).
narrative_ontology:measurement(gene_tr_t1975, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement_basis(gene_tr_t1975, observed).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement_basis(gene_tr_t1990, observed).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.44).
narrative_ontology:measurement_basis(gene_tr_t2001, observed).
narrative_ontology:measurement(gene_tr_t2012, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2012, 0.46).
narrative_ontology:measurement_basis(gene_tr_t2012, observed).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.48).
narrative_ontology:measurement_basis(gene_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement_basis(gene_be_t1949, observed).
narrative_ontology:measurement(gene_be_t1975, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement_basis(gene_be_t1975, observed).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1990, 0.54).
narrative_ontology:measurement_basis(gene_be_t1990, observed).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement_basis(gene_be_t2001, observed).
narrative_ontology:measurement(gene_be_t2012, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2012, 0.61).
narrative_ontology:measurement_basis(gene_be_t2012, observed).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(gene_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.52).
narrative_ontology:measurement_basis(gene_su_t1949, observed).
narrative_ontology:measurement(gene_su_t1975, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1975, 0.61).
narrative_ontology:measurement_basis(gene_su_t1975, observed).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement_basis(gene_su_t1990, observed).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement_basis(gene_su_t2001, observed).
narrative_ontology:measurement(gene_su_t2012, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement_basis(gene_su_t2012, observed).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(gene_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__conditional_reciprocity_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, article_4_combatant_status_distinction).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, unlawful_combatant_classification_regime).

% DUAL FORMULATION NOTE:
% The Geneva Conventions (1949) kernel admits three structurally distinct readings: conditional_reciprocity_reading (this constraint), humanitarian_ceiling_reading (absolute protections regardless of reciprocity), and security_maximization_reading (protections yield to operational necessity). Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different classifications. They share the kernel (the Conventions' text and authority structures) but diverge on whether the humanitarian intent is absolute, conditional, or yield-able. The conditional reciprocity reading frames the Conventions as reciprocal restraints where compliance with Article 4 criteria (organized command, distinctive insignia, open carrying, compliance with IHL) gates full protections. The humanitarian ceiling reading frames the Conventions as establishing inviolable minimum protections regardless of how an adversary organizes or what they do. The security maximization reading frames the Conventions as peacetime aspirations that states can suspend when irregular threats justify it. All three readings share the Conventions as their kernel; only the reading-specific claims differ. The three constraints are linked via network.affects_constraints: changes in institutional interpretation of the kernel affect all three readings' operationalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
