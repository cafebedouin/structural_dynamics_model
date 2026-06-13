% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment Individual Right to Firearms
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the INDIVIDUAL-RIGHT READING of the
 *   Second Amendment kernel. The reading asserts that the Second Amendment
 *   protects an individual right to own firearms for lawful purposes
 *   (self-defense, hunting, sport) that is not dependent on militia service
 *   or state authorization. This reading was established as Supreme Court
 *   doctrine in District of Columbia v. Heller (2008) and reaffirmed in
 *   McDonald v. City of Chicago (2010). It competes with two sibling
 *   readings: the collective-right reading (which locates the right in the
 *   state militia function only) and the civic-right reading (which
 *   conditions individual ownership on militia participation). This
 *   constraint story describes ONLY the individual-right reading—its
 *   beneficiary structure, extractive mechanisms, and enforcement dynamics.
 *   The sibling readings are separate constraint stories with different ε
 *   values and stakeholder configurations. The reading is NOT claimed to be
 *   natural law (emerges_naturally: false); it is a constructed
 *   constitutional commitment that grounds its legitimacy in originalist
 *   interpretation of 1791 meaning and historical redress for
 *   Reconstruction-era disarmament.
 *
 * KEY AGENTS:
 *   - individual_firearm_owners: widespread beneficiaries whose ownership right is protected and validated
 *   - gun_rights_advocacy_organizations: institutional agenda-setter and beneficiary; maintains enforcement through litigation
 *   - firearms_manufacturers_and_dealers: powerful beneficiaries collecting rents from a protected market
 *   - communities_targeted_by_gun_violence: powerless victims trapped in high-violence neighborhoods; largely excluded from constitutional conversation
 *   - public_health_regulators: institutional payers bearing elevated review standards
 *   - state_legislatures: institutional payers whose police power is substantially curtailed
 *   - constitutional_originalists: analytical beneficiaries whose interpretive methodology is vindicated
 *   - collective_right_reading_adherents: excluded analytical observers whose competing reading is constitutionally foreclosed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.42).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right to Firearms").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'ca45b6f8-30cf-41e7-8df1-fa24a535aeb0').
narrative_ontology:cs_kernel_codification('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', fixed_text).
narrative_ontology:cs_authority_grounding('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', lineage).
narrative_ontology:cs_interpretation_layer_present('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0').
narrative_ontology:cs_reading_relation('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', second_amendment_scope__civic_right_reading, influences).
narrative_ontology:cs_axiom('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', foundational, individual_autonomy_natural_right).
narrative_ontology:cs_axiom_status(individual_autonomy_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', individual_autonomy_natural_right, deontological).
narrative_ontology:cs_axiom('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', foundational, militia_independence_unconnected).
narrative_ontology:cs_axiom_status(militia_independence_unconnected, holdable).
narrative_ontology:cs_axiom_grounding('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', militia_independence_unconnected, empirically_contingent).
narrative_ontology:cs_reference_frame('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', individual_armed_autonomy_regime).
narrative_ontology:cs_drift_state('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', contemporary_public_health_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ca45b6f8-30cf-41e7-8df1-fa24a535aeb0', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_firearm_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_manufacturers_and_dealers).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, communities_targeted_by_gun_violence).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, assault_weapons_ban_advocates).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_health_regulators).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.68 at interval end because the individual-right reading creates a broad, constitutionally-protected entitlement that constrains state regulatory authority across the entire population of gun owners (high coverage) and establishes strict scrutiny as the review standard (high obstacle for regulation). The reading extracts regulatory capacity from legislatures and redistributes it to federal courts and individual rights-holders. The trajectory shows steady climb from 0.45 to plateau at 0.68 (t=25–40), reflecting the reading's gradual entrenchment after Heller: initial uncertainty and contestation (0–15 years) gave way to jurisprudential consolidation and broadened scope (15–25 years), with stabilization thereafter. Suppression (0.42) is lower than extractiveness because the constraint does NOT require massive coercive enforcement against the beneficiary set; it is self-maintaining through constitutional doctrine and individual exercise. Theater is low-to-moderate (0.28) because the enforcement is primarily doctrinal (courts striking down regulations) rather than performative, though public advocacy by gun-rights organizations carries theatrical elements. Accessibility collapse (0.71) reflects the reading's closure of the policy alternative (state-led comprehensive regulation) within the constitutional frame—but alternatives remain politically live (repeal, amendment, reframing), which prevents total collapse. Resistance (0.76) is high because many legislative bodies, public health advocates, and communities affected by gun violence actively resist the individual-right reading, mount counter-litigation, and push for constitutional amendment or re-reading. The measurements document baseline uncertainty (t=0) reflecting the pre-Heller era when the reading was not yet settled doctrine, consolidation and scope-expansion (t=5–25) as the reading defeated challenges and courts applied it broadly, and stabilization (t=25–40) as the reading became established constitutional baseline.
 *
 * PERSPECTIVAL GAP:
 *   The gun-rights advocacy organizations and originalist constitutional scholars experience this reading as genuine coordination—a stable property right that protects individual autonomy against state overreach, grounded in historical redress and principled constitutional method. Public health regulators and communities experiencing gun violence experience it as enforced extraction—a reading that privileges manufacturer interests and individual gun ownership over their legitimate regulatory interests and their own constitutionally protected interests (life, liberty, property via due process). State legislatures experience a loss of authority rather than a gain in coordination. The engine computes per-seat classification: from the beneficiary seats (individual owners, manufacturers, advocacy organizations), the constraint should compute as rope or even positive coordination; from the payer seats (legislatures, public health authorities, victimized communities), it should compute as snare-adjacent or tangled-rope-extractive. The claimed_type (rope) reflects the beneficiary-seat framing; the authored metrics reflect the system-wide operation including extraction from the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual firearm owners sit at d ≈ 0.1–0.2 (beneficiary end): they gain a protected right, face no regulation as the default state, and have high exit mobility (can exercise the right in friendly jurisdictions or abstain). Gun-rights advocacy organizations sit near d ≈ 0.05 (strong beneficiary): they set the agenda, collect legitimacy from court victories, and face minimal regulatory cost—they could abandon litigation and still benefit from the reading. Firearms manufacturers sit at d ≈ 0.15–0.25 (beneficiary-leaning): they collect economic rents from a protected market and face regulatory obstacles, but they do bear litigation costs to defend the market. Public health regulators sit at d ≈ 0.75–0.85 (target end): they bear elevated review standards for any regulation, face repeated litigation losses, and have constrained exit (they cannot exit the regulatory domain without losing their institutional seat). State legislatures sit at d ≈ 0.70–0.80 (target): they lose regulatory authority and must justify any gun control under strict scrutiny; exit requires constitutional amendment (minimal available exit). Communities experiencing gun violence sit at d ≈ 0.85–0.95 (full target): they bear the direct costs of high firearm availability, are trapped in affected areas, and face institutional exclusion from the constitutional conversation—their interests are not recognized as constitutional interests but as policy preferences subject to the heightened review standard. No directionality_overrides are needed; the derived directionality from beneficiary/victim + exit aligns with the structural analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by maintaining a live founding problem. The founding problem (state-enabled disarmament of disfavored populations; Reconstruction-era gun confiscation targeting freedmen) remains politically live in debates about police violence, racial justice, and armed self-defense in communities distrusting state institutions. This gives the reading durability beyond narrow interest protection. However, there is a latent mandatrophy risk: if the founding problem (historical disarmament) becomes sufficiently resolved by changing state behavior (non-racial, content-neutral regulation), the individual-right reading may persist as inertia rather than as response to a live problem. The measurement series shows extractiveness stabilizing rather than rising after t=25, which suggests the reading has achieved its primary goal (establishing the right as constitutional doctrine) and is now maintained more by precedent and organized advocacy than by active solving of the founding problem. This is not yet pitonization (there is still real beneficiary mobilization and real regulatory contestation), but it is a warning sign: if public health regulation continues to face strict scrutiny while the founding problem (racial disarmament) fades from the discourse, the reading may degrade into theater-and-inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_methodology_contingency,
    'Is the individual-right reading''s grounding in originalist constitutional method contingent on the specifics of that method, or would a different interpretive methodology (living constitution, progressive constitutionalism, public-meaning originalism) produce a different ε and beneficiary structure?',
    'Genealogical analysis of how the individual-right reading would emerge under alternative methodologies; comparison of pre-Heller jurisprudence (which used different methods and produced the collective-right reading) to post-Heller originalist readings.',
    'If the reading is contingent on originalist method, its ε depends partly on methodological authority—a shift in interpretive consensus could dislodge the reading without changing facts about firearm harm or state capacity. If the reading is robust to method, it is more deeply rooted. Currently, the evidence suggests heavy methodological contingency: the reading''s ascendance tracks the rise of originalism as a court-dominant method, not a change in underlying harm or capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_methodology_contingency, conceptual, 'Whether the individual-right reading''s authority rests on originalist method or on substantive constitutional principle independent of method.').

omega_variable(
    founding_problem_scope_ambiguity,
    'Is the founding problem that the individual-right reading addresses the specific history of Reconstruction-era racial disarmament, or the broader principle that individuals should retain armed capacity against tyranny (state or non-state)?',
    'Historical scholarship on the Framers'' and Ratifiers'' intentions (Reconstruction Congress specifically); comparison of how contemporary gun-rights advocates invoke the founding problem (do they cite Reconstruction history or appeal to universal right to resistance?).',
    'A narrow reading (Reconstruction-specific) means the founding problem may be resolved by modern civil rights law and integrated law enforcement, which would undermine mandatrophy and create room for regulation targeted at other problems (contemporary gun violence). A broad reading (universal resistance right) makes the founding problem perpetual and justifies maximal protection of gun ownership as permanent insurance against tyranny. The authorship of the founding problem feeds into whether the reading is a durable response to a live problem or a cover story for gun-industry interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_scope_ambiguity, conceptual, 'Whether the reading''s founding problem is historically specific or universally perpetual.').

omega_variable(
    strict_scrutiny_gatekeeping,
    'Does strict scrutiny, as applied to gun regulations, function as a genuine heightened review standard testing actual means-fit-to-ends, or has it become a categorical bar that implicitly privileges gun ownership over other constitutional interests?',
    'Empirical analysis of strict-scrutiny application post-Heller: what percentage of gun regulations survive strict scrutiny (and what are their characteristics) versus what percentage fail? Comparison to strict-scrutiny outcomes in other domains (speech, religion, equal protection).',
    'If strict scrutiny is functioning as genuine high-level review, payers can craft regulations that pass (narrow, evidence-based, minimally restrictive). If it has become a bar, the constraint extracts more than the doctrine nominally permits—it forecloses entire categories of regulation regardless of fit or efficacy. Early post-Heller evidence suggests the latter; if confirmed, the ε should rise above 0.68 to account for categorical exclusion rather than high review standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_scrutiny_gatekeeping, empirical, 'Whether strict scrutiny functions as high-level meaningful review or as categorical bar on gun regulations.').

omega_variable(
    competing_constitutional_interests,
    'How should courts weigh the individual-right reading''s protection of gun ownership against the Fourteenth Amendment due-process and equal-protection interests of communities disproportionately harmed by gun violence, or the Eighth Amendment interests against cruel and unusual punishment where gun violence is weaponized by state actors?',
    'Development of constitutional doctrine that elevates the harm-side constitutional interests to equivalent weight with the property right. State constitutional amendments protecting right to safety. Litigation in state courts that balance individual-right against collective-harm interests.',
    'Currently, the individual-right reading operates with implicit constitutional primacy: individual property rights to guns are recognized; community interests in safety are treated as policy preferences subject to strict scrutiny. A doctrinal rebalancing would reduce ε by introducing a competing constitutional interest that could justify regulation even under heightened review. This is the domain where the reading faces the most organized resistance and where mandatrophy risk is sharpest (the founding problem is Reconstruction disarmament; the contemporary problem is gun homicide in communities—these are different problems with different solutions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_constitutional_interests, conceptual, 'Whether competing constitutional interests (community safety, due process, equal protection of vulnerable populations) should constrain the individual-right reading.').

omega_variable(
    kernel_reading_committer_structure,
    'Is the individual-right reading one legitimate reading of a genuinely ambiguous kernel, or is it a committer-determined reading that assumes which interpretive methodology and authority structure are legitimate?',
    'Genealogy of the reading''s adoption: did it emerge as discovery of the kernel''s true meaning, or as institutional choice to adopt originalism as the governing methodology? Analysis of which committer structure (originalist jurisprudence, gun-rights advocacy, judicial conservatism) drove the reading''s ascendance.',
    'If the reading is a legitimate discovery, its ε reflects its actual constraints and structure. If it is a committer choice, then its ε is partly a function of which institutional actors have power to enforce the reading. The question does not change the ε calculation (the engine computes from the structural data), but it reframes the reading''s authority: is it finding constitutional truth or exercising constitutional power? The same structural data (beneficiaries, victims, enforcement mechanism) is interpreted differently depending on the answer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, preference, 'Whether the individual-right reading reflects genuine kernel ambiguity or committer-determined institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__individual_right_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(seco_tr_t5, second_amendment_scope__individual_right_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__individual_right_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(seco_tr_t15, second_amendment_scope__individual_right_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__individual_right_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(seco_tr_t25, second_amendment_scope__individual_right_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__individual_right_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(seco_tr_t35, second_amendment_scope__individual_right_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__individual_right_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__individual_right_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(seco_be_t5, second_amendment_scope__individual_right_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__individual_right_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(seco_be_t15, second_amendment_scope__individual_right_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__individual_right_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(seco_be_t25, second_amendment_scope__individual_right_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__individual_right_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(seco_be_t35, second_amendment_scope__individual_right_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__individual_right_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__individual_right_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(seco_su_t5, second_amendment_scope__individual_right_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__individual_right_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(seco_su_t15, second_amendment_scope__individual_right_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__individual_right_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(seco_su_t25, second_amendment_scope__individual_right_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__individual_right_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(seco_su_t35, second_amendment_scope__individual_right_reading, suppression_requirement, 35, 0.42).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__individual_right_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, fourteenth_amendment_state_action_doctrine).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, strict_scrutiny_review_standard).

% DUAL FORMULATION NOTE:
% The second_amendment_scope kernel decomposes into three constraint stories corresponding to three competing readings: individual_right_reading (this story), collective_right_reading (militia-protection reading), and civic_right_reading (conditioned-on-service reading). Each reading instantiates a different constraint with distinct ε values, beneficiary/victim sets, and enforcement mechanisms. They share a kernel (the Amendment's text and ratification history) but produce structurally different claims about what the text requires. The individual-right reading constrains state regulatory authority most severely and produces the broadest beneficiary set (all individuals). The collective-right reading constrains the reading itself by narrowing the beneficiary set to states and militias. The civic-right reading sits between, conditioning individual benefit on civic participation. These are not different measurements of the same constraint; they are different constraints that compete for institutional authority. This story links to both sibling readings via network.affects_constraints because the individual-right reading's institutional dominance crowds out (influences or coexists with) the other readings' policy space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
