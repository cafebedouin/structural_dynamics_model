% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individualist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_individualist, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: second_amendment_text__individualist_reading
 *   human_readable: Second Amendment as Individual Right to Armed Self-Defense
 *   domain: constitutional_law/political_philosophy/gun_policy
 *
 * SUMMARY:
 *   The individualist reading of the Second Amendment constructs the right to
 *   bear arms as a pre-political natural right of self-defense independent of
 *   militia service. This reading interprets the operative clause ('the right
 *   of the people to keep and bear Arms shall not be infringed') as
 *   self-standing, with the militia clause providing historical context
 *   rather than limiting scope. Under this reading, the Second Amendment
 *   protects individual armed citizens from government disarmament and
 *   constrains regulatory authority to impose background checks, licensing,
 *   and categorical bans on civilian firearms including semi-automatic
 *   rifles. This constraint exhibits classic tangled_rope structure: it
 *   coordinates individual liberty and state constitutional boundaries
 *   (genuine coordination function) while simultaneously extracting
 *   regulatory capacity from states and private-safety costs from
 *   gun-violence-impacted communities (asymmetric extraction). The
 *   classification is stable across time horizons but
 *   perspectival—beneficiaries experience rope; victims experience snare; the
 *   analytical observer sees tangled structure. Extractiveness has risen from
 *   0.35 (1791, when armed citizenship was standard) to 0.58 (2024, as modern
 *   epidemiological data reveals aggregate harms and regulatory capacity has
 *   grown), indicating accumulating extraction as the constraint's
 *   application scope expands to modern firearms and populations.
 *
 * KEY AGENTS:
 *   - Armed Citizens: Primary beneficiaries (organized/mobile) — gain constitutional protection for firearm ownership and use; have high political mobilization capacity (NRA, state gun-owner coalitions); arbitrage options (can relocate to permissive jurisdictions if state-level regulation tightens).
 *   - Gun Manufacturers: Institutional beneficiaries (institutional/arbitrage) — access to protected domestic market; minimal regulatory constraint on product design and sales channels.
 *   - Gun Violence Targets: Primary victims (powerless/trapped) — communities experiencing elevated firearm homicide, suicide, and accident rates; no structural exit from exposed population; suppressed from regulatory remedy.
 *   - State Regulatory Authority: Secondary victims (institutional/trapped) — prevented from implementing epidemiologically justified public-health measures by constitutional constraint; incorporated via Fourteenth Amendment so cannot opt out at state level.
 *   - Gun Regulatory Advocates: Secondary victims (moderate/constrained) — activists, public-health researchers, and advocates seeking stricter regulations face constitutional ceiling; can advocate within bounds but cannot achieve full regulatory package they deem optimal.
 *   - Analytical Observer: Neutral observer (analytical/analytical) — sees the constraint as a formal boundary that coordinates individual liberty with state regulatory authority while extracting public-health options from states and safety from vulnerable populations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individualist_reading, 0.58).
domain_priors:suppression_score(second_amendment_text__individualist_reading, 0.65).
domain_priors:theater_ratio(second_amendment_text__individualist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individualist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__individualist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__individualist_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individualist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individualist_reading, "Second Amendment as Individual Right to Armed Self-Defense").
narrative_ontology:topic_domain(second_amendment_text__individualist_reading, "constitutional_law/political_philosophy/gun_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individualist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individualist_reading, 'eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02').
narrative_ontology:cs_kernel_codification('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', formalized).
narrative_ontology:cs_authority_grounding('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', lineage).
narrative_ontology:cs_interpretation_layer_present('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02').
narrative_ontology:cs_reading_relation('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', second_amendment_text__collectivist_reading, coexists_with).
narrative_ontology:cs_reading_relation('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', second_amendment_text__hybrid_scope_limitation_reading, influences).
narrative_ontology:cs_axiom('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', foundational, armed_self_defense_natural_right).
narrative_ontology:cs_axiom_status(armed_self_defense_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', armed_self_defense_natural_right, deontological).
narrative_ontology:cs_axiom('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', foundational, minimal_regulatory_categorization).
narrative_ontology:cs_axiom_status(minimal_regulatory_categorization, holdable).
narrative_ontology:cs_axiom_grounding('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', minimal_regulatory_categorization, empirically_contingent).
narrative_ontology:cs_reference_frame('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', individual_natural_right_framework).
narrative_ontology:cs_drift_state('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', contemporary_2024, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('eaf0df9e-c2e1-4ffc-b79c-ff0a5b31ab02', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individualist_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individualist_reading, armed_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_text__individualist_reading, gun_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_text__individualist_reading, self_defense_practitioners).
narrative_ontology:constraint_victim(second_amendment_text__individualist_reading, gun_violence_targets).
narrative_ontology:constraint_victim(second_amendment_text__individualist_reading, state_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_text__individualist_reading, public_safety_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GUN VIOLENCE TARGETS (SNARE) — Communities experiencing elevated firearm homicide, suicide, and accident rates have no structural exit from the armed civilian population. Suppression is enforced through constitutional immunity from regulatory remedies. Maximum extraction: targets bear mortality costs; armed citizens extract freedom from regulation.
constraint_indexing:constraint_classification(second_amendment_text__individualist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GUN REGULATORY ADVOCATES (TANGLED ROPE) — State and local actors seeking to regulate firearms face constitutional barriers (constrained exit). Yet they also benefit from the individualist reading's coordination function: clarity on the constitutional floor enables rational policy design within constitutional bounds. Some extraction (ability to regulate capped by Second Amendment interpretation) mixed with genuine coordination benefit (knowing the boundary enables planning).
constraint_indexing:constraint_classification(second_amendment_text__individualist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ARMED CITIZENS (ROPE) — Beneficiaries of the individualist reading with organized mobilization capacity (NRA, gun owner coalitions). Experience the constraint as pure coordination: individual ownership rights protected against regulatory encroachment. Low effective extraction because beneficiaries have political power and exit options (mobility: can relocate to permissive jurisdictions).
constraint_indexing:constraint_classification(second_amendment_text__individualist_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE REGULATORY AUTHORITY (TRAPPED) — States and municipalities cannot exit the constitutional constraint. Suppression enforced through Supremacy Clause and Fourteenth Amendment incorporation. States bear extraction: prevented from implementing public-health regulations that would rationally follow from their epidemiological data. The institutional actor is structurally trapped despite organizational capacity.
constraint_indexing:constraint_classification(second_amendment_text__individualist_reading, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: GUN MANUFACTURERS (ROPE) — Institutional beneficiaries with arbitrage options (can operate in U.S. or shift to permissive international markets). Experience the constraint as coordination: the individualist reading creates a large protected domestic market for civilian firearms, removing the extraction mechanism for these actors. Net beneficiary from access to constrained regulatory regime.
constraint_indexing:constraint_classification(second_amendment_text__individualist_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a long-historical perspective, the individualist reading creates genuine coordination (clarity on constitutional protections for citizens) while enabling extraction (regulatory exclusion for public-health measures). The constraint is neither pure law nor pure policy—it is a formal boundary that channels regulation into specific pathways while forbidding others. The analytical position sees the tangled structure as designed: the Framers intended to protect a specific right while leaving other regulatory paths open.
constraint_indexing:constraint_classification(second_amendment_text__individualist_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individualist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_text__individualist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_text__individualist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individualist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individualist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε = 0.58): Moderate-high. The individualist reading benefits armed citizens and gun manufacturers by protecting civilian firearm ownership from regulatory restriction. It extracts regulatory capacity from states and safety-access from gun-violence-impacted communities. The extraction is not as high as snare (0.66+) because genuine coordination exists — the constraint provides legal clarity on constitutional boundaries, enabling rational policy design within permitted ranges. The value reflects that extractiveness has accumulated over time (0.35→0.58) as modern firearms and population densities enable higher aggregate harm than in 1791. Suppression (0.65): High. The constraint enforces suppression through constitutional immunity—states cannot regulate; victims cannot exit the armed civilian population; the Fourteenth Amendment incorporation removes federalism escape hatches. Suppression is structural (legal doctrine) rather than merely practical. Theater ratio (0.48): Moderate. Some judicial activity is performative (litigation that reaffirms existing doctrine without changing outcomes), but much is substantive—McDonald v. Chicago (2010) and New York State Rifle & Pistol Association v. Bruen (2022) represent genuine doctrinal shifts that expanded or clarified protections. The constraint is less theatrical than regulatory capture because the boundary is formally codified in constitutional text.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a marked gap between beneficiary and victim perspectives. Armed citizens (organized/arbitrage) classify the constraint as pure coordination (rope)—they gain clarity on their rights and face low effective extraction because they have political power and can relocate. Gun violence targets (powerless/trapped) classify it as pure extraction (snare)—they cannot escape the armed civilian population or obtain the regulatory protections they seek, and they bear the mortality cost. The state regulatory authority (institutional/trapped) experiences mixed extraction and coordination at tangled_rope—they can coordinate within the constitutional boundary but are barred from the full policy menu they would choose. The gap reveals the constraint's core function: it transfers regulatory authority from democratically accountable state officials (who would likely choose stricter rules) to constitutional doctrine (which protects individual rights). This transfer is experienced as liberation by beneficiaries and as oppression by victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position: armed citizens as beneficiaries with arbitrage exit options yield low d (0.15–0.20); gun violence targets as victims with trapped exit yield high d (0.90–0.95); state regulatory actors as institutional victims with trapped exit yield high d (0.88–0.92); regulatory advocates as moderate victims with constrained exit yield moderate-high d (0.65–0.75). The beneficiary-side agents experience the constraint as coordination (rope classification, effective χ is negative or low); the victim-side agents experience it as extraction (snare or tangled-rope classification, effective χ is high). The analytical observer positions itself at d=0.72 (observer distance), experiencing the tangled structure as designed rather than as natural law or as pure extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the tangled_rope classification is structurally stable: the coordination function (clarifying constitutional boundaries) is genuine and serves the constitutional system; the extraction function (preventing regulatory responses to public-health data) is also genuine and asymmetric. Neither function dominates or negates the other. The beneficiary's rope perspective is not an error—it reflects their accurate structural position. The victim's snare perspective is also accurate—trapped agents without voice in the constraint's design experience it as extraction. The mandatrophy is not 'which perspective is correct?' but 'what is the constraint doing simultaneously?'—coordinating individual liberty against majority regulation AND extracting public-health authority from democratically accountable institutions. The tangled classification is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_positive_construction,
    'Is the individual right to bear arms a pre-political natural right, or is it a positive right constructed and delimited by the Constitution itself?',
    'Historical-philosophical analysis of Framers'' intent; comparison with pre-1776 English law on individual firearm ownership; examination of whether contemporary legal scholars treat the right as pre-constitutional or as created by constitutional text.',
    'If natural right: the individualist reading''s foundational axiom (armed_self_defense_natural_right) is deontological and unchallengeable by regulation. If constructed: the right is only as broad as the Constitution defines, permitting narrower readings and stronger regulatory authority. Classification remains tangled_rope either way, but the omega resolution determines whether suppression manifests as natural law or policy choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_right_vs_positive_construction, conceptual, 'Whether right is pre-political natural right or constitutionally constructed').

omega_variable(
    militia_clause_semantic_scope,
    'Does the militia clause (''A well regulated Militia being necessary to the security of a free State'') logically scope or merely provide context for the operative clause (''the right of the people to keep and bear Arms shall not be infringed'')?',
    'Linguistic analysis of 18th-century legal syntax; comparative reading with other constitutional provisions; Supreme Court precedent evolution (Heller vs. DC; McDonald vs. Chicago; Bruen). Historical practice re: individual ownership rights pre- and post-ratification.',
    'If militia clause scopes the operative clause: only militia-service-related arms are protected, and collectivist_reading becomes structurally viable as equally valid interpretation (coexists_with relation downgraded to different epistemic status). If context-only: individualist reading has stronger textual basis and forecloses collectivist reading more cleanly. The empirical research (how was the right understood by ratifiers?) resolves the reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_clause_semantic_scope, empirical, 'Whether militia clause logically scopes operative clause or provides contextual grounding').

omega_variable(
    regulatory_floor_determination,
    'What types and categories of firearms are logically protected under ''arms'' in the operative clause? Are semi-automatic rifles, handguns, and high-capacity magazines protected, or only categories in common use at ratification (muskets)?',
    'Originalist methodology (what arms existed in 1791?); living-constitution methodology (what arms are in common use today?); comparative-law analysis of how other jurisdictions with similar natural-rights provisions resolve analogous questions. Empirical data on arms in common use (NICS data, manufacturer production figures).',
    'If only 1791-era arms protected: extractiveness drops to ~0.25 (mountain-adjacent), broad regulation permitted, victims'' suppression weakens. If contemporary common-use arms protected: extractiveness rises to ~0.72 (snare), minimal regulation permitted, victims'' suppression hardens. If spectrum with vague boundary: extractiveness remains ~0.58 (tangled_rope), litigation-driven boundary-drawing creates ongoing extraction from regulatory actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_floor_determination, empirical, 'Which categories of firearms are protected by Second Amendment').

omega_variable(
    incorporationist_pathway_stability,
    'Is the Fourteenth Amendment incorporation of the Second Amendment to the states (McDonald v. Chicago, 2010) as firmly settled as incorporation of First Amendment protections, or is it vulnerable to future doctrinal reversal?',
    'Historical analysis of incorporation doctrine precedent; examination of contemporary Supreme Court composition and originalist jurisprudence toward incorporation; comparison with states'' pre-McDonald regulatory environments and litigation volume.',
    'If firmly settled: states remain structurally trapped, suppression is stable at 0.65, constraint is durable. If vulnerable: states have strategic hope for reversal via litigation, exit options upgrade from trapped to constrained, suppression may be performative (theater_ratio rises). If reversal occurs: this reading''s constraint dissolves or fragments into state-level constraints, structure shifts to distributed authority (collectivist_reading becomes operative in reversal scenario).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incorporationist_pathway_stability, empirical, 'Whether Fourteenth Amendment incorporation of Second Amendment is stable').

omega_variable(
    self_defense_scope_versus_insurrection_scope,
    'Does the individualist reading''s ''armed self-defense'' scope include preparation for insurrection/armed resistance to tyranny (a theme in some Framers'' writings), or is it strictly personal/home/street-level self-defense?',
    'Textual analysis of Framers'' writings (Jefferson, Madison, Federalist Papers); case law treating political violence and militia context; contemporary judicial treatment of ''insurrectionary purpose'' as limiting factor (e.g., 18 U.S.C. § 2331 definitions). Empirical study of how armed citizen groups interpret their own role (constitutional militia, personal defense, or political resistance).',
    'If insurrectionary scope included: beneficiaries expand to include armed political movements, victims include state monopoly on force and political authority, extractiveness potentially rises above 0.58 (approaches snare), suppression manifests as anti-insurrection law, constraint becomes explicitly political rather than individualist-neutral. If personal-defense-only: extractiveness stable at 0.58, constraint maintains framing as neutral legal boundary, insurrectionary reading is foreclosed or relegated to alternative kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_defense_scope_versus_insurrection_scope, conceptual, 'Scope of armed self-defense: personal only or including political insurrection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individualist_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_ind_theater_1791, second_amendment_text__individualist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sa_ind_theater_1868, second_amendment_text__individualist_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(sa_ind_theater_1980, second_amendment_text__individualist_reading, theater_ratio, 2, 0.42).
narrative_ontology:measurement(sa_ind_theater_2024, second_amendment_text__individualist_reading, theater_ratio, 3, 0.48).

% Extraction over time
narrative_ontology:measurement(sa_ind_extract_1791, second_amendment_text__individualist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sa_ind_extract_1868, second_amendment_text__individualist_reading, base_extractiveness, 1, 0.42).
narrative_ontology:measurement(sa_ind_extract_1980, second_amendment_text__individualist_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(sa_ind_extract_2024, second_amendment_text__individualist_reading, base_extractiveness, 3, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sa_ind_supp_1791, second_amendment_text__individualist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sa_ind_supp_1868, second_amendment_text__individualist_reading, suppression_requirement, 1, 0.48).
narrative_ontology:measurement(sa_ind_supp_1980, second_amendment_text__individualist_reading, suppression_requirement, 2, 0.62).
narrative_ontology:measurement(sa_ind_supp_2024, second_amendment_text__individualist_reading, suppression_requirement, 3, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individualist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__individualist_reading, second_amendment_text__collectivist_reading).
narrative_ontology:affects_constraint(second_amendment_text__individualist_reading, second_amendment_text__hybrid_scope_limitation_reading).
narrative_ontology:affects_constraint(second_amendment_text__individualist_reading, gun_regulation_constitutional_scope).
narrative_ontology:affects_constraint(second_amendment_text__individualist_reading, state_public_health_firearm_authority).

% DUAL FORMULATION NOTE:
% The Second Amendment text constitutes a single kernel with multiple structural readings. This constraint (individualist_reading) is one of three readings, each with different extractiveness and structural implications. The sibling constraints (collectivist_reading, hybrid_scope_limitation_reading) instantiate the same constitutional text under different interpretations and should be compiled as separate stories. All three readings are linked via affects_constraints to enable comparative analysis of how interpretation choices map to constraint type, extractiveness, and downstream structural effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__individualist_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
