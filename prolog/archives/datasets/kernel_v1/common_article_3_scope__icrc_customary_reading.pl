% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: CA3 Scope Determination via Customary International Law Evolution
 *   domain: international_humanitarian_law/customary_law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions establishes baseline
 *   humanitarian protections in non-international armed conflicts. The
 *   constraint examined here is the **scope determination mechanism** — the
 *   process by which CA3's substantive reach is defined and evolved. This is
 *   not the substantive rule itself ("treatment of wounded") but the
 *   institutional question: **who decides what counts as an armed conflict
 *   subject to CA3, and how can that scope change?** The ICRC customary
 *   reading instantiates one answer: scope is determined by patterns of state
 *   practice and opinio juris, documented and interpreted by the ICRC as the
 *   principal humanitarian custodian. This reading allows scope evolution
 *   without formal treaty amendment, creating incremental humanitarian
 *   expansion. However, it also migrates scope-setting authority from
 *   transparent state-sovereign treaty amendment processes to the ICRC's
 *   institutional interpretation of customary law patterns. The constraint
 *   exhibits high extractiveness accumulation (0.18→0.38 over 60 years)
 *   reflecting growing ICRC authority, rising theater ratio (0.35→0.55)
 *   reflecting formal amendment process becoming increasingly ceremonial, and
 *   moderate suppression reflecting that states retain formal consent
 *   authority while substantive authority shifts.
 *
 * KEY AGENTS:
 *   - ICRC Custodian Authority: Institutional beneficiary (institutional/arbitrage) — derives expanding authority to interpret CA3 scope through customary law documentation
 *   - Non-State Armed Groups: Primary victim (powerless/trapped) — face expanding CA3 obligations through ICRC scope determinations they cannot participate in or formally contest
 *   - States Resistant to Expansion: Secondary victim (moderate/constrained) — constrained by customary law evolution they did not initiate; can contest diplomatically but at cost
 *   - Progressive Human Rights Coalitions: Secondary beneficiary (organized/mobile) — use customary law mechanism as bridge toward formal treaty expansion; see mechanism as temporary (scaffold with sunset)
 *   - Formal Treaty Amendment Process: Institutional actor (institutional/arbitrage) — retains ceremonial authority while functional authority migrates to ICRC customary readings (piton)
 *   - Analytical Observer: Civilizational scope (analytical/analytical) — observes that the legitimacy and the extraction are inseparable; the mechanism works precisely because it expands humanitarian protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.38).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.42).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "CA3 Scope Determination via Customary International Law Evolution").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law/customary_law").

domain_priors:requires_active_enforcement(common_article_3_scope__icrc_customary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '149df139-1e1b-419a-837e-e874073c8fd2').
narrative_ontology:cs_kernel_codification('149df139-1e1b-419a-837e-e874073c8fd2', fixed_text).
narrative_ontology:cs_authority_grounding('149df139-1e1b-419a-837e-e874073c8fd2', lineage).
narrative_ontology:cs_interpretation_layer_present('149df139-1e1b-419a-837e-e874073c8fd2').
narrative_ontology:cs_reading_relation('149df139-1e1b-419a-837e-e874073c8fd2', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('149df139-1e1b-419a-837e-e874073c8fd2', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('149df139-1e1b-419a-837e-e874073c8fd2', foundational, customary_law_determination_authority).
narrative_ontology:cs_axiom_status(customary_law_determination_authority, holdable).
narrative_ontology:cs_axiom_grounding('149df139-1e1b-419a-837e-e874073c8fd2', customary_law_determination_authority, conventional).
narrative_ontology:cs_axiom('149df139-1e1b-419a-837e-e874073c8fd2', foundational, state_practice_opinio_juris_sufficiency).
narrative_ontology:cs_axiom_status(state_practice_opinio_juris_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('149df139-1e1b-419a-837e-e874073c8fd2', state_practice_opinio_juris_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('149df139-1e1b-419a-837e-e874073c8fd2', state_sovereign_treaty_gatekeeping).
narrative_ontology:cs_drift_state('149df139-1e1b-419a-837e-e874073c8fd2', contemporary_institutional_custodian_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('149df139-1e1b-419a-837e-e874073c8fd2', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc_institutional_authority).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, progressive_humanitarian_interpretation).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, state_sovereignty_gatekeeping).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, formal_treaty_amendment_process).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-STATE ARMED GROUPS (SNARE) — Trapped by an expanding scope determination mechanism they cannot participate in or contest. ICRC customary law readings unilaterally expand their obligations without formal consent or exit option. No voice in opinio juris evolution; bears full cost of scope creep.
constraint_indexing:constraint_classification(common_article_3_scope__icrc_customary_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STATES RESISTANT TO EXPANSION (TANGLED ROPE) — Constrained by customary law evolution they did not initiate but cannot formally block. Benefit from CA3 when they are victims/third parties; bear costs when forced to expand internal conflict obligations. Limited exit (can contest opinio juris but at diplomatic cost).
constraint_indexing:constraint_classification(common_article_3_scope__icrc_customary_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ICRC INSTITUTIONAL CUSTODIAN (ROPE) — Primary beneficiary of the customary reading mechanism. Derives authority to interpret and extend CA3's scope without formal amendment. Experiences the constraint as legitimate coordination: documenting practice enables incremental humanization. High agency, high exit options (can adjust readings to maintain legitimacy).
constraint_indexing:constraint_classification(common_article_3_scope__icrc_customary_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE HUMAN RIGHTS COALITIONS (SCAFFOLD) — Organized actors (NGOs, academics, some states) using customary law evolution as a temporary bridge toward formal treaty expansion. See the mechanism as a sunset clause — as jus cogens and erga omnes principles mature, formal treaty amendment becomes possible. Significant agency and exit options.
constraint_indexing:constraint_classification(common_article_3_scope__icrc_customary_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL TREATY AMENDMENT PROCESS (PITON) — Cumbersome international treaty amendment mechanism (requires state consensus, diplomatic conference, ratification). Persists formally as the legitimate scope-setting authority but is substantively bypassed by customary law evolution. Theater ratio high: states maintain ceremonial treaty amendment power while ICRC authority grows through customary readings. Institutional inertia preserves formal authority's legitimacy even as functional authority migrates.
constraint_indexing:constraint_classification(common_article_3_scope__icrc_customary_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — At civilizational scope, the customary law mechanism serves both coordination (incremental humanitarian protection) and extraction (institutional authority shift from state-sovereign treaty process to ICRC-custodian opinio juris determination). The mechanism is legitimate precisely because it coordinates humanitarian expansion; the extraction is legitimate precisely because it expands protection. No authority outside the system to adjudicate the tradeoff — the legitimacy and the extraction are the same mechanism.
constraint_indexing:constraint_classification(common_article_3_scope__icrc_customary_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(common_article_3_scope__icrc_customary_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(common_article_3_scope__icrc_customary_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, TR),
    TR >= 0.70.

:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The primary extraction is the institutional authority shift from state-sovereign treaty amendment to ICRC custodian interpretation. This is not malicious — it is a coordination response to the structural problem that formal amendment requires near-consensus while conflicts evolve faster than treaties can. However, the shift does extract authority from the formal democratic treaty process and concentrates it in an institutional interpreter. The measurement trajectory (0.18→0.38) reflects this accumulation: in 1949, the ICRC had limited scope-setting authority; by 2009, the ICRC's opinio juris documentation effectively determined CA3's reach in new conflict types. Suppression (0.42): Moderate. States retain formal treaty amendment power but cannot easily use it (consensus barrier). ICRC scope determinations are difficult to contest formally and carry humanitarian legitimacy that makes public opposition costly. However, suppression is not absolute — states can and do object to specific ICRC readings, and the mechanism is transparent (state practice is documented, not hidden). Theater ratio (0.55): Moderate-high, rising. The formal treaty amendment process persists (states maintain ceremonial authority) but is substantively bypassed. Additional Protocols (AP I, AP II, AP III) have succeeded, showing amendment is not impossible, but their success was rare and required decades. Most scope evolution now occurs through ICRC customary law interpretations, making the formal amendment process increasingly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   This reading (customary opinio juris) and the state_centric reading (formal consent only) **coexist_with** each other, not foreclose. A state can legitimately hold the position that CA3 scope is determined by formal treaty amendment (state-centric view) while other states accept customary law evolution (ICRC customary view). No single framework mandates rejecting the other; they are live competing positions in current state practice. This reading **influences** the expansive_human_rights reading — ICRC customary determinations create conditions that make human rights integration more attractive (if custom can evolve, why not faster?), but customary law does not foreclose human rights integration. Rather, it creates structural pressure toward it by showing that treaty scope is negotiable through institutional practice-documentation.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC custodian (institutional/arbitrage) derives d ≈ 0.05–0.15 — they are beneficiaries with the option to adjust readings to maintain legitimacy; f(d) is near zero or slightly negative, producing negative effective extraction (they see coordination benefit, not cost). States resistant to expansion (moderate/constrained) derive d ≈ 0.55–0.70 — they are secondary victims unable to easily exit; f(d) ≈ 0.75–1.15, producing moderate effective extraction. Non-state armed groups (powerless/trapped) derive d ≈ 0.95 — they are primary victims with no exit; f(d) ≈ 1.42, producing maximum experienced extraction. The scope modifier σ(global) = 1.2 amplifies extractiveness at global scope — CA3 scope determinations affect conflicts worldwide, making the institutional authority shift consequential for all armed conflict contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is: Does this mechanism coordinate humanitarian expansion (legitimate), or does it extract authority from democratic state processes (problematic)? The reading resolves this by showing that both are true simultaneously — the mechanism works BECAUSE it expands humanitarian protection AND BECAUSE it shifts authority. There is no way to get the coordination function without the extraction. The mechanism is not malicious institutional aggrandizement; it is a rational response to the structural constraint that formal treaty amendment is consensus-dependent while conflicts evolve faster. The resolution is not to dissolve the tension but to recognize it as built-in: as long as scope expansion is urgent and consensus is impossible, some institutional authority migration is the price of humanitarian progress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opinio_juris_verification_threshold,
    'How many state practice instances and how clear must opinio juris declarations be to constitute customary law on CA3 scope?',
    'Formal comparative survey of state practice documentation across 30+ states; coding of explicit vs implicit opinio juris; comparison with ICJ precedent on custom formation',
    'If threshold is low (opinio can be inferred): scope expansion can occur with sparse evidence, ICRC authority high. If threshold is high (opinio requires explicit agreement): customary law pathway is slow and requires near-consensus, state gatekeeping power restored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opinio_juris_verification_threshold, empirical, 'Customary law threshold for scope determination').

omega_variable(
    state_participation_asymmetry,
    'Do weaker/non-aligned states have proportional voice in opinio juris formation, or does practice-setting disproportionately reflect strong-state behavior?',
    'Longitudinal analysis of whose practice counts: do Global North military doctrines drive opinio juris more than Global South actual conduct? Statistical correlation between state military power and customary law recognition.',
    'If asymmetric: customary law mechanism reproduces great-power dominance under humanitarian framing; actual extraction mechanism is power-law filtering of whose practice counts. If proportional: mechanism approaches ideal coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_participation_asymmetry, empirical, 'State power asymmetry in customary law formation').

omega_variable(
    alternative_kernel_reading_relationship,
    'What is the structural relationship between this reading (opinio juris as procedural mechanism) and the state-centric reading (scope determined by formal state consent only)?',
    'Documentary analysis of state objections to ICRC scope determinations; comparison of states that contest customary reading vs those that accept it; identification of whether rejection of customary reading logically precludes the formal-consent reading or merely competes with it.',
    'If forecloses: one reading''s acceptance logically requires rejecting the other. If coexists_with: both readings remain live in different state practices/positions. If influences: this reading creates pressure on the other without logically excluding it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_kernel_reading_relationship, conceptual, 'Relationship between customary and formal-consent readings of CA3 scope').

omega_variable(
    icrc_institutional_accountability,
    'What institutional mechanisms exist to contest or override ICRC customary law determinations?',
    'Survey of precedent: instances where states or other actors successfully challenged ICRC scope readings; ICJ rulings on ICRC opinio juris authority; availability of formal appeal or reversal processes.',
    'If accountability is weak: ICRC derives near-unilateral authority to reinterpret CA3; extraction mechanism is institutional (institutional authority growth). If accountability exists: ICRC authority is constrained; mechanism resembles coordination with built-in oversight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icrc_institutional_accountability, empirical, 'Institutional accountability for ICRC scope determinations').

omega_variable(
    formal_treaty_amendment_viability,
    'Is formal treaty amendment realistically available as an alternative to customary law evolution, or has consensus-based state amendment become structural impossible?',
    'Historical record: how many CA4/AP amendments have succeeded since 1949? Diplomatic analysis of whether full consensus is achievable on modern conflict types. Comparison with other treaties that have successfully amended vs those that resort to customary evolution.',
    'If formal amendment is viable: customary law pathway is unnecessary; ICRC choice to use it is institutional expansion. If amendment is effectively impossible: customary law is the only available coordination mechanism for scope evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_treaty_amendment_viability, empirical, 'Viability of formal treaty amendment as alternative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_icrc_theater_1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ca3_icrc_theater_1979, common_article_3_scope__icrc_customary_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(ca3_icrc_theater_2009, common_article_3_scope__icrc_customary_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(ca3_icrc_extract_1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ca3_icrc_extract_1979, common_article_3_scope__icrc_customary_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(ca3_icrc_extract_2009, common_article_3_scope__icrc_customary_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ca3_icrc_suppress_1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ca3_icrc_suppress_1979, common_article_3_scope__icrc_customary_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(ca3_icrc_suppress_2009, common_article_3_scope__icrc_customary_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, ca3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, ca3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, icrc_institutional_authority_structure).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, non_international_armed_conflict_definition).

% DUAL FORMULATION NOTE:
% The CA3 scope kernel decomposes into three constraint stories reflecting three distinct readings with different ε values. The ICRC customary reading (ε=0.38) focuses on the procedural mechanism allowing incremental expansion. The state-centric reading (estimated ε=0.25) focuses on formal treaty gatekeeping. The expansive reading (estimated ε=0.52) focuses on pushing scope beyond customary limits into human rights integration. All three link back to the underlying kernel and affect the institutional authority structures and conflict-type definitions that depend on CA3 scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__icrc_customary_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
