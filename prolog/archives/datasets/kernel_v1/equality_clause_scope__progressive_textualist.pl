% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope Under Progressive Textualism
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The equality clause scope constraint presents a constitutional design
 *   problem: a founding text that guarantees equal protection but applies
 *   narrowly (originally to a limited demographic) must expand to reach
 *   excluded groups. The progressive textualist reading holds that this
 *   expansion is legitimate when achieved through formal democratic amendment
 *   (13th, 14th, 15th, 19th Amendments, later Civil Rights Act leverage) but
 *   NOT when achieved through unilateral judicial reinterpretation. This
 *   creates a tangled rope: the amendment process genuinely coordinates
 *   democratic will (rope function) while systematically delaying equal
 *   protection for excluded groups (extraction). The constraint's
 *   extractiveness has declined over the observation period (0.52 → 0.38) as
 *   formal amendments accumulated and statutory frameworks (Civil Rights Act,
 *   Voting Rights Act) provided faster-path expansions. Theater ratio has
 *   risen (0.25 → 0.68) as courts perform amendment-equivalent work through
 *   living constitutionalism interpretation, creating performative legitimacy
 *   without formal process. Suppression has declined (0.68 → 0.45) as the
 *   amendment process itself has become more accessible and political
 *   consensus for expansion has solidified.
 *
 * KEY AGENTS:
 *   - Historically Excluded Groups: Primary victims (powerless/trapped) — denied equal protection by narrow text scope; require supermajority political consensus to gain coverage
 *   - Progressive Reform Movements: Secondary actor (organized/constrained) — mobilize across generations for amendments; benefit from democratic legitimacy but extract through temporal delay
 *   - Constitutional Authority (Legislature): Primary beneficiary (institutional/arbitrage) — controls amendment process; benefits from power to define scope of equality through supermajority requirement
 *   - Intermediate-Rights Claimants: Secondary victim/beneficiary (moderate/constrained) — experience mixed: coordination benefit from stable constitutional framework, extraction from delayed access
 *   - Judicial Interpretation System: Institutional actor (institutional/arbitrage) — performs amendment-like functions through living constitutionalism; experiences constraint as degraded/theatrical
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing the amendment bottleneck as inherent constitutional law rather than contingent political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.38).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.45).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope Under Progressive Textualism").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, 'cb6cf61a-fb22-449e-96c4-9a56b95a0c06').
narrative_ontology:cs_kernel_codification('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', fixed_text).
narrative_ontology:cs_authority_grounding('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', lineage).
narrative_ontology:cs_interpretation_layer_present('cb6cf61a-fb22-449e-96c4-9a56b95a0c06').
narrative_ontology:cs_reading_relation('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', foundational, text_scope_is_fixed_at_ratification).
narrative_ontology:cs_axiom_status(text_scope_is_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', text_scope_is_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', foundational, scope_expansion_requires_supermajority_democratic_consent).
narrative_ontology:cs_axiom_status(scope_expansion_requires_supermajority_democratic_consent, holdable).
narrative_ontology:cs_axiom_grounding('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', scope_expansion_requires_supermajority_democratic_consent, conventional).
narrative_ontology:cs_reference_frame('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', original_text_with_amendment_pathway).
narrative_ontology:cs_drift_state('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', contemporary_post_civil_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cb6cf61a-fb22-449e-96c4-9a56b95a0c06', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, constitutional_legitimacy).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, democratic_amendment_process).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, historically_excluded_groups).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, judicial_parity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICALLY EXCLUDED GROUPS — Trapped by the original text's narrow coverage (e.g., equality guarantees written for white male citizens). Exit requires supermajority amendment process, which is deliberately costly. The constraint extracts exclusion at biographical scale with no exit option — full structural entrapment. Maximum experienced extraction.
constraint_indexing:constraint_classification(equality_clause_scope__progressive_textualist, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROGRESSIVE REFORM MOVEMENTS — Organized agents coordinating amendment campaigns across generations (13th, 14th, 15th, 19th Amendments, Civil Rights Act leverage). The constraint coordinates genuine political mobilization (genuine rope function) while extracting enormous mobilization costs and temporal delays (victims delay entry by generations). Mixed: both coordination and asymmetric extraction. Constrained exit — can pursue amendments but at high mobilization cost and uncertain outcome.
constraint_indexing:constraint_classification(equality_clause_scope__progressive_textualist, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL AUTHORITY (LEGISLATURE) — Institutional actor with formal amendment power. Experiences the constraint as pure coordination: the amendment process organizes political will, aggregates preferences, and generates supermajority consensus. The legislative body benefits from the legitimacy that comes from democratic amendment. Net beneficiary — structured as coordination with institutional capture of amendment authority.
constraint_indexing:constraint_classification(equality_clause_scope__progressive_textualist, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERMEDIATE-RIGHTS CLAIMANTS — Agents whose eligibility expanded during living memory (women post-1920, racial minorities post-1960s, LGBTQ+ post-2010s). Experience the constraint as mixed: genuine coordination benefit (stable constitutional framework attracts them) plus extraction (delayed access by generations prior). Constrained exit — can advocate for amendment but must work within the system; exit to alternative legal order costly.
constraint_indexing:constraint_classification(equality_clause_scope__progressive_textualist, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL INTERPRETATION SYSTEM — Courts initially interpreting equality clause according to text and original scope, later applying 'living constitutionalism' without formal amendment. This perspective sees the constraint as degraded: the court performs amendment-like work (expanding coverage) through interpretation rather than the formal amendment process. Theater-high because courts mime democratic legitimacy while wielding unilateral power. Piton classification reflects that judicial expansion, though real-world effective, lacks the procedural legitimacy of formal amendment.
constraint_indexing:constraint_classification(equality_clause_scope__progressive_textualist, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — From a civilizational universalist position, some constraints on equal protection are inherent to written constitutionalism: fixed text always lags behind moral understanding, and the gap between text and expanding consciousness is an immutable structural feature of any written law. This perspective naturalizes the amendment bottleneck as lawlike. However, the structural data reveals false summitry: the supermajority requirement is a deliberate institutional choice, not a law of nature.
constraint_indexing:constraint_classification(equality_clause_scope__progressive_textualist, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equality_clause_scope__progressive_textualist, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equality_clause_scope__progressive_textualist, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, TR),
    TR >= 0.70.

:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The progressive textualist reading permits expansion but only through supermajority democratic process, delaying equal protection for excluded groups by generational timescales. This is extraction (temporal delay in rights realization) but bounded by legitimacy: the supermajority requirement is democratically proceduralized, not arbitrary. Over the 100-year interval, extractiveness declined as formal amendments (13th, 14th, 15th, 19th) and statutory frameworks (Civil Rights Act 1964, Voting Rights Act 1965) provided expansion pathways, reducing the gap between text and coverage. Suppression (0.45): Moderate. The supermajority amendment requirement creates high barriers (requires consensus across diverse regions, ideological camps, generational cohorts). But suppression is not total — formal paths exist (amendments have been ratified), coalition-building is possible, and alternative statutory paths (federal civil rights statutes passed by simple majority) provide partial bypasses. Theater ratio (0.52): Moderate. The constraint exhibits mixed functionality: genuine democratic coordination (amendment process aggregates preferences supramajoratarily) coexists with performative theater (courts interpreting away the supermajority requirement through living constitutionalism, achieving expansion without formal amendment). The rising theater reflects that judicial expansion has increasingly substituted for amendment, creating the appearance of legitimacy without the procedural substance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows divergent classification across structural positions. Historically excluded groups at the time of narrow text ratification experience Snare (trapped, no exit). Progressive reform movements experience Tangled Rope (mixed: genuine coordination benefit from supermajority process, extraction cost in temporal delay). The legislature experiences Rope (pure coordination; benefits from amendment authority). Intermediate claimants (whose eligibility expanded mid-century) experience Tangled Rope (mixed benefit and extraction). Courts experience Piton (genuine amendment work accomplished, but through increasingly performative interpretation rather than formal process — the functional outcome is achieved but the procedural legitimacy decays). The analytical observer risks Mountain (naturalizing the amendment bottleneck as inherent constitutional law). The perspectival range spans four types (Snare, Tangled Rope, Rope, Piton) plus one false summit (Mountain), revealing that the same constraint is legitimately experienced as extraction by some agents and coordination by others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Historically excluded groups (time 0): full victims, trapped exit → d ≈ 0.95 → high χ. Progressive reform movements (generational): partial beneficiaries (build momentum, accumulate constitutional precedent), but constrained exit (limited to political coalition-building) → d ≈ 0.55 → moderate χ. Legislature (institutional, arbitrage exit): beneficiary (controls amendment process, gains legitimacy from democratic procedure) → d ≈ 0.10 → low/negative χ. Intermediate claimants (moderate power, constrained exit): both beneficiary (inherit expanded coverage from prior movements) and victim (lived through delay period) → d ≈ 0.50 → symmetric χ. Judicial system (institutional, arbitrage): institutional beneficiary (gains authority from living constitutionalism interpretation) → d ≈ 0.15 → low χ. Analytical observer (analytical context): structural observer → canonical d ≈ 0.72 → moderate χ, classifying Mountain at universal/civilizational scope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by holding that the tangled rope classification (mixed coordination and extraction) is the stable truth at most contexts. The amendment process genuinely coordinates political will (legitimate rope function) while systematically delaying equal protection for excluded groups (real extraction). The piton classification for courts reflects institutional inertia, not misclassification: courts have taken on amendment-equivalent work through living constitutionalism, and this work is performatively legitimized. The false summit (mountain perspective) is explicitly marked — the constraint is not a law of nature but a contingent institutional design. Resolution: this reading's claim is that the design is democratically defensible (supermajority is a legitimate legitimacy gate) even though it extracts temporal costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    progressive_vs_original_intent,
    'Does the progressive textualist reading genuinely hold the original text fixed while permitting expansive amendment, or does it subtly privilege judicial interpretation that achieves the effect of amendment without formal process?',
    'Historical analysis of 20th-century judicial expansion (e.g., Warren Court) vs. formal amendment rate; comparison of constitutional coverage achieved through amendment vs. judicial decree; tracking whether judicial expansions survive when supermajority political consensus is absent.',
    'If progressive textualism truly constrains to formal amendment: this reading is structurally distinct from expansive_universalist and coexists with it (both live, different premises). If judicial expansion routinely substitutes for amendment: progressive textualism collapses toward expansive_universalist functionally (same outcome, different legitimacy narrative), suggesting influence rather than coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(progressive_vs_original_intent, empirical, 'Whether progressive textualism maintains amendment-process constraint in practice or drifts toward judicial universalism').

omega_variable(
    supermajority_threshold_legitimacy,
    'What makes a supermajority amendment requirement democratically legitimate rather than an anti-majoritarian entrenchment device?',
    'Political philosophy analysis of amendment ratification in comparative constitutionalism; historical outcomes showing how often supermajority requirements reflect vs. frustrate sustained popular will; empirical tracking of whether amendments align with repeated electoral majorities.',
    'If supermajority requirement reflects genuine constitutional convention with durable consensus: this reading coordinates legitimacy (Rope from legislative perspective). If supermajority requirement systematically frustrates electoral majorities: the constraint extracts disproportionate power from veto-coalition members (shifts toward Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajority_threshold_legitimacy, conceptual, 'Democratic legitimacy of supermajority amendment requirement').

omega_variable(
    unamended_text_moral_authority,
    'When the text speaks to equality but application scope is narrow, which interpretation honors the text: holding to narrow application or reading the principle expansively to cover historical exclusions?',
    'Textual analysis of equality clauses in multiple constitutions; historical study of drafter intentions vs. written scope; philosophical debate on whether principles (equality) outrank specific applications (who counts) in constitutional hierarchy.',
    'If text-as-principle dominates: progressive expansionism is the true textualist position, undermining this reading''s core premise that amendment is required for expansion. If text-as-specific-application dominates: this reading''s constraint is accurate, and judicial expansion is extra-textual overreach (Piton classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unamended_text_moral_authority, conceptual, 'Textual meaning: equality principle vs. narrow historical application').

omega_variable(
    amendment_process_substitutability,
    'To what extent do federal civil rights statutes (passed by simple majority, not supermajority) functionally achieve what amendments do, thereby bypassing the supermajority gate?',
    'Empirical tracking of constitutional amendments vs. federal statutes enforcing equality (Civil Rights Act 1964, Voting Rights Act 1965, Title IX, ADA); analysis of durability and scope — can statutes be repealed more easily than amendments? Are statute-based expansions more fragile?',
    'If statutes achieve amendment-equivalent scope and durability: supermajority requirement is functionally bypassed, and this reading''s constraint is theater (Piton). If statutes are fragile and repeatable: the supermajority gate retains structural force, and this reading''s constraint is genuine (Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_process_substitutability, empirical, 'Whether federal statutes functionally substitute for constitutional amendment').

omega_variable(
    reading_kernel_committer_ambiguity,
    'Which reading — progressive_textualist or expansive_universalist — is the kernel''s ''natural'' or default position, and which is the reading imposed by a specific political tradition?',
    'Comparative constitutional history: how do other nations'' equality clauses handle this tension? Are there constitutional regimes that formally encode the progressive textualist position (amendment-only expansion) vs. those that encode expansive universalism (judicial flexibility)? What is the meta-level constitutional choice each tradition has made?',
    'If progressive textualism is the default modern liberal position: restrictive_originalist is the aberration, and expansive_universalist is the natural evolution. If expansive universalism is the default: progressive textualism is the attempted stabilization. The reading''s legitimacy depends on which direction the meta-constitutional drift runs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_committer_ambiguity, conceptual, 'Default vs. revisionist status of progressive textualist reading within constitutional tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eqprog_tr_t0, equality_clause_scope__progressive_textualist, theater_ratio, 0, 0.25).
narrative_ontology:measurement(eqprog_tr_t50, equality_clause_scope__progressive_textualist, theater_ratio, 50, 0.52).
narrative_ontology:measurement(eqprog_tr_t100, equality_clause_scope__progressive_textualist, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(eqprog_be_t0, equality_clause_scope__progressive_textualist, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(eqprog_be_t50, equality_clause_scope__progressive_textualist, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(eqprog_be_t100, equality_clause_scope__progressive_textualist, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(eqprog_su_t0, equality_clause_scope__progressive_textualist, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(eqprog_su_t50, equality_clause_scope__progressive_textualist, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(eqprog_su_t100, equality_clause_scope__progressive_textualist, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, judicial_living_constitutionalism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, federal_civil_rights_statutory_framework).

% DUAL FORMULATION NOTE:
% The three readings of the equality_clause_scope kernel are structurally distinct constraints with different ε values, beneficiary/victim structures, and classifications. Progressive textualism (this file) ε≈0.38, tangled_rope. Restrictive originalism ε≈0.55, snare (minimal expansion possible). Expansive universalism ε≈0.15, rope (maximum flexibility, minimal structural constraint). Network links document the kernel kinship and show how each reading produces different structural predictions about courts, legislatures, and excluded groups.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
