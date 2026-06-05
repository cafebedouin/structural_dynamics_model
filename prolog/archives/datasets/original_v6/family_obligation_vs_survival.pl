% ============================================================================
% CONSTRAINT STORY: family_obligation_vs_survival
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_obligation_vs_survival, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_obligation_vs_survival
 *   human_readable: Family Obligation vs Survival: Identity-Lock in Kinship Systems
 *   domain: social/interpersonal/economic
 *
 * SUMMARY:
 *   Family obligation — the social and legal expectation that adult family
 *   members will financially and emotionally support dependents (children,
 *   elderly, disabled relatives, unemployed siblings) — creates a structural
 *   constraint that systematically extracts from obligated individuals while
 *   naturalizing the extraction as kinship love and moral duty. This
 *   constraint is especially severe in contexts with weak social safety nets,
 *   strong kinship-based identity systems, and gendered care work norms. The
 *   extractiveness has increased over the measurement interval as economic
 *   precarity has deepened, requiring obligated adults to work longer hours
 *   and forgo educational and career advancement. The theater ratio has also
 *   increased as state programs (child tax credits, elderly care subsidies)
 *   have become more prominent while remaining insufficient to replace actual
 *   care costs — the performative appearance of public support masks the
 *   persistence of private extraction. This constraint exhibits all six DR
 *   types, but the primary classification is Snare because the obligated
 *   adult (typically female, typically powerless relative to household
 *   dependents) bears extraction with minimal coordination benefit and no
 *   genuine exit option.
 *
 * KEY AGENTS:
 *   - Obligated Adult: Primary victim (powerless/trapped and identity-locked) — bears full cost of household survival, reduced opportunity, career sacrifice, psychological burden
 *   - Primary Dependent: Secondary beneficiary (moderate/mobile) — receives care, food, shelter; lacks awareness of extraction asymmetry
 *   - Secondary Family Adults: Co-obligators (moderate/constrained) — experience both coordination benefits and unequal burden distribution
 *   - State Social Support Apparatus: Institutional beneficiary (institutional/arbitrage) — subsidizes family care through insufficient programs, maintaining private extraction while claiming public support
 *   - Organized Care Worker Movement: Organized agents (organized/constrained) — building alternative care infrastructure and socialized care systems with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing socially contingent care arrangements as biological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_obligation_vs_survival, 0.68).
domain_priors:suppression_score(family_obligation_vs_survival, 0.72).
domain_priors:theater_ratio(family_obligation_vs_survival, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_obligation_vs_survival, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_obligation_vs_survival, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_obligation_vs_survival, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_obligation_vs_survival, snare).
narrative_ontology:human_readable(family_obligation_vs_survival, "Family Obligation vs Survival: Identity-Lock in Kinship Systems").
narrative_ontology:topic_domain(family_obligation_vs_survival, "social/interpersonal/economic").

domain_priors:requires_active_enforcement(family_obligation_vs_survival).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_obligation_vs_survival, primary_dependent_beneficiaries).
narrative_ontology:constraint_victim(family_obligation_vs_survival, obligated_adult_member).
narrative_ontology:constraint_victim(family_obligation_vs_survival, household_economic_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBLIGATED ADULT (SNARE) — Structurally trapped by economic dependency of family members, legal parental/filial obligations, and absence of institutional safety net. Exit would require abandoning dependents. Maximum extraction: the obligated agent bears full cost of household survival while forgoing personal development, education, career advancement, and mobility. No coordination benefit perceived — the constraint is pure extraction.
constraint_indexing:constraint_classification(family_obligation_vs_survival, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: OBLIGATED ADULT (SNARE, IDENTITY-LOCKED) — Structurally mobile (could walk away) but identity-fused with the role of caregiver/provider. The obligated agent's self-concept is constituted through the family dependency relationship. Exit would require becoming 'a different person' — the caregiver identity is the agent's primary source of meaning. The lock is cognitive rather than material. Perceived extraction is identical to trapped perspective but the binding mechanism is internal.
constraint_indexing:constraint_classification(family_obligation_vs_survival, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: PRIMARY DEPENDENT (ROPE) — Receives material and emotional care through the obligated adult's extraction. From the dependent's immediate perspective, the constraint appears as coordination: the family system coordinates care, food, shelter. The dependent has limited time horizon and mobility. The extraction is not perceived because the dependent lacks the cognitive or social capacity to recognize the asymmetry. This perspective is the 'innocent' beneficiary.
constraint_indexing:constraint_classification(family_obligation_vs_survival, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: SECONDARY FAMILY ADULTS (TANGLED ROPE) — May be co-obligated or partially obligated. Experience both coordination benefit (shared household resources, labor division, emotional support) and extraction (unequal labor distribution, opportunity costs, pressure to contribute to primary dependent's care). Generational time horizon captures the possibility of shifting burdens or negotiating exit as life circumstances change. Constrained exit reflects that leaving imposes costs on remaining obligators without fully breaking kinship bonds.
constraint_indexing:constraint_classification(family_obligation_vs_survival, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: STATE SOCIAL SUPPORT APPARATUS (PITON) — The constraint is maintained partly through the state's arbitrage: subsidizing family care is cheaper than providing public care infrastructure. The state has formal programs (tax credits, child allowances, elderly care subsidies) that are largely performative — insufficient to replace household extraction but sufficient to justify calling it 'family responsibility.' Theater ratio reflects the gap between stated policy support and actual resource availability. The state's own process is degraded (maintenance through inertia and political ease rather than functional effectiveness).
constraint_indexing:constraint_classification(family_obligation_vs_survival, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZED CARE WORKER MOVEMENT (SCAFFOLD) — Labor organizing, care worker collectives, and public care infrastructure campaigns see family obligation as a temporary extraction mechanism that will be replaced by socialized care systems. The constraint has a sunset: universal childcare, public long-term care, robust social safety nets reduce obligatory extraction by shifting care provisioning from individuals to institutions. Sunset timeline: 15-30 years in developed nations; longer in low-income countries. Organizations have exit paths (unionization, policy advocacy) and perceive the constraint as solvable.
constraint_indexing:constraint_classification(family_obligation_vs_survival, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/biological perspective, kinship obligation is rooted in human reproduction and parental investment requirements — some form of kin-based care is inevitable. The constraint appears as a natural law: humans have dependent periods; dependents require care; someone must provide it; kinship structures encode this necessity. However, the structural data contradicts the mountain classification. The high extractiveness, suppression, and theater values reveal that what is naturalized as 'biological necessity' is actually a contingent institutional arrangement (absence of public care systems, exclusion of care work from economic compensation, identity fusion in gender roles).
constraint_indexing:constraint_classification(family_obligation_vs_survival, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_obligation_vs_survival_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_obligation_vs_survival, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_obligation_vs_survival, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_obligation_vs_survival, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_obligation_vs_survival, TR),
    TR >= 0.70.

:- end_tests(family_obligation_vs_survival_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The obligated adult bears full household survival costs while forgoing education, career development, and mobility. The extraction is not symmetrical — dependents do not reciprocate proportionally. Base extractiveness started at 0.45 (partial coordination, some benefit from household interdependence) but increased to 0.68 as economic precarity and dependent needs intensified. Suppression (0.72): High. Barriers to exit include legal parental obligations, economic dependency of dependents, absence of institutional care alternatives, social stigma and guilt, internalized identity-lock. The obligated adult cannot exit without abandoning dependents to hardship. Suppression is both structural (material barriers) and internalized (identity fusion, moral framing). Theater ratio (0.58): Moderate-high and increasing. State programs create the appearance of public care support while remaining insufficient. The obligated adult's care work is invisible (unpaid labor, domestic work) and reframed as love/duty rather than economic activity. The theater increased as formal programs were introduced without commensurate funding — the performance of support now covers what is actually continued private extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival disagreement: the obligated adult and dependent perceive opposite classifications (snare vs rope) from the same structural phenomenon. The gap reveals that the constraint's function depends entirely on position. From the obligated adult's view, extraction is severe and inescapable. From the dependent's view (or the state's view), the constraint is coordination or beneficial. The gap also reveals the identity-lock mechanism: the obligated adult cannot see their own objectivity (that they are being extracted from) because their identity is fused with the extraction. Identity-locked perspective 2 produces snare (same classification as trapped perspective 1) but for different binding mechanisms. The analytical observer's mountain classification is a false summit — what is naturalized as biological kinship necessity is actually a contingent institutional arrangement (absence of public care systems, economic structures that exclude care work, gender norms).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for this constraint centers on victim/beneficiary declarations and exit options. The obligated adult is a victim (bears costs) with trapped or identity-locked exit → d = 0.90-0.95 (maximum victimhood). The dependent is a beneficiary (receives care) with mobile exit (constrained by age/capability) → d = 0.20 (low, innocent party). Secondary family adults are both victims and partial beneficiaries, constrained exit → d = 0.55-0.60 (moderate, experience both extraction and benefit). The state is a beneficiary (saves on public care costs) with arbitrage exit (could shift to public care) → d = 0.10-0.15 (low, institutional beneficiary). For the identity-locked perspective, d is not reduced by the fact that the agent could technically exit — identity-lock prevents the agent from exercising exit capacity, so d remains high (0.88-0.92). This shows that directionality captures structural victim status, not agent choice.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint is a pure Snare from the obligated adult's perspective (the primary victim). Mandatrophy asks: is this extraction disguised as coordination? The answer is YES. The constraint is presented in family/kinship framing (coordination language: love, duty, interdependence) while delivering pure extraction (obligated adult's opportunity, autonomy, resources stolen for benefit of dependents and, secondarily, for the state's savings on care infrastructure). The tangled_rope and rope perspectives are perspectival illusions — they perceive coordination because they are beneficiaries or have low extraction costs. From the obligated adult's actual structural position, coordination is minimal and asymmetric. The constraint is solvable through public care investment (scaffold perspective), but in contexts where such investment is absent or insufficient, it remains a snare. The mandatrophy is resolved by recognizing that family obligation is a synthetic constraint — it exists only because institutional alternatives (public childcare, public elderly care, robust social safety net) do not exist. It is not a natural law but a policy failure naturalized through kinship rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_trap,
    'Is the obligated adult genuinely structurally trapped by economic necessity, or is the trap primarily cognitive (identity-locked)?',
    'Post-exit analysis: if obligated adults who leave their families show persistent psychological attachment and guilt-driven return to care roles, reclassify as internalized constraint. If departure leads to clean break and psychological relief, reclassify as structural entrapment.',
    'If identity-locked: the constraint''s effective suppression is higher than structural measures suggest because the agent carries the lock after exit. Therapeutic intervention becomes relevant alongside economic intervention. If structurally trapped: economic policy (universal care, wage support) is sufficient to break the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Whether suppression is structural entrapment or internalized identity-lock').

omega_variable(
    dependent_legitimacy_threshold,
    'At what age or capability level does a dependent cease to be legitimate and become a beneficiary of extraction rather than an innocent party?',
    'Ethnographic analysis of caregiver role transitions; study of when obligated adults shift blame from systemic factors to dependent family members'' behavior choices.',
    'If threshold is low (childhood only): most adult dependents (unemployed relatives, elderly with assets, disabled adults with earning capacity) are considered extractive beneficiaries, not innocent parties. If threshold is high: system maintains fiction of innocence even when dependents could contribute more.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dependent_legitimacy_threshold, preference, 'Legitimate dependency vs extractive beneficiary threshold').

omega_variable(
    public_care_substitutability,
    'Can public care systems actually substitute for family-provided care without loss of relational continuity and emotional attachment quality?',
    'Longitudinal outcomes study: countries with high public care investment (Scandinavia, OECD average) vs. low (low-income countries, US); measurement of elder and child well-being, obligated adult mental health, family relational stability.',
    'If substitutable: scaffold sunset is real and the constraint is solvable through policy. If not: some residual family obligation is inevitable, and the constraint never fully disappears — reclassify as persistent tangled_rope rather than solvable snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_care_substitutability, empirical, 'Whether public care systems can substitute family care provision').

omega_variable(
    gendered_obligation_asymmetry,
    'Is family obligation equally distributed across genders, or does gender systematically determine who becomes the obligated adult?',
    'Statistical analysis of care responsibilities by gender, household-by-household data on labor division, measurement of career impact differentiation by gender, intergenerational transmission of obligation.',
    'If gender-asymmetric: the constraint is layered with a distinct gender extraction mechanism, and the snare classification understates the extraction''s asymmetry. Obligated female adults experience higher suppression and lower exit optionality than obligated male adults. Classification should separate gender-explicit and gender-neutral versions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gendered_obligation_asymmetry, empirical, 'Gender asymmetry in obligation distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_obligation_vs_survival, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fobs_tr_t0, family_obligation_vs_survival, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fobs_tr_t5, family_obligation_vs_survival, theater_ratio, 5, 0.48).
narrative_ontology:measurement(fobs_tr_t10, family_obligation_vs_survival, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(fobs_be_t0, family_obligation_vs_survival, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fobs_be_t5, family_obligation_vs_survival, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(fobs_be_t10, family_obligation_vs_survival, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_obligation_vs_survival, attachment_coordination).
narrative_ontology:boltzmann_floor_override(family_obligation_vs_survival, 0.12).
narrative_ontology:affects_constraint(family_obligation_vs_survival, gendered_care_work_exclusion).
narrative_ontology:affects_constraint(family_obligation_vs_survival, inadequate_social_safety_net).
narrative_ontology:affects_constraint(family_obligation_vs_survival, identity_fusion_in_kinship).

% DUAL FORMULATION NOTE:
% Family obligation decomposes into three structurally distinct constraints with different ε values. This story captures the primary extraction mechanism (obligated adult bearing care costs). Gendered_care_work_exclusion (ε=0.62) focuses on the labor extraction and wage penalty aspects. Inadequate_social_safety_net (ε=0.75, policy-level) focuses on the institutional mechanism that enforces private extraction. Identity_fusion_in_kinship (ε=0.55) focuses on the cognitive lock mechanism. All three are upstream/downstream of family_obligation_vs_survival and linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_obligation_vs_survival, powerful, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
